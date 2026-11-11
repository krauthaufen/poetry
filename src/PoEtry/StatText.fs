namespace PoEtry

/// Turns item / passive text back into game stat ids, using the game's own
/// stat description templates (the files the game uses to render those lines).
///
/// Every number in a line and every placeholder in a template becomes "#". The
/// resulting skeleton is an exact dictionary key. Values are then read back in
/// order and run through the inverse of the template's display handler.
module StatText =
    type private Slot =
        /// Placeholder showing stat number `stat` (0-based).
        | Shows of stat: int
        /// A literal number in the template, must match exactly.
        | Literal of float

    type private Variant =
        { Stats: string list
          Slots: Slot list
          /// Per stat: inclusive (min, max) or None for unbounded, from "limit".
          Limits: (float option * float option) list
          /// Per stat: display handler names.
          Handlers: Map<int, string list> }

    type Index = private { ByKey: Map<string, Variant list> }

    type private Token =
        | Text of string
        | Number of float

    let private isDigit c = c >= '0' && c <= '9'

    /// Split a line into text and numbers. A sign directly before a number is part
    /// of it. "(5-10)" ranges read as their average.
    let private tokenize (line: string) =
        let out = ResizeArray<Token>()
        let sb = System.Text.StringBuilder()
        let flush () =
            if sb.Length > 0 then
                out.Add(Text(sb.ToString()))
                sb.Clear() |> ignore
        let n = line.Length
        let mutable i = 0
        let mutable negateNext = false
        let readNumber () =
            let start = i
            if line.[i] = '+' || line.[i] = '-' then i <- i + 1
            while i < n && (isDigit line.[i] || (line.[i] = '.' && i + 1 < n && isDigit line.[i + 1])) do
                i <- i + 1
            float (line.Substring(start, i - start))
        while i < n do
            let c = line.[i]
            let signed = (c = '+' || c = '-') && i + 1 < n && isDigit line.[i + 1] && (i = 0 || not (isDigit line.[i - 1]))
            let signedRange = (c = '+' || c = '-') && i + 2 < n && line.[i + 1] = '(' && isDigit line.[i + 2]
            if signedRange then
                // "+(5-10)": the sign belongs to the range that follows
                if c = '-' then negateNext <- true
                i <- i + 1
            elif c = '(' && i + 1 < n && isDigit line.[i + 1] then
                // "(5-10)" roll range
                let save = i
                i <- i + 1
                let lo = readNumber ()
                if i < n && line.[i] = '-' && i + 1 < n && isDigit line.[i + 1] then
                    i <- i + 1
                    let hi = readNumber ()
                    if i < n && line.[i] = ')' then
                        i <- i + 1
                        flush ()
                        out.Add(Number((if negateNext then -1.0 else 1.0) * (lo + hi) / 2.0))
                        negateNext <- false
                    else
                        i <- save
                        sb.Append(c) |> ignore
                        i <- i + 1
                else
                    i <- save
                    sb.Append(c) |> ignore
                    i <- i + 1
            elif isDigit c || signed then
                flush ()
                out.Add(Number(readNumber ()))
            else
                sb.Append(System.Char.ToLowerInvariant c) |> ignore
                i <- i + 1
        flush ()
        List.ofSeq out

    let private skeleton tokens =
        tokens
        |> List.map (function
            | Text t -> t
            | Number _ -> "#")
        |> String.concat ""

    /// Parse a template like "{0:+d}% to {1} Fire Resistance" into skeleton + slots.
    let private template (text: string) =
        let sb = System.Text.StringBuilder()
        let slots = ResizeArray<Slot>()
        let mutable next = 0
        let mutable i = 0
        let n = text.Length
        while i < n do
            let c = text.[i]
            if c = '{' then
                let close = text.IndexOf('}', i)
                let inner = text.Substring(i + 1, close - i - 1)
                let idxText = (inner.Split(':')).[0]
                let idx = if idxText = "" then next else int idxText
                next <- idx + 1
                sb.Append('#') |> ignore
                slots.Add(Shows idx)
                i <- close + 1
            elif (c = '+' || c = '-') && i + 1 < n && text.[i + 1] = '{' then
                // A literal sign right before a placeholder is part of the number.
                i <- i + 1
            elif isDigit c || ((c = '+' || c = '-') && i + 1 < n && isDigit text.[i + 1]) then
                // a literal number, its sign included ("down to a minimum of -50%")
                let negative = c = '-'
                if c = '+' || c = '-' then i <- i + 1
                let start = i
                while i < n && (isDigit text.[i] || (text.[i] = '.' && i + 1 < n && isDigit text.[i + 1])) do
                    i <- i + 1
                sb.Append('#') |> ignore
                let v = float (text.Substring(start, i - start))
                slots.Add(Literal(if negative then -v else v))
            else
                sb.Append(System.Char.ToLowerInvariant c) |> ignore
                i <- i + 1
        sb.ToString(), List.ofSeq slots

    /// Undo a display handler: displayed value -> stat value.
    let private undo handler (v: float) =
        match handler with
        | "negate" -> -v
        | "negate_and_double" -> -v / 2.0
        | "double" -> v / 2.0
        | "times_twenty" -> v / 20.0
        | "divide_by_two_0dp" -> v * 2.0
        | "divide_by_three" -> v * 3.0
        | "divide_by_four" -> v * 4.0
        | "divide_by_five" -> v * 5.0
        | "divide_by_fifteen_0dp" -> v * 15.0
        | "divide_by_fifty" -> v * 50.0
        | "divide_by_twenty_then_double_0dp" -> v * 10.0
        | "divide_by_one_hundred_and_negate" -> -v * 100.0
        | "plus_two_hundred" -> v - 200.0
        | "add_one" -> v - 1.0
        | "subtract_one" -> v + 1.0
        | "deciseconds_to_seconds" -> v * 10.0
        | h when h.StartsWith "per_minute_to_per_second" -> v * 60.0
        | h when h.StartsWith "milliseconds_to_seconds" -> v * 1000.0
        | h when h.StartsWith "divide_by_one_hundred" -> v * 100.0
        | h when h.StartsWith "divide_by_ten" -> v * 10.0
        | _ -> v

    let private limitPart j =
        match j with
        | JNum v -> Some v
        | _ -> None

    let private variants (entry: Json) =
        let stats = Json.list (Json.field "stats" entry) |> List.choose Json.str
        Json.list (Json.field "1" entry)
        |> List.choose (fun v ->
            match Json.strField "text" v with
            | None -> None
            | Some text ->
                let key, slots = template text
                let limits =
                    Json.list (Json.field "limit" v)
                    |> List.map (fun l ->
                        match Json.list (Some l) with
                        | [ lo; hi ] -> limitPart lo, limitPart hi
                        | _ -> None, None)
                let handlers =
                    Json.items (Some v)
                    |> List.choose (fun (_, h) ->
                        match Json.strField "k" h, Json.numField "v" h with
                        | Some k, Some idx -> Some(int idx - 1, k)
                        | _ -> None)
                    |> List.groupBy fst
                    |> List.map (fun (i, hs) -> i, List.map snd hs)
                    |> Map.ofList
                Some(
                    key,
                    { Stats = stats
                      Slots = slots
                      Limits = limits
                      Handlers = handlers }
                ))

    /// Build an index from exported stat description files (statdesc_*.json).
    let build (descriptionFiles: string list) : Index =
        let all =
            descriptionFiles
            |> List.collect (fun json ->
                match Json.parse json with
                | JObj m ->
                    m
                    |> Map.toList
                    |> List.filter (fun (k, _) -> k.Length > 0 && isDigit k.[0])
                    |> List.sortBy (fun (k, _) -> int k)
                    |> List.collect (snd >> variants)
                | _ -> [])
        { ByKey =
            all
            |> List.groupBy fst
            |> List.map (fun (k, vs) -> k, List.map snd vs)
            |> Map.ofList }

    let private within (lo, hi) v =
        Option.forall (fun l -> v >= l) lo && Option.forall (fun h -> v <= h) hi

    let private read (variant: Variant) (numbers: float list) =
        if numbers.Length <> variant.Slots.Length then
            None
        else
            let pairs = List.zip variant.Slots numbers
            let literalsOk =
                pairs |> List.forall (function
                    | Literal l, v -> l = v
                    | _ -> true)
            let shown =
                pairs
                |> List.choose (function
                    | Shows s, v ->
                        let handlers = Map.tryFind s variant.Handlers |> Option.defaultValue []
                        Some(s, List.fold (fun x h -> undo h x) v handlers)
                    | _ -> None)
                |> Map.ofList
            let values =
                variant.Stats
                |> List.mapi (fun i stat ->
                    let limit = List.tryItem i variant.Limits |> Option.defaultValue (None, None)
                    match Map.tryFind i shown with
                    | Some v -> if within limit v then Some(Some(stat, v)) else None
                    | None ->
                        // Not displayed: only known if the limit pins it to one value.
                        match limit with
                        | Some lo, Some hi when lo = hi -> Some(Some(stat, lo))
                        // flag-like lines with no number ("Gem Quality grants ... an additional effect"):
                        // the stat is at least its lower limit
                        | lo, _ when variant.Slots.IsEmpty -> Some(Some(stat, lo |> Option.defaultValue 1.0))
                        // no placeholder, one literal number: that number is the value
                        // ("down to a minimum of -50%")
                        // several fixed numbers (keystones: "30% less ... 30% more ..."): the stat
                        // is set; its value is the first number
                        | _ when variant.Slots |> List.forall (function Literal _ -> true | _ -> false) ->
                            match variant.Slots with
                            | Literal v :: _ -> Some(Some(stat, v))
                            | _ -> Some None
                        | _ -> Some None)
            if literalsOk && List.forall Option.isSome values then
                Some(values |> List.choose id |> List.choose id)
            else
                None

    /// Where a line comes from. The same text can be a local item stat ("15% increased
    /// Evasion Rating" on a helmet) or a global one (on the tree or a jewel).
    type Context =
        | ItemLocal
        | Global

    let private isLocal (stats: (string * float) list) = stats |> List.exists (fun (s, _) -> s.StartsWith "local_")

    /// Parse one line, preferring local or global readings by context.
    /// A line may contain "\n" for descriptions that span several lines.
    let parseIn (context: Context) (index: Index) (line: string) : (string * float) list option =
        let tokens = tokenize (line.Trim())
        let numbers =
            tokens |> List.choose (function
                | Number v -> Some v
                | _ -> None)
        match Map.tryFind (skeleton tokens) index.ByKey with
        | None -> None
        | Some variants ->
            let readings = variants |> List.choose (fun v -> read v numbers)
            let preferred =
                match context with
                | ItemLocal -> readings |> List.tryFind isLocal
                | Global -> readings |> List.tryFind (isLocal >> not)
            preferred |> Option.orElse (List.tryHead readings)

    /// Lines with a skill name in them ("+1 to Level of all Comet Skills"): the stat
    /// description has a name placeholder our number templates can't hold.
    let private named (line: string) =
        let l = line.Trim()
        let prefix, suffix = "to Level of all ", " Skills"
        let i = l.IndexOf prefix
        // "+1 to Level of all Nova Skill Gems": skills of a type
        let gemSuffix = " Skill Gems"
        let between (sfx: string) = l.Length - i - prefix.Length - sfx.Length
        let value () = match System.Double.TryParse(l.Substring(0, i).Trim()) with | true, v -> Some v | _ -> None
        if not ((l.StartsWith "+" || l.StartsWith "-") && i > 0) then None
        elif l.EndsWith gemSuffix && between gemSuffix > 0 then
            value () |> Option.map (fun v -> [ "typed_skill_level:" + l.Substring(i + prefix.Length, between gemSuffix).Replace(" ", ""), v ])
        // a name must sit between them ("+1 to Level of all Skills" has none)
        elif l.EndsWith suffix && between suffix > 0 then
            value () |> Option.map (fun v -> [ "specific_skill_level:" + l.Substring(i + prefix.Length, between suffix), v ])
        else None

    let private parseInWithNamed context index line =
        parseIn context index line |> Option.orElse (named line)

    /// Parse one line of item / passive text into (stat id, value) pairs (global context).
    let parse (index: Index) (line: string) = parseInWithNamed Global index line

    /// Parse a block of lines. Lines that only make sense together (the game wraps
    /// some descriptions) are joined, up to three at a time.
    /// Returns parsed stats and the lines nobody understood.
    let parseLinesIn (context: Context) (index: Index) (lines: string list) =
        let rec go acc unknown rest =
            match rest with
            | [] -> List.rev acc, List.rev unknown
            | l :: tail ->
                let joined k = String.concat "\n" (List.truncate k rest)
                let attempt =
                    [ 1; 2; 3 ]
                    |> List.filter (fun k -> k <= List.length rest)
                    |> List.tryPick (fun k -> parseInWithNamed context index (joined k) |> Option.map (fun s -> k, s))
                match attempt with
                | Some (k, stats) -> go (List.rev stats @ acc) unknown (List.skip k rest)
                | None -> go acc (l :: unknown) tail
        go [] [] lines

    let parseLines (index: Index) (lines: string list) = parseLinesIn Global index lines
