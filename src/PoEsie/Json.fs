namespace PoEsie

/// Minimal JSON reader. Hand-written so it behaves the same on .NET and under Fable,
/// with no dependencies.
type Json =
    | JNull
    | JBool of bool
    | JNum of float
    | JStr of string
    | JArr of Json list
    | JObj of Map<string, Json>

module Json =
    let parse (text: string) : Json =
        let mutable i = 0
        let n = text.Length
        let fail msg = failwithf "JSON: %s at %d" msg i

        let skipWs () =
            while i < n && (let c = text.[i] in c = ' ' || c = '\n' || c = '\r' || c = '\t') do
                i <- i + 1

        let parseString () =
            i <- i + 1 // opening quote
            let sb = System.Text.StringBuilder()
            let mutable fin = false
            while not fin do
                if i >= n then fail "unterminated string"
                let c = text.[i]
                if c = '"' then
                    fin <- true
                    i <- i + 1
                elif c = '\\' then
                    let e = text.[i + 1]
                    i <- i + 2
                    match e with
                    | 'n' -> sb.Append('\n') |> ignore
                    | 't' -> sb.Append('\t') |> ignore
                    | 'r' -> sb.Append('\r') |> ignore
                    | 'b' -> sb.Append('\b') |> ignore
                    | 'f' -> sb.Append('\f') |> ignore
                    | 'u' ->
                        let code = System.Convert.ToInt32(text.Substring(i, 4), 16)
                        sb.Append(char code) |> ignore
                        i <- i + 4
                    | c -> sb.Append(c) |> ignore
                else
                    sb.Append(c) |> ignore
                    i <- i + 1
            sb.ToString()

        let parseNumber () =
            let start = i
            while i < n && (let c = text.[i] in (c >= '0' && c <= '9') || c = '-' || c = '+' || c = '.' || c = 'e' || c = 'E') do
                i <- i + 1
            float (text.Substring(start, i - start))

        let rec value () =
            skipWs ()
            if i >= n then fail "unexpected end"
            match text.[i] with
            | '{' ->
                i <- i + 1
                let mutable fields = Map.empty
                skipWs ()
                if text.[i] = '}' then
                    i <- i + 1
                else
                    let mutable more = true
                    while more do
                        skipWs ()
                        let key = parseString ()
                        skipWs ()
                        if text.[i] <> ':' then fail "expected ':'"
                        i <- i + 1
                        fields <- Map.add key (value ()) fields
                        skipWs ()
                        if text.[i] = ',' then i <- i + 1
                        elif text.[i] = '}' then
                            i <- i + 1
                            more <- false
                        else fail "expected ',' or '}'"
                JObj fields
            | '[' ->
                i <- i + 1
                let items = ResizeArray()
                skipWs ()
                if text.[i] = ']' then
                    i <- i + 1
                else
                    let mutable more = true
                    while more do
                        items.Add(value ())
                        skipWs ()
                        if text.[i] = ',' then i <- i + 1
                        elif text.[i] = ']' then
                            i <- i + 1
                            more <- false
                        else fail "expected ',' or ']'"
                JArr(List.ofSeq items)
            | '"' -> JStr(parseString ())
            | 't' ->
                i <- i + 4
                JBool true
            | 'f' ->
                i <- i + 5
                JBool false
            | 'n' ->
                i <- i + 4
                JNull
            | _ -> JNum(parseNumber ())

        value ()

    // ---- accessors, tolerant of missing fields ----

    let field key j =
        match j with
        | JObj m -> Map.tryFind key m
        | _ -> None

    let num j =
        match j with
        | JNum v -> Some v
        | JBool b -> Some(if b then 1.0 else 0.0)
        | _ -> None

    let str j =
        match j with
        | JStr s -> Some s
        | _ -> None

    let numField key j = field key j |> Option.bind num
    let strField key j = field key j |> Option.bind str

    /// Lua-style sequence: a JSON array, or an object with keys "1", "2", ...
    /// Returns (1-based index, value) pairs.
    let items j =
        match j with
        | Some (JArr xs) -> xs |> List.mapi (fun i x -> i + 1, x)
        | Some (JObj m) ->
            m
            |> Map.toList
            |> List.choose (fun (k, v) ->
                match System.Int32.TryParse k with
                | true, idx -> Some(idx, v)
                | _ -> None)
            |> List.sortBy fst
        | _ -> []

    let list j = items j |> List.map snd

    /// Keys of a set-like object ({ "Spell": true, ... }).
    let keys j =
        match j with
        | Some (JObj m) -> m |> Map.toList |> List.map fst
        | _ -> []
