/// PoEtry's front end: a pure FableCube application. The simulator runs in a worker behind
/// urls (`__sim?op=...`, answered by the bridge in index.html), so asking it is a `Dom.Fetch`
/// and its answers are messages. `?build=<url>` names the character to load.
module App

open Pure

type Row = { Label : string; Value : string }
type Share = { Name : string; Percent : float; Text : string }

type Msg =
    | Located of string
    | Sheet of Fetched
    | Fought of Fetched
    | Scenario of string

type Model =
    { /// The character's url ("" until the page knows its address, or when none is given).
      Build : string
      Scenario : string
      Status : string
      Title : string
      Pools : Row list
      Defences : Row list
      Resistances : Row list
      Minions : Row list
      NotUnderstood : int
      /// The fight: what is pressed, its headline, and who dealt the damage.
      Fighting : bool
      Gem : string
      Dps : Row list
      Shares : Share list }

let init : Model =
    { Build = ""; Scenario = "boss"; Status = "locating"; Title = ""; Pools = []; Defences = []; Resistances = []; Minions = []
      NotUnderstood = 0; Fighting = false; Gem = ""; Dps = []; Shares = [] }

let private sheetUrl (m : Model) = "__sim?op=character&build=" + m.Build
let private fightUrl (m : Model) = "__sim?op=fight&runs=20&time=30&scenario=" + m.Scenario + "&build=" + m.Build

/// 1234567 as "1.23M", 45678 as "45.7k".
let private short (v : float) =
    if v >= 1e9 then sprintf "%.2fB" (v / 1e9)
    elif v >= 1e6 then sprintf "%.2fM" (v / 1e6)
    elif v >= 1e4 then sprintf "%.1fk" (v / 1e3)
    else sprintf "%.0f" v

let private row (label : string) (v : float) = { Label = label; Value = sprintf "%.0f" v }

let update (msg : Msg) (m : Model) : Model =
    match msg with
    | Located href ->
        match Native.queryParam "build" href with
        | Some b -> { m with Build = b; Status = "reading the character" }
        | None -> { m with Status = "no character: add ?build=<url of a character or a PoB build> to the address" }
    | Scenario s -> { m with Scenario = s; Fighting = true; Dps = []; Shares = [] }
    | Sheet r when r.Url <> sheetUrl m -> m
    | Sheet r ->
        match r.Status, Native.parseJson r.Text with
        | 200, Ok j when Json.string (Json.field "error" j) <> "" -> { m with Status = "the character could not be read: " + Json.string (Json.field "error" j) }
        | 200, Ok j ->
            let n (path : string list) = path |> List.fold (fun x k -> Json.field k x) j |> Json.number
            let named key = Json.field key j |> Json.list |> List.map (fun x -> { Label = Json.string (Json.field "name" x); Value = sprintf "%.0f" (Json.number (Json.field (if key = "minions" then "count" else "value") x)) })
            let missing = [ "unmapped"; "unsimulated"; "unknown"; "minions" ] |> List.sumBy (fun k -> Json.field "notUnderstood" j |> Json.field k |> Json.list |> List.length)
            { m with
                Status = ""
                Title = sprintf "Level %.0f %s" (n [ "level" ]) (Json.string (Json.field "class" j))
                Pools = [ row "Life" (n [ "pools"; "life" ]); row "Mana" (n [ "pools"; "mana" ]); row "Energy Shield" (n [ "pools"; "energyShield" ]); row "Spirit" (n [ "pools"; "spirit" ]) ]
                Defences = [ row "Armour" (n [ "defences"; "armour" ]); row "Evasion" (n [ "defences"; "evasion" ]); row "Deflection" (n [ "defences"; "deflection" ]); row "Block" (n [ "defences"; "block" ]) ]
                Resistances = named "resistances"
                Minions = named "minions"
                NotUnderstood = missing
                Fighting = true }
        | s, _ -> { m with Status = "the simulator did not answer (status " + string s + ")" }
    | Fought r when r.Url <> fightUrl m -> m
    | Fought r ->
        match r.Status, Native.parseJson r.Text with
        | 200, Ok j when Json.string (Json.field "error" j) <> "" -> { m with Fighting = false; Status = "the fight failed: " + Json.string (Json.field "error" j) }
        | 200, Ok j ->
            let dps k = Json.field "dps" j |> Json.field k |> Json.number
            { m with
                Fighting = false
                Gem = Json.string (Json.field "gem" j)
                Dps = [ { Label = "DPS"; Value = short (dps "mean") }; { Label = "one in ten below"; Value = short (dps "p10") }; { Label = "median"; Value = short (dps "p50") }; { Label = "one in ten above"; Value = short (dps "p90") } ]
                Shares =
                    Json.field "damageShare" j |> Json.list
                    |> List.map (fun x ->
                        let v = Json.number (Json.field "value" x) * 100.0
                        { Name = Json.string (Json.field "name" x); Percent = v; Text = sprintf "%.1f%%" v })
                    |> List.filter (fun s -> s.Percent >= 0.5) }
        | s, _ -> { m with Fighting = false; Status = "the fight did not come back (status " + string s + ")" }

let private panel (title : string) (rows : Row list) : Node<Msg> =
    section {
        Dom.Class "panel"
        h3 { title }
        for r in rows do
            div {
                // (the label names the line's colour: fire, cold, lightning, chaos, life, mana ...)
                Dom.Class ("line " + r.Label.ToLower().Replace(" ", "-"))
                span { Dom.Class "what"; r.Label }
                span { Dom.Class "amount"; r.Value }
            }
    }

let private scenario (m : Model) (key : string) (name : string) : Node<Msg> =
    button {
        Dom.Class (if m.Scenario = key then "tab active" else "tab")
        Dom.OnClick (fun _ -> Scenario key)
        name
    }

let view (m : Model) : Node<Msg> =
    body {
        Dom.OnLocation Located
        if m.Build <> "" then span { Dom.Fetch (sheetUrl m, Sheet) }
        if m.Title <> "" then span { Dom.Fetch (fightUrl m, Fought) }
        header {
            h1 { "PoEtry" }
            div { Dom.Class "title"; Dom.Attr ("data-check", "title"); m.Title }
            div { Dom.Class "status"; Dom.Attr ("data-check", "status"); m.Status }
        }
        div {
            Dom.Class "main"
            div {
                Dom.Class "column"
                panel "Pools" m.Pools
                panel "Defences" m.Defences
                panel "Resistances" m.Resistances
                if not m.Minions.IsEmpty then panel "Minions" m.Minions
                if m.NotUnderstood > 0 then
                    div { Dom.Class "note"; sprintf "%d things on this character are not understood yet" m.NotUnderstood }
            }
            div {
                Dom.Class "column wide"
                nav {
                    scenario m "boss" "Boss"
                    scenario m "pinnacle" "Pinnacle"
                    scenario m "pack" "Pack"
                    scenario m "surrounded" "Surrounded"
                }
                if m.Fighting then div { Dom.Class "status"; "fighting ..." }
                if m.Gem <> "" then div { Dom.Class "pressing"; Dom.Attr ("data-check", "gem"); "pressing " + m.Gem }
                panel "Damage" m.Dps
                section {
                    Dom.Class "panel"
                    h3 { "Who deals it" }
                    for s in m.Shares do
                        div {
                            Dom.Class "share"
                            div { Dom.Class "bar"; Dom.Style [ Css.Width (sprintf "%.1f%%" s.Percent) ] }
                            span { Dom.Class "name"; s.Name }
                            span { Dom.Class "pct"; s.Text }
                        }
                }
            }
        }
    }
