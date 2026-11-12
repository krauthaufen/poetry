/// PoEtry's front end: a pure FableCube application. The simulator runs in a worker behind
/// urls (`__sim?op=...`, answered by the bridge in index.html), so asking it is a `Dom.Fetch`
/// and its answers are messages. `?build=<url>` names the character to load.
module App

open Aardvark.Portable
open FableCube
open Pure

type Row = { Label : string; Value : string }
type Share = { Name : string; Percent : float; Text : string }

/// A passive as the tree draws it: where, how big, and its art in the sprite sheets (a
/// sheet's url and the sprite's rectangle there as u, v, width, height).
type Passive =
    { Id : int
      X : float
      Y : float
      Size : float
      IconSize : float
      Kind : string
      /// Not shown until it is allocated (a Sinister Jewel Socket).
      Hidden : bool
      Name : string
      /// What it grants, and for one with a choice (an attribute) what can be chosen.
      Stats : string list
      Options : string list
      Ascendancy : string
      Icon : string
      IconRect : V4d
      Frame : string
      FrameOn : V4d
      FrameOff : V4d }

/// A jewel in a socket of the tree, and its picture in the jewel sheet.
type Jewel = { Called : string; Base : string; Rarity : string; Mods : string list; Sprite : string; SpriteRect : V4d }

/// A connection: its two passives and its ribbon (triangles); an ascendancy's own are laid
/// out for the middle of the tree.
type Link = { A : int; B : int; Of : string; Ribbon : V3f list }

/// An ascendancy: where its part of the tree is laid out (the middle of its background), and
/// its class art in the small background sheet.
type Ascendancy = { Called : string; Middle : V2d; Art : string }

type Msg =
    | Located of string
    | Sheet of Fetched
    | Fought of Fetched
    | Scenario of string
    | Tab of string
    | TreeData of Fetched
    | Toggle of int
    /// A click on the tree, at this place of it.
    | Click of V3d
    /// From the tooltip: allocate a passive (as this choice, "" for none), give it back, close.
    | Allocate of int * string
    | Deallocate of int
    | Close
    /// The place of the tree under a pointer that came down, with where the camera was when
    /// it was worked out.
    /// The page's size, as the bridge in index.html answers `__window`.
    | Sized of Fetched
    | PointerDown of Event
    | PointerMove of Event
    | PointerUp of Event
    | Zoom of V3d * V3d * float

type Model =
    { /// The character's url ("" until the page knows its address, or when none is given).
      Build : string
      Scenario : string
      Status : string
      Title : string
      Class : string
      Ascendancy : string
      Tab : string
      Pools : Row list
      Defences : Row list
      Resistances : Row list
      Minions : Row list
      NotUnderstood : int
      /// The fight: what is pressed, its headline, and who dealt the damage.
      Fighting : bool
      Gem : string
      Dps : Row list
      Shares : Share list
      /// The passive tree: its passives and connections, who neighbours whom, the ribbons of
      /// all connections as one mesh, what is allocated (and whether that was changed here).
      /// (Arrays: read by the update alone, and as arrays they are one value each to the view,
      /// where a list in the model costs load time and memory element by element.)
      Passives : Passive[]
      Ascendancies : Ascendancy list
      /// Those that can be drawn: the tree's, every ascendancy's (laid out for the middle of
      /// the tree; only the character's own is shown) and the hidden sockets.
      Shown : Passive list
      /// What is drawn as one mesh each (the view has a single loop, over the passives: with
      /// a second one FableCube rebuilds the whole render control on every change, see its
      /// dogfood/poetry.md): the character's ascendancy's connections, the connections
      /// between allocated passives, and the ascendancy's class art.
      AscendancyWeb : Buffer<V3f>
      Path : Buffer<V3f>
      /// Texture coordinates for those three and for `Web` (all zero: the shader they are
      /// drawn with asks for them, and iOS Safari draws nothing for a mesh that lacks an
      /// input its shader names, where Chrome does).
      /// Whether the tree's data has arrived, and how many vertices all its connections have
      /// together. A mesh keeps the number of vertices it was made with (the toy gives its
      /// draw call once), so the meshes are only made once there is something to draw, and
      /// the path's has always that many vertices, the unused ones all in one point.
      Loaded : bool
      Capacity : int
      WebUv : Buffer<V2f>
      AscendancyWebUv : Buffer<V2f>
      PathUv : Buffer<V2f>
      Art : string
      Links : Link[]
      Neighbours : Map<int, int list>
      Web : Buffer<V3f>
      Allocated : Set<int>
      Edited : bool
      /// The view on the tree: its centre, and the height of what is seen (tree units).
      Centre : V2d
      Height : float
      /// The camera for that view and its depth range, kept here by `look`: the render
      /// control wants them as fields of the model (given values worked out in the view, it
      /// is made anew, canvas and all, whenever they change; FableCube dogfood/poetry.md).
      Camera : Camera
      Near : float
      Far : float
      /// The pointers that are down on the tree (mouse, or one or two fingers), by id, where
      /// they are on the page; how far they have moved since the first came down (a pointer
      /// let go without having moved is a click).
      Touches : (int * V2d) list
      Travel : float
      /// The page's size in its own pixels (0 until the bridge has said), asked again with
      /// every pointer that comes down (`Asked` counts them), so that a turned phone or a
      /// resized window is followed.
      Page : V2d
      Asked : int
      /// A click that could not be placed yet (the page's size was not known).
      Pending : V2d option
      /// What an allocated passive with a choice was chosen to be ("Strength", ...).
      Choices : Map<int, string>
      /// The passive whose tooltip is pinned (clicked), and the one under the mouse.
      /// The pictures of jewels in sockets, by unique name or base type; the character's jewels by socket.
      JewelSprites : Map<string, string * V4d>
      Jewels : Map<int, Jewel>
      Selected : int option
      Hovered : int option
      Level : int }

let init : Model =
    { Build = ""; Scenario = "boss"; Status = "locating"; Title = ""; Class = ""; Ascendancy = ""; Tab = "character"
      Pools = []; Defences = []; Resistances = []; Minions = []
      NotUnderstood = 0; Fighting = false; Gem = ""; Dps = []; Shares = []
      Passives = [||]; Ascendancies = []; Shown = []; AscendancyWeb = Data.pack (Data.ofList ([] : V3f list)); Path = Data.pack (Data.ofList ([] : V3f list))
      Loaded = false; Capacity = 0
      WebUv = Data.pack (Data.ofList ([] : V2f list)); AscendancyWebUv = Data.pack (Data.ofList ([] : V2f list)); PathUv = Data.pack (Data.ofList ([] : V2f list)); Art = ""; Links = [||]; Neighbours = Map.empty; Web = Data.pack (Data.ofList ([] : V3f list)); Allocated = Set.empty; Edited = false
      Centre = V2d (0.0, 0.0); Height = 9000.0
      Camera = { Sky = V3d (0.0, 1.0, 0.0); Location = V3d (0.0, 0.0, 7794.2); Forward = V3d (0.0, 0.0, -1.0); Up = V3d (0.0, 1.0, 0.0); Right = V3d (1.0, 0.0, 0.0) }
      Near = 7000.0; Far = 8600.0
      Touches = []; Travel = 0.0; Page = V2d (0.0, 0.0); Asked = 0; Pending = None; Choices = Map.empty; JewelSprites = Map.empty; Jewels = Map.empty; Selected = None; Hovered = None; Level = 0 }

/// The camera straight above the centre, so far up that `Height` of the tree is seen through
/// its 60 degree lens; the depth range close around the tree's plane (its layers are a unit
/// apart: with a wide range they are told apart by luck).
let private look (m : Model) : Model =
    let distance = m.Height / (2.0 * tan (System.Math.PI / 6.0))
    { m with
        Camera = { Sky = V3d (0.0, 1.0, 0.0); Location = V3d (m.Centre.X, m.Centre.Y, distance); Forward = V3d (0.0, 0.0, -1.0); Up = V3d (0.0, 1.0, 0.0); Right = V3d (1.0, 0.0, 0.0) }
        Near = distance * 0.9
        Far = distance * 1.1 }

/// The passives as the simulator is told them (only once they were changed here).
let private passives (m : Model) =
    if m.Edited then
        "&passives=" + (m.Allocated |> Set.toList |> List.map string |> String.concat ",")
        + "&choices=" + (m.Choices |> Map.toList |> List.filter (fun (i, _) -> m.Allocated.Contains i) |> List.map (fun (i, c) -> string i + ":" + c) |> String.concat ",")
    else ""

let private sheetUrl (m : Model) = "__sim?op=character" + passives m + "&build=" + m.Build
let private fightUrl (m : Model) = "__sim?op=fight&runs=5&time=20&life=1e12&scenario=" + m.Scenario + passives m + "&build=" + m.Build

// ---- the passive tree ----

/// The ribbon of a connection, as triangles: a straight one between its passives, or along
/// its arc ([a, b, cx, cy, r, from, sweep]: degrees clockwise from the top).
let private ribbonAt (width : float) (at : Map<int, V2d>) (middle : V2d) (scale : float) (edge : float list) : V3f list =
    // (an ascendancy's connections are drawn around the middle of the tree: `middle` is
    // where they are laid out, `scale` how much smaller they are drawn)
    let at = at |> Map.map (fun _ p -> (p - middle) * scale)
    let quad (p0 : V2d) (p1 : V2d) (q0 : V2d) (q1 : V2d) =
        let v (p : V2d) = V3f (float32 p.X, float32 -p.Y, 0.0f)
        [ v p0; v p1; v q0; v q0; v p1; v q1 ]
    match edge with
    | [ a; b ] ->
        let p, q = at.[int a], at.[int b]
        let d = q - p
        let len = sqrt (d.X * d.X + d.Y * d.Y)
        if len <= 0.0 then []
        else
            let n = V2d (-d.Y / len, d.X / len) * (width / 2.0)
            quad (p - n) (p + n) (q - n) (q + n)
    | [ _; _; cx; cy; r; from; sweep ] ->
        let cx, cy, r = (cx - middle.X) * scale, (cy - middle.Y) * scale, r * scale
        let steps = max 2 (int (ceil (sweep / 8.0)))
        let point (radius : float) (i : int) =
            let a = (from + sweep * float i / float steps) * System.Math.PI / 180.0
            V2d (cx + radius * sin a, cy - radius * cos a)
        [ for i in 0 .. steps - 1 do
            yield! quad (point (r - width / 2.0) i) (point (r + width / 2.0) i) (point (r - width / 2.0) (i + 1)) (point (r + width / 2.0) (i + 1)) ]
    | _ -> []

let private ribbon (width : float) (at : Map<int, V2d>) (edge : float list) : V3f list =
    ribbonAt width at (V2d (0.0, 0.0)) 1.0 edge

/// How much smaller an ascendancy is drawn than it is laid out, and how big its class art
/// and the ring around it are, in widths of its background (pob-web's PassiveTree).
let private ascendancyScale = 0.8025
let private artSize = 1500.0 * 3.1
let private ringSize = 1500.0 * 2.675

/// As many zero texture coordinates as a mesh has vertices.
let private noUv (vertices : V3f list) : Buffer<V2f> =
    Data.pack (Data.ofList (vertices |> List.map (fun _ -> V2f (0.0f, 0.0f))))

/// The tree as fetched (web/public/tree.json, see tools/export/tree_ui.py).
let private readTree (j : Json) (m : Model) : Model =
    let sheets = Json.field "sheets" j |> Json.list |> List.map (fun s -> Json.string (Json.field "name" s), Json.number (Json.field "w" s), Json.number (Json.field "h" s))
    let sprite (x : Json) : string * V4d =
        match Json.list x |> List.map Json.number with
        | [ s; px; py; w; h ] when int s < sheets.Length ->
            let name, sw, sh = sheets.[int s]
            name, V4d (px / sw, py / sh, w / sw, h / sh)
        | _ -> "", V4d (0.0, 0.0, 0.0, 0.0)
    let frames = Json.field "frames" j
    let nodes = Json.field "nodes" j |> Json.list
    let passives =
        nodes
        |> List.map (fun n ->
            let kind = Json.string (Json.field "kind" n)
            let icon, rect = sprite (Json.field "icon" n)
            let frame, on = sprite (Json.field kind frames |> Json.field "alloc")
            let _, off = sprite (Json.field kind frames |> Json.field "normal")
            { Id = Json.int (Json.field "id" n)
              X = Json.number (Json.field "x" n)
              Y = Json.number (Json.field "y" n)
              // sizes in tree units as pob-web draws them (PoB's GetNodeTargetSize x 2)
              Size = (match kind with "keystone" -> 240.0 | "notable" -> 160.0 | "jewel" -> 152.0 | "start" -> 0.0 | "ascstart" -> 100.0 | _ -> 108.0)
              IconSize = (match kind with "keystone" -> 164.0 | "notable" -> 108.0 | "jewel" -> 152.0 | "ascstart" -> 32.0 | _ -> 74.0)
              Kind = kind
              Hidden = Json.bool (Json.field "hidden" n)
              Name = Json.string (Json.field "name" n)
              Stats = Json.field "stats" n |> Json.list |> List.map Json.string
              Options = (if kind = "attribute" then Json.field "options" n |> Json.list |> List.map (fun o -> Json.string (Json.field "name" o)) else [])
              Ascendancy = Json.string (Json.field "ascendancy" n)
              Icon = icon
              IconRect = rect
              Frame = frame
              FrameOn = on
              FrameOff = off })
    let at = passives |> List.map (fun p -> p.Id, V2d (p.X, p.Y)) |> Map.ofList
    let ascendancies =
        Json.field "ascendancies" j |> Json.list
        |> List.map (fun a ->
            { Called = Json.string (Json.field "name" a); Middle = V2d (Json.number (Json.field "x" a), Json.number (Json.field "y" a)); Art = Json.string (Json.field "art" a) })
    let middleOf = ascendancies |> List.map (fun a -> a.Called, a.Middle) |> Map.ofList
    let ascendancyOf = passives |> List.map (fun p -> p.Id, p.Ascendancy) |> Map.ofList
    let edges = Json.field "edges" j |> Json.list |> List.map (fun e -> Json.list e |> List.map Json.number)
    let own (e : float list) = ascendancyOf.[int e.[0]]
    let links = edges |> List.filter (fun e -> own e = "") |> List.map (fun e -> { A = int e.[0]; B = int e.[1]; Of = ""; Ribbon = ribbon 26.0 at e })
    let ascendedLinks =
        edges |> List.filter (fun e -> own e <> "" && middleOf.ContainsKey (own e))
        |> List.map (fun e -> { A = int e.[0]; B = int e.[1]; Of = own e; Ribbon = ribbonAt 22.0 at middleOf.[own e] ascendancyScale e })
    // an ascendancy's passives, where they are drawn: around the middle of the tree
    let ascended =
        passives |> List.filter (fun p -> p.Ascendancy <> "" && middleOf.ContainsKey p.Ascendancy)
        |> List.map (fun p ->
            let q = (V2d (p.X, p.Y) - middleOf.[p.Ascendancy]) * ascendancyScale
            { p with X = q.X; Y = q.Y })
    let web = edges |> List.filter (fun e -> own e = "") |> List.collect (ribbon 14.0 at)
    let shown = (passives |> List.filter (fun p -> p.Kind <> "start" && p.Ascendancy = "")) @ ascended
    { m with
        JewelSprites = Json.field "jewels" j |> Json.members |> List.map (fun (k, v) -> k, sprite v) |> Map.ofList
        Passives = Array.ofList passives
        Ascendancies = ascendancies
        Shown = shown
        Links = Array.ofList (links @ ascendedLinks)
        Neighbours = nodes |> List.map (fun n -> Json.int (Json.field "id" n), (Json.field "to" n |> Json.list |> List.map Json.int)) |> Map.ofList
        // (packed here: a buffer is one value to the view, where a list would be followed element by element)
        Web = Data.pack (Data.ofList web)
        WebUv = noUv web
        Loaded = true
        // (room for some 400 connections; a character has 150 passives at most)
        Capacity = 24000 }

/// The connections between allocated passives, as one mesh.
let private paths (m : Model) : Model =
    let ribbons =
        List.ofArray m.Links
        |> List.filter (fun l -> (l.Of = "" || l.Of = m.Ascendancy) && m.Allocated.Contains l.A && m.Allocated.Contains l.B)
        |> List.collect (fun l -> l.Ribbon)
    // (always `Capacity` vertices: what is left over lies in one point and draws nothing)
    let ribbons = List.truncate m.Capacity ribbons
    let padded = ribbons @ List.replicate (max 0 (m.Capacity - ribbons.Length)) (V3f (0.0f, 0.0f, 0.0f))
    { m with Path = Data.pack (Data.ofList padded); PathUv = noUv padded }

/// The character's ascendancy, once the tree and the character are both there: the game's
/// character data names it as the class ("Spirit Walker"); else the one its allocated
/// passives belong to; none for a character that has not ascended.
let private settle (m : Model) : Model =
    let m =
        if m.Ascendancies |> List.exists (fun a -> a.Called = m.Class) then { m with Ascendancy = m.Class }
        else
            match List.ofArray m.Passives |> List.tryFind (fun p -> p.Ascendancy <> "" && m.Allocated.Contains p.Id) with
            | Some p -> { m with Ascendancy = p.Ascendancy }
            | None -> m
    let web = List.ofArray m.Links |> List.filter (fun l -> l.Of <> "" && l.Of = m.Ascendancy) |> List.collect (fun l -> l.Ribbon)
    { m with
        Art = (match m.Ascendancies |> List.tryFind (fun a -> a.Called = m.Ascendancy) with Some a -> a.Art | None -> "")
        AscendancyWeb = Data.pack (Data.ofList web)
        AscendancyWebUv = noUv web }
    |> paths

/// Whether a passive is drawn: the tree's own, the character's ascendancy's, and a hidden
/// socket only once it is allocated.
let private visible (m : Model) (p : Passive) =
    (p.Ascendancy = "" || p.Ascendancy = m.Ascendancy) && (not p.Hidden || m.Allocated.Contains p.Id)

/// Where a character's passives start: its class' start and its ascendancy's.
let private roots (m : Model) : Set<int> =
    List.ofArray m.Passives
    |> List.filter (fun p -> (p.Kind = "start" && m.Neighbours.ContainsKey p.Id) || (p.Kind = "ascstart" && p.Ascendancy = m.Ascendancy))
    |> List.map (fun p -> p.Id)
    |> Set.ofList

/// Whether a passive can be taken by this character at all (another ascendancy's can't).
let private open' (m : Model) (asc : Map<int, string>) (id : int) =
    match Map.tryFind id asc with
    | Some a -> a = "" || a = m.Ascendancy
    | None -> false

/// Of these passives, those that hang on a start (the class' own start is whichever touches
/// them) or on the ascendancy's.
let private hanging (m : Model) (all : Set<int>) : Set<int> =
    let asc = List.ofArray m.Passives |> List.map (fun p -> p.Id, p.Ascendancy) |> Map.ofList
    let starts = List.ofArray m.Passives |> List.filter (fun p -> p.Kind = "start") |> List.map (fun p -> p.Id) |> Set.ofList
    let ascStarts = List.ofArray m.Passives |> List.filter (fun p -> p.Kind = "ascstart" && p.Ascendancy = m.Ascendancy) |> List.map (fun p -> p.Id) |> Set.ofList
    let next (i : int) = Map.tryFind i m.Neighbours |> Option.defaultValue [] |> List.filter (fun n -> open' m asc n && not (starts.Contains n))
    let seeds = all |> Set.filter (fun i -> ascStarts.Contains i || (Map.tryFind i m.Neighbours |> Option.defaultValue [] |> List.exists (fun n -> starts.Contains n || ascStarts.Contains n)))
    let rec grow (seen : Set<int>) (front : int list) =
        match front with
        | [] -> seen
        | i :: rest ->
            let more = next i |> List.filter (fun n -> all.Contains n && not (seen.Contains n))
            grow (more |> List.fold (fun s n -> Set.add n s) seen) (rest @ more)
    grow seeds (Set.toList seeds)

/// Allocating a passive takes the shortest way to it from what is allocated; deallocating
/// one drops it and all that then comes loose from the start.
let private toggle (id : int) (m : Model) : Model =
    let asc = List.ofArray m.Passives |> List.map (fun p -> p.Id, p.Ascendancy) |> Map.ofList
    let starts = List.ofArray m.Passives |> List.filter (fun p -> p.Kind = "start") |> List.map (fun p -> p.Id) |> Set.ofList
    let ascStarts = List.ofArray m.Passives |> List.filter (fun p -> p.Kind = "ascstart" && p.Ascendancy = m.Ascendancy) |> List.map (fun p -> p.Id) |> Set.ofList
    let next (i : int) = Map.tryFind i m.Neighbours |> Option.defaultValue [] |> List.filter (fun n -> open' m asc n && not (starts.Contains n))
    if m.Allocated.Contains id then
        let hanging = hanging m
        // what was loose before stays (passives a jewel or an item allocates without a way to them)
        let loose = Set.difference m.Allocated (hanging m.Allocated) |> Set.remove id
        { m with Allocated = Set.union (hanging (m.Allocated.Remove id)) loose; Edited = true }
    else
        // breadth first from the passive towards anything allocated (or a start to begin from)
        let rec search (seen : Map<int, int>) (front : int list) =
            match front with
            | [] -> None
            | i :: rest ->
                let near = Map.tryFind i m.Neighbours |> Option.defaultValue []
                if m.Allocated.Contains i || ascStarts.Contains i || near |> List.exists ascStarts.Contains || (near |> List.exists starts.Contains && m.Allocated.IsEmpty) then Some (i, seen)
                else
                    let more = next i |> List.filter (fun n -> not (seen.ContainsKey n))
                    search (more |> List.fold (fun s n -> Map.add n i s) seen) (rest @ more)
        match search (Map.ofList [ id, id ]) [ id ] with
        | None -> m
        | Some (reached, from) ->
            let rec path (i : int) (acc : int list) = if i = id then i :: acc else path from.[i] (i :: acc)
            { m with Allocated = path reached [] |> List.fold (fun s n -> Set.add n s) m.Allocated; Edited = true }


/// 1234567 as "1.23M", 45678 as "45.7k".
let private short (v : float) =
    if v >= 1e9 then sprintf "%.2fB" (v / 1e9)
    elif v >= 1e6 then sprintf "%.2fM" (v / 1e6)
    elif v >= 1e4 then sprintf "%.1fk" (v / 1e3)
    else sprintf "%.0f" v

let private row (label : string) (v : float) = { Label = label; Value = sprintf "%.0f" v }

// ---- pointers on the tree ----

/// The tree starts this far down the page (the header above it).
let private top = 64.0

/// The tree's width and height on the page: all of it below the header.
let private size (m : Model) : V2d = V2d (m.Page.X, m.Page.Y - top)

/// The place of the tree at a point of the page, for a view with this centre and height.
/// (`Height` of the tree is what the 60 degree lens sees across the control's width.)
let private placeAt (m : Model) (centre : V2d) (height : float) (p : V2d) : V2d =
    let s = size m
    let u = height / s.X
    V2d (centre.X + (p.X - s.X / 2.0) * u, centre.Y - (p.Y - top - s.Y / 2.0) * u)

/// One pointer moved from `before` to `now`: with a single pointer down the tree follows it;
/// with two, the place between them stays between them and the view grows or shrinks with
/// their distance (panning and zooming at once).
let private gesture (id : int) (before : V2d) (now : V2d) (m : Model) : Model =
    let touches = m.Touches |> List.map (fun (i, p) -> if i = id then i, now else i, p)
    let travel = m.Travel + abs (now.X - before.X) + abs (now.Y - before.Y)
    let m = { m with Touches = touches; Travel = travel }
    if m.Page.X <= 0.0 then m
    else
        match m.Touches |> List.filter (fun (i, _) -> i <> id) with
        | [] ->
            let u = m.Height / (size m).X
            look { m with Centre = V2d (m.Centre.X - (now.X - before.X) * u, m.Centre.Y + (now.Y - before.Y) * u) }
        | (_, other) :: _ ->
            let span (a : V2d) (b : V2d) = sqrt ((a.X - b.X) * (a.X - b.X) + (a.Y - b.Y) * (a.Y - b.Y))
            let between (a : V2d) (b : V2d) = V2d ((a.X + b.X) / 2.0, (a.Y + b.Y) / 2.0)
            let height = max 600.0 (min 60000.0 (m.Height * span before other / max 1.0 (span now other)))
            let held = placeAt m m.Centre m.Height (between before other)
            // the centre for which `held` is under the new place between the fingers
            let at = placeAt m (V2d (0.0, 0.0)) height (between now other)
            look { m with Height = height; Centre = V2d (held.X - at.X, held.Y - at.Y) }

/// The passive drawn at a place of the tree (y up), if any.
let private passiveAt (m : Model) (x : float) (y : float) : Passive option =
    let near (ps : Passive list) =
        ps
        |> List.filter (fun q -> (q.X - x) * (q.X - x) + (q.Y + y) * (q.Y + y) <= q.Size * q.Size / 4.0)
        |> List.sortBy (fun q -> (q.X - x) * (q.X - x) + (q.Y + y) * (q.Y + y))
        |> List.tryHead
    // (the ascendancy's lie over the tree's own there: they come first)
    near (m.Shown |> List.filter (fun q -> q.Ascendancy <> "" && visible m q)) |> Option.orElse (near (m.Shown |> List.filter (visible m)))

let rec update (msg : Msg) (m : Model) : Model =
    match msg with
    | Located href ->
        match Native.queryParam "build" href with
        | Some b -> { m with Build = b; Status = "reading the character" }
        | None -> { m with Status = "no character: add ?build=<url of a character or a PoB build> to the address" }
    | Scenario s -> { m with Scenario = s; Fighting = true; Dps = []; Shares = [] }
    | Tab t -> { m with Tab = t }
    | TreeData r ->
        match r.Status, Native.parseJson r.Text with
        | 200, Ok j -> settle (readTree j m)
        | s, _ -> { m with Status = "the passive tree could not be read (status " + string s + ")" }
    // the passive under the click: the nearest one whose frame covers the place (the
    // character's own ascendancy is drawn in the middle, over the tree's passives there)
    | Click p ->
        match passiveAt m p.X p.Y with
        | Some q -> { m with Selected = Some q.Id }
        | None -> { m with Selected = None }
    | Allocate (id, choice) ->
        let m = if m.Allocated.Contains id then m else toggle id m
        paths { m with Choices = (if choice = "" then m.Choices else Map.add id choice m.Choices); Edited = true; Fighting = true }
    | Deallocate id -> if m.Allocated.Contains id then paths { toggle id m with Fighting = true } else m
    | Close -> { m with Selected = None }
    | Toggle id -> paths { toggle id m with Fighting = true }
    // A place on the tree comes from the last frame drawn; under an older camera than the
    // model's it is no use.
    | Zoom (_, eye, _) when abs (eye.X - m.Centre.X) + abs (eye.Y - m.Centre.Y) > m.Height / 1e5 -> m
    | Sized r ->
        match Native.parseJson r.Text with
        | Ok j when Json.number (Json.field "width" j) > 0.0 ->
            let m = { m with Page = V2d (Json.number (Json.field "width" j), Json.number (Json.field "height" j)) }
            // a click that waited for this
            match m.Pending with
            | Some p ->
                let q = placeAt m m.Centre m.Height p
                update (Click (V3d (q.X, q.Y, 0.0))) { m with Pending = None }
            | None -> m
        | _ -> m
    | PointerDown e ->
        let others = m.Touches |> List.filter (fun (i, _) -> i <> e.PointerId)
        let m = { m with Touches = (e.PointerId, e.Position) :: others }
        if others.IsEmpty then { m with Travel = 0.0; Asked = m.Asked + 1 } else m
    | PointerMove e ->
        match m.Touches |> List.tryFind (fun (i, _) -> i = e.PointerId) with
        | Some (_, before) -> gesture e.PointerId before e.Position m
        // no pointer down: the mouse, over a passive or not
        | None when m.Page.X > 0.0 ->
            let p = placeAt m m.Centre m.Height e.Position
            let over = passiveAt m p.X p.Y |> Option.map (fun q -> q.Id)
            if over = m.Hovered then m else { m with Hovered = over }
        | None -> m
    | PointerUp e ->
        let left = m.Touches |> List.filter (fun (i, _) -> i <> e.PointerId)
        let lifted = { m with Touches = left }
        // let go without having moved, and the only one down: a click on that place (kept
        // for later while the control's size is unknown)
        if m.Touches.Length = 1 && left.IsEmpty && m.Travel < 8.0 then
            if m.Page.X > 0.0 then
                let p = placeAt m m.Centre m.Height e.Position
                update (Click (V3d (p.X, p.Y, 0.0))) lifted
            else { lifted with Pending = Some e.Position }
        else lifted
    | Zoom (p, _, wheel) ->
        // towards the point under the pointer
        let k = if wheel > 0.0 then 1.15 else 1.0 / 1.15
        let height = max 600.0 (min 60000.0 (m.Height * k))
        let k = height / m.Height
        look { m with Height = height; Centre = V2d (p.X + (m.Centre.X - p.X) * k, p.Y + (m.Centre.Y - p.Y) * k) }
    | Sheet r when r.Url <> sheetUrl m -> m
    | Sheet r ->
        match r.Status, Native.parseJson r.Text with
        | 200, Ok j when Json.string (Json.field "error" j) <> "" -> { m with Status = "the character could not be read: " + Json.string (Json.field "error" j) }
        | 200, Ok j ->
            let n (path : string list) = path |> List.fold (fun x k -> Json.field k x) j |> Json.number
            let named key = Json.field key j |> Json.list |> List.map (fun x -> { Label = Json.string (Json.field "name" x); Value = sprintf "%.0f" (Json.number (Json.field (if key = "minions" then "count" else "value") x)) })
            let missing = [ "unmapped"; "unsimulated"; "unknown"; "minions" ] |> List.sumBy (fun k -> Json.field "notUnderstood" j |> Json.field k |> Json.list |> List.length)
            settle
                { m with
                    Status = ""
                    Title = sprintf "Level %.0f %s" (n [ "level" ]) (Json.string (Json.field "class" j))
                    Class = Json.string (Json.field "class" j)
                    Ascendancy = Json.string (Json.field "ascendancy" j)
                    // what the character has allocated, until it is changed here
                    Allocated = (if m.Edited then m.Allocated else Json.field "passives" j |> Json.list |> List.map Json.int |> Set.ofList)
                    Choices = (if m.Edited then m.Choices else Json.field "choices" j |> Json.list |> List.map (fun c -> Json.int (Json.field "id" c), Json.string (Json.field "name" c)) |> Map.ofList)
                    Level = int (n [ "level" ])
                    Jewels =
                        Json.field "jewels" j |> Json.list
                        |> List.map (fun x ->
                            let s k = Json.string (Json.field k x)
                            let pic = Map.tryFind (s "name") m.JewelSprites |> Option.orElse (Map.tryFind (s "base") m.JewelSprites) |> Option.defaultValue ("", V4d (0.0, 0.0, 0.0, 0.0))
                            Json.int (Json.field "node" x), { Called = s "name"; Base = s "base"; Rarity = s "rarity"; Mods = Json.field "mods" x |> Json.list |> List.map Json.string; Sprite = fst pic; SpriteRect = snd pic })
                        |> Map.ofList
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

/// A square of side 1 around the origin, its texture coordinates from the top left. (Two
/// triangles, not a strip: many strips drawn together come out joined, see FableCube
/// dogfood/poetry.md.)
let quad<'m> : Scene<'m> =
    mesh Topology.Triangles {
        Mesh.Attribute ("Positions", Data.pack (Data.ofList [ V3f (-0.5f, -0.5f, 0.0f); V3f (0.5f, -0.5f, 0.0f); V3f (-0.5f, 0.5f, 0.0f); V3f (-0.5f, 0.5f, 0.0f); V3f (0.5f, -0.5f, 0.0f); V3f (0.5f, 0.5f, 0.0f) ]))
        Mesh.Attribute ("DiffuseColorCoordinates", Data.pack (Data.ofList [ V2f (0.0f, 1.0f); V2f (1.0f, 1.0f); V2f (0.0f, 0.0f); V2f (0.0f, 0.0f); V2f (1.0f, 1.0f); V2f (1.0f, 0.0f) ]))
    }

/// (A tint's fourth part is how transparent the art may be before it is cut away.)
let white = V4d (1.0, 1.0, 1.0, 0.35)

let dim = V4d (0.42, 0.42, 0.45, 0.35)

/// An allocated passive's tint: an attribute's by what was chosen, as in the game.
let private chosen (m : Model) (p : Passive) : V4d =
    match Map.tryFind p.Id m.Choices with
    | Some "Strength" -> V4d (1.0, 0.38, 0.3, 0.35)
    | Some "Dexterity" -> V4d (0.4, 1.0, 0.4, 0.35)
    | Some "Intelligence" -> V4d (0.4, 0.6, 1.0, 0.35)
    | _ -> white

/// A passive's icon: out of the sheet bound above, dim until it is allocated.
let icon (m : Model) (p : Passive) : Scene<Msg> =
    // (a socket with a jewel in it shows the jewel, over its frame)
    let jewel = Map.tryFind p.Id m.Jewels |> Option.filter (fun j -> j.Sprite <> "" && m.Allocated.Contains p.Id)
    let z = if jewel.IsSome then 3.0 else 1.0
    let size = if jewel.IsSome then p.Size else p.IconSize
    let sheet = match jewel with Some j -> j.Sprite | None -> p.Icon
    let rect = match jewel with Some j -> j.SpriteRect | None -> p.IconRect
    sg {
        Sg.Translate (p.X, -p.Y, z)
        Sg.Scale (size)
        Sg.Texture ("Atlas", sheet)
        Sg.Uniform ("Rect", rect)
        Sg.Uniform ("Tint", (if m.Allocated.Contains p.Id then chosen m p else dim))
        quad
    }

/// Its frame, the allocated one once it is.
let frame (m : Model) (p : Passive) : Scene<Msg> =
    sg {
        Sg.Translate (p.X, -p.Y, 2.0)
        Sg.Scale (p.Size)
        Sg.Texture ("Atlas", p.Frame)
        Sg.Uniform ("Rect", (if m.Allocated.Contains p.Id then p.FrameOn else p.FrameOff))
        Sg.Uniform ("Tint", white)
        quad
    }

let tree (m : Model) : Node<Msg> =
    renderControl {
        Dom.Style [ Css.Position "absolute"; Css.Left "0"; Css.Top "64px"; Css.Width "100%"; Css.Height "calc(100% - 64px)"; Css.TouchAction "none"; Css.UserSelect "none"
                    Css.Prop ("visibility", (if m.Tab = "tree" then "visible" else "hidden")) ]
        Dom.OnPointerDown PointerDown
        Dom.OnPointerMove PointerMove
        Dom.OnPointerUp PointerUp
        RenderControl.Perspective (60.0, m.Near, m.Far)
        RenderControl.Camera m.Camera
        sg {
            // the ground: what the pointer holds while dragging, and zooms towards
            sg {
                Sg.Shader { Shaders.flat }
                Sg.Uniform ("Color", V4d (0.047, 0.047, 0.055, 1.0))
                Sg.Translate (0.0, 0.0, -1.0)
                Sg.Scale (200000.0)
                // the only thing that takes the pointer, and by a ray against its box: picking
                // by pixel would draw every sprite once more on each pointer move (and with
                // thousands of them the canvas came out empty for two frames after each)
                Sg.Intersectable (ProxyBox (V3d (-0.5, -0.5, -0.000001), V3d (0.5, 0.5, 0.000001)))
                Sg.OnScroll (fun e -> Zoom (e.WorldPosition, e.Eye, e.Wheel.Y))
                quad
            }
            // every connection, dim; the ascendancy's own; the allocated ones over them in the
            // game's orange. Each is one mesh, made in the update.
            sg {
                Sg.Shader { Shaders.flat }
                Sg.NoEvents true
                if m.Loaded then
                    sg {
                        Sg.Uniform ("Color", V4d (0.227, 0.227, 0.306, 1.0))
                        mesh Topology.Triangles { Mesh.Attribute ("Positions", m.Web); Mesh.Attribute ("DiffuseColorCoordinates", m.WebUv) }
                    }
                    sg {
                        Sg.Uniform ("Color", V4d (0.82, 0.74, 0.47, 1.0))
                        Sg.Translate (0.0, 0.0, 0.7)
                        mesh Topology.Triangles { Mesh.Attribute ("Positions", m.Path); Mesh.Attribute ("DiffuseColorCoordinates", m.PathUv) }
                    }
                if m.Art <> "" then
                    sg {
                        Sg.Uniform ("Color", V4d (0.227, 0.227, 0.306, 1.0))
                        Sg.Translate (0.0, 0.0, 0.6)
                        mesh Topology.Triangles { Mesh.Attribute ("Positions", m.AscendancyWeb); Mesh.Attribute ("DiffuseColorCoordinates", m.AscendancyWebUv) }
                    }
            }
            // the character's ascendancy in the middle: its class art and the ring around it
            sg {
                Sg.Shader { Shaders.sprite }
                Sg.NoEvents true
                sg {
                    Sg.Translate (0.0, 0.0, 0.2)
                    Sg.Scale (artSize)
                    Sg.Texture ("Atlas", (if m.Art = "" then "art/ring.png" else m.Art))
                    Sg.Uniform ("Rect", V4d (0.0, 0.0, 1.0, 1.0))
                    Sg.Uniform ("Tint", (if m.Art = "" then V4d (0.0, 0.0, 0.0, 2.0) else V4d (0.75, 0.75, 0.75, 0.01)))
                    quad
                }
                sg {
                    Sg.Translate (0.0, 0.0, 0.3)
                    Sg.Scale (ringSize)
                    Sg.Texture ("Atlas", "art/ring.png")
                    Sg.Uniform ("Rect", V4d (0.0, 0.0, 1.0, 1.0))
                    Sg.Uniform ("Tint", (if m.Art = "" then V4d (0.0, 0.0, 0.0, 2.0) else V4d (0.8, 0.8, 0.8, 0.45)))
                    quad
                }
            }
            // the passives, icon under frame: the one loop of this view
            sg {
                Sg.Shader { Shaders.sprite }
                Sg.NoEvents true
                for p in m.Shown do
                    if visible m p then
                        if p.Icon <> "" then icon m p
                        if p.Frame <> "" then frame m p
            }
        }
    }

let private tab (m : Model) (key : string) (name : string) : Node<Msg> =
    button {
        Dom.Class (if m.Tab = key then "tab active" else "tab")
        Dom.OnClick (fun _ -> Tab key)
        name
    }

/// Passive points spent of those a character of this level has (one a level after the
/// first, 24 from quests), the ascendancy's apart; what a jewel or an item allocates
/// without a way to it costs none.
let private points (m : Model) : string =
    let spent = hanging m m.Allocated
    let asc = m.Passives |> Array.filter (fun p -> p.Ascendancy <> "" && p.Kind <> "ascstart" && spent.Contains p.Id) |> Array.length
    let starts = m.Passives |> Array.filter (fun p -> p.Kind = "ascstart" && spent.Contains p.Id) |> Array.length
    let free = m.Allocated.Count - spent.Count
    sprintf "%d of %d points, %d ascendancy%s" (spent.Count - asc - starts) (m.Level - 1 + 24) asc (if free > 0 then sprintf ", %d granted" free else "")

/// The tooltip of the pinned passive (with what can be done to it) or the one under the mouse.
let private tooltip (m : Model) : Node<Msg> =
    let pinned = m.Selected.IsSome
    match (if pinned then m.Selected else m.Hovered) |> Option.bind (fun i -> m.Passives |> Array.tryFind (fun p -> p.Id = i)) with
    | None -> div { Dom.Class "passive-tip none" }
    | Some p ->
        let has = m.Allocated.Contains p.Id
        let title = match Map.tryFind p.Id m.Choices with Some c when has -> c | _ -> p.Name
        // beside the passive where it is drawn (an ascendancy's: around the middle), kept on the page
        let q = m.Shown |> List.tryFind (fun s -> s.Id = p.Id) |> Option.defaultValue p
        let s = size m
        let u = m.Height / max 1.0 s.X
        let x = (q.X - m.Centre.X) / u + s.X / 2.0 + q.Size / u / 2.0 + 10.0
        let y = top + s.Y / 2.0 + (q.Y + m.Centre.Y) / u + q.Size / u / 2.0 + 6.0
        let wide = min 380.0 (m.Page.X - 24.0)
        let state = if has then "Allocated" else "Unallocated"
        // (a chosen attribute grants that one)
        let stats = match Map.tryFind p.Id m.Choices with Some c when has -> p.Stats |> List.map (fun s -> s.Replace ("any Attribute", c)) | _ -> p.Stats
        let j = Map.tryFind p.Id m.Jewels
        let jewel = match j with Some j -> j.Mods | None -> []
        let called = match j with Some j -> j.Called | None -> ""
        let based = match j with Some j -> j.Base | None -> ""
        let rarity = match j with Some j -> j.Rarity.ToLower () | None -> ""
        div {
            Dom.Style [ Css.Left (sprintf "%.0fpx" (max 12.0 (min x (m.Page.X - wide - 12.0)))); Css.Top (sprintf "%.0fpx" (max (top + 8.0) (min y (m.Page.Y - 190.0 - 22.0 * float jewel.Length)))); Css.Width (sprintf "%.0fpx" wide) ]
            Dom.Class ("passive-tip " + p.Kind + (if pinned then " pinned" else ""))
            Dom.Attr ("data-check", "tooltip")
            div { Dom.Class "name"; title }
            div {
                Dom.Class "stats"
                for s in stats do
                    div { s }
            }
            if has && jewel.Length > 0 then
                div {
                    Dom.Class ("jewel " + rarity)
                    div { Dom.Class "called"; called }
                    div { Dom.Class "base"; based }
                    for l in jewel do
                        div { Dom.Class (if l = "Corrupted" then "corrupted" else "mod"); l }
                }
            if pinned then
                div {
                    Dom.Class "actions"
                    if not p.Options.IsEmpty then
                        for o in p.Options do
                            button { Dom.Class ("choice " + o + (if has && Map.tryFind p.Id m.Choices = Some o then " active" else "")); Dom.OnClick (fun _ -> Allocate (p.Id, o)); o }
                    if p.Options.IsEmpty && not has then
                        button { Dom.Class "tab"; Dom.OnClick (fun _ -> Allocate (p.Id, "")); "Allocate" }
                    if has then
                        button { Dom.Class "tab"; Dom.OnClick (fun _ -> Deallocate p.Id); "Refund" }
                    button { Dom.Class "tab"; Dom.OnClick (fun _ -> Close); "Close" }
                }
            if not pinned then
                div { Dom.Class "state"; state }
        }

let private status (m : Model) = if m.Fighting then "fighting ..." else m.Status

let view (m : Model) : Node<Msg> =
    body {
        Dom.OnLocation Located
        // The tree comes first among the page's children, and the conditional ones after it
        // sit in an element of their own: a conditional child makes the list dynamic from
        // there on, and FableCube then builds everything after it as one plain value, made
        // anew on every change (for a render control: a new canvas and every draw again).
        // Being fixed in FableCube; see its dogfood/poetry.md.
        tree m
        div {
            if m.Build <> "" then span { Dom.Fetch (sheetUrl m, Sheet) }
            if m.Title <> "" then span { Dom.Fetch (fightUrl m, Fought) }
        }
        span { Dom.Fetch ("tree.json", TreeData) }
        span { Dom.Fetch ("__window?" + string m.Asked, Sized) }
        div {
            if m.Tab = "tree" then
                tooltip m
        }
        header {
            h1 { "PoEtry" }
            div { Dom.Class "title"; Dom.Attr ("data-check", "title"); m.Title }
            nav {
                tab m "character" "Character"
                tab m "tree" "Passive tree"
            }
            if m.Tab = "tree" then
                div { Dom.Class "points"; Dom.Attr ("data-check", "points"); points m }
            div { Dom.Attr ("data-check", "pointer"); Dom.Style [ Css.Display "none" ]; sprintf "%.0fx%.0f touches %d" m.Page.X m.Page.Y m.Touches.Length }
            if not m.Dps.IsEmpty then
                div { Dom.Class "headline"; Dom.Attr ("data-check", "dps"); m.Dps.Head.Value + " DPS" }
            div { Dom.Class "status"; Dom.Attr ("data-check", "status"); status m }
        }
        if m.Tab = "character" then
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
