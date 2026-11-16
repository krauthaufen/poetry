/// PoEtry's front end: a pure FableCube application. The simulator runs in a worker behind
/// urls (`__sim?op=...`, answered by the bridge in index.html), so asking it is a `Dom.Fetch`
/// and its answers are messages. `?build=<url>` names the character to load.
module App

open Aardvark.Portable
open FableCube
open Pure

type Row = { Label : string; Value : string }
/// A socketed gem (`Key`: how the simulator is told to leave it out) and a group of them.
/// (`At`: its place in its group, `Socketed`: its level before what gear and supports add,
/// `Gives`: the percent of the damage lost without it, as an analysis found; not a number: unknown.)
type Gem = { Key : string; Named : string; Level : int; Quality : int; Support : bool; Icon : string; Own : string; Spirit : float; Persistent : bool; At : int; Socketed : int; Gives : float; Minions : int; Reserved : float }
type GemGroup = { Slot : string; Number : int; Gems : Gem list }

/// An item of the inventory: where it is worn (`Place`: its number, as "unequip" names it;
/// `Col`: which of several in one slot, the flasks and charms), what it says, its picture.
/// (`Lost`: the percent of the damage lost without the line; not a number: unknown.)
type ItemLine = { Mark : string; Reads : string; Lost : float }
type Socketed = { Rune : string; Pic : string; Does : string list }
type Granted = { Grant : string; Shows : string }
type Item =
    { Place : int; Worn : string; Col : int; Headed : string; Type : string; Grade : string; Picture : string
      Says : string list; Requires : string; Lines : ItemLine list; Holds : Socketed list; Grants : Granted list; Spoiled : bool
      /// A pasted item: its text as the game copies it and its item class ("": the character's own).
      Class : string; Raw : string
      /// The percent of the damage lost without the item (not a number: unknown).
      Weighs : float }
/// A gem that can be socketed: what it is, and whether it supports a skill of the group it is offered for.
type Offer = { Choice : string; Supports : bool; Colour : int; Tags : string; Tells : string; Fits : bool; Adds : float }
/// What is pressed in the fight: the gem, when, its picture.
type Pressed = { Skill : string; How : string; Face : string; Code : string }

type Share = { Name : string; Percent : float; Text : string; Part : bool }

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
      /// Only there once these passives are allocated (the Oracle's "The Unseen Path").
      Unlock : int list
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
/// (`Inner`, `Outer`: its radius in tree units, 0 for none; `Leap`: passives in it can be
/// allocated without being connected; `Centres`: the keystones it lies around, not its socket.)
type Jewel = { Called : string; Base : string; Rarity : string; Mods : string list; Sprite : string; SpriteRect : V4d; Inner : float; Outer : float; Leap : bool; Centres : string list }

/// An area of the tree: around a point, from `Near` to `Far`; `Free`: passives in it can be
/// allocated without being connected (`Keystones`: keystones too).
type Area = { Middle : V2d; Near : float; Far : float; Free : bool; Keystones : bool }

/// A connection: its two passives and its ribbon (triangles); an ascendancy's own are laid
/// out for the middle of the tree.
/// (`Needs`: the passives that must be allocated for it to be there at all, with its thin
/// ribbon for the web of all connections; the web itself holds the others'.)
type Link = { A : int; B : int; Of : string; Ribbon : V3f list; Needs : int list; Thin : V3f list }

/// An ascendancy: where its part of the tree is laid out (the middle of its background), and
/// its class art in the small background sheet.
type Ascendancy = { Called : string; Middle : V2d; Art : string }

/// A fight as it is shown: a mark of a skill use ("r": an area at x, y with a radius, so
/// long after the use; "f": a projectile's way, x and y in turn; "l": a line), something
/// that happened ("u": a use, "s": an enemy is hit, "m": an enemy is at X, Y now, "h": an
/// enemy hits the player, "k": an enemy dies), an enemy as the fight begins, and one colour's
/// triangles of the picture.
type Shape = { Sort : string; Nums : float list }
type Happening = { Kind : string; T : float; Who : string; Shapes : Shape list; X : float; Y : float }
/// (`From`: when it comes into the fight; below zero: there from the beginning)
type Foe = { Called : string; Start : V2d; Radius : float; Rank : string; From : float }
type Layer = { Color : V4d; Points : Buffer<V3f>; Uvs : Buffer<V2f> }
/// Who fights the enemies: the player, or a minion (one that fights in melee stands at the
/// enemy; a companion is a monster of its own size).
type Ally = { Named : string; Minion : bool; Melee : bool; Body : float; Companion : bool }

type Msg =
    | Located of string
    /// A frame of the fight's picture was drawn, at this time in seconds.
    | Tick of float
    /// Another scenario's fights are done (asked ahead, so that choosing it shows them at once).
    | Warmed of Fetched
    | Sheet of Fetched
    | Fought of Fetched
    | Scenario of string
    /// The text searched in the tree.
    | Seek of string
    /// A count the character begins the fight with (below zero: as the fight decides).
    | Begin of string * int
    | Switch of string
    /// A gem is turned off, or on again.
    | Socket of string
    /// The gem whose tooltip stays ("": none).
    | Pin of string
    /// A change of the gems (see the simulator's `Report.editGems`), the last one taken back;
    /// the list of gems to choose from opens (for what: "add:<group>", "swap:<group>:<gem>",
    /// "new"; "": closes), arrives, is searched; a gem of it is chosen; they are put in order
    /// of what they do to the damage.
    | GemEdit of string
    | UndoGems
    | Pick of string
    | Picked of Fetched
    | Filter of string
    | Choose of string
    | Rank
    /// What every line and gem is worth is asked for, and comes back bit by bit.
    | Analyse
    | Analysed of Fetched
    /// How many minions each gem should have is asked for (the best of every full split of the Spirit).
    | Tune
    | Tuned of Fetched
    /// What is pressed is put in another order: ("up" | "down" | "drop" | "add" | "auto", the gem).
    | Order of string * string
    /// The enemies ("level" | "eres" | "cres", the value typed) and lines of modifiers of one's own.
    | Enemy of string * string
    | Lines of string
    | Apply
    /// Where the character's numbers come from: the list arrives, is searched.
    | Sources of Fetched
    | Among of string
    /// What was changed here is kept in the browser (by character) and comes back on the next visit.
    | Saved of Fetched
    /// A character from outside the account: what is pasted (a Path of Building code, a pobb.in
    /// link, a poe.ninja character's address), it is opened, what came of it.
    | Link of string
    | Import
    | Imported of Fetched
    /// Share the build as it is set up: a short address anyone can open.
    | Share
    | Shared of Fetched
    | Unshare
    /// The game account (where the page is hosted for it): who is logged in and their
    /// characters arrive; logging in and out; another character is opened.
    | Account of Fetched
    | Login
    | Logout
    | Open of string
    | Restore of Fetched
    | Note of string
    /// The inventory: the box to paste an item into opens or closes, what is typed there, the
    /// item is added; where the pinned one is tried; a pasted item is worn (or taken off
    /// again), or thrown away.
    | Paste
    | Draft of string
    | Add
    | Aim of string
    | Wear of int
    | Discard of int
    /// What changing the pinned passive does to the damage came back.
    | Compared of Fetched
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
    /// The tree's control was drawn at this size (and whenever it changes).
    | Sized of ControlSize
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
      /// The defence page: its panels by title, and how the shown fight went for the character.
      Guards : (string * Row list) list
      Survival : Row list
      Defences : Row list
      Resistances : Row list
      Minions : Row list
      NotUnderstood : int
      /// The fight: what is pressed, its headline, and who dealt the damage.
      Fighting : bool
      Gem : string
      Dps : Row list
      Shares : Share list
      /// The character's socket groups, and the gems turned off here.
      Groups : GemGroup list
      Items : Item list
      /// Items pasted in (`Bag`), those of them worn instead of the character's own (by where:
      /// a place of the doll, or "at:<place>" for a jewel's), where the pinned one is tried
      /// (`Aim`, "": the first place it fits), and the box to paste one into.
      Bag : Item list
      Swapped : Map<string, Item>
      /// (`Swapped` as a list and as the simulator is told it: made in the update, a map read
      /// in the view went wrong there)
      Wearing : (string * Item) list
      Equip : string
      Aim : string
      Pasting : bool
      Draft : string
      /// What the pinned passive's change does (scenario, percent more damage), and the question that was answered.
      /// (scenario, percent more damage, and how far that may be off by chance)
      Deltas : (string * float * float) list
      /// The question about the pinned passive ("": none), asked at once and again for more
      /// fights: worked out when what it depends on changes, not whenever it is looked at.
      Ask : string
      AskMore : string
      DeltaOf : string
      Dropped : Set<string>
      /// The changes made to the gems here; the list of gems to choose from (for what, all of
      /// them, what is searched, those shown).
      GemEdits : string list
      Picker : string
      Offers : Offer list
      Wanted : string
      Offered : Offer list
      /// The analysis: its question ("": none), which asking this is, whether more is coming,
      /// how far it is, and what is known (key, percent of the damage lost without it).
      Probe : string
      ProbeRound : int
      Probing : bool
      /// The question of the best minion counts that is out ("": none), and what came of the last.
      Tuning : string
      TuneSaid : string
      Probed : string
      Worth : (string * float) list
      /// What is pressed as the fight had it, the order chosen here ([]: by skill type), and
      /// what else could be pressed.
      Play : Pressed list
      Rotation : string list
      Pressable : string list
      /// What a condition of pressing can be about: states an enemy can have, skills that leave something standing.
      States : string list
      Standing : string list
      /// The skill whose conditions are being changed ("": none).
      Editing : string
      /// The enemies as said here ("": as they are), and lines of modifiers of one's own (as
      /// typed, and as applied).
      EnemyLevel : string
      EnemyEle : string
      EnemyChaos : string
      CustomDraft : string
      Custom : string
      /// Where the numbers come from: by what is changed, searched, those shown.
      Calcs : (string * Row list) list
      /// What is kept between visits as text, whether what was kept has been read, and notes
      /// (as typed, and as they were when read: what the box shows to begin with).
      /// The game account: whether one can log in here, who is logged in (""), their
      /// characters (name, what they are, the url of each), and where the page is going
      /// ("login", "logout", "open&name=..": asked of the bridge, which goes there).
      CanLogin : bool
      /// Whether the account's answer has come (until then nothing is said about logging in).
      Asked : bool
      Player : string
      Characters : (string * string * string) list
      /// The same by league (its name, how many, its characters): the running leagues, shown
      /// open, and the lasting ones (Standard), shown folded.
      Running : (string * string * (string * string) list) list
      Lasting : (string * string * (string * string) list) list
      Going : string
      Kept : string
      Restored : bool
      Notes : string
      NotesWere : string
      /// What an analysis found a passive worth (percent of the damage lost without it).
      NodeWorth : Map<int, float>
      Among : string
      CalcShown : (string * Row list) list
      Pinned : string
      /// The passives the items allocate (an anoint, a jewel): shown as allocated, not to be refunded.
      Anointed : Set<int>
      /// The damage by damage type.
      Types : Share list
      /// What the character has a count of going into a fight (name, most), the counts said
      /// here, and the switches that are on.
      Counts : (string * int) list
      Start : Map<string, int>
      Switches : Set<string>
      /// What was pasted to open a character from outside the account, the question that opens
      /// it while it is asked, and why it could not be opened.
      Link : string
      Importing : string
      ImportSaid : string
      /// The request that shares the build, its address once there, or why not.
      Sharing : string
      ShareLink : string
      ShareSaid : string
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
      /// The page's size in its own pixels (0 until the tree's control has said its size).
      Page : V2d
      /// A click that could not be placed yet (the page's size was not known).
      Pending : V2d option
      /// What an allocated passive with a choice was chosen to be ("Strength", ...).
      Choices : Map<int, string>
      /// The passive whose tooltip is pinned (clicked), and the one under the mouse.
      /// The pictures of jewels in sockets, by unique name or base type; the character's jewels by socket.
      JewelSprites : Map<string, string * V4d>
      Jewels : Map<int, Jewel>
      /// The passives that are only there once others are allocated, with those others; and
      /// the connections of the ones that are there now, as a mesh.
      Unlocks : Map<int, int list>
      Opened : Buffer<V3f>
      OpenedUv : Buffer<V2f>
      /// The radii of the jewels in allocated sockets, as a mesh.
      Rings : Buffer<V3f>
      /// What pressing the button of the passive looked at would do: the ways and passives
      /// it would take (`Gain`) or give back (`Loss`), and how many passives that is (negative:
      /// given back).
      Gain : Buffer<V3f>
      GainUv : Buffer<V2f>
      Loss : Buffer<V3f>
      LossUv : Buffer<V2f>
      Change : int
      /// What is searched in the tree, how many passives have it, and the rings around them.
      Sought : string
      Hits : int
      Found : Buffer<V3f>
      FoundUv : Buffer<V2f>
      RingsUv : Buffer<V2f>
      /// The gem the fights after a change of passives press: the one the first fight found
      /// strongest ("" before; finding it costs as much as the fight itself).
      Pressing : string
      /// The fight shown: its enemies, who fights them (name, whether a minion), what
      /// happened, how long it took (seconds of the fight), how wide the field is; when its
      /// showing began (-1: with the next frame), the picture now; and the scenarios whose
      /// fights were asked ahead.
      Foes : Foe list
      Allies : Ally list
      Happened : Happening list
      Lasts : float
      Field : float
      /// (the middle of what is shown: between the player and the enemies)
      Middle : V2d
      Began : float
      /// Where the minions are shown now, and the fight's time that was shown last.
      Stands : Map<string, V2d>
      Clock : float
      Stage : Layer list
      Warm : string list
      /// The fight is asked again while runs are still out: which asking this is, how many
      /// runs the numbers shown are of, and whether more are coming.
      Round : int
      Runs : int
      More : bool
      Selected : int option
      Hovered : int option
      Level : int }

let init : Model =
    { Build = ""; Scenario = "pack"; Status = "locating"; Title = ""; Class = ""; Ascendancy = ""; Tab = "character"
      Pools = []; Guards = []; Survival = []; Defences = []; Resistances = []; Minions = []
      NotUnderstood = 0; Fighting = false; Gem = ""; Dps = []; Shares = []; Groups = []; Items = []; Bag = []; Swapped = Map.empty; Wearing = []; Equip = ""; Aim = ""; Pasting = false; Draft = ""; Dropped = Set.empty; GemEdits = []; Picker = ""; States = []; Standing = []; Editing = ""; Offers = []; Wanted = ""; Offered = []; Probe = ""; ProbeRound = 0; Probing = false; Tuning = ""; TuneSaid = ""; Probed = ""; Worth = []; Play = []; Rotation = []; Pressable = []; EnemyLevel = ""; EnemyEle = ""; EnemyChaos = ""; CustomDraft = ""; Custom = ""; Calcs = []; CanLogin = false; Asked = false; Player = ""; Characters = []; Running = []; Lasting = []; Going = ""; Kept = ""; Restored = false; Notes = ""; NotesWere = ""; NodeWorth = Map.empty; Among = ""; CalcShown = []; Pinned = ""; Deltas = []; DeltaOf = ""; Ask = ""; AskMore = ""; Anointed = Set.empty; Types = []; Counts = []; Start = Map.empty; Switches = Set.empty; Link = ""; Importing = ""; ImportSaid = ""; Sharing = ""; ShareLink = ""; ShareSaid = ""
      Passives = [||]; Ascendancies = []; Shown = []; AscendancyWeb = Data.pack (Data.ofList ([] : V3f list)); Path = Data.pack (Data.ofList ([] : V3f list))
      Loaded = false; Capacity = 0
      WebUv = Data.pack (Data.ofList ([] : V2f list)); AscendancyWebUv = Data.pack (Data.ofList ([] : V2f list)); PathUv = Data.pack (Data.ofList ([] : V2f list)); Art = ""; Links = [||]; Neighbours = Map.empty; Web = Data.pack (Data.ofList ([] : V3f list)); Allocated = Set.empty; Edited = false
      Centre = V2d (0.0, 0.0); Height = 9000.0
      Camera = { Sky = V3d (0.0, 1.0, 0.0); Location = V3d (0.0, 0.0, 7794.2); Forward = V3d (0.0, 0.0, -1.0); Up = V3d (0.0, 1.0, 0.0); Right = V3d (1.0, 0.0, 0.0) }
      Near = 7000.0; Far = 8600.0
      Touches = []; Travel = 0.0; Page = V2d (0.0, 0.0); Pending = None; Choices = Map.empty; JewelSprites = Map.empty; Jewels = Map.empty; Unlocks = Map.empty; Opened = Data.pack (Data.ofList ([] : V3f list)); OpenedUv = Data.pack (Data.ofList ([] : V2f list)); Sought = ""; Hits = 0; Found = Data.pack (Data.ofList ([] : V3f list)); FoundUv = Data.pack (Data.ofList ([] : V2f list)); Gain = Data.pack (Data.ofList ([] : V3f list)); GainUv = Data.pack (Data.ofList ([] : V2f list)); Loss = Data.pack (Data.ofList ([] : V3f list)); LossUv = Data.pack (Data.ofList ([] : V2f list)); Change = 0; Rings = Data.pack (Data.ofList ([] : V3f list)); RingsUv = Data.pack (Data.ofList ([] : V2f list)); Pressing = ""; Foes = []; Allies = []; Happened = []; Lasts = 0.0; Field = 100.0; Middle = V2d (0.0, 0.0); Began = -1.0; Stands = Map.empty; Clock = 0.0; Stage = []; Warm = []; Round = 0; Runs = 0; More = false; Selected = None; Hovered = None; Level = 0 }

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
        "&passives=" + (Set.difference m.Allocated m.Anointed |> Set.toList |> List.map string |> String.concat ",")
        + "&choices=" + (m.Choices |> Map.toList |> List.filter (fun (i, _) -> m.Allocated.Contains i) |> List.map (fun (i, c) -> string i + ":" + c) |> String.concat ",")
    else ""

/// Where an item is worn on the doll ("": not on it, a jewel).
let private area (i : Item) : string =
    match i.Worn with
    | "Weapon 1" -> "Weapon" | "Weapon 2" -> "Offhand" | "Helmet" -> "Helm" | "Body Armour" -> "Body"
    | "Amulet" -> "Amulet" | "Gloves" -> "Gloves" | "Belt" -> "Belt" | "Boots" -> "Boots"
    | "Ring 1" -> "Ring1" | "Ring 2" -> "Ring2"
    | "Flask 1" -> "Flask1" | "Flask 2" -> "Flask2" | "Charm 1" -> "Charm1" | "Charm 2" -> "Charm2" | "Charm 3" -> "Charm3"
    | "Flask" -> (match i.Col with | 0 -> "Flask1" | 1 -> "Flask2" | 2 -> "Charm1" | 3 -> "Charm2" | 4 -> "Charm3" | _ -> "")
    | _ -> ""

let private places : (string * string) list =
    [ "Weapon", "Weapon"; "Offhand", "Offhand"; "Helm", "Helmet"; "Body", "Body"; "Amulet", "Amulet"; "Gloves", "Gloves"; "Belt", "Belt"; "Boots", "Boots"
      "Ring1", "Ring"; "Ring2", "Ring"; "Flask1", "Flask"; "Flask2", "Flask"; "Charm1", "Charm"; "Charm2", "Charm"; "Charm3", "Charm" ]

/// The simulator's name of a place of the doll.
let private slotOf (spot : string) : string =
    match spot with
    | "Weapon" -> "Weapon 1" | "Offhand" -> "Weapon 2" | "Helm" -> "Helmet" | "Body" -> "Body Armour" | "Ring1" -> "Ring 1" | "Ring2" -> "Ring 2"
    | other -> other

/// A text as part of an address.
let private esc (t : string) : string =
    t.Replace("\r", "").Replace("%", "%25").Replace("+", "%2B").Replace("&", "%26").Replace("#", "%23").Replace("\n", "%0A").Replace(" ", "%20")

/// Pasted items worn, as the simulator is told them: for each where (the place among the
/// character's items of the one it replaces, or the slot's name if that is empty) and its text.
let private equipText (items : Item list) (swapped : Map<string, Item>) : string =
    swapped |> Map.toList
    |> List.map (fun (spot, it) ->
        let where =
            if spot.StartsWith "at:" then spot.Substring 3
            else match items |> List.tryFind (fun i -> area i = spot) with | Some i -> string i.Place | None -> slotOf spot
        where + "\n" + it.Raw)
    |> String.concat "\n====\n"

/// A pasted item, read from the text the game (or the trade site) copies: item class, rarity,
/// name and base, then its properties and its modifiers in parts.
let rec private pasted (place : int) (text : string) : Item option =
  // (as plain text off the trade site: name, base and the kind of item on lines of their own,
  // then properties and modifiers with nothing between them: put into the game's form first)
  let plain = text.Replace("\r", "").Split('\n') |> List.ofArray |> List.map (fun l -> l.Trim ()) |> List.filter (fun l -> l <> "")
  if not (text.Contains "Rarity:") && not (text.Contains "--------") && plain.Length >= 2 then
    let about (l : string) = l.Contains ": " && not (l.StartsWith "Bonded:") && not (l.StartsWith "Grants")
    let isName (l : string) = not (about l) && not (l |> Seq.exists System.Char.IsDigit)
    let kinds = [ "Helmet"; "Body Armour"; "Gloves"; "Boots"; "Belt"; "Amulet"; "Ring"; "Shield"; "Buckler"; "Focus"; "Quiver"; "Jewel"; "Wand"; "Sceptre"; "Spear"; "Flail"; "Dagger"; "Claw"; "Quarterstaff"; "Staff"; "Bow"; "Crossbow"; "One Hand Mace"; "Two Hand Mace"; "One Hand Sword"; "Two Hand Sword"; "One Hand Axe"; "Two Hand Axe" ]
    let names = plain |> List.takeWhile isName
    let rest = plain |> List.skip names.Length
    // one name: a magic or normal item's; two: name and base; a third line says what it is
    let headed, based, kind =
        match names with
        | [ a ] -> "", a, ""
        // (two: a rare's name and base, or a base and what it is)
        | [ a; b ] -> if List.contains b kinds then "", a, b else a, b, ""
        | a :: b :: c :: _ -> a, b, c
        | [] -> "", "", ""
    let ends (w : string) = based.EndsWith w
    let has (w : string) = rest |> List.exists (fun l -> l.StartsWith w)
    let cls =
        match kind with
        | "Helmet" -> "Helmets" | "Body Armour" -> "Body Armours" | "Gloves" -> "Gloves" | "Boots" -> "Boots" | "Belt" -> "Belts" | "Amulet" -> "Amulets" | "Ring" -> "Rings"
        | "Shield" -> "Shields" | "Buckler" -> "Bucklers" | "Focus" -> "Foci" | "Quiver" -> "Quivers" | "Jewel" -> "Jewels"
        | "Wand" -> "Wands" | "Sceptre" -> "Sceptres" | "Spear" -> "Spears" | "Flail" -> "Flails" | "Dagger" -> "Daggers" | "Claw" -> "Claws"
        | "" ->
            if ends "Ring" then "Rings" elif ends "Amulet" then "Amulets" elif ends "Belt" || ends "Sash" then "Belts"
            elif ends "Ruby" || ends "Emerald" || ends "Sapphire" || ends "Diamond" then "Jewels"
            elif has "Attacks per Second:" then "Weapon"
            elif has "Armour:" || has "Evasion Rating:" || has "Energy Shield:" then "Armour"
            else "Weapon"
        | other -> other
    if based = "" then None
    else
        let rarity = if headed <> "" then "Rare" else "Magic"
        let canonical =
            [ "Item Class: " + cls; "Rarity: " + rarity ] @ (if headed <> "" then [ headed ] else []) @ [ based; "--------" ]
            @ (rest |> List.filter about) @ [ "--------" ] @ (rest |> List.filter (fun l -> not (about l)))
        pasted place (String.concat "\n" canonical)
  else
    let lines = text.Replace("\r", "").Split('\n') |> List.ofArray |> List.map (fun l -> l.Trim ()) |> List.filter (fun l -> l <> "")
    let rec parts (acc : string list list) (cur : string list) (ls : string list) =
        match ls with
        | [] -> List.rev (List.rev cur :: acc)
        | l :: rest when l.StartsWith "----" -> parts (List.rev cur :: acc) [] rest
        | l :: rest -> parts acc (l :: cur) rest
    match parts [] [] lines |> List.filter (fun p -> not p.IsEmpty) with
    | head :: rest ->
        let value (k : string) = head |> List.tryFind (fun l -> l.StartsWith k) |> Option.map (fun l -> l.Substring(k.Length).Trim ()) |> Option.defaultValue ""
        let names = head |> List.filter (fun l -> not (l.StartsWith "Item Class:") && not (l.StartsWith "Rarity:"))
        let headed, based = match names with | [ a ] -> "", a | a :: b :: _ -> a, b | [] -> "", ""
        let about (p : string list) = p |> List.forall (fun l -> (l.Contains ": " && not (l.StartsWith "Grants")) || l.StartsWith "Requires" || l.EndsWith ":")
        let marks = [ "implicit"; "rune"; "enchant"; "crafted"; "fractured"; "desecrated"; "mutated" ]
        let line (l : string) =
            match marks |> List.tryFind (fun k -> l.EndsWith (" (" + k + ")")) with
            | Some k -> { Mark = k; Reads = l.Substring (0, l.Length - k.Length - 3); Lost = nan }
            | None -> { Mark = "explicit"; Reads = l; Lost = nan }
        if based = "" then None
        else
            Some { Place = place; Worn = ""; Col = 0; Headed = headed; Type = based; Grade = (value "Rarity:").ToLower (); Picture = ""
                   Says = rest |> List.filter about |> List.concat; Requires = ""
                   Lines = rest |> List.filter (fun p -> not (about p) && p <> [ "Corrupted" ]) |> List.concat |> List.map line
                   Holds = []; Grants = []; Spoiled = rest |> List.contains [ "Corrupted" ]; Class = value "Item Class:"; Raw = text; Weighs = nan }
    | [] -> None

/// How the character goes into the fight, as the simulator is told it.
let private knobs (m : Model) =
    (if m.Start.IsEmpty then "" else "&start=" + (m.Start |> Map.toList |> List.map (fun (k, v) -> k + ":" + string v) |> String.concat ","))
    + (m.Switches |> Set.toList |> List.map (fun k -> "&" + k + "=1") |> String.concat "")
    + (if m.Dropped.IsEmpty then "" else "&without=" + (m.Dropped |> Set.toList |> String.concat ",").Replace (" ", "%20"))
    + (if m.Equip = "" then "" else "&equip=" + m.Equip)
    + (if m.GemEdits.IsEmpty then "" else "&gems=" + esc (String.concat ";" m.GemEdits))
    + (if m.Custom = "" then "" else "&custom=" + esc m.Custom)

/// How the fight goes, as the simulator is told it: what is pressed, who the enemies are.
let private fightKnobs (m : Model) =
    (if m.Rotation.IsEmpty then "" else "&rotation=" + esc (String.concat "|" m.Rotation))
    + (if m.EnemyLevel = "" then "" else "&level=" + esc m.EnemyLevel)
    + (if m.EnemyEle = "" then "" else "&eres=" + esc m.EnemyEle)
    + (if m.EnemyChaos = "" then "" else "&cres=" + esc m.EnemyChaos)

/// A question that is answered at once with nothing (for a fetch that is not wanted).
let private nothing (m : Model) = "__sim?op=fight&scenario=&build=" + m.Build

/// Where a pasted item can be worn: places of the doll, or a jewel's ("at:<place>").
let private fits (m : Model) (i : Item) : string list =
    match i.Class with
    | "Helmets" -> [ "Helm" ] | "Body Armours" -> [ "Body" ] | "Gloves" -> [ "Gloves" ] | "Boots" -> [ "Boots" ] | "Belts" -> [ "Belt" ] | "Amulets" -> [ "Amulet" ]
    | "Rings" -> [ "Ring1"; "Ring2" ]
    | "Shields" | "Bucklers" | "Foci" | "Quivers" -> [ "Offhand" ]
    | "Jewels" -> m.Items |> List.filter (fun x -> x.Worn.StartsWith "Jewel") |> List.map (fun x -> "at:" + string x.Place)
    | "" | "Life Flasks" | "Mana Flasks" | "Charms" -> []
    // (armour that does not say which piece it is: any of them)
    | "Armour" -> [ "Helm"; "Body"; "Gloves"; "Boots" ]
    | c when c.StartsWith "One Hand" || c = "Wands" || c = "Sceptres" || c = "Daggers" || c = "Claws" || c = "Flails" || c = "Spears" -> [ "Weapon"; "Offhand" ]
    | c when c.Contains "Gem" || c.Contains "Currency" || c.Contains "Relic" || c.Contains "Tablet" || c.Contains "Waystone" -> []
    | _ -> [ "Weapon" ]

/// Where the pinned pasted item is tried: where it was aimed, or the first place it fits.
let private aimOf (m : Model) (i : Item) : string =
    let f = fits m i
    if List.contains m.Aim f then m.Aim else f |> List.tryHead |> Option.defaultValue ""

/// What a place is called: the item there, or the place itself.
let private spotName (m : Model) (spot : string) : string =
    let there = if spot.StartsWith "at:" then m.Items |> List.tryFind (fun i -> "at:" + string i.Place = spot) else m.Items |> List.tryFind (fun i -> area i = spot)
    match there with
    | Some i -> (if i.Headed <> "" then i.Headed else i.Type) + (if spot = "Ring1" then " (left)" elif spot = "Ring2" then " (right)" else "")
    | None -> slotOf spot + " (empty)"

let private sheetUrl (m : Model) = "__sim?op=character" + knobs m + passives m + "&build=" + m.Build
let private fightOf (m : Model) (scenario : string) (ask : string) = "__sim?op=fight&runs=96" + (if scenario = "pack" then "&time=8" elif scenario = "endless" then "&time=30&least=4" else "&time=30") + ask + "&scenario=" + scenario + (if m.Pressing <> "" then "&gem=" + m.Pressing.Replace (" ", "%20") else "") + fightKnobs m + knobs m + passives m + "&build=" + m.Build
let private fightUrl (m : Model) = fightOf m m.Scenario ("&round=" + string m.Round)

let private scenarios = [ "pack"; "boss"; "pinnacle"; "endless" ]

/// The scenario whose fights are asked ahead now: the first not shown and not asked yet,
/// once the shown one's fights are all in.
let private warming (m : Model) : string =
    if m.Fighting || m.More || m.Runs = 0 then ""
    else scenarios |> List.tryFind (fun s -> s <> m.Scenario && not (List.contains s m.Warm)) |> Option.defaultValue ""

// ---- the fight's picture ----

/// How much of the fight a second of showing covers: all of it, the fight is shown as fast
/// as it is fought.
let private pace (m : Model) = 1.0

let private disc (c : V2d) (r : float) (z : float) : V3f list =
    let n = 20
    let pt (i : int) = let a = 6.283185307 * float i / float n in V3f (float32 (c.X + r * cos a), float32 (c.Y + r * sin a), float32 z)
    let mid = V3f (float32 c.X, float32 c.Y, float32 z)
    [ for i in 0 .. n - 1 do
        yield! [ mid; pt i; pt (i + 1) ] ]

let private band (c : V2d) (r : float) (width : float) (z : float) : V3f list =
    let n = 48
    let pt (i : int) (rad : float) = let a = 6.283185307 * float i / float n in V3f (float32 (c.X + rad * cos a), float32 (c.Y + rad * sin a), float32 z)
    [ for i in 0 .. n - 1 do
        let a0, a1, b0, b1 = pt i (r - width / 2.0), pt (i + 1) (r - width / 2.0), pt i (r + width / 2.0), pt (i + 1) (r + width / 2.0)
        yield! [ a0; b0; a1; a1; b0; b1 ] ]

let private stroke (a : V2d) (b : V2d) (width : float) (z : float) : V3f list =
    let dx, dy = b.X - a.X, b.Y - a.Y
    let l = sqrt (dx * dx + dy * dy)
    if l < 1e-6 then []
    else
        let nx, ny = -dy / l * width / 2.0, dx / l * width / 2.0
        let p (x : float) (y : float) = V3f (float32 x, float32 y, float32 z)
        [ p (a.X + nx) (a.Y + ny); p (a.X - nx) (a.Y - ny); p (b.X + nx) (b.Y + ny); p (b.X + nx) (b.Y + ny); p (a.X - nx) (a.Y - ny); p (b.X - nx) (b.Y - ny) ]

/// A way of points up to a share of its length, and where that is.
let rec private along (points : V2d list) (left : float) (width : float) (z : float) : V3f list * V2d =
    match points with
    | a :: b :: rest ->
        let l = sqrt ((b.X - a.X) * (b.X - a.X) + (b.Y - a.Y) * (b.Y - a.Y))
        if l >= left then
            let k = if l < 1e-6 then 0.0 else left / l
            let at = V2d (a.X + (b.X - a.X) * k, a.Y + (b.Y - a.Y) * k)
            stroke a at width z, at
        else
            let more, at = along (b :: rest) (left - l) width z
            stroke a b width z @ more, at
    | [ a ] -> [], a
    | [] -> [], V2d (0.0, 0.0)

let rec private pairs (xs : float list) : V2d list =
    match xs with
    | x :: y :: rest -> V2d (x, y) :: pairs rest
    | _ -> []

let private lengthOf (points : V2d list) : float =
    List.pairwise points |> List.sumBy (fun (a, b) -> sqrt ((b.X - a.X) * (b.X - a.X) + (b.Y - a.Y) * (b.Y - a.Y)))

/// The picture of the fight at a time of it: the enemies where they are (by rarity), the
/// player in the middle with the minions around (they have no place of their own in the
/// simulation), the marks of the uses just made (the player's, the minions'), the enemies'
/// hits on the player, and a flash on whoever was just hit.
let private stage (m : Model) (c : float) : Layer list * Map<string, V2d> =
    let speed = pace m
    let w = 0.45 * speed
    let u = m.Field / 110.0
    let gone =
        (m.Happened |> List.filter (fun h -> h.Kind = "k" && h.T <= c - w * 0.3) |> List.map (fun h -> h.Who))
        @ (m.Foes |> List.filter (fun f -> f.From > c) |> List.map (fun f -> f.Called))
        |> Set.ofList
    // where an enemy is: between the two places noted around this time, walking from the one
    // to the other (when the next was noted much later it stood still first: frozen, stunned)
    let noted =
        m.Happened |> List.fold (fun (p : Map<string, (float * V2d) * (float * V2d) option>) h ->
            if h.Kind <> "m" then p
            elif h.T <= c then Map.add h.Who ((h.T, V2d (h.X, h.Y)), None) p
            else
                match Map.tryFind h.Who p with
                | Some (was, None) -> Map.add h.Who (was, Some (h.T, V2d (h.X, h.Y))) p
                | _ -> p)
            (m.Foes |> List.map (fun f -> f.Called, ((0.0, f.Start), None)) |> Map.ofList)
    let place (name : string) =
        match Map.tryFind name noted with
        | Some ((_, a), None) -> a
        | Some ((t0, a), Some (t1, b)) ->
            let from = max t0 (t1 - 0.5)
            let k = if c <= from then 0.0 else (c - from) / max 1e-6 (t1 - from)
            V2d (a.X + (b.X - a.X) * k, a.Y + (b.Y - a.Y) * k)
        | None -> V2d (0.0, 0.0)
    let struck = m.Happened |> List.filter (fun h -> h.Kind = "s" && h.T <= c && h.T > c - w * 0.3) |> List.map (fun h -> h.Who) |> Set.ofList
    let foes (rank : string) = m.Foes |> List.filter (fun f -> f.Rank = rank && not (gone.Contains f.Called)) |> List.collect (fun f -> disc (place f.Called) f.Radius 1.0)
    let flashes = m.Foes |> List.filter (fun f -> struck.Contains f.Called) |> List.collect (fun f -> disc (place f.Called) (f.Radius * 0.55) 9.0)
    // The simulation gives minions no place. Shown where they would be: those that fight in
    // melee around the enemy nearest to the player (ring after ring, as many as fit), the
    // others in an arc beside the player.
    let alive = m.Foes |> List.filter (fun f -> not (gone.Contains f.Called))
    let focus = alive |> List.sortBy (fun f -> let p = place f.Called in p.X * p.X + p.Y * p.Y) |> List.tryHead
    let body (a : Ally) = if a.Body > 0.0 then max (u * 0.9) (a.Body * 0.7) else u * 1.1
    let melee, ranged = m.Allies |> List.filter (fun a -> a.Minion) |> List.partition (fun a -> a.Melee && focus.IsSome)
    let around =
        match focus with
        | None -> []
        | Some f ->
            let c = place f.Called
            let towards = atan2 (-c.Y) (-c.X)
            let widest = melee |> List.fold (fun w a -> max w (body a)) (u * 1.1)
            let rec rings (left : Ally list) (k : int) =
                if left.IsEmpty then []
                else
                    let r = f.Radius + widest * (1.2 + 2.2 * float k)
                    let fits = max 1 (int (6.283185307 * r / (widest * 2.2)))
                    let here = List.truncate fits left
                    (here |> List.mapi (fun i a ->
                        // (the first faces the player, the next ones to both sides of it)
                        let step = 6.283185307 / float (max fits here.Length)
                        let side = if i % 2 = 0 then 1.0 else -1.0
                        let ang = towards + side * step * float ((i + 1) / 2)
                        a, V2d (c.X + r * cos ang, c.Y + r * sin ang)))
                    @ rings (List.skip here.Length left) (k + 1)
            rings melee 0
    let minions = m.Allies |> List.filter (fun a -> a.Minion)
    let facing = match focus with Some f -> (let c = place f.Called in atan2 c.Y c.X) | None -> 0.0
    // (beside the player, to both sides of where it looks; `i` counts the minions that stand there)
    let home (i : int) =
        let side = if i % 2 = 0 then 1.0 else -1.0
        let ang = facing + side * (1.2 + 0.35 * float (i / 2))
        V2d (u * 7.0 * cos ang, u * 7.0 * sin ang)
    let beside = ranged |> List.mapi (fun i a -> a, home i)
    // A minion walks to its place, it is not there at once: from where it was shown last
    // (at the fight's start beside the player), no faster than a minion runs.
    let again = c < m.Clock
    let reach = 55.0 * max 0.0 (c - m.Clock)
    let placed =
        around @ beside |> List.map (fun (a, goal) ->
            let was =
                match (if again then None else Map.tryFind a.Named m.Stands) with
                | Some p -> p
                | None -> home (minions |> List.findIndex (fun x -> x.Named = a.Named))
            let dx, dy = goal.X - was.X, goal.Y - was.Y
            let l = sqrt (dx * dx + dy * dy)
            a, (if l <= reach || l < 1e-6 then goal else V2d (was.X + dx / l * reach, was.Y + dy / l * reach)))
    let stand (name : string) = placed |> List.tryFind (fun (a, _) -> a.Named = name) |> Option.map snd |> Option.defaultValue (V2d (0.0, 0.0))
    // (the player: a ring with a dot, so that it is no monster's disc)
    let self = band (V2d (0.0, 0.0)) (u * 2.4) (u * 0.8) 3.0 @ disc (V2d (0.0, 0.0)) (u * 1.0) 3.0
    let allies = (placed |> List.filter (fun (a, _) -> not a.Companion) |> List.collect (fun (a, p) -> disc p (body a) 2.0))
    let companions = placed |> List.filter (fun (a, _) -> a.Companion) |> List.collect (fun (a, p) -> disc p (body a) 2.0)
    let player = m.Allies |> List.tryFind (fun a -> not a.Minion) |> Option.map (fun a -> a.Named) |> Option.defaultValue ""
    let marks (own : bool) =
        m.Happened
        |> List.filter (fun h -> h.Kind = "u" && h.T <= c && h.T > c - w - 2.0 && (h.Who = player) = own)
        |> List.collect (fun h ->
            h.Shapes |> List.collect (fun s ->
                match s.Sort, s.Nums with
                | "r", [ x; y; r; delay ] ->
                    let p = (c - h.T - delay) / w
                    // the area's true size as a thin line, and a wave running out to it
                    if p < 0.0 || p > 1.0 then [] else band (V2d (x, y)) r (u * 0.3) 5.0 @ band (V2d (x, y)) (r * (0.3 + 0.7 * p)) (u * 0.9) 5.0
                | "f", nums ->
                    let p = (c - h.T) / w
                    if p < 0.0 || p > 1.0 then []
                    else
                        let way = pairs nums
                        let trail, head = along way (lengthOf way * p) (u * 0.35) 5.0
                        trail @ disc head (u * 0.9) 5.0
                | "l", [ x1; y1; x2; y2 ] ->
                    let p = (c - h.T) / (w * 0.5)
                    // (a minion's strike begins where the minion is shown, not at the player)
                    let from = if not own && x1 = 0.0 && y1 = 0.0 then stand h.Who else V2d (x1, y1)
                    if p < 0.0 || p > 1.0 then [] else stroke from (V2d (x2, y2)) (u * 0.5) 5.0
                | _ -> []))
    let hurts =
        m.Happened |> List.filter (fun h -> h.Kind = "h" && h.T <= c && h.T > c - w * 0.5)
        |> List.collect (fun h -> stroke (place h.Who) (V2d (0.0, 0.0)) (u * 0.6) 6.0)
    let layer (r : float) (g : float) (b : float) (points : V3f list) = { Color = V4d (r, g, b, 1.0); Points = Data.pack (Data.ofList points); Uvs = Data.pack (Data.ofList (points |> List.map (fun _ -> V2f (0.0f, 0.0f)))) }
    [ layer 0.55 0.2 0.18 (foes "normal")
      layer 0.4 0.45 0.95 (foes "magic")
      layer 0.95 0.95 0.4 (foes "rare")
      layer 0.8 0.42 0.14 (foes "unique")
      layer 0.9 0.86 0.62 allies
      layer 1.0 1.0 1.0 self
      layer 0.35 0.8 0.75 companions
      layer 0.4 0.78 1.0 (marks true)
      layer 0.45 0.85 0.5 (marks false)
      layer 1.0 0.22 0.18 hurts
      layer 1.0 1.0 1.0 flashes ],
    (placed |> List.map (fun (a, p) -> a.Named, p) |> Map.ofList)

/// The fight to show, out of a fight's answer (none: what is shown stays).
/// Damage by skill: a skill's lines (infused or not, each gem that triggers it) are one line,
/// and under it what it came from when that is more than one thing.
let private shares (lines : (string * float) list) : Share list =
    let skill (n : string) =
        let cut = n.IndexOf " ("
        (if cut < 0 then n else n.Substring(0, cut)).Replace(" infused", "")
    let from (n : string) =
        let a = n.IndexOf " ("
        let b = n.IndexOf " #"
        if a >= 0 && b > a then "triggered by " + n.Substring(a + 2, b - a - 2) else "pressed"
    let line part name v = { Name = name; Percent = v; Text = sprintf "%.1f%%" v; Part = part }
    lines
    |> List.groupBy (fun (n, _) -> skill n)
    |> List.map (fun (name, ls) -> name, List.sumBy snd ls, ls)
    |> List.sortByDescending (fun (_, v, _) -> v)
    |> List.filter (fun (_, v, _) -> v >= 0.5)
    |> List.collect (fun (name, v, ls) ->
        let parts =
            ls |> List.groupBy (fun (n, _) -> from n)
            |> List.map (fun (f, xs) -> f, List.sumBy snd xs)
            |> List.sortByDescending snd
        line false name v
        :: (if parts.Length > 1 then parts |> List.filter (fun (_, x) -> x >= 0.5) |> List.map (fun (f, x) -> line true f x) else []))

let private readReplay (j : Json) (m : Model) : Model =
    let r = Json.field "replay" j
    let events = Json.field "events" r |> Json.list
    if events.IsEmpty then m
    else
        let foes =
            Json.field "enemies" r |> Json.list
            |> List.map (fun e -> { Called = Json.string (Json.field "name" e); Start = V2d (Json.number (Json.field "x" e), Json.number (Json.field "y" e)); Radius = max 2.0 (Json.number (Json.field "size" e)); Rank = Json.string (Json.field "rarity" e); From = -1.0 })
        // (those that come into the fight later)
        let came =
            events |> List.choose (fun e ->
                match Json.list e with
                | [ k; t; who; x; y; size; rarity ] when Json.string k = "e" ->
                    Some { Called = Json.string who; Start = V2d (Json.number x, Json.number y); Radius = max 2.0 (Json.number size); Rank = Json.string rarity; From = Json.number t }
                | _ -> None)
        let happened =
            events |> List.map (fun e ->
                match Json.list e with
                | k :: t :: who :: rest ->
                    let kind = Json.string k
                    let shapes =
                        match kind, rest with
                        | "u", [ _; marks ] -> Json.list marks |> List.map (fun s -> match Json.list s with | sort :: nums -> { Sort = Json.string sort; Nums = nums |> List.map Json.number } | [] -> { Sort = ""; Nums = [] })
                        | _ -> []
                    let x, y = (match kind, rest with | "m", [ x; y ] -> Json.number x, Json.number y | _ -> 0.0, 0.0)
                    { Kind = kind; T = Json.number t; Who = Json.string who; Shapes = shapes; X = x; Y = y }
                | _ -> { Kind = ""; T = 0.0; Who = ""; Shapes = []; X = 0.0; Y = 0.0 })
        // the field: the player (with room for the minions beside) and the enemies, a margin around
        let low (pick : V2d -> float) = foes |> List.fold (fun d f -> min d (pick f.Start - f.Radius)) -14.0
        let high (pick : V2d -> float) = foes |> List.fold (fun d f -> max d (pick f.Start + f.Radius)) 14.0
        // (and the areas of what is used, so that a slam around an enemy is seen whole; not
        // where projectiles fly off to, and no area counts for more than 6 m across)
        let rings = happened |> List.collect (fun h -> h.Shapes |> List.choose (fun s -> match s.Sort, s.Nums with | "r", [ x; y; r; _ ] -> Some (x, y, min r 60.0) | _ -> None))
        let x0 = rings |> List.fold (fun d (x, _, r) -> min d (x - r)) (low (fun p -> p.X))
        let x1 = rings |> List.fold (fun d (x, _, r) -> max d (x + r)) (high (fun p -> p.X))
        let y0 = rings |> List.fold (fun d (_, y, r) -> min d (y - r)) (low (fun p -> p.Y))
        let y1 = rings |> List.fold (fun d (_, y, r) -> max d (y + r)) (high (fun p -> p.Y))
        { m with
            Foes = foes @ came
            Allies =
                Json.field "actors" r |> Json.list
                |> List.map (fun a ->
                    { Named = Json.string (Json.field "name" a); Minion = Json.bool (Json.field "minion" a); Melee = Json.bool (Json.field "melee" a)
                      Body = Json.number (Json.field "size" a); Companion = Json.bool (Json.field "companion" a) })
            Happened = happened
            Lasts = max 0.5 (Json.number (Json.field "duration" r))
            // (the picture is wider than high: what is high counts for more)
            Field = max 60.0 (max ((x1 - x0) * 1.15) ((y1 - y0) * 2.1))
            Middle = V2d ((x0 + x1) / 2.0, (y0 + y1) / 2.0)
            Began = -1.0
            Stands = Map.empty
            Clock = 0.0 }

// ---- the passive tree ----

/// The ribbon of a connection, as triangles: a straight one between its passives, or along
/// its arc ([a, b, cx, cy, r, from, sweep]: degrees clockwise from the top).
let private ribbonAt (width : float) (at : Map<int, V2d>) (middle : V2d) (scale : float) (edge : float list) : V3f list =
    // (an ascendancy's connections are drawn around the middle of the tree: `middle` is
    // where they are laid out, `scale` how much smaller they are drawn)
    // (only the two passives asked for: this is called once a connection)
    let place (id : int) = (at.[id] - middle) * scale
    let quad (p0 : V2d) (p1 : V2d) (q0 : V2d) (q1 : V2d) =
        let v (p : V2d) = V3f (float32 p.X, float32 -p.Y, 0.0f)
        [ v p0; v p1; v q0; v q0; v p1; v q1 ]
    match edge with
    | [ a; b ] ->
        let p, q = place (int a), place (int b)
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
              Unlock = Json.field "unlock" n |> Json.list |> List.map Json.int
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
    let unlocks = passives |> List.filter (fun p -> not p.Unlock.IsEmpty) |> List.map (fun p -> p.Id, p.Unlock) |> Map.ofList
    let needs (e : float list) = (Map.tryFind (int e.[0]) unlocks |> Option.defaultValue []) @ (Map.tryFind (int e.[1]) unlocks |> Option.defaultValue []) |> List.distinct
    let links =
        edges |> List.filter (fun e -> own e = "")
        |> List.map (fun e ->
            let n = needs e
            { A = int e.[0]; B = int e.[1]; Of = ""; Ribbon = ribbon 26.0 at e; Needs = n; Thin = (if n.IsEmpty then [] else ribbon 14.0 at e) })
    let ascendedLinks =
        edges |> List.filter (fun e -> own e <> "" && middleOf.ContainsKey (own e))
        |> List.map (fun e -> { A = int e.[0]; B = int e.[1]; Of = own e; Ribbon = ribbonAt 22.0 at middleOf.[own e] ascendancyScale e; Needs = []; Thin = [] })
    // an ascendancy's passives, where they are drawn: around the middle of the tree
    let ascended =
        passives |> List.filter (fun p -> p.Ascendancy <> "" && middleOf.ContainsKey p.Ascendancy)
        |> List.map (fun p ->
            let q = (V2d (p.X, p.Y) - middleOf.[p.Ascendancy]) * ascendancyScale
            { p with X = q.X; Y = q.Y })
    let web = edges |> List.filter (fun e -> own e = "" && (needs e).IsEmpty) |> List.collect (ribbon 14.0 at)
    let shown = (passives |> List.filter (fun p -> p.Kind <> "start" && p.Ascendancy = "")) @ ascended
    { m with
        JewelSprites = Json.field "jewels" j |> Json.members |> List.map (fun (k, v) -> k, sprite v) |> Map.ofList
        Passives = Array.ofList passives
        Unlocks = unlocks
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

/// The areas of the tree that jewels in allocated sockets lie over, and the one passive that
/// frees what is around allocated keystones ("Non-Keystone Passive Skills in Medium Radius of
/// allocated Keystone Passive Skills can be allocated without being connected to your tree").
let private areas (m : Model) (allocated : Set<int>) : Area list =
    let at = m.Passives |> Array.map (fun p -> p.Id, p) |> Map.ofArray
    let jewels =
        m.Jewels |> Map.toList
        |> List.filter (fun (node, j) -> allocated.Contains node && j.Outer > 0.0)
        |> List.collect (fun (node, j) ->
            let middles =
                if j.Centres.IsEmpty then Map.tryFind node at |> Option.map (fun p -> V2d (p.X, p.Y)) |> Option.toList
                else j.Centres |> List.choose (fun name -> m.Passives |> Array.tryFind (fun p -> p.Kind = "keystone" && p.Name = name) |> Option.map (fun p -> V2d (p.X, p.Y)))
            middles |> List.map (fun c -> { Middle = c; Near = j.Inner; Far = j.Outer; Free = j.Leap; Keystones = true }))
    let around =
        if m.Passives |> Array.exists (fun p -> allocated.Contains p.Id && p.Stats |> List.exists (fun s -> s.Contains "Radius of allocated Keystone")) then
            m.Passives |> Array.filter (fun p -> p.Kind = "keystone" && allocated.Contains p.Id) |> List.ofArray
            |> List.map (fun p -> { Middle = V2d (p.X, p.Y); Near = 0.0; Far = 1150.0 * 1.2; Free = true; Keystones = false })
        else []
    jewels @ around

/// Whether a passive can be allocated without a way to it: it lies in such an area.
let private free (areas : Area list) (p : Passive) : bool =
    p.Ascendancy = "" && p.Kind <> "start"
    && areas |> List.exists (fun a ->
        let d = sqrt ((p.X - a.Middle.X) * (p.X - a.Middle.X) + (p.Y - a.Middle.Y) * (p.Y - a.Middle.Y))
        a.Free && d >= a.Near && d <= a.Far && (a.Keystones || p.Kind <> "keystone"))

/// A circle as a thin band of triangles.
let private circle (c : V2d) (r : float) (width : float) (z : float) : V3f list =
    let n = 96
    let pt (i : int) (rad : float) = let a = 6.283185307 * float i / float n in V3f (float32 (c.X + rad * cos a), float32 (-(c.Y + rad * sin a)), float32 z)
    [ for i in 0 .. n - 1 do
        let a0, a1, b0, b1 = pt i (r - width / 2.0), pt (i + 1) (r - width / 2.0), pt i (r + width / 2.0), pt (i + 1) (r + width / 2.0)
        yield! [ a0; b0; a1; a1; b0; b1 ] ]

/// The passives whose name or text has what is searched (two letters at least), ringed.
let private seek (text : string) (m : Model) : Model =
    let t = text.Trim().ToLower ()
    let hits =
        if t.Length < 2 then []
        else
            m.Shown |> List.filter (fun p ->
                p.Kind <> "ascstart" && (p.Ascendancy = "" || p.Ascendancy = m.Ascendancy)
                && (p.Name.ToLower().Contains t || p.Stats |> List.exists (fun s -> s.ToLower().Contains t)))
    let rings = hits |> List.truncate 600 |> List.collect (fun p -> circle (V2d (p.X, p.Y)) (p.Size * 0.72) 26.0 0.6)
    { m with Sought = text; Hits = hits.Length; Found = Data.pack (Data.ofList rings); FoundUv = noUv rings }

/// The connections between allocated passives, as one mesh.
let private paths (m : Model) : Model =
    // (an ascendancy's start is no passive one allocates: the way from it to a taken passive is taken)
    let begun = List.ofArray m.Passives |> List.filter (fun p -> p.Kind = "ascstart" && p.Ascendancy = m.Ascendancy) |> List.map (fun p -> p.Id) |> Set.ofList
    let has (i : int) = m.Allocated.Contains i || begun.Contains i
    let ribbons =
        List.ofArray m.Links
        |> List.filter (fun l -> (l.Of = "" || l.Of = m.Ascendancy) && has l.A && has l.B && l.Needs |> List.forall m.Allocated.Contains)
        |> List.collect (fun l -> l.Ribbon)
    let opened = List.ofArray m.Links |> List.filter (fun l -> not l.Needs.IsEmpty && l.Needs |> List.forall m.Allocated.Contains) |> List.collect (fun l -> l.Thin)
    let rings = areas m m.Allocated |> List.collect (fun a -> circle a.Middle a.Far 14.0 0.5 @ (if a.Near > 0.0 then circle a.Middle a.Near 14.0 0.5 else []))
    { m with Rings = Data.pack (Data.ofList rings); RingsUv = noUv rings; Path = Data.pack (Data.ofList ribbons); PathUv = noUv ribbons; Opened = Data.pack (Data.ofList opened); OpenedUv = noUv opened }

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
    (p.Ascendancy = "" || p.Ascendancy = m.Ascendancy) && (not p.Hidden || m.Allocated.Contains p.Id) && p.Unlock |> List.forall m.Allocated.Contains

/// Where a character's passives start: its class' start and its ascendancy's.
let private roots (m : Model) : Set<int> =
    List.ofArray m.Passives
    |> List.filter (fun p -> (p.Kind = "start" && m.Neighbours.ContainsKey p.Id) || (p.Kind = "ascstart" && p.Ascendancy = m.Ascendancy))
    |> List.map (fun p -> p.Id)
    |> Set.ofList

/// Whether a passive can be taken by this character at all (another ascendancy's can't).
let private open' (m : Model) (asc : Map<int, string>) (id : int) =
    match Map.tryFind id asc with
    | Some a -> (a = "" || a = m.Ascendancy) && (Map.tryFind id m.Unlocks |> Option.defaultValue [] |> List.forall m.Allocated.Contains)
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
        // what was loose before stays (passives a jewel or an item allocates without a way to
        // them), but not what was only there for an area that goes with this one
        let left = m.Allocated.Remove id
        let before, after = areas m m.Allocated, areas m left
        let lost (i : int) = m.Passives |> Array.tryFind (fun p -> p.Id = i) |> Option.exists (fun p -> free before p && not (free after p))
        let loose = Set.difference m.Allocated (hanging m.Allocated) |> Set.remove id |> Set.filter (fun i -> not (lost i))
        { m with Allocated = Set.union (hanging (m.Allocated.Remove id)) loose; Edited = true }
    elif m.Passives |> Array.exists (fun p -> p.Id = id && free (areas m m.Allocated) p) then
        // in a jewel's radius that lets it be allocated without a way to it
        { m with Allocated = m.Allocated.Add id; Edited = true }
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

/// What pressing the button of the pinned passive (or the one under the mouse) would do, to
/// be drawn over the tree: the passives it would take with the ways between them, or those it
/// would give back (all that comes loose with it).
let private plan (m : Model) : Model =
    let none (m : Model) = if m.Change = 0 then m else { m with Change = 0; Gain = Data.pack (Data.ofList ([] : V3f list)); GainUv = noUv []; Loss = Data.pack (Data.ofList ([] : V3f list)); LossUv = noUv [] }
    match (if m.Selected.IsSome then m.Selected else m.Hovered) with
    | Some id when not (m.Anointed.Contains id) ->
        let after = (toggle id m).Allocated
        let added, dropped = Set.difference after m.Allocated, Set.difference m.Allocated after
        if added.IsEmpty && dropped.IsEmpty then none m
        else
            let begun = List.ofArray m.Passives |> List.filter (fun p -> p.Kind = "ascstart" && p.Ascendancy = m.Ascendancy) |> List.map (fun p -> p.Id) |> Set.ofList
            // the ways between passives of `whole` that touch one of `some`, and rings around those
            let drawn (some : Set<int>) (whole : Set<int>) : V3f list =
                let has (i : int) = whole.Contains i || begun.Contains i
                let ways =
                    List.ofArray m.Links
                    |> List.filter (fun l -> (l.Of = "" || l.Of = m.Ascendancy) && has l.A && has l.B && (some.Contains l.A || some.Contains l.B))
                    |> List.collect (fun l -> l.Ribbon)
                ways @ (m.Shown |> List.filter (fun p -> some.Contains p.Id) |> List.collect (fun p -> circle (V2d (p.X, p.Y)) (p.Size * 0.66) 24.0 0.0))
            let gain, loss = drawn added after, drawn dropped m.Allocated
            { m with Change = added.Count - dropped.Count; Gain = Data.pack (Data.ofList gain); GainUv = noUv gain; Loss = Data.pack (Data.ofList loss); LossUv = noUv loss }
    | _ -> none m

/// 1234567 as "1.23M", 45678 as "45.7k".
let private short (v : float) =
    if v >= 1e9 then sprintf "%.2fB" (v / 1e9)
    elif v >= 1e6 then sprintf "%.2fM" (v / 1e6)
    elif v >= 1e4 then sprintf "%.1fk" (v / 1e3)
    else sprintf "%.0f" v

let private row (label : string) (v : float) = { Label = label; Value = sprintf "%.0f" v }

/// The defence page's panels, out of the character's sheet.
let private guards (j : Json) : (string * Row list) list =
    let n (path : string list) : float = path |> List.fold (fun x k -> Json.field k x) j |> Json.number
    let d (k : string) : float = n [ "defence"; k ]
    let pct (label : string) (v : float) : Row = { Label = label; Value = sprintf "%.0f%%" v }
    let per (label : string) (v : float) : Row = { Label = label; Value = sprintf "%.1f per second" v }
    let level = sprintf "%.0f" (d "level")
    let lucky = if Json.bool (Json.field "defence" j |> Json.field "blockLucky") then " (lucky)" else ""
    let hits =
        Json.field "defence" j |> Json.field "maxHit" |> Json.list
        |> List.map (fun x -> let v = Json.number (Json.field "value" x) in { Label = Json.string (Json.field "name" x); Value = (if v < 0.0 then "immune" else short v) })
    let resists = Json.field "resistances" j |> Json.list |> List.map (fun x -> { Label = Json.string (Json.field "name" x); Value = sprintf "%.0f%%" (Json.number (Json.field "value" x)) })
    [ "Pools", [ row "Life" (n [ "pools"; "life" ]); row "Energy Shield" (n [ "pools"; "energyShield" ]); row "Runic Ward" (n [ "pools"; "ward" ]); row "Mana" (n [ "pools"; "mana" ]) ]
      "Largest hit survived", hits
      "Against a level " + level + " monster",
      [ pct "Its hits land" (d "hitChance"); pct "Hits deflected" (d "deflectChance"); pct "A deflected hit deals less by" (d "deflectEffect")
        { Label = "Hits blocked"; Value = sprintf "%.0f%%" (n [ "defences"; "block" ]) + lucky }
        pct "A blocked hit still deals" (d "blockedTaken") ]
      "Mitigation", [ row "Armour" (n [ "defences"; "armour" ]); row "Evasion" (n [ "defences"; "evasion" ]); row "Deflection" (n [ "defences"; "deflection" ]) ] @ resists
      "Recovery",
      [ per "Life regenerated" (n [ "recovery"; "lifeRegen" ]); per "Mana regenerated" (n [ "recovery"; "manaRegen" ])
        per "Energy Shield recharge" (n [ "recovery"; "energyShieldRecharge" ]); { Label = "Recharge starts after"; Value = sprintf "%.1f s" (d "esDelay") }
        per "Energy Shield regenerated" (d "esRegen"); per "Runic Ward regenerated" (d "wardRegen") ]
      "Thresholds", [ row "Stun" (n [ "defences"; "stunThreshold" ]); row "Ailments" (n [ "defences"; "ailmentThreshold" ]); row "Freeze" (d "freezeThreshold") ] ]

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
/// The question what pressing the pinned passive's button would do to the damage: the
/// passives as they are and as they would be then.
let private compareOf (m : Model) (deep : bool) : string =
    let listed (x : Model) =
        (Set.difference x.Allocated x.Anointed |> Set.toList |> List.map string |> String.concat ","),
        (x.Choices |> Map.toList |> List.filter (fun (i, _) -> x.Allocated.Contains i) |> List.map (fun (i, c) -> string i + ":" + c) |> String.concat ",")
    match m.Selected with
    // (in the inventory: what taking the pinned item off would do)
    | _ when m.Tab = "items" ->
        if m.Pinned.StartsWith "bag:" then
            // a pasted item: worn where it is tried, or taken off again if it is worn
            match m.Bag |> List.tryFind (fun i -> "bag:" + string i.Place = m.Pinned) with
            | None -> ""
            | Some it ->
                let then' =
                    match m.Swapped |> Map.tryFindKey (fun _ x -> x.Place = it.Place) with
                    | Some k -> Some (Map.remove k m.Swapped)
                    | None -> (match aimOf m it with | "" -> None | a -> Some (Map.add a it m.Swapped))
                match then' with
                | None -> ""
                | Some swapped ->
                    let was, wasChosen = listed m
                    "__sim?op=compare" + (if deep then "&depth=1" else "") + (if m.Gem <> "" then "&gem=" + m.Gem.Replace (" ", "%20") else "") + fightKnobs m + knobs m
                    + (if m.Edited then "&base=" + was + "&basechoices=" + wasChosen + "&passives=" + was + "&choices=" + wasChosen else "")
                    + "&then_equip=" + esc (equipText m.Items swapped) + "&build=" + m.Build
        elif not (m.Pinned.StartsWith "item:") then ""
        else
            let was, wasChosen = listed m
            "__sim?op=compare" + (if deep then "&depth=1" else "") + (if m.Gem <> "" then "&gem=" + m.Gem.Replace (" ", "%20") else "") + fightKnobs m + knobs m
            + (if m.Edited then "&base=" + was + "&basechoices=" + wasChosen + "&passives=" + was + "&choices=" + wasChosen else "")
            + "&then_unequip=" + m.Pinned.Substring 5 + "&build=" + m.Build
    | None -> ""
    | Some id ->
        let was, wasChosen = listed m
        let then', thenChosen = listed (toggle id m)
        if then' = was then ""
        else
            "__sim?op=compare" + (if deep then "&depth=1" else "") + (if m.Gem <> "" then "&gem=" + m.Gem.Replace (" ", "%20") else "") + fightKnobs m + knobs m
            + (if m.Edited then "&base=" + was + "&basechoices=" + wasChosen else "")
            + "&passives=" + then' + "&choices=" + thenChosen + "&build=" + m.Build

let private compareUrl (m : Model) : string = compareOf m false

let private passiveAt (m : Model) (x : float) (y : float) : Passive option =
    // (a little more than the passive itself, and never less than a fingertip on the page:
    // of several that reach, the nearest is the one)
    let least = 16.0 * m.Height / max 1.0 (size m).X
    let near (ps : Passive list) =
        ps
        |> List.filter (fun q -> let r = max (q.Size * 0.65) least in (q.X - x) * (q.X - x) + (q.Y + y) * (q.Y + y) <= r * r)
        |> List.sortBy (fun q -> (q.X - x) * (q.X - x) + (q.Y + y) * (q.Y + y))
        |> List.tryHead
    // (the ascendancy's lie over the tree's own there: they come first)
    near (m.Shown |> List.filter (fun q -> q.Ascendancy <> "" && visible m q)) |> Option.orElse (near (m.Shown |> List.filter (visible m)))

/// What an analysis found is written to what it is about: the items' lines, the gems.
let private weigh (m : Model) : Model =
    // (found in every scene; the one number a thing has is that of the scene looked at)
    let of' (key : string) = m.Worth |> List.tryFind (fun (k, _) -> k = key + "@" + m.Scenario) |> Option.map snd |> Option.defaultValue nan
    let item (i : Item) = { i with Weighs = of' ("item:" + string i.Place); Lines = i.Lines |> List.mapi (fun n l -> { l with Lost = of' ("line:" + string i.Place + ":" + string n) }) }
    { m with
        Items = m.Items |> List.map item
        Groups = m.Groups |> List.map (fun g -> { g with Gems = g.Gems |> List.map (fun x -> { x with Gives = of' ("gem:" + x.Key) }) })
        Offers = m.Offers |> List.map (fun o -> { o with Adds = of' ("pick:" + o.Choice) })
        NodeWorth = m.Worth |> List.filter (fun (k, _) -> k.StartsWith "node:" && k.EndsWith ("@" + m.Scenario)) |> List.map (fun (k, v) -> int (k.Substring (5, k.IndexOf '@' - 5)), v) |> Map.ofList }

/// The gems shown in the list to choose from: of the kind asked for, those that are searched
/// (without a search: those that fit), the ones known to add most first.
let private offer (m : Model) : Model =
    let supports = m.Picker.StartsWith "add:" || (m.Picker.StartsWith "swap:" && m.Picker.EndsWith ":s")
    let text = m.Wanted.Trim().ToLower ()
    let shown =
        m.Offers
        |> List.filter (fun o -> o.Supports = supports && (if text = "" then o.Fits else o.Choice.ToLower().Contains text || o.Tags.ToLower().Contains text))
        |> List.sortBy (fun o -> (if System.Double.IsNaN o.Adds then 1.0 else 0.0), (if System.Double.IsNaN o.Adds then 0.0 else -o.Adds), (if o.Fits then 0 else 1), o.Choice)
    { m with Offered = (if shown.Length > 80 then List.take 80 shown else shown) }

/// The change of the gems that choosing this gem from the list is.
let private choice (m : Model) (name : string) : string =
    match m.Picker.Split ':' |> List.ofArray with
    | [ "add"; g ] -> "add|" + g + "|" + name + "|1|0"
    | "swap" :: g :: k :: _ -> "swap|" + g + "|" + k + "|" + name
    | _ -> "new|" + name + "|20|0"

/// What an analysis asks: each of these alone is another (its key, the parameter, the value).
let private probes (m : Model) : (string * string * string) list =
    if m.Picker <> "" then
        m.Offered |> List.filter (fun o -> o.Fits) |> List.map (fun o -> "pick:" + o.Choice, "gems", String.concat ";" (m.GemEdits @ [ choice m o.Choice ]))
    elif m.Tab = "tree" then
        // every notable and keystone allocated, given back alone
        let ids = Set.difference m.Allocated m.Anointed
        m.Passives |> Array.filter (fun p -> ids.Contains p.Id && (p.Kind = "notable" || p.Kind = "keystone")) |> List.ofArray
        |> List.map (fun p -> "node:" + string p.Id, "passives", Set.remove p.Id ids |> Set.toList |> List.map string |> String.concat ",")
    elif m.Tab = "gems" then
        m.Groups |> List.collect (fun g -> g.Gems) |> List.filter (fun x -> not (m.Dropped.Contains x.Key))
        |> List.map (fun x -> "gem:" + x.Key, "without", String.concat "," (Set.toList (Set.add x.Key m.Dropped)))
    else
        // (an item worn here in place of the character's own is not asked about)
        m.Items |> List.filter (fun i -> not (m.Wearing |> List.exists (fun (spot, _) -> spot = area i || spot = "at:" + string i.Place)))
        |> List.collect (fun i ->
            ("item:" + string i.Place, "unequip", string i.Place)
            :: (i.Lines |> List.mapi (fun n l -> "line:" + string i.Place + ":" + string n, "strip", string i.Place + "\n" + l.Reads)))

let private probeOf (m : Model) : string =
    "__sim?op=analyse&scenario=" + m.Scenario + (if m.Gem <> "" then "&gem=" + m.Gem.Replace (" ", "%20") else "") + fightKnobs m + knobs m + passives m
    + "&list=" + esc (probes m |> List.map (fun (k, p, v) -> k + "\n" + p + "\n" + v) |> String.concat "\n@@\n")

let private probeUrl (m : Model) : string =
    if m.Probe = "" || not m.Probing then nothing m else m.Probe + "&round=" + string m.ProbeRound + "&build=" + m.Build

let private pickUrl (m : Model) : string =
    if m.Picker = "" then nothing m
    else "__sim?op=gems&group=" + (match m.Picker.Split ':' with | [| _; g |] | [| _; g; _ |] | [| _; g; _; _ |] -> g | _ -> "0") + (if m.GemEdits.IsEmpty then "" else "&gems=" + esc (String.concat ";" m.GemEdits)) + "&build=" + m.Build

let private calcsUrl (m : Model) : string = "__sim?op=calcs" + knobs m + passives m + "&build=" + m.Build

/// The sources shown: of the numbers whose name is searched.
let private among (m : Model) : Model =
    let text = m.Among.Trim().ToLower ()
    let shown = m.Calcs |> List.filter (fun (k, rows) -> text = "" || k.ToLower().Contains text || rows |> List.exists (fun r -> r.Label.ToLower().Contains text))
    { m with CalcShown = (if shown.Length > 60 then List.take 60 shown else shown) }

/// A modifier as it is said: "+44", "12% increased", "8% less".
let private said (kind : string) (v : float) : string =
    match kind with
    | "base" -> sprintf "%+g" v
    | "increased" -> if v < 0.0 then sprintf "%g%% reduced" (-v) else sprintf "%g%% increased" v
    | "more" -> if v < 0.0 then sprintf "%g%% less" (-v) else sprintf "%g%% more" v
    | "flag" -> "yes"
    | _ -> sprintf "= %g" v

let private sourcesOf (j : Json) : (string * Row list) list =
    j |> Json.list |> List.map (fun x ->
        Json.string (Json.field "stat" x),
        Json.field "mods" x |> Json.list |> List.map (fun l ->
            let on = Json.string (Json.field "when" l)
            { Label = Json.string (Json.field "source" l) + (if on = "" then "" else " (" + on + ")"); Value = said (Json.string (Json.field "kind" l)) (Json.number (Json.field "value" l)) }))

/// The order of what is pressed after a change to it.
let private ordered (m : Model) (how : string) (gem : string) : string list =
    let now = if m.Rotation.IsEmpty then m.Play |> List.map (fun p -> p.Skill) |> List.distinct else m.Rotation
    let named (entry : string) = entry.Split('@').[0]
    let at = now |> List.tryFindIndex (fun g -> named g = gem) |> Option.defaultValue -1
    // when a skill can be said to be pressed: as its type says, when ready, kept up, kept on
    // the enemy, when out of something the character counts, while that is not full
    let whens = [ ""; "ready"; "buff"; "curse" ] @ (m.Counts |> List.collect (fun (k, most) -> [ "below:" + k + ":1" ] @ (if most > 1 then [ "below:" + k + ":" + string most ] else [])))
    let swap a b = now |> List.mapi (fun i g -> if i = a then now.[b] elif i = b then now.[a] else g)
    match how with
    | "up" when at > 0 -> swap at (at - 1)
    | "down" when at >= 0 && at < now.Length - 1 -> swap at (at + 1)
    | "drop" when now.Length > 1 -> now |> List.filter (fun g -> named g <> gem)
    | "when" when at >= 0 ->
        let code = match now.[at].IndexOf '@' with | -1 -> "" | k -> now.[at].Substring (k + 1)
        // (what it does by its type is not offered again as a choice)
        let own = match m.Play |> List.tryFind (fun p -> p.Skill = gem && p.Code = "") |> Option.map (fun p -> p.How) with | Some "when ready" -> "ready" | Some "kept up" -> "buff" | Some "kept on the enemy" -> "curse" | _ -> "-"
        let whens = whens |> List.filter (fun w -> w <> own)
        let next = whens.[((whens |> List.tryFindIndex (fun w -> w = code) |> Option.defaultValue 0) + 1) % whens.Length]
        now |> List.mapi (fun i g -> if i = at then (if next = "" then gem else gem + "@" + next) else g)
    // (a condition more or less: "<skill>\n<condition>"; one of a kind, and "has" and "lacks" of
    // the same state do not go together)
    | "cond+" | "cond-" ->
        (match gem.Split '\n' with
         | [| skill; part |] ->
            let kind (c : string) = if c.StartsWith "has:" || c.StartsWith "lacks:" then "state:" + c.Substring (c.IndexOf ':' + 1) elif c.StartsWith "up:" || c.StartsWith "down:" then "stands:" + c.Substring (c.IndexOf ':' + 1) else "when"
            now |> List.map (fun g ->
                if named g <> skill then g
                else
                    let parts = match g.IndexOf '@' with | -1 -> [] | k -> g.Substring(k + 1).Split '+' |> List.ofArray |> List.filter (fun c -> c <> "")
                    let parts = if how = "cond-" then parts |> List.filter (fun c -> c <> part) else (parts |> List.filter (fun c -> kind c <> kind part)) @ [ part ]
                    if parts.IsEmpty then skill else skill + "@" + String.concat "+" parts)
         | _ -> now)
    // (before the last: that one is what is pressed when nothing else is ready)
    | "add" when at < 0 -> (match List.rev now with | last :: before -> List.rev before @ [ gem; last ] | [] -> [ gem ])
    | "auto" -> []
    | _ -> now

/// A fight is to be fought anew: what is shown of the last one goes.
let private afresh (m : Model) : Model =
    { m with Fighting = true; Dps = []; Shares = []; Types = []; Runs = 0; Round = 0; Happened = []; Warm = [] }

let private quote (t : string) : string =
    "\"" + t.Replace("\\", "\\\\").Replace("\"", "\\\"").Replace("\r", "").Replace("\n", "\\n").Replace("\t", " ") + "\""

/// What was changed here, as text to keep.
let private kept (m : Model) : string =
    let list (xs : string list) = "[" + String.concat "," xs + "]"
    let at (i : Item) = m.Bag |> List.tryFindIndex (fun x -> x.Place = i.Place) |> Option.defaultValue -1
    "{" + String.concat ","
            [ "\"gems\":" + list (m.GemEdits |> List.map quote)
              "\"dropped\":" + list (m.Dropped |> Set.toList |> List.map quote)
              "\"rotation\":" + list (m.Rotation |> List.map quote)
              "\"custom\":" + quote m.Custom
              "\"level\":" + quote m.EnemyLevel; "\"eres\":" + quote m.EnemyEle; "\"cres\":" + quote m.EnemyChaos
              "\"bag\":" + list (m.Bag |> List.map (fun i -> quote i.Raw))
              "\"worn\":" + list (m.Wearing |> List.map (fun (spot, i) -> list [ quote spot; string (at i) ]))
              "\"start\":" + list (m.Start |> Map.toList |> List.map (fun (k, v) -> list [ quote k; string v ]))
              "\"switches\":" + list (m.Switches |> Set.toList |> List.map quote)
              "\"passives\":" + (if m.Edited then list (Set.difference m.Allocated m.Anointed |> Set.toList |> List.map string) else "[]")
              "\"choices\":" + (if m.Edited then list (m.Choices |> Map.toList |> List.map (fun (i, c) -> list [ string i; quote c ])) else "[]")
              "\"notes\":" + quote m.Notes ] + "}"

let private storeKey (m : Model) : string = m.Build.Replace("%", "%25").Replace("&", "%26").Replace("#", "%23").Replace("+", "%2B")
let private saveUrl (m : Model) : string = "__store?op=put&key=" + storeKey m + "&value=" + esc m.Kept
let private restoreUrl (m : Model) : string = "__store?op=get&key=" + storeKey m

/// What was kept, put back.
let private restore (m : Model) (j : Json) : Model =
    let strs (k : string) = Json.field k j |> Json.list |> List.map Json.string
    let bag = strs "bag" |> List.mapi (fun i raw -> pasted (1000 + i) raw) |> List.choose id
    let worn =
        Json.field "worn" j |> Json.list
        |> List.choose (fun w ->
            match Json.list w with
            | [ spot; i ] -> bag |> List.tryFind (fun x -> x.Place = 1000 + Json.int i) |> Option.map (fun it -> Json.string spot, it)
            | _ -> None)
        |> Map.ofList
    let ids = Json.field "passives" j |> Json.list |> List.map Json.int
    let m =
        { m with
            GemEdits = strs "gems"; Dropped = Set.ofList (strs "dropped"); Rotation = strs "rotation"
            Custom = Json.string (Json.field "custom" j); CustomDraft = Json.string (Json.field "custom" j)
            EnemyLevel = Json.string (Json.field "level" j); EnemyEle = Json.string (Json.field "eres" j); EnemyChaos = Json.string (Json.field "cres" j)
            Bag = bag; Swapped = worn
            Start = Json.field "start" j |> Json.list |> List.choose (fun x -> match Json.list x with | [ k; v ] -> Some (Json.string k, Json.int v) | _ -> None) |> Map.ofList
            Switches = Set.ofList (strs "switches")
            Notes = Json.string (Json.field "notes" j); NotesWere = Json.string (Json.field "notes" j) }
    if ids.IsEmpty then m
    else
        { m with
            Edited = true; Allocated = Set.union (Set.ofList ids) m.Anointed
            Choices = Json.field "choices" j |> Json.list |> List.choose (fun x -> match Json.list x with | [ i; c ] -> Some (Json.int i, Json.string c) | _ -> None) |> Map.ofList }

let private picturesOf (m : Model) (name : string) : string =
    m.Groups |> List.collect (fun g -> g.Gems) |> List.tryFind (fun x -> x.Named = name) |> Option.map (fun x -> if x.Icon <> "" then x.Icon else x.Own) |> Option.defaultValue ""

let rec private step (msg : Msg) (m : Model) : Model =
    match msg with
    | Located href ->
        match Native.queryParam "build" href with
        | Some b -> { m with Build = b; Status = "reading the character" }
        | None -> { m with Status = "" }
    | Scenario s -> offer (weigh { m with Scenario = s; Fighting = true; Dps = []; Shares = []; Types = []; Runs = 0; Round = 0; Happened = [] })
    | Begin (name, count) ->
        let most = m.Counts |> List.tryFind (fun (k, _) -> k = name) |> Option.map snd |> Option.defaultValue 0
        { m with Start = (if count < 0 || count > most then Map.remove name m.Start else Map.add name count m.Start); Fighting = true; Dps = []; Shares = []; Types = []; Runs = 0; Round = 0; Happened = []; Warm = [] }
    | Compared r when r.Url <> m.Ask && r.Url <> m.AskMore -> m
    | Compared r ->
        (match r.Status, Native.parseJson r.Text with
         | 200, Ok j ->
            let deltas =
                Json.field "scenarios" j |> Json.list |> List.choose (fun x ->
                    let b, c = Json.number (Json.field "base" x), Json.number (Json.field "changed" x)
                    // (without an item there may be nothing left to fight with)
                    if b > 0.0 && c > 0.0 then Some (Json.string (Json.field "name" x), (c / b - 1.0) * 100.0, Json.number (Json.field "noise" x))
                    elif b > 0.0 && (r.Url.Contains "then_unequip" || r.Url.Contains "then_equip") then Some (Json.string (Json.field "name" x), -100.0, 0.0)
                    else None)
            if deltas.IsEmpty then m else { m with Deltas = deltas; DeltaOf = r.Url }
         | _ -> m)
    | Pin key -> { m with Pinned = key; Aim = "" }
    | GemEdit cmd -> afresh { m with GemEdits = m.GemEdits @ [ cmd ]; Pressing = ""; Pinned = (if cmd.StartsWith "del" || cmd.StartsWith "swap" then "" else m.Pinned) }
    | UndoGems -> if m.GemEdits.IsEmpty then m else afresh { m with GemEdits = List.take (m.GemEdits.Length - 1) m.GemEdits; Pressing = ""; Pinned = "" }
    | Pick what -> offer { m with Picker = what; Wanted = ""; Offers = []; Pinned = "" }
    | Picked r when r.Url <> pickUrl m || m.Picker = "" -> m
    | Picked r ->
        (match r.Status, Native.parseJson r.Text with
         | 200, Ok j ->
            offer
                { m with
                    Offers =
                        j |> Json.list |> List.map (fun x ->
                            { Choice = Json.string (Json.field "name" x); Supports = Json.bool (Json.field "support" x); Colour = Json.int (Json.field "color" x)
                              Tags = Json.field "tags" x |> Json.list |> List.map Json.string |> String.concat ", "; Tells = Json.string (Json.field "says" x)
                              Fits = Json.bool (Json.field "fits" x); Adds = nan }) }
         | _ -> m)
    | Filter text -> offer { m with Wanted = text }
    | Choose name -> afresh { m with GemEdits = m.GemEdits @ [ choice m name ]; Picker = ""; Offers = []; Offered = []; Pressing = "" }
    | Rank | Analyse ->
        let ask = probeOf m
        { m with Probe = ask; ProbeRound = 0; Probing = true; Probed = "asking ..."; Worth = [] }
    | Tune -> { m with Tuning = "__sim?op=minions&scenario=" + m.Scenario + fightKnobs m + knobs m + passives m + "&build=" + m.Build; TuneSaid = "" }
    | Tuned r when r.Url <> m.Tuning -> m
    | Tuned r ->
        (match r.Status, Native.parseJson r.Text with
         | 200, Ok j when Json.string (Json.field "error" j) = "" ->
            let best, tried = Json.string (Json.field "best" j), Json.int (Json.field "tried" j)
            if best = "" then { m with Tuning = ""; TuneSaid = (if tried < 2 then "nothing to choose" else sprintf "best as it is (%d tried)" tried) }
            else afresh { m with Tuning = ""; TuneSaid = sprintf "+%.1f%% (%d tried)" (Json.number (Json.field "gain" j)) tried; GemEdits = m.GemEdits @ [ best ]; Pressing = "" }
         | _ -> { m with Tuning = ""; TuneSaid = "did not come back" })
    | Analysed r when m.Probe = "" || not (r.Url.StartsWith m.Probe) -> m
    | Analysed r ->
        (match r.Status, Native.parseJson r.Text with
         | 200, Ok j when Json.string (Json.field "error" j) = "" ->
            let found =
                Json.field "results" j |> Json.list |> List.choose (fun x ->
                    let b, c = Json.number (Json.field "base" x), Json.number (Json.field "changed" x)
                    // (for a gem to choose: what it adds; otherwise what is lost without)
                    let key = Json.string (Json.field "key" x)
                    if b > 0.0 then Some (key + "@" + Json.string (Json.field "scenario" x), (if key.StartsWith "pick:" then (c / b - 1.0) else (1.0 - c / b)) * 100.0) else None)
            let more = Json.bool (Json.field "more" j)
            let kept = m.Worth |> List.filter (fun (k, _) -> not (found |> List.exists (fun (f, _) -> f = k)))
            offer (weigh { m with Worth = kept @ found; Probing = more; ProbeRound = m.ProbeRound + 1
                                  Probed = (if more then sprintf "%d of %d" (Json.int (Json.field "done" j)) (Json.int (Json.field "total" j)) else "") })
         | _ -> { m with Probing = false; Probed = "the analysis did not come back" })
    | Order ("edit", gem) -> { m with Editing = (if m.Editing = gem then "" else gem) }
    | Order (how, gem) -> afresh { m with Rotation = ordered m how gem }
    | Enemy (what, value) ->
        let v = value.Trim ()
        afresh (match what with | "level" -> { m with EnemyLevel = v } | "eres" -> { m with EnemyEle = v } | _ -> { m with EnemyChaos = v })
    | Lines text -> { m with CustomDraft = text }
    | Apply -> if m.CustomDraft = m.Custom then m else afresh { m with Custom = m.CustomDraft; Pressing = "" }
    | Sources r when r.Url <> calcsUrl m -> m
    | Sources r ->
        (match r.Status, Native.parseJson r.Text with
         | 200, Ok j -> among { m with Calcs = sourcesOf j }
         | _ -> m)
    | Among text -> among { m with Among = text }
    | Saved _ -> m
    | Link t -> { m with Link = t; ImportSaid = "" }
    | Import -> if m.Link.Trim () = "" then m else { m with Importing = "__poe?op=paste&text=" + esc (m.Link.Trim ()); ImportSaid = "" }
    | Imported r ->
        (match Native.parseJson r.Text with
         | Ok j when Json.string (Json.field "error" j) <> "" -> { m with Importing = ""; ImportSaid = Json.string (Json.field "error" j) }
         | Ok _ -> m
         | _ -> { m with Importing = ""; ImportSaid = "that did not work" })
    | Share -> if m.Build = "" || m.Sharing <> "" then m else { m with Sharing = "__poe?op=share&name=" + storeKey m; ShareLink = ""; ShareSaid = "sharing ..." }
    | Shared r ->
        (match Native.parseJson r.Text with
         | Ok j when Json.string (Json.field "url" j) <> "" -> { m with Sharing = ""; ShareLink = Json.string (Json.field "url" j); ShareSaid = "" }
         | Ok j when Json.string (Json.field "error" j) <> "" -> { m with Sharing = ""; ShareSaid = Json.string (Json.field "error" j) }
         | _ -> { m with Sharing = ""; ShareSaid = "that did not work" })
    | Unshare -> { m with ShareLink = ""; ShareSaid = "" }
    | Account r ->
        (match r.Status, Native.parseJson r.Text with
         | 200, Ok j ->
            let failed = Json.string (Json.field "error" j)
            // (a character from outside the account goes by its address and says what it is itself)
            let said (c : Json) = if Json.bool (Json.field "outside" c) then Json.string (Json.field "about" c) else Json.string (Json.field "name" c) + ": " + Json.string (Json.field "about" c)
            let leagues (lasting : bool) =
                Json.field "characters" j |> Json.list |> List.filter (fun c -> Json.bool (Json.field "lasting" c) = lasting)
                |> List.groupBy (fun c -> Json.string (Json.field "league" c))
                |> List.map (fun (league, cs) -> league, string cs.Length, cs |> List.map (fun c -> Json.string (Json.field "name" c), said c))
            { m with
                Asked = true
                Running = leagues false
                Lasting = leagues true
                CanLogin = Json.bool (Json.field "can" j); Player = Json.string (Json.field "name" j)
                // (the one that is open first: a list shows its first)
                Characters =
                    Json.field "characters" j |> Json.list
                    |> List.map (fun c -> Json.string (Json.field "name" c), said c, Json.string (Json.field "url" c))
                    |> List.sortBy (fun (_, _, url) -> if url = m.Build then 0 else 1)
                Status = (if failed <> "" then failed elif m.Build = "" && Json.bool (Json.field "can" j) then (if Json.string (Json.field "name" j) = "" then "log in to see your characters" else "choose a character") else m.Status) }
         | _ -> m)
    | Login -> { m with Going = "login" }
    | Logout -> { m with Going = "logout" }
    | Open name -> if name = "" then m elif name = "*" then { m with Going = "home" } else { m with Going = "open&name=" + esc name }
    | Note text -> { m with Notes = text }
    | Restore _ when m.Restored -> m
    | Restore r ->
        (match r.Status, Native.parseJson r.Text with
         | 200, Ok j ->
            (match Native.parseJson (Json.string (Json.field "value" j)) with
             | Ok kept when Json.string (Json.field "value" j) <> "" -> afresh { restore m kept with Restored = true; Pressing = "" }
             | _ -> { m with Restored = true })
         | _ -> { m with Restored = true })
    | Paste -> { m with Pasting = not m.Pasting; Draft = "" }
    | Draft text -> { m with Draft = text }
    | Add ->
        let place = 1000 + (m.Bag |> List.map (fun i -> i.Place - 999) |> List.fold max 0)
        (match pasted place m.Draft with
         | Some it -> { m with Bag = m.Bag @ [ it ]; Pasting = false; Draft = ""; Pinned = "bag:" + string place; Aim = "" }
         | None -> m)
    | Aim spot -> { m with Aim = spot }
    | Wear place ->
        (match m.Bag |> List.tryFind (fun i -> i.Place = place) with
         | None -> m
         | Some it ->
            let swapped =
                match m.Swapped |> Map.tryFindKey (fun _ x -> x.Place = place) with
                | Some k -> Map.remove k m.Swapped
                | None -> (match aimOf m it with | "" -> m.Swapped | a -> Map.add a it m.Swapped)
            if swapped = m.Swapped then m
            else { m with Swapped = swapped; Pressing = ""; Fighting = true; Dps = []; Shares = []; Types = []; Runs = 0; Round = 0; Happened = []; Warm = [] })
    | Discard place ->
        let swapped = m.Swapped |> Map.filter (fun _ x -> x.Place <> place)
        let m = { m with Bag = m.Bag |> List.filter (fun i -> i.Place <> place); Pinned = "" }
        if swapped = m.Swapped then m
        else { m with Swapped = swapped; Pressing = ""; Fighting = true; Dps = []; Shares = []; Types = []; Runs = 0; Round = 0; Happened = []; Warm = [] }
    | Socket key ->
        { m with Dropped = (if m.Dropped.Contains key then Set.remove key m.Dropped else Set.add key m.Dropped); Pressing = ""; Fighting = true; Dps = []; Shares = []; Types = []; Runs = 0; Round = 0; Happened = []; Warm = [] }
    | Switch key ->
        { m with Switches = (if m.Switches.Contains key then Set.remove key m.Switches else Set.add key m.Switches); Fighting = true; Dps = []; Shares = []; Types = []; Runs = 0; Round = 0; Happened = []; Warm = [] }
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
        paths { m with Choices = (if choice = "" then m.Choices else Map.add id choice m.Choices); Edited = true; Fighting = true; Warm = []; Runs = 0; Round = 0; Happened = []; Pressing = (if m.Pressing = "" then m.Gem else m.Pressing) }
    | Deallocate id -> if m.Allocated.Contains id then paths { toggle id m with Fighting = true; Warm = []; Runs = 0; Round = 0; Happened = []; Pressing = (if m.Pressing = "" then m.Gem else m.Pressing) } else m
    | Close -> { m with Selected = None }
    | Seek text -> seek text m
    | Toggle id -> paths { toggle id m with Fighting = true }
    // A place on the tree comes from the last frame drawn; under an older camera than the
    // model's it is no use.
    | Zoom (_, eye, _) when abs (eye.X - m.Centre.X) + abs (eye.Y - m.Centre.Y) > m.Height / 1e5 -> m
    | Sized s ->
        // (the page's pixels: the control's own divided by the device's ratio; it lies below the header)
        let m = { m with Page = V2d (s.Pixels.X / max 0.01 s.PixelRatio, s.Pixels.Y / max 0.01 s.PixelRatio + top) }
        // a click that waited for this
        match m.Pending with
        | Some p ->
            let q = placeAt m m.Centre m.Height p
            step (Click (V3d (q.X, q.Y, 0.0))) { m with Pending = None }
        | None -> m
    | PointerDown e ->
        let others = m.Touches |> List.filter (fun (i, _) -> i <> e.PointerId)
        let m = { m with Touches = (e.PointerId, e.Position) :: others }
        if others.IsEmpty then { m with Travel = 0.0 } else m
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
                step (Click (V3d (p.X, p.Y, 0.0))) lifted
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
            let named key = Json.field key j |> Json.list |> List.map (fun x -> { Label = Json.string (Json.field "name" x); Value = (if key = "minions" then sprintf "×%.0f" (Json.number (Json.field "count" x)) else sprintf "%.0f" (Json.number (Json.field "value" x))) })
            let missing = [ "unmapped"; "unsimulated"; "unknown"; "minions" ] |> List.sumBy (fun k -> Json.field "notUnderstood" j |> Json.field k |> Json.list |> List.length)
            (fun x -> settle (weigh x))
                { m with
                    Status = ""
                    Title = sprintf "Level %.0f %s" (n [ "level" ]) (Json.string (Json.field "class" j))
                    Class = Json.string (Json.field "class" j)
                    Ascendancy = Json.string (Json.field "ascendancy" j)
                    // what the character has allocated, until it is changed here
                    Allocated = Set.union (if m.Edited then m.Allocated else Json.field "passives" j |> Json.list |> List.map Json.int |> Set.ofList) (Json.field "anointed" j |> Json.list |> List.map Json.int |> Set.ofList)
                    Anointed = Json.field "anointed" j |> Json.list |> List.map Json.int |> Set.ofList
                    Choices = (if m.Edited then m.Choices else Json.field "choices" j |> Json.list |> List.map (fun c -> Json.int (Json.field "id" c), Json.string (Json.field "name" c)) |> Map.ofList)
                    Level = int (n [ "level" ])
                    Jewels =
                        Json.field "jewels" j |> Json.list
                        |> List.map (fun x ->
                            let s k = Json.string (Json.field k x)
                            let pic = Map.tryFind (s "name") m.JewelSprites |> Option.orElse (Map.tryFind (s "base") m.JewelSprites) |> Option.defaultValue ("", V4d (0.0, 0.0, 0.0, 0.0))
                            Json.int (Json.field "node" x), { Called = s "name"; Base = s "base"; Rarity = s "rarity"; Mods = Json.field "mods" x |> Json.list |> List.map Json.string; Sprite = fst pic; SpriteRect = snd pic
                                                              Inner = Json.number (Json.field "inner" x); Outer = Json.number (Json.field "outer" x); Leap = Json.bool (Json.field "leap" x)
                                                              Centres = Json.field "centres" x |> Json.list |> List.map Json.string })
                        |> Map.ofList
                    Counts = Json.field "resources" j |> Json.list |> List.map (fun x -> Json.string (Json.field "name" x), Json.int (Json.field "max" x)) |> List.filter (fun (_, most) -> most > 0)
                    Items =
                        Json.field "items" j |> Json.list
                        |> List.map (fun x ->
                            let s k = Json.string (Json.field k x)
                            { Place = Json.int (Json.field "index" x); Worn = s "slot"; Col = Json.int (Json.field "at" x); Headed = s "name"; Type = s "base"; Grade = s "rarity"; Picture = s "icon"
                              Says = Json.field "props" x |> Json.list |> List.map Json.string; Requires = s "requires"
                              Lines = Json.field "mods" x |> Json.list |> List.map (fun l -> { Mark = Json.string (Json.field "kind" l); Reads = Json.string (Json.field "text" l); Lost = nan })
                              Holds = Json.field "sockets" x |> Json.list |> List.map (fun l -> { Rune = Json.string (Json.field "name" l); Pic = Json.string (Json.field "icon" l); Does = Json.field "lines" l |> Json.list |> List.map Json.string })
                              Grants = Json.field "grants" x |> Json.list |> List.map (fun l -> { Grant = Json.string (Json.field "text" l); Shows = Json.string (Json.field "icon" l) })
                              Spoiled = Json.bool (Json.field "corrupted" x); Class = ""; Raw = ""; Weighs = nan })
                    Groups =
                        Json.field "groups" j |> Json.list
                        |> List.map (fun g ->
                            let index = Json.int (Json.field "index" g)
                            { Slot = Json.string (Json.field "slot" g)
                              Number = index
                              Gems =
                                Json.field "gems" g |> Json.list
                                |> List.map (fun x ->
                                    let name = Json.string (Json.field "name" x)
                                    { Key = string index + ":" + name; Named = name; Level = Json.int (Json.field "level" x); Quality = Json.int (Json.field "quality" x)
                                      Support = Json.bool (Json.field "support" x); Icon = Json.string (Json.field "icon" x); Own = Json.string (Json.field "gemIcon" x); Spirit = Json.number (Json.field "spirit" x); Persistent = Json.bool (Json.field "persistent" x)
                                      At = Json.int (Json.field "at" x); Socketed = Json.int (Json.field "socketed" x); Gives = nan; Minions = Json.int (Json.field "minions" x); Reserved = Json.number (Json.field "reserved" x) }) })
                    Pressable = Json.field "pressable" j |> Json.list |> List.map Json.string
                    States = (match Json.field "states" (Json.field "conditions" j) |> Json.list |> List.map Json.string with | [] -> [ "Ignite"; "Shock"; "Chill"; "Freeze"; "Bleed"; "Poison" ] | xs -> xs)
                    Standing = Json.field "skills" (Json.field "conditions" j) |> Json.list |> List.map Json.string
                    // (Runic Ward beside life and energy shield, for a character that has any)
                    Pools = [ row "Life" (n [ "pools"; "life" ]); row "Energy Shield" (n [ "pools"; "energyShield" ]) ] @ (if n [ "pools"; "ward" ] > 0.0 then [ row "Runic Ward" (n [ "pools"; "ward" ]) ] else []) @ [ row "Mana" (n [ "pools"; "mana" ]); row "Spirit" (n [ "pools"; "spirit" ]) ]
                    Guards = guards j
                    Defences = [ row "Armour" (n [ "defences"; "armour" ]); row "Evasion" (n [ "defences"; "evasion" ]); row "Deflection" (n [ "defences"; "deflection" ]); row "Block" (n [ "defences"; "block" ]) ]
                    Resistances = named "resistances"
                    Minions = named "minions"
                    NotUnderstood = missing
                    Fighting = true }
        | s, _ -> { m with Status = "the simulator did not answer (status " + string s + ")" }
    | Tick t ->
        if m.Happened.IsEmpty || m.Tab <> "character" then m
        else
            let began = if m.Began < 0.0 then t else m.Began
            // the fight, a breath, and again
            let whole = m.Lasts / pace m + 2.0
            let shown = t - began
            let c = (shown - whole * floor (shown / whole)) * pace m
            let layers, stands = stage m c
            { m with Began = began; Stage = layers; Stands = stands; Clock = c }
    | Warmed r -> { m with Warm = (warming m) :: m.Warm }
    | Fought r when r.Url <> fightUrl m -> m
    | Fought r ->
        match r.Status, Native.parseJson r.Text with
        | 200, Ok j when Json.string (Json.field "error" j) <> "" -> { m with Fighting = false; Status = "the fight failed: " + Json.string (Json.field "error" j) }
        | 200, Ok j ->
            let dps k = Json.field "dps" j |> Json.field k |> Json.number
            // (a boss with the game's life: how soon it is dead comes first)
            let rate = Json.number (Json.field "killRate" j)
            let clear k = Json.field "clear" j |> Json.field k |> Json.number
            let killed =
                if m.Scenario <> "boss" && m.Scenario <> "pinnacle" then []
                elif rate >= 1.0 then [ { Label = "Dead in"; Value = sprintf "%.2f s" (clear "mean") }; { Label = "one in ten slower than"; Value = sprintf "%.2f s" (clear "p90") } ]
                else [ { Label = "Dead in"; Value = sprintf "%.0f%% of fights" (100.0 * rate) } ]
            // (while runs are still out the fight is asked again: the next answer is of more of them)
            let more = Json.bool (Json.field "more" j)
            let m = if m.Round = 0 || m.Happened.IsEmpty then readReplay j m else m
            { m with
                Fighting = false
                Runs = Json.int (Json.field "runs" j)
                Survival =
                    [ { Label = "Died in"; Value = sprintf "%.0f%% of fights" (100.0 * Json.number (Json.field "deathRate" j)) }
                      { Label = "Damage taken"; Value = short (Json.number (Json.field "damageTakenPerSecond" j)) + " per second" }
                      { Label = "Lowest life, on average"; Value = sprintf "%.0f%%" (100.0 * (Json.field "lowestLife" j |> Json.field "mean" |> Json.number)) }
                      { Label = "Lowest life, worst fight"; Value = sprintf "%.0f%%" (100.0 * (Json.field "lowestLife" j |> Json.field "worst" |> Json.number)) } ]
                More = more
                Round = (if more then m.Round + 1 else m.Round)
                Gem = Json.string (Json.field "gem" j)
                Play = Json.field "play" j |> Json.list |> List.map (fun x -> { Skill = Json.string (Json.field "gem" x); How = Json.string (Json.field "how" x); Face = picturesOf m (Json.string (Json.field "gem" x)); Code = Json.string (Json.field "cond" x) }) |> List.distinctBy (fun p -> p.Skill)
                Dps = killed @ [ { Label = "DPS"; Value = short (dps "mean") }; { Label = "one in ten below"; Value = short (dps "p10") }; { Label = "median"; Value = short (dps "p50") }; { Label = "one in ten above"; Value = short (dps "p90") } ]
                Shares =
                    Json.field "damageShare" j |> Json.list
                    |> List.map (fun x -> Json.string (Json.field "name" x), Json.number (Json.field "value" x) * 100.0)
                    |> shares
                Types =
                    Json.field "damageType" j |> Json.list
                    |> List.map (fun x -> let v = Json.number (Json.field "value" x) * 100.0 in { Name = Json.string (Json.field "name" x); Percent = v; Text = sprintf "%.1f%%" v; Part = false })
                    |> List.filter (fun s -> s.Percent >= 0.05) }
        | s, _ -> { m with Fighting = false; Status = "the fight did not come back (status " + string s + ")" }

/// One message: the model after it, and with it the question about the pinned passive when
/// something it depends on is another now.
let update (msg : Msg) (m : Model) : Model =
    let n = step msg m
    let n = if n.Swapped = m.Swapped && n.Items.Length = m.Items.Length then n else { n with Wearing = Map.toList n.Swapped; Equip = esc (equipText n.Items n.Swapped) }
    let same =
        n.Selected = m.Selected && n.Gem = m.Gem && n.Edited = m.Edited
        && n.Allocated = m.Allocated && n.Choices = m.Choices && n.Anointed = m.Anointed
        && n.Start = m.Start && n.Switches = m.Switches && n.Dropped = m.Dropped
        && n.Passives.Length = m.Passives.Length && n.Tab = m.Tab && n.Pinned = m.Pinned
        && n.Swapped = m.Swapped && n.Aim = m.Aim && n.Bag.Length = m.Bag.Length
    let n = if same then n else { n with Ask = compareOf n false; AskMore = compareOf n true }
    // (what an analysis found is of the character as it was then)
    let n = if n.Probe = "" || (match msg with | Rank | Analyse -> true | _ -> false) || (knobs n + fightKnobs n + passives n = knobs m + fightKnobs m + passives m) then n else weigh { n with Probe = ""; Probing = false; Probed = ""; Worth = [] }
    // (what is kept between visits: after anything but the pointer, a frame, an answer)
    let n =
        match msg with
        | PointerDown _ | PointerMove _ | PointerUp _ | Zoom _ | Sized _ | Tick _ | Fought _ | Warmed _ | Compared _ | Analysed _ | Tuned _ | Sources _ | Saved _ | Link _ | Import | Imported _ | Share | Shared _ | Unshare | TreeData _ | Sheet _ | Picked _ -> n
        | _ when not n.Restored -> n
        | _ -> (let k = kept n in if k = n.Kept then n else { n with Kept = k })
    // (what the passive looked at would take or give back, drawn on the tree)
    if n.Selected = m.Selected && n.Hovered = m.Hovered && n.Allocated = m.Allocated && n.Passives.Length = m.Passives.Length && n.Ascendancy = m.Ascendancy then n
    else plan n

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

/// "PowerCharge" as it is said: "Power Charge".
let private spaced (name : string) : string =
    name.Replace("Mote:", "Mote ").Replace("Charge", " Charge")

/// A count to begin the fight with: fewer, the count (pressed: as the fight decides again), more.
let private count (m : Model) (name : string) (most : int) : Node<Msg> =
    let said = Map.tryFind name m.Start
    let shown = match said with Some v -> string v + " of " + string most | None -> "as it comes"
    let fewer = match said with Some v -> v - 1 | None -> most - 1
    let more = match said with Some v -> v + 1 | None -> most
    div {
        Dom.Class (if said.IsSome then "knob said" else "knob")
        span { Dom.Class "what"; spaced name }
        button { Dom.OnClick (fun _ -> Begin (name, max 0 fewer)); "−" }
        button { Dom.Class "value"; Dom.OnClick (fun _ -> Begin (name, -1)); shown }
        button { Dom.OnClick (fun _ -> Begin (name, more)); "+" }
    }

let private switch (m : Model) (key : string) (name : string) : Node<Msg> =
    button {
        Dom.Class (if m.Switches.Contains key then "tab active" else "tab")
        Dom.OnClick (fun _ -> Switch key)
        name
    }

/// How far a difference may be off by chance, as it is written beside it.
let private margin (off : float) : string = if off >= 0.05 then sprintf "±%.1f" (2.0 * off) else ""

/// How much of the damage something is worth, as a class: "w" and how much ("0": nothing to
/// speak of, up to "3"), "wneg" where the damage is higher without it; "" while unknown.
let private worthClass (v : float) : string =
    if System.Double.IsNaN v then "" elif v < -1.0 then " wneg" elif v < 1.0 then " w0" elif v < 4.0 then " w1" elif v < 12.0 then " w2" else " w3"

let private worthText (v : float) : string =
    if System.Double.IsNaN v then "" elif abs v < 0.5 then "~0" else sprintf "%+.0f%%" v

/// What something is worth in every scene, in the order of the tabs (the scene looked at
/// stands out; "·": not known yet). Nothing before an analysis.
let private worths (m : Model) (key : string) : Node<Msg> =
    let found = scenarios |> List.map (fun s -> s, (m.Worth |> List.tryFind (fun (k, _) -> k = key + "@" + s) |> Option.map snd |> Option.defaultValue nan))
    let any = found |> List.exists (fun (_, v) -> not (System.Double.IsNaN v))
    let shown = found |> List.map (fun (s, v) -> s, (if s = m.Scenario then "here" else "") + worthClass v, (if System.Double.IsNaN v then "·" else worthText v))
    span {
        Dom.Class "worth four"
        if any then
            for (s, cls, text) in shown do
                span { Dom.Class cls; Dom.Attr ("title", s); text }
    }

/// A gem as the game's skill panel shows it: its picture (a skill's with its name beside).
/// What it is in a tooltip, under the mouse or once pressed; pressed, the tooltip stays and
/// has the button that turns the gem off (and the fights are fought again) or on.
let private gem (m : Model) (g : Gem) : Node<Msg> =
    let off = m.Dropped.Contains g.Key
    let pinned = m.Pinned = g.Key
    let kind = if g.Support then "gem support" elif g.Persistent then "gem persistent" else "gem"
    let level = "Level " + string g.Level
    let quality = "+" + string g.Quality + "% quality"
    let minions = string g.Minions + (if g.Minions = 1 then " minion" else " minions")
    let spirit = if g.Minions > 0 then sprintf "%s, %.0f Spirit each, %.0f in all" minions g.Spirit g.Reserved else sprintf "reserves %.0f Spirit" g.Spirit
    let turn = if off then "Turn on" else "Turn off"
    // (a skill's own picture; a support's gem)
    let picture = if g.Support then g.Own else g.Icon
    let faced = if g.Minions > 0 then string g.Minions + " × " + g.Named else g.Named
    let lvl = if g.Support then "" elif g.Quality > 0 then string g.Level + " / " + string g.Quality else string g.Level
    let what = if g.Support then "Support gem" elif g.Persistent then "Persistent skill" else "Skill gem"
    let group = g.Key.Substring (0, g.Key.IndexOf ':')
    let place = "|" + group + "|" + string g.At + "|"
    let without = sprintf "%.1f%% of the damage is lost without it" g.Gives
    let known = not (System.Double.IsNaN g.Gives)
    let least = if g.Support then 1 else 1
    div {
        Dom.Class (kind + (if off then " off" else "") + (if pinned then " pinned" else "") + worthClass g.Gives)
        Dom.Attr ("data-gem", g.Named)
        button {
            Dom.Class "face"
            Dom.OnClick (fun _ -> Pin (if pinned then "" else g.Key))
            if picture <> "" then img { Dom.Attr ("src", picture); Dom.Attr ("alt", "") }
            span { Dom.Class "name"; faced }
            span { Dom.Class "lvl"; lvl }
            if known then worths m ("gem:" + g.Key)
        }
        div {
            Dom.Class "gem-tip"
            button { Dom.Class "shut"; Dom.Attr ("aria-label", "close"); Dom.OnClick (fun _ -> Pin ""); "×" }
            if picture <> "" then img { Dom.Class "pic"; Dom.Attr ("src", picture); Dom.Attr ("alt", "") }
            div { Dom.Class "name"; g.Named }
            div { Dom.Class "what"; what }
            div { Dom.Class "line"; level }
            if g.Quality > 0 then div { Dom.Class "line"; quality }
            if g.Spirit > 0.0 then div { Dom.Class "line"; spirit }
            if off then div { Dom.Class "line offed"; "turned off" }
            if known then div { Dom.Class "line"; without }
            if pinned then
                div {
                    Dom.Class "actions steps"
                    button { Dom.Class "act"; Dom.OnClick (fun _ -> GemEdit ("set" + place + string (max least (g.Socketed - 1)) + "|" + string g.Quality)); "−" }
                    span { "level" }
                    button { Dom.Class "act"; Dom.OnClick (fun _ -> GemEdit ("set" + place + string (g.Socketed + 1) + "|" + string g.Quality)); "+" }
                    button { Dom.Class "act"; Dom.OnClick (fun _ -> GemEdit ("set" + place + string g.Socketed + "|" + string (max 0 (g.Quality - 5)))); "−" }
                    span { "quality" }
                    button { Dom.Class "act"; Dom.OnClick (fun _ -> GemEdit ("set" + place + string g.Socketed + "|" + string (min 20 (g.Quality + 5)))); "+" }
                }
            if pinned && g.Minions > 0 then
                div {
                    Dom.Class "actions steps"
                    button { Dom.Class "act"; Dom.OnClick (fun _ -> GemEdit ("count" + place + string (max 1 (g.Minions - 1)))); "−" }
                    span { minions }
                    button { Dom.Class "act"; Dom.OnClick (fun _ -> GemEdit ("count" + place + string (g.Minions + 1))); "+" }
                }
            if pinned then
                div {
                    Dom.Class "actions"
                    button { Dom.Class "act"; Dom.OnClick (fun _ -> Socket g.Key); turn }
                    button { Dom.Class "act"; Dom.OnClick (fun _ -> Pick ("swap:" + group + ":" + string g.At + (if g.Support then ":s" else ":a"))); "Swap" }
                    button { Dom.Class "act"; Dom.OnClick (fun _ -> GemEdit ("del|" + group + "|" + string g.At)); "Remove" }
                    button { Dom.Class "act close"; Dom.OnClick (fun _ -> Pin ""); "Close" }
                }
        }
    }

/// A socket group as a card (as poe.ninja lists them): its gems one under the other, a
/// skill's before its supports, each with its picture, name and level.
let private group (m : Model) (g : GemGroup) : Node<Msg> =
    let spirit = g.Gems |> List.sumBy (fun x -> if m.Dropped.Contains x.Key then 0.0 else x.Reserved)
    let reserved = sprintf "%.0f Spirit" spirit
    section {
        Dom.Class "group"
        for x in g.Gems do
            gem m x
        button { Dom.Class "act more"; Dom.OnClick (fun _ -> Pick ("add:" + string g.Number)); "+ support" }
        if spirit > 0.0 then div { Dom.Class "spirit"; reserved }
    }

/// The list of gems to choose from: a search, and the gems (those that fit the group they
/// are for; searched, all of the kind), each with what it says under the mouse.
let private picker (m : Model) : Node<Msg> =
    let title = if m.Picker.StartsWith "add" then "Add a support" elif m.Picker.StartsWith "swap" then "Swap for" else "Add a skill"
    let ranks = m.Picker.StartsWith "add" && not m.Offered.IsEmpty
    let waiting = m.Offers.IsEmpty
    div {
        Dom.Class "picker"
        div {
            Dom.Class "picker-head"
            h3 { title }
            input { Dom.Class "seek"; Dom.Attr ("type", "search"); Dom.Attr ("placeholder", "search by name or tag"); Dom.OnInput Filter }
            if ranks then button { Dom.Class "act"; Dom.OnClick (fun _ -> Rank); "Rank by damage" }
            if m.Probed <> "" then span { Dom.Class "depth"; m.Probed }
            button { Dom.Class "act close"; Dom.OnClick (fun _ -> Pick ""); "Close" }
        }
        if waiting then div { Dom.Class "depth"; "reading the gems ..." }
        div {
            Dom.Class "offers"
            for o in m.Offered do
                button {
                    Dom.Class ("offer c" + string o.Colour + (if o.Fits then "" else " unfit") + worthClass o.Adds)
                    Dom.Attr ("title", o.Tells)
                    Dom.OnClick (fun _ -> Choose o.Choice)
                    span { Dom.Class "name"; o.Choice }
                    span { Dom.Class "tags"; o.Tags }
                    worths m ("pick:" + o.Choice)
                }
        }
    }

/// The button that asks what everything is worth, and how far that is.
let private analyse (m : Model) (about : string) : Node<Msg> =
    let label = if m.Probing then "Analysing ... " + m.Probed else "Analyse"
    div {
        Dom.Class "analyse"
        button { Dom.Class "act"; Dom.OnClick (fun _ -> Analyse); label }
        span { Dom.Class "depth"; about }
        span { Dom.Class "depth"; "The four numbers: pack, map boss and pinnacle (how soon they are dead), and the pinnacle that lasts (damage)." }
        span { Dom.Class "legend"; span { Dom.Class "w3"; "much" }; span { Dom.Class "w2"; "some" }; span { Dom.Class "w1"; "little" }; span { Dom.Class "w0"; "none" }; span { Dom.Class "wneg"; "costs damage" } }
    }

/// A condition of pressing as it reads (a resource's name may have colons of its own).
let private condition (c : string) : string =
    let rest (k : int) = c.Substring (k + 1)
    match c.Split ':' |> List.ofArray with
    | [ "ready" ] -> "when ready" | [ "buff" ] -> "kept up" | [ "curse" ] -> "kept on the enemy" | [ "held" ] -> "when ready, not first"
    | "below" :: _ ->
        let r, n = c.Substring (6, c.LastIndexOf ':' - 6), rest (c.LastIndexOf ':')
        if n = "1" then "when out of " + r else "while below " + n + " " + r
    | "has" :: _ -> "enemy has " + rest 3 | "lacks" :: _ -> "enemy lacks " + rest 5
    | "up" :: _ -> rest 2 + " stands" | "down" :: _ -> "no " + rest 4 + " stands"
    | _ -> c

/// The conditions of one skill: those it has, to take away, and by kind those to add.
let private conditions (m : Model) (p : Pressed) : Node<Msg> =
    let parts = p.Code.Split '+' |> List.ofArray |> List.filter (fun c -> c <> "" && c <> "held")
    let free (cs : (string * string) list) = cs |> List.filter (fun (c, _) -> not (List.contains c parts))
    let rows =
        [ "pressed", free [ "ready", "when ready"; "buff", "kept up"; "curse", "kept on the enemy" ]
          "when out of", free (m.Counts |> List.map (fun (k, _) -> "below:" + k + ":1", k))
          "while not full of", free (m.Counts |> List.filter (fun (_, most) -> most > 1) |> List.map (fun (k, most) -> "below:" + k + ":" + string most, k))
          "enemy has", free (m.States |> List.map (fun x -> "has:" + x, x))
          "enemy lacks", free (m.States |> List.map (fun x -> "lacks:" + x, x))
          "stands", free (m.Standing |> List.map (fun x -> "up:" + x, x))
          "none stands", free (m.Standing |> List.map (fun x -> "down:" + x, x)) ]
        |> List.filter (fun (_, cs) -> not cs.IsEmpty)
    div {
        Dom.Class "conds"
        if not parts.IsEmpty then
            div {
                Dom.Class "actions"
                for c in parts do
                    button { Dom.Class "act on"; Dom.OnClick (fun _ -> Order ("cond-", p.Skill + "\n" + c)); condition c + " ×" }
            }
        for (kind, cs) in rows do
            div {
                Dom.Class "actions"
                span { Dom.Class "kind"; kind }
                for (c, name) in cs do
                    button { Dom.Class "act"; Dom.OnClick (fun _ -> Order ("cond+", p.Skill + "\n" + c)); name }
            }
    }

/// What is pressed, first to last, with the buttons that change the order.
let private pressing (m : Model) : Node<Msg> =
    let listed = m.Play |> List.map (fun p -> p.Skill)
    let others = m.Pressable |> List.filter (fun g -> not (List.contains g listed))
    let chosen = not m.Rotation.IsEmpty
    // (the conditions as they were said here, where they were)
    let play = m.Play |> List.map (fun p -> match m.Rotation |> List.tryFind (fun g -> g.Split('@').[0] = p.Skill) with | Some g when g.Contains "@" -> { p with Code = g.Substring (g.IndexOf '@' + 1) } | _ -> p)
    // (how it reads: what the simulator says of it, or the conditions said here)
    let how (p : Pressed) = if p.Code = "" || p.Code = "held" then p.How else (match p.Code.Split '+' with | [| one |] -> condition one | many -> string many.Length + " conditions")
    section {
        Dom.Class "panel pressed"
        h3 { "What is pressed" }
        for p in play do
            div {
                Dom.Class "press"
                if p.Face <> "" then img { Dom.Attr ("src", p.Face); Dom.Attr ("alt", "") }
                span { Dom.Class "name"; p.Skill }
                button { Dom.Class "act how"; Dom.Attr ("title", "when it is pressed: press to change"); Dom.OnClick (fun _ -> Order ("edit", p.Skill)); how p }
                button { Dom.Class "act"; Dom.OnClick (fun _ -> Order ("up", p.Skill)); "↑" }
                button { Dom.Class "act"; Dom.OnClick (fun _ -> Order ("down", p.Skill)); "↓" }
                button { Dom.Class "act"; Dom.OnClick (fun _ -> Order ("drop", p.Skill)); "×" }
            }
            if m.Editing = p.Skill then conditions m p
        div {
            Dom.Class "actions"
            for g in others do
                button { Dom.Class "act"; Dom.OnClick (fun _ -> Order ("add", g)); "+ " + g }
            if chosen then button { Dom.Class "act close"; Dom.OnClick (fun _ -> Order ("auto", "")); "As the skills say" }
        }
        div { Dom.Class "depth"; "The first whose conditions hold is pressed; the last whenever nothing else is. Press a condition to change it: all of a skill's conditions must hold." }
    }

/// An item in its place: its picture with what is in its sockets. What it says in a tooltip,
/// under the mouse or once pressed; pressed, the tooltip stays and tells what taking the
/// item off would do to the damage.
let private item (m : Model) (i : Item) (where : string) : Node<Msg> =
    let own = i.Raw = ""
    let key = (if own then "item:" else "bag:") + string i.Place
    let pinned = m.Pinned = key
    let asked = if pinned then m.Ask else ""
    // a pasted one: where it can be worn, where it is tried, whether it is worn
    let spots = if own then [] else fits m i
    let aim = if own then "" else aimOf m i
    let worn = not own && m.Wearing |> List.exists (fun (_, x) -> x.Place = i.Place)
    let about = if own then "Without it" elif worn then "Taken off again" elif aim = "" then "" else "Worn in place of " + spotName m aim
    let nowhere = not own && spots.IsEmpty
    let wear = if worn then "Take off" else "Wear"
    let refined = asked <> "" && m.DeltaOf = m.AskMore
    let known = asked <> "" && (m.DeltaOf = asked || refined)
    let depth = if refined then "10 fights each; grey: not told apart from chance" else "3 short fights each, refining ..."
    let needs = "Requires " + i.Requires
    let face = if i.Headed <> "" then i.Headed else i.Type
    let weighed = not (System.Double.IsNaN i.Weighs)
    let weight = sprintf "%.1f%% of the damage is lost without this item" i.Weighs
    div {
        Dom.Class ("item " + i.Grade + worthClass i.Weighs + (if pinned then " pinned" else "") + (if where = "auto" then " loose" else "") + (if own then "" else " pasted") + (if worn then " worn" else ""))
        Dom.Style [ Css.Prop ("grid-area", where) ]
        Dom.Attr ("data-item", i.Worn)
        button {
            Dom.Class "face"
            Dom.OnClick (fun _ -> Pin (if pinned then "" else key))
            if i.Picture <> "" then img { Dom.Attr ("src", i.Picture); Dom.Attr ("alt", "") }
            if i.Picture = "" then span { Dom.Class "name"; face }
        }
        // what is in its sockets, each with a tooltip of its own (under the mouse, or tapped)
        if not i.Holds.IsEmpty then
            div {
                Dom.Class "sockets"
                for h in i.Holds do
                    span {
                        Dom.Class "socket"
                        Dom.Attr ("tabindex", "0")
                        if h.Pic <> "" then img { Dom.Attr ("src", h.Pic); Dom.Attr ("alt", "") }
                        if h.Rune <> "" then
                            div {
                                Dom.Class "item-tip aug-tip"
                                div { Dom.Class "head"; h.Rune }
                                div {
                                    Dom.Class "body"
                                    for l in h.Does do
                                        div { Dom.Class "mod rune"; l }
                                }
                            }
                    }
            }
        div {
            Dom.Class "item-tip"
            button { Dom.Class "shut"; Dom.Attr ("aria-label", "close"); Dom.OnClick (fun _ -> Pin ""); "×" }
            div {
                Dom.Class "head"
                if i.Headed <> "" then div { i.Headed }
                div { i.Type }
            }
            if i.Picture <> "" then img { Dom.Class "pic"; Dom.Attr ("src", i.Picture); Dom.Attr ("alt", "") }
            div {
                Dom.Class "body"
                for p in i.Says do
                    div { Dom.Class "prop"; p }
                if i.Requires <> "" then div { Dom.Class "prop"; needs }
                for g in i.Grants do
                    div {
                        Dom.Class "mod grant"
                        if g.Shows <> "" then img { Dom.Attr ("src", g.Shows); Dom.Attr ("alt", "") }
                        span { g.Grant }
                    }
                for (n, l) in List.indexed i.Lines do
                    div { Dom.Class ("mod " + l.Mark + worthClass l.Lost); span { l.Reads }; worths m ("line:" + string i.Place + ":" + string n) }
                if weighed then div { Dom.Class ("depth" + worthClass i.Weighs); weight; worths m ("item:" + string i.Place) }
                if i.Spoiled then div { Dom.Class "corrupted"; "Corrupted" }
                for h in i.Holds do
                    if h.Rune <> "" then
                        div {
                            Dom.Class "holds"
                            if h.Pic <> "" then img { Dom.Attr ("src", h.Pic); Dom.Attr ("alt", "") }
                            span { h.Rune }
                            for l in h.Does do
                                div { Dom.Class "does"; l }
                        }
                if pinned && asked <> "" then
                    div {
                        Dom.Class "deltas"
                        div { Dom.Class "depth"; about }
                        if not known then div { Dom.Class "depth"; "fighting it out ..." }
                        if known then
                            for (name, by, off) in m.Deltas do
                                div {
                                    Dom.Class (if abs by <= 2.0 * off then "delta unsure" elif by > 0.05 then "delta up" elif by < -0.05 then "delta down" else "delta")
                                    span { name }
                                    span { Dom.Class "by"; Dom.Attr ("data-check", "delta"); sprintf "%+.1f%% damage" by }
                                    span { Dom.Class "pm"; margin off }
                                }
                        if known then div { Dom.Class "depth"; depth }
                    }
                if nowhere then div { Dom.Class "depth"; "nothing here to wear it in place of" }
                if pinned && not worn && spots.Length > 1 then
                    div {
                        Dom.Class "actions spots"
                        for spot in spots do
                            button { Dom.Class (if spot = aim then "act active" else "act"); Dom.OnClick (fun _ -> Aim spot); spotName m spot }
                    }
                if pinned then
                    div {
                        Dom.Class "actions"
                        if not own && not nowhere then button { Dom.Class "act"; Dom.OnClick (fun _ -> Wear i.Place); wear }
                        if not own then button { Dom.Class "act"; Dom.OnClick (fun _ -> Discard i.Place); "Remove" }
                        button { Dom.Class "act close"; Dom.OnClick (fun _ -> Pin ""); "Close" }
                    }
            }
        }
    }

/// The inventory: what is worn where it is worn, as the game lays it out, and under it the
/// jewels and whatever else has no place there.
let private inventory (m : Model) : Node<Msg> =
    // (a pasted item worn is shown where the character's own was)
    let worn =
        (m.Items |> List.filter (fun i -> area i <> "" && not (m.Wearing |> List.exists (fun (spot, _) -> spot = area i))) |> List.map (fun i -> area i, i))
        @ (m.Wearing |> List.filter (fun (spot, _) -> not (spot.StartsWith "at:")))
    let loose = m.Items |> List.filter (fun i -> area i = "") |> List.map (fun i -> m.Wearing |> List.tryFind (fun (spot, _) -> spot = "at:" + string i.Place) |> Option.map snd |> Option.defaultValue i)
    let taken = worn |> List.map fst |> Set.ofList
    let empty = places |> List.filter (fun (a, _) -> not (taken.Contains a))
    let adding = if m.Pasting then "×" else "+"
    // (one that is worn is on the doll, not here as well)
    let unworn = m.Bag |> List.filter (fun i -> not (m.Wearing |> List.exists (fun (_, x) -> x.Place = i.Place)))
    div {
        Dom.Class "main items"
        analyse m "what every line is worth: each is taken away and the fight fought without it (this takes a minute)"
        div {
            Dom.Class "doll"
            for (a, label) in empty do
                div { Dom.Class "item empty"; Dom.Style [ Css.Prop ("grid-area", a) ]; label }
            for (spot, i) in worn do
                item m i spot
        }
        div {
            Dom.Class "loose-items"
            for i in loose do
                item m i "auto"
        }
        // items pasted in, to try in place of what is worn
        div {
            Dom.Class "bag"
            for i in unworn do
                item m i "auto"
            button { Dom.Class "item loose add"; Dom.Attr ("title", "paste an item"); Dom.OnClick (fun _ -> Paste); adding }
        }
        if m.Pasting then
            div {
                Dom.Class "paste"
                textarea { Dom.Class "draft"; Dom.Attr ("placeholder", "paste an item here: copied from the trade site, or Ctrl+C on it in the game"); Dom.Attr ("rows", "8"); Dom.OnInput Draft }
                button { Dom.Class "act"; Dom.OnClick (fun _ -> Add); "Add" }
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

/// The colour of what an analysis found a passive worth.
let private worthTint (v : float) : V4d =
    if v < -1.0 then V4d (1.0, 0.25, 0.5, 0.35) elif v < 1.0 then V4d (0.45, 0.45, 0.45, 0.35) elif v < 4.0 then V4d (0.85, 0.8, 0.6, 0.35) elif v < 12.0 then V4d (1.0, 0.8, 0.25, 0.35) else V4d (1.0, 0.5, 0.1, 0.35)

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
        Sg.Uniform ("Tint", (match Map.tryFind p.Id m.NodeWorth with | Some v -> worthTint v | None -> white))
        quad
    }

/// The fight's picture: a small field seen from above, the player in the middle.
let arena (m : Model) : Node<Msg> =
    renderControl {
        Dom.Style [ Css.Prop ("display", "block"); Css.Width "min(340px, 100%)"; Css.Height "min(170px, 44vw)"; Css.Prop ("pointer-events", "none")
                    Css.Prop ("visibility", (if m.Happened.IsEmpty then "hidden" else "visible")) ]
        RenderControl.Orthographic (m.Field, 1.0, 400.0)
        RenderControl.Camera { Sky = V3d (0.0, 1.0, 0.0); Location = V3d (m.Middle.X, m.Middle.Y, 200.0); Forward = V3d (0.0, 0.0, -1.0); Up = V3d (0.0, 1.0, 0.0); Right = V3d (1.0, 0.0, 0.0) }
        RenderControl.OnRendered Tick
        RenderControl.Animating (not m.Happened.IsEmpty && m.Tab = "character")
        sg {
            Sg.Shader { Shaders.flat }
            Sg.NoEvents true
            sg {
                Sg.Uniform ("Color", V4d (0.047, 0.036, 0.027, 1.0))
                Sg.Translate (0.0, 0.0, -1.0)
                Sg.Scale (100000.0)
                quad
            }
            for l in m.Stage do
                sg {
                    Sg.Uniform ("Color", l.Color)
                    mesh Topology.Triangles { Mesh.Attribute ("Positions", l.Points); Mesh.Attribute ("DiffuseColorCoordinates", l.Uvs) }
                }
        }
    }

let tree (m : Model) : Node<Msg> =
    renderControl {
        // (fixed to the window: as tall as a long page, it would be more than a phone's GPU draws)
        Dom.Style [ Css.Position "fixed"; Css.Left "0"; Css.Top "64px"; Css.Width "100%"; Css.Height "calc(100% - 64px)"; Css.TouchAction "none"; Css.UserSelect "none"
                    Css.Prop ("visibility", (if m.Tab = "tree" then "visible" else "hidden")) ]
        Dom.OnPointerDown PointerDown
        Dom.OnPointerMove PointerMove
        Dom.OnPointerUp PointerUp
        RenderControl.Perspective (60.0, m.Near, m.Far)
        RenderControl.Camera m.Camera
        RenderControl.OnResize Sized
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
                sg {
                    Sg.Uniform ("Color", V4d (0.227, 0.227, 0.306, 1.0))
                    mesh Topology.Triangles { Mesh.Attribute ("Positions", m.Web); Mesh.Attribute ("DiffuseColorCoordinates", m.WebUv) }
                }
                // what the search found
                sg {
                    Sg.Uniform ("Color", V4d (1.0, 0.25, 0.85, 1.0))
                    mesh Topology.Triangles { Mesh.Attribute ("Positions", m.Found); Mesh.Attribute ("DiffuseColorCoordinates", m.FoundUv) }
                }
                // the radii of jewels
                sg {
                    Sg.Uniform ("Color", V4d (0.45, 0.68, 1.0, 1.0))
                    mesh Topology.Triangles { Mesh.Attribute ("Positions", m.Rings); Mesh.Attribute ("DiffuseColorCoordinates", m.RingsUv) }
                }
                // (the connections of passives that are only there for some: the Oracle's)
                sg {
                    Sg.Uniform ("Color", V4d (0.227, 0.227, 0.306, 1.0))
                    mesh Topology.Triangles { Mesh.Attribute ("Positions", m.Opened); Mesh.Attribute ("DiffuseColorCoordinates", m.OpenedUv) }
                }
                sg {
                    Sg.Uniform ("Color", V4d (0.82, 0.74, 0.47, 1.0))
                    Sg.Translate (0.0, 0.0, 0.7)
                    mesh Topology.Triangles { Mesh.Attribute ("Positions", m.Path); Mesh.Attribute ("DiffuseColorCoordinates", m.PathUv) }
                }
                // what the passive looked at would take (green) or give back (red)
                sg {
                    Sg.Uniform ("Color", V4d (0.2, 1.0, 0.47, 1.0))
                    Sg.Translate (0.0, 0.0, 0.8)
                    mesh Topology.Triangles { Mesh.Attribute ("Positions", m.Gain); Mesh.Attribute ("DiffuseColorCoordinates", m.GainUv) }
                }
                sg {
                    Sg.Uniform ("Color", V4d (0.95, 0.1, 0.15, 1.0))
                    Sg.Translate (0.0, 0.0, 0.8)
                    mesh Topology.Triangles { Mesh.Attribute ("Positions", m.Loss); Mesh.Attribute ("DiffuseColorCoordinates", m.LossUv) }
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
    // (a passive allocated without a way to it in a jewel's radius costs its point all the same)
    let zones = areas m m.Allocated
    let leapt = m.Passives |> Array.filter (fun p -> m.Allocated.Contains p.Id && not (spent.Contains p.Id) && free zones p) |> Array.length
    let granted = m.Allocated.Count - spent.Count - leapt
    sprintf "%d of %d points, %d ascendancy%s" (spent.Count - asc - starts + leapt) (m.Level - 1 + 24) asc (if granted > 0 then sprintf ", %d granted" granted else "")

/// The tooltip of the pinned passive (with what can be done to it) or the one under the mouse.
let private tooltip (m : Model) : Node<Msg> =
    let pinned = m.Selected.IsSome
    match (if pinned then m.Selected else m.Hovered) |> Option.bind (fun i -> m.Passives |> Array.tryFind (fun p -> p.Id = i)) with
    | None -> div { Dom.Class "passive-tip none" }
    | Some p ->
        let has = m.Allocated.Contains p.Id
        let asked = if pinned then m.Ask else ""
        let refined = asked <> "" && m.DeltaOf = m.AskMore
        let known = asked <> "" && (m.DeltaOf = asked || refined)
        let depth = if refined then "10 fights each; grey: not told apart from chance" else "3 short fights each, refining ..."
        let title = match Map.tryFind p.Id m.Choices with Some c when has -> c | _ -> p.Name
        // beside the passive where it is drawn (an ascendancy's: around the middle), kept on the page
        let q = m.Shown |> List.tryFind (fun s -> s.Id = p.Id) |> Option.defaultValue p
        let s = size m
        let u = m.Height / max 1.0 s.X
        let x = (q.X - m.Centre.X) / u + s.X / 2.0 + q.Size / u / 2.0 + 10.0
        let y = top + s.Y / 2.0 + (q.Y + m.Centre.Y) / u + q.Size / u / 2.0 + 6.0
        let wide = min 380.0 (m.Page.X - 24.0)
        let state = if m.Anointed.Contains p.Id then "Allocated by an item" elif has then "Allocated" else "Unallocated"
        let weighs = Map.tryFind p.Id m.NodeWorth |> Option.defaultValue nan
        let worth = if System.Double.IsNaN weighs then "" else sprintf "%.1f%% of the damage is lost without it" weighs
        // how many passives the button takes or gives back, this one with them
        let cost =
            if m.Change > 1 then sprintf "takes %d passives (%d on the way)" m.Change (m.Change - 1)
            elif m.Change = 1 then "takes 1 passive"
            elif m.Change < -1 then sprintf "frees %d passives (%d that hang on it)" (-m.Change) (-m.Change - 1)
            elif m.Change = -1 then "frees 1 passive"
            else ""
        let costs = if m.Change > 0 then "cost up" else "cost down"
        // (a chosen attribute grants that one)
        let stats = match Map.tryFind p.Id m.Choices with Some c when has -> p.Stats |> List.map (fun s -> s.Replace ("any Attribute", c)) | _ -> p.Stats
        let j = Map.tryFind p.Id m.Jewels
        let jewel = match j with Some j -> j.Mods | None -> []
        let called = match j with Some j -> j.Called | None -> ""
        let based = match j with Some j -> j.Base | None -> ""
        let rarity = match j with Some j -> j.Rarity.ToLower () | None -> ""
        // (about how tall it gets, to keep all of it on the page)
        let tall = 110.0 + 22.0 * float p.Stats.Length + (if jewel.IsEmpty then 0.0 else 80.0 + 22.0 * float jewel.Length) + (if pinned then 60.0 else 0.0) + (if pinned && not p.Options.IsEmpty then 120.0 else 0.0)
        div {
            Dom.Style [ Css.Left (sprintf "%.0fpx" (max 12.0 (min x (m.Page.X - wide - 12.0)))); Css.Top (sprintf "%.0fpx" (max (top + 8.0) (min y (m.Page.Y - tall)))); Css.Width (sprintf "%.0fpx" wide) ]
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
            if cost <> "" then div { Dom.Class costs; Dom.Attr ("data-check", "cost"); cost }
            if worth <> "" then div { Dom.Class ("cost" + worthClass weighs); worth; worths m ("node:" + string p.Id) }
            if not p.Options.IsEmpty && pinned then
                div {
                    Dom.Class "select"
                    div { Dom.Class "one"; "Select One:" }
                    for o in p.Options do
                        button { Dom.Class ("choice " + o + (if has && Map.tryFind p.Id m.Choices = Some o then " active" else "")); Dom.OnClick (fun _ -> Allocate (p.Id, o)); "+5 to " + o }
                }
            if pinned then
                div {
                    Dom.Class "actions"
                    if p.Options.IsEmpty && not has then
                        button { Dom.Class "act"; Dom.OnClick (fun _ -> Allocate (p.Id, "")); "Allocate" }
                    if has && not (m.Anointed.Contains p.Id) then
                        button { Dom.Class "act"; Dom.OnClick (fun _ -> Deallocate p.Id); "Refund" }
                    button { Dom.Class "act close"; Dom.OnClick (fun _ -> Close); "Close" }
                }
            if pinned && asked <> "" then
                div {
                    Dom.Class "deltas"
                    if not known then div { Dom.Class "depth"; "fighting it out ..." }
                    if known then
                        for (name, by, off) in m.Deltas do
                            div {
                                Dom.Class (if abs by <= 2.0 * off then "delta unsure" elif by > 0.05 then "delta up" elif by < -0.05 then "delta down" else "delta")
                                span { name }
                                span { Dom.Class "by"; Dom.Attr ("data-check", "delta"); sprintf "%+.1f%% damage" by }
                                span { Dom.Class "pm"; margin off }
                            }
                    if known then div { Dom.Class "depth"; Dom.Attr ("data-check", "depth"); depth }
                }
            if not pinned then
                div { Dom.Class "state"; state }
        }

let private status (m : Model) =
    if m.Fighting then "fighting ..."
    elif m.More then sprintf "%d fights, more coming" m.Runs
    elif m.Status = "" && m.Runs > 0 then sprintf "%d fights" m.Runs
    else m.Status

let view (m : Model) : Node<Msg> =
    // (the game's data names an ascendancy it has no word for as "Mercenary3": the one the
    // character's passives belong to is the one)
    // (never the head of a list that may be empty: the text is worked out again before the
    // element that holds it is taken away)
    let headline = match m.Dps with | d :: _ when d.Label = "DPS" -> d.Value + " DPS" | d :: _ -> "dead in " + d.Value | [] -> ""
    let undo = "Undo (" + string m.GemEdits.Length + ")"
    let hasMinions = m.Groups |> List.exists (fun g -> g.Gems |> List.exists (fun x -> x.Minions > 0))
    let tune = if m.Tuning <> "" then "Trying minion counts ..." else "Best minion counts"
    let here = m.Characters |> List.exists (fun (_, _, url) -> url = m.Build)
    let found = if m.Sought.Trim().Length >= 2 then string m.Hits + " found" else ""
    let opening = if m.Importing <> "" then "Opening ..." else "Open"
    let analysing = if m.Probing then "Analysing " + m.Probed else "Analyse"
    let named = if m.Title = "" then "" else sprintf "Level %d %s" m.Level (if m.Ascendancy <> "" then m.Ascendancy else m.Class)
    let chosen' = if named = "" then "choose a character" else named
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
            if warming m <> "" then span { Dom.Fetch (fightOf m (warming m) "&wait=1", Warmed) }
            if (m.Tab = "tree" || m.Tab = "items") && m.Ask <> "" then span { Dom.Fetch (m.Ask, Compared) }
            if m.Probing then span { Dom.Fetch (probeUrl m, Analysed) }
            if m.Tuning <> "" then span { Dom.Fetch (m.Tuning, Tuned) }
            if m.Build <> "" then span { Dom.Fetch (restoreUrl m, Restore) }
            if m.Going <> "" then span { Dom.Fetch ("__poe?op=" + m.Going, Saved) }
            if m.Sharing <> "" then span { Dom.Fetch (m.Sharing, Shared) }
            if m.Restored && m.Kept <> "" then span { Dom.Fetch (saveUrl m, Saved) }
            if m.Picker <> "" then span { Dom.Fetch (pickUrl m, Picked) }
            if m.Tab = "calcs" && m.Title <> "" then span { Dom.Fetch (calcsUrl m, Sources) }
            if (m.Tab = "tree" || m.Tab = "items") && m.Ask <> "" && (m.DeltaOf = m.Ask || m.DeltaOf = m.AskMore) then span { Dom.Fetch (m.AskMore, Compared) }
        }
        span { Dom.Fetch ("tree.json", TreeData) }
        span { Dom.Fetch ("__poe?op=account", Account) }
        div {
            if m.Tab = "tree" then
                tooltip m
            // (the tree's own bar, under the header: points, the search, the analysis)
            if m.Tab = "tree" then
                div {
                    Dom.Class "treebar"
                    div { Dom.Class "points"; Dom.Attr ("data-check", "points"); points m }
                    input { Dom.Class "seek"; Dom.Attr ("type", "search"); Dom.Attr ("placeholder", "search the tree"); Dom.OnInput Seek }
                    div { Dom.Class "found"; Dom.Attr ("data-check", "found"); found }
                    button { Dom.Class "act"; Dom.Attr ("title", "what every notable and keystone allocated is worth: each is given back alone and the fight fought without it"); Dom.OnClick (fun _ -> Analyse); analysing }
                }
        }
        if m.ShareLink <> "" || m.ShareSaid <> "" then
            div {
                Dom.Class "shared"
                if m.ShareLink <> "" then input { Dom.Attr ("type", "text"); Dom.Attr ("readonly", "readonly"); Dom.Attr ("value", m.ShareLink); Dom.Attr ("onfocus", "this.select()") }
                if m.ShareSaid <> "" then span { Dom.Class "said"; m.ShareSaid }
                if m.Sharing = "" then button { Dom.Class "act"; Dom.OnClick (fun _ -> Unshare); "close" }
            }
        header {
            h1 { "PoEtry" }
            if m.Art <> "" then img { Dom.Class "crest"; Dom.Attr ("src", m.Art); Dom.Attr ("alt", "") }
            if m.Characters.IsEmpty then
                div { Dom.Class "title"; Dom.Attr ("data-check", "title"); named }
            if not m.Characters.IsEmpty then
                select {
                    Dom.Class "pick"
                    Dom.OnChange Open
                    if not here then option { Dom.Attr ("value", ""); chosen' }
                    for (name, about, _) in m.Characters do
                        option { Dom.Attr ("value", name); about }
                    option { Dom.Attr ("value", "*"); "all characters ..." }
                }
            if m.Build <> "" then button { Dom.Class "login share"; Dom.Attr ("title", "a short address for this build as it is set up here: rotation, changed gems, items and passives"); Dom.OnClick (fun _ -> Share); "share" }
            if m.CanLogin && m.Player = "" then button { Dom.Class "login"; Dom.OnClick (fun _ -> Login); "login" }
            if m.CanLogin && m.Player <> "" then button { Dom.Class "login"; Dom.Attr ("title", "logged in as " + m.Player); Dom.OnClick (fun _ -> Logout); "logout" }
            // (on a phone the tabs are a list to choose from)
            select {
                Dom.Class "tabs"
                Dom.OnChange Tab
                option { Dom.Attr ("value", "character"); "Character" }
                option { Dom.Attr ("value", "defence"); "Defence" }
                option { Dom.Attr ("value", "items"); "Items" }
                option { Dom.Attr ("value", "gems"); "Gems" }
                option { Dom.Attr ("value", "tree"); "Passive tree" }
                option { Dom.Attr ("value", "calcs"); "Calcs" }
                option { Dom.Attr ("value", "config"); "Config" }
                option { Dom.Attr ("value", "notes"); "Notes" }
            }
            nav {
                tab m "character" "Character"
                tab m "defence" "Defence"
                tab m "items" "Items"
                tab m "gems" "Gems"
                tab m "tree" "Passive tree"
                tab m "calcs" "Calcs"
                tab m "config" "Config"
                tab m "notes" "Notes"
            }
            div { Dom.Attr ("data-check", "pointer"); Dom.Style [ Css.Display "none" ]; sprintf "%.0fx%.0f touches %d" m.Page.X m.Page.Y m.Touches.Length }
            if not m.Dps.IsEmpty then
                div { Dom.Class "headline"; Dom.Attr ("data-check", "dps"); headline }
            div { Dom.Class "status"; Dom.Attr ("data-check", "status"); status m }
        }
        if m.Tab = "defence" then
            div {
                Dom.Class "main defence"
                for (title, rows) in m.Guards do
                    panel title rows
                div {
                    Dom.Class "survival"
                    nav {
                        scenario m "pack" "Pack"
                        scenario m "boss" "Map boss"
                        scenario m "pinnacle" "Pinnacle"
                        scenario m "endless" "Endless"
                    }
                    panel "In this fight" m.Survival
                }
            }
        if m.Tab = "items" then
            inventory m
        if m.Tab = "gems" then
            div {
                Dom.Class "main gems"
                analyse m "what every gem is worth: each is turned off and the fight fought without it"
                div {
                    Dom.Class "actions gem-acts"
                    button { Dom.Class "act"; Dom.OnClick (fun _ -> Pick "new"); "+ skill" }
                    if not m.GemEdits.IsEmpty then button { Dom.Class "act"; Dom.OnClick (fun _ -> UndoGems); undo }
                    if hasMinions then button { Dom.Class "act"; Dom.OnClick (fun _ -> Tune); tune }
                    if m.TuneSaid <> "" then span { Dom.Class "depth"; m.TuneSaid }
                }
                if m.Picker <> "" then picker m
                for g in m.Groups do
                    group m g
            }
        if m.Tab = "calcs" then
            div {
                Dom.Class "main calcs"
                div {
                    Dom.Class "analyse"
                    input { Dom.Class "seek"; Dom.Attr ("type", "search"); Dom.Attr ("placeholder", "search a number or a source"); Dom.OnInput Among }
                    span { Dom.Class "depth"; "every modifier of the character, by what it changes and where it comes from" }
                }
                for (title, rows) in m.CalcShown do
                    panel title rows
            }
        if m.Tab = "notes" then
            div {
                Dom.Class "main notes"
                section {
                    Dom.Class "panel custom"
                    h3 { "Notes" }
                    textarea { Dom.Class "draft"; Dom.Attr ("placeholder", "kept in this browser, with everything else changed here"); Dom.Attr ("rows", "18"); Dom.OnInput Note; m.NotesWere }
                }
            }
        if m.Tab = "config" then
            div {
                Dom.Class "main config"
                section {
                    Dom.Class "panel"
                    h3 { "The enemies" }
                    div { Dom.Class "knob"; span { Dom.Class "what"; "Level" }; input { Dom.Attr ("type", "number"); Dom.Attr ("placeholder", "83"); Dom.OnInput (fun v -> Enemy ("level", v)) } }
                    div { Dom.Class "knob"; span { Dom.Class "what"; "Elemental resistances" }; input { Dom.Attr ("type", "number"); Dom.Attr ("placeholder", "as they are"); Dom.OnInput (fun v -> Enemy ("eres", v)) } }
                    div { Dom.Class "knob"; span { Dom.Class "what"; "Chaos resistance" }; input { Dom.Attr ("type", "number"); Dom.Attr ("placeholder", "as it is"); Dom.OnInput (fun v -> Enemy ("cres", v)) } }
                }
                section {
                    Dom.Class "panel custom"
                    h3 { "Modifiers of your own" }
                    textarea { Dom.Class "draft"; Dom.Attr ("placeholder", "one line each, as an item would say it:\n20% increased Attack Speed\n+100 to maximum Life"); Dom.Attr ("rows", "8"); Dom.OnInput Lines }
                    div { Dom.Class "actions"; button { Dom.Class "act"; Dom.OnClick (fun _ -> Apply); "Apply" } }
                }
            }
        // no character yet: the account's characters to choose from, or the way to log in
        if m.Build = "" then
            div {
                Dom.Class "main landing"
                if m.CanLogin && m.Player = "" then
                    button { Dom.Class "act enter"; Dom.OnClick (fun _ -> Login); "Log in with your Path of Exile account" }
                if not m.Asked then
                    div { Dom.Class "depth waiting"; "looking for your characters ..." }
                div {
                    Dom.Class "import"
                    textarea { Dom.Class "draft"; Dom.Attr ("rows", "3"); Dom.Attr ("placeholder", "or paste a build: a Path of Building code, a pobb.in link, or the address of a character's page on poe.ninja"); Dom.OnInput Link }
                    div {
                        Dom.Class "actions"
                        button { Dom.Class "act"; Dom.OnClick (fun _ -> Import); opening }
                        if m.ImportSaid <> "" then span { Dom.Class "said"; m.ImportSaid }
                    }
                    if m.Importing <> "" then span { Dom.Fetch (m.Importing, Imported) }
                }
                for (league, count, heroes) in m.Running do
                    details {
                        Dom.Attr ("open", "open")
                        summary { span { league }; span { Dom.Class "count"; count } }
                        for (name, about) in heroes do
                            button { Dom.Class "act character"; Dom.OnClick (fun _ -> Open name); about }
                    }
                for (league, count, heroes) in m.Lasting do
                    details {
                        summary { span { league }; span { Dom.Class "count"; count } }
                        for (name, about) in heroes do
                            button { Dom.Class "act character"; Dom.OnClick (fun _ -> Open name); about }
                    }
            }
        if m.Tab = "character" && m.Build <> "" then
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
                        scenario m "pack" "Pack"
                        scenario m "boss" "Map boss"
                        scenario m "pinnacle" "Pinnacle"
                        scenario m "endless" "Endless"
                    }
                    section {
                        Dom.Class "panel going"
                        h3 { "Going into the fight" }
                        for (name, most) in m.Counts do
                            count m name most
                        div {
                            Dom.Class "switches"
                            switch m "lowlife" "On low life"
                            switch m "infused" "Always infused"
                            switch m "noflask" "No flasks"
                            switch m "self" "Cast on self"
                            switch m "immortal" "Takes no damage, pays no costs"
                        }
                    }
                    section {
                        Dom.Class "panel fight"
                        h3 { "The fight" }
                        arena m
                    }
                    if m.Gem <> "" then div { Dom.Class "pressing"; Dom.Attr ("data-check", "gem"); "pressing " + m.Gem }
                    pressing m
                    panel "Damage" m.Dps
                    section {
                        Dom.Class "panel"
                        h3 { "By damage type" }
                        for s in m.Types do
                            div {
                                Dom.Class ("share " + s.Name.ToLower())
                                div { Dom.Class "bar"; Dom.Style [ Css.Width (sprintf "%.1f%%" s.Percent) ] }
                                span { Dom.Class "name"; s.Name }
                                span { Dom.Class "pct"; s.Text }
                            }
                    }
                    section {
                        Dom.Class "panel"
                        h3 { "Who deals it" }
                        for s in m.Shares do
                            div {
                                Dom.Class (if s.Part then "share part" else "share")
                                div { Dom.Class "bar"; Dom.Style [ Css.Width (sprintf "%.1f%%" s.Percent) ] }
                                span { Dom.Class "name"; s.Name }
                                span { Dom.Class "pct"; s.Text }
                            }
                    }
                }
            }
    }
