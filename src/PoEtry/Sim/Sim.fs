namespace PoEtry

/// The reference simulator. This module is all the rest of the program sees of it; the
/// engine itself is the files beside this one (namespace `PoEtry.Engine`, see ENGINE.md).
module Sim =
    /// Simulate one fight with the given seed.
    let run (fight: Fight) (seed: uint32) : Outcome = Engine.Run.runWith None false fight seed

    /// The same fight (the same numbers), with what happened kept for showing it.
    let shown (fight: Fight) (seed: uint32) : Outcome = Engine.Run.runWith None true fight seed

    /// The same fight (the same numbers), telling `tell` every event as it is taken from the
    /// queue (time, kind, who, what, its number in the queue's order) and every hit that lands
    /// (time, "hit" or "crit", the enemy, the skill, the damage dealt): for comparing another
    /// engine with this one, event by event.
    let traced (tell: float -> string -> string -> string -> float -> unit) (fight: Fight) (seed: uint32) : Outcome =
        Engine.Run.runWith (Some tell) false fight seed

    /// The same fight (the same numbers), with what the player could take at every `every`
    /// seconds of it: the largest hit of each type, and how much the fight's monster would
    /// have to deal. Read off the worlds the fight goes through; it changes nothing in it.
    let defences (every: float) (fight: Fight) (seed: uint32) : Outcome * DefenceSample list = Engine.Run.runProbed every fight seed

    // ---- parts of the engine that the fast engine (`Flat`) and the setup use as they are ----

    let streamHash (key: string) : uint32 = Engine.State.streamHash key
    let streamSeedOf (seed: uint32) (h: uint32) (n: int) : uint32 = Engine.State.streamSeedOf seed h n
    let dotKiller : Skill = Engine.Lookup.dotKiller
    let asksBuff (fight: Fight) (name: string) = Engine.Lookup.asksBuff fight name
    let waitLimit (p: Skill) = Engine.Effects.waitLimit p
    let partDelay (p: Skill) = Engine.Effects.partDelay p
    let groundRadius (g: Ground) (age: float) = Engine.Effects.groundRadius g age
    let groundCentre (g: Ground) (from: Vec) (towards: Vec) (age: float) = Engine.Effects.groundCentre g from towards age
    let outliving (actor: Actor) : Set<string> = Engine.Skills.outliving actor
    let nextRevival (now: float) (seconds: float) (waiting: (float * int) option) : float * int = Engine.Monsters.nextRevival now seconds waiting
