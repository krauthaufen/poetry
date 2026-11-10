namespace PoEsie

/// The five damage types, declared in PoE2's conversion order.
/// Damage only ever converts to a type further down this list.
type DamageType =
    | Physical
    | Lightning
    | Cold
    | Fire
    | Chaos

module DamageType =
    let all = [ Physical; Lightning; Cold; Fire; Chaos ]
    let elemental = [ Lightning; Cold; Fire ]

    let name t =
        match t with
        | Physical -> "Physical"
        | Lightning -> "Lightning"
        | Cold -> "Cold"
        | Fire -> "Fire"
        | Chaos -> "Chaos"

    let isElemental t =
        match t with
        | Lightning | Cold | Fire -> true
        | Physical | Chaos -> false

/// Amount of damage per type. Missing keys mean zero.
type Dmg = Map<DamageType, float>

module Dmg =
    let zero: Dmg = Map.empty
    let get t (d: Dmg) = Map.tryFind t d |> Option.defaultValue 0.0
    let add t v (d: Dmg) = if v = 0.0 then d else Map.add t (get t d + v) d
    let total (d: Dmg) = d |> Map.fold (fun acc _ v -> acc + v) 0.0
    let map f (d: Dmg) : Dmg = d |> Map.map (fun t v -> f t v)
    let sumOf types (d: Dmg) = types |> List.sumBy (fun t -> get t d)

    let combine (a: Dmg) (b: Dmg) : Dmg =
        b |> Map.fold (fun acc t v -> add t v acc) a

type Range = { Min: float; Max: float }

module Range =
    let zero = { Min = 0.0; Max = 0.0 }
    let fixedAt v = { Min = v; Max = v }
    let scale k r = { Min = r.Min * k; Max = r.Max * k }
    let add a b = { Min = a.Min + b.Min; Max = a.Max + b.Max }
    let avg r = (r.Min + r.Max) / 2.0

/// Skill types, as used by modifier conditions ("attack damage", "spell crit"...).
type SkillTag =
    | Attack
    | Spell
    | Melee
    | Projectile
    | Area
    | Strike
    | Slam
    | Duration
    | Channelling
    | Triggered
    | Minion
    | Totem
    | Warcry
    | Bow
    | Crossbow
    | Mace
    | Quarterstaff
    | Spear
    | Sword
    | Axe
    | Dagger
    | Claw
    | Flail
    | Wand
    | Staff
    | Unarmed
    | Herald
    | Curse
    | Nova
    | Chaining
    | Tag of string // escape hatch for skill-specific tags

/// Everything that can be inflicted on a target besides plain damage.
type Ailment =
    // damaging
    | Ignite
    | Bleed
    | Poison
    // elemental, chance from hit damage vs ailment threshold
    | Shock
    | Chill
    // buildup against poise / ailment threshold
    | Freeze
    | Electrocute
    | HeavyStun
    | Pin
    // flat chance
    | Daze
    | Maim
    | Blind
    | Hinder

module Ailment =
    let damaging = [ Ignite; Bleed; Poison ]
    let buildup = [ Freeze; Electrocute; HeavyStun; Pin ]
    let flatChance = [ Bleed; Poison; Daze; Maim; Blind; Hinder ]

    let name a =
        match a with
        | Ignite -> "Ignite"
        | Bleed -> "Bleed"
        | Poison -> "Poison"
        | Shock -> "Shock"
        | Chill -> "Chill"
        | Freeze -> "Freeze"
        | Electrocute -> "Electrocute"
        | HeavyStun -> "HeavyStun"
        | Pin -> "Pin"
        | Daze -> "Daze"
        | Maim -> "Maim"
        | Blind -> "Blind"
        | Hinder -> "Hinder"

    /// Status key the ailment shows up as on the target ("Ignited", "Shocked", ...).
    let status a =
        match a with
        | Ignite -> "Ignited"
        | Bleed -> "Bleeding"
        | Poison -> "Poisoned"
        | Shock -> "Shocked"
        | Chill -> "Chilled"
        | Freeze -> "Frozen"
        | Electrocute -> "Electrocuted"
        | HeavyStun -> "HeavilyStunned"
        | Pin -> "Pinned"
        | Daze -> "Dazed"
        | Maim -> "Maimed"
        | Blind -> "Blinded"
        | Hinder -> "Hindered"
