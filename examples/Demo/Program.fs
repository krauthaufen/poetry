/// Illustrative build (numbers are made up, not from game data): a lightning
/// spell that shocks, a meta gem that triggers a fire spell on crit, a curse
/// that lowers fire resistance, and a minion. Shows the pieces fitting together.
module Demo

open PoEsie

let bolt =
    { Skill.make "Arc Bolt" [ Spell; Projectile ] with
        BaseDamage = Map.ofList [ Lightning, { Min = 20.0; Max = 380.0 } ]
        BaseCrit = 9.0
        BaseTime = 0.8 }

let flare =
    { Skill.make "Flare" [ Spell; Area; Triggered ] with
        BaseDamage = Map.ofList [ Fire, { Min = 900.0; Max = 1300.0 } ]
        BaseCrit = 12.0 }

let curse =
    { Skill.make "Ember Curse" [ Spell; Curse ] with
        BaseTime = 0.6
        Cooldown = Some 6.0
        Hits = [] }

let player =
    { Actor.make
          "Player"
          [ Mod.inc (Damage(OfType Lightning)) 120.0
            Mod.inc (Damage(OfType Fire)) 90.0
            Mod.inc (Damage AllDamage) 40.0
            Mod.inc CritChance 150.0
            Mod.base' CritBonus 60.0
            Mod.inc Speed 25.0
            Mod.base' (Penetration Fire) 15.0
            Mod.inc (AilmentMagnitude(Some Ignite)) 60.0
            Mod.inc (Damage AllDamage) 30.0 |> Mod.whenever (EnemyHas "Shocked")
            Mod.inc (ForMinions(Damage AllDamage)) 80.0 ]
          [ bolt; flare; curse ] with
        Rotation =
            [ { Use = "Ember Curse"; When = fun v -> not (v.EnemyStatus.ContainsKey "Curse:Ember Curse") }
              Actor.always "Arc Bolt" ]
        Rules =
            [ { Hook = OnCrit; Filter = Called "Arc Bolt"; Chance = 100.0; Reaction = GainEnergy("Cast on Crit", 40.0) }
              { Hook = OnUse; Filter = Called "Ember Curse"; Chance = 100.0
                Reaction = ApplyCurse("Ember Curse", 6.0, [ Mod.base' (Resist Fire) -25.0 ]) } ]
        Metas = [ { Name = "Cast on Crit"; MaxEnergy = 100.0; Triggers = "Flare" } ] }

let hound =
    let bite =
        { Skill.make "Bite" [ Attack; Melee; Minion ] with
            BaseDamage = Map.ofList [ Physical, { Min = 80.0; Max = 120.0 } ]
            BaseTime = 0.7 }
    let mods = Mod.base' (AilmentChance Bleed) 25.0 :: Mods.forMinions player.Mods
    { Actor.make "Hound" mods [ bite ] with
        HitPattern = Map.ofList [ "Bite", [ DuringUse 0.5, 1.0 ] ] }

let fight =
    { Enemies = Encounter.boss 83 Rarity.defaults 30.0 |> List.map (fun b -> { b with Life = 100_000.0 })
      Player = player
      Minions = [ hound ]
      Targeting = Strongest
      Reach = infinity
      Walls = []
      MaxTime = 120.0 }

[<EntryPoint>]
let main _ =
    fight |> Summary.simulate 2000 |> Summary.format |> printfn "%s"
    0
