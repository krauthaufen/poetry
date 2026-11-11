namespace PoEtry

module Defence =
    /// Class base attributes from tree.json ("classes": name, base_str, base_dex, base_int),
    /// also under each ascendancy's name (the API names an ascended character by it).
    let classBases (treeJson: string) =
        Json.list (Json.field "classes" (Json.parse treeJson))
        |> List.collect (fun c ->
            match Json.strField "name" c with
            | Some n ->
                let g k = Json.numField k c |> Option.defaultValue 0.0
                let b = g "base_str", g "base_dex", g "base_int"
                (n, b) :: (Json.list (Json.field "ascendancies" c) |> List.choose (Json.strField "name") |> List.map (fun a -> a, b))
            | None -> [])
        |> Map.ofList

    /// "Mercenary3" (API class id) -> "Mercenary".
    let baseClass (name: string) = name.TrimEnd([| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' |])

    // Character.ot / PoB CalcSetup: life 16 + 12 per level, mana 30 + 4 per level;
    // 2 life per Str, 2 mana per Int; mana regen 4% per second; ES recharge 12.5% per second.
    let private lifePerLevel = 12.0
    let private baseLife = 16.0
    let private manaPerLevel = 4.0
    let private baseMana = 30.0
    let private manaRegenPercent = 4.0
    let private esRechargePercent = 12.5
    /// Runic Ward regenerates 5% of its maximum per second (guides; user confirmed).
    let private wardRegenPercent = 5.0
    let playerMaxResist = 75.0
    let playerResistCap = 90.0
    /// Recoup returns its share of damage taken over 8 seconds (game keyword text).
    let private recoupSeconds = 8.0

    /// A flask with the character's flask modifiers: amount, recovery rate (same amount,
    /// shorter time), charges used and gained.
    let flask (pool: string) (db: ModDb) (spec: ((float * float * float * float) * float) option) : ManaFlask option =
        let c = Ctx.empty
        spec
        |> Option.map (fun ((amount, duration, perUse, maxC), ownGain) ->
            { Amount = amount * Mods.mult c [ Named(pool + "FlaskAmount") ] db
              Duration = duration / max 0.1 (Mods.mult c [ Named(pool + "FlaskRate") ] db)
              ChargesPerUse = perUse * Mods.mult c [ Named(pool + "FlaskChargesUsed") ] db
              MaxCharges = maxC
              ChargesPerSecond = Mods.sumBase c [ Named "FlaskChargesPerSecond"; Named(pool + "FlaskChargesPerSecond") ] db
              ChargeGainMult = 1.0 + (ownGain + Mods.sumInc c [ Named(pool + "FlaskChargesGained") ] db) / 100.0
              Instant = min 100.0 (Mods.sumBase c [ Named "FlaskInstant" ] db) / 100.0 })

    /// `resources`: current charges, motes, rage... (conditional defences read them).
    /// A resistance before its cap: the modifiers' sum (chaos doubled by its keystone), what
    /// "Modifiers to Fire Resistance also grant Cold and Lightning Resistance at N% of their
    /// value" adds, and the campaign's elemental resistance penalty, complete in the endgame.
    let uncappedResist (c: Ctx) (db: ModDb) (t: DamageType) =
        let penalty = if DamageType.isElemental t then Constants.endgameResistPenalty else 0.0
        let doubled = if t = Chaos && Mods.flag c (Named "ChaosResistDoubled") db then 2.0 else 1.0
        let fromFire = if t = Cold || t = Lightning then Mods.sumBase c [ Resist Fire ] db * Mods.sumBase c [ Named "FireResToColdLightning" ] db / 100.0 else 0.0
        Mods.sumBase c [ Resist t ] db * doubled + fromFire + penalty

    /// What the items of the active weapon set show: the other set's weapons are put away.
    let activeSet (setTwo: bool) (defences: (string * Stat * float) list) =
        defences |> List.choose (fun (slot, s, v) ->
            if slot.Contains "Swap" then (if setTwo then Some(slot.Replace(" Swap", ""), s, v) else None)
            elif setTwo && slot.StartsWith "Weapon" then None
            else Some(slot, s, v))

    let computeWith (resources: Map<string, int>) (bases: Map<string, float * float * float>) (sheet: CharacterSheet) : PlayerStats =
        let db = ModDb.ofList sheet.Mods
        // "while wielding a Quarterstaff" and similar read the main hand
        let wielding =
            sheet.MainHand |> Option.map (fun w -> w.Tags |> List.choose (function Tag _ -> None | t -> Some(sprintf "Wielding%A" t))) |> Option.defaultValue []
        // the sheet of weapon set one, or of set two when asked for ("WeaponSetTwo")
        let setTwo = Map.containsKey "WeaponSetTwo" resources
        let sheet = { sheet with ItemDefences = activeSet setTwo sheet.ItemDefences }
        let c =
            { Ctx.empty with
                Buffs = wielding |> List.fold (fun m b -> Map.add b 1 m) resources
                SkillTags = if setTwo then Set.singleton (Tag "WeaponSet2") else Set.empty }
        // "Evasion Rating from Equipped Helmet, Gloves and Boots is doubled", "from Equipped Body Armour is halved"
        let sheet =
            let double = Mods.flag c (Named "DoubleEvasionHelmetGlovesBoots") db
            let halve = Mods.flag c (Named "HalveEvasionBodyArmour") db
            if not double && not halve then sheet
            else
                { sheet with
                    ItemDefences =
                        sheet.ItemDefences |> List.map (fun (slot, s, v) ->
                            if s = Evasion && double && (slot = "Helmet" || slot = "Gloves" || slot = "Boots") then slot, s, v * 2.0
                            elif s = Evasion && halve && slot = "Body Armour" then slot, s, v / 2.0
                            else slot, s, v) }
        let bStr, bDex, bInt = Map.tryFind (baseClass sheet.ClassName) bases |> Option.defaultValue (0.0, 0.0, 0.0)
        let attr k b = (b + Mods.sumBase c [ Attribute k ] db) * Mods.mult c [ Attribute k ] db |> floor
        let str, dex, int' = attr "Str" bStr, attr "Dex" bDex, attr "Int" bInt
        // "per 10 Intelligence", "per Dexterity": attributes as stack counts
        let c = { c with Buffs = c.Buffs |> Map.add "Str" (int str) |> Map.add "Dex" (int dex) |> Map.add "Int" (int int') }
        let level = float sheet.Level
        // item values as the items show them
        let itemValue stat slotFilter =
            sheet.ItemDefences |> List.sumBy (fun (slot, s, v) -> if s = stat && slotFilter slot then v else 0.0)
        // increases that only count for one slot's value ("increased Evasion Rating from Equipped
        // Body Armour", "increased Energy Shield from Equipped Focus"): additive with the
        // global increases, for that item's share (PoB CalcDefence slotCfg)
        let slotInc stat slot =
            (if slot = "Body Armour" then
                match stat with
                | Evasion -> Mods.sumInc c [ Named "BodyArmourEvasion" ] db
                | MaxEnergyShield -> Mods.sumInc c [ Named "BodyArmourEnergyShield" ] db
                | Armour -> Mods.sumInc c [ Named "BodyArmourArmour" ] db
                | _ -> 0.0
             else 0.0)
            + (match stat with
               | MaxEnergyShield -> Mods.sumInc c [ Named("SlotInc:ES:" + slot) ] db
               | Armour -> Mods.sumInc c [ Named("SlotInc:Armour:" + slot) ] db
               | Evasion -> Mods.sumInc c [ Named("SlotInc:Evasion:" + slot) ] db
               | _ -> 0.0)
        // "50% more Armour from Equipped Body Armour"
        let slotMore stat slot = if stat = Armour then Mods.more c [ Named("SlotMore:Armour:" + slot) ] db else 1.0
        /// Armour, evasion or energy shield: item values and flat amounts, increased and multiplied.
        let itemPoolIn (c: Ctx) stat (flat: float) =
            let inc = Mods.sumInc c [ stat ] db
            let fromSlots =
                sheet.ItemDefences
                |> List.sumBy (fun (slot, s, v) -> if s = stat then v * max 0.0 (1.0 + (inc + slotInc stat slot) / 100.0) * slotMore stat slot else 0.0)
            (fromSlots + (flat + Mods.sumBase c [ stat ] db) * max 0.0 (1.0 + inc / 100.0)) * Mods.more c [ stat ] db
        let fromItems stat = itemValue stat (fun _ -> true)
        let bodyEvasion = itemValue Evasion ((=) "Body Armour")
        let esFromBodyEvasion =
            match Mods.sumBase c [ Named "EnergyShieldPerBodyEvasion" ] db with
            | per when per > 0.0 -> floor (bodyEvasion / per)
            | _ -> 0.0
        let pool stat baseValue = (baseValue + Mods.sumBase c [ stat ] db) * Mods.mult c [ stat ] db
        let ci = Mods.flag c (Named "Keystone:Chaos Inoculation") db
        let flag n = Mods.flag c (Named n) db
        // Giant's Blood halves the life Strength gives
        let lifePerStr = if flag "NoStrengthBonus" then 0.0 elif flag "HalfLifeFromStrength" then 1.0 else 2.0
        let bodyES = itemValue MaxEnergyShield ((=) "Body Armour")
        let lifeFull = pool MaxLife (baseLife + lifePerLevel * level + lifePerStr * str + bodyES * Mods.sumBase c [ Named "LifeFromBodyES" ] db / 100.0)
        // "X% of maximum Life converted to / gained as Energy Shield"
        let converted = lifeFull * min 100.0 (Mods.sumBase c [ Named "LifeConvertedToES" ] db) / 100.0
        let gainedAsES = lifeFull * Mods.sumBase c [ Named "LifeGainedAsES" ] db / 100.0
        let life = if ci then 1.0 else lifeFull - converted
        let itemPool = itemPoolIn c
        let esFull = itemPool MaxEnergyShield esFromBodyEvasion + converted + gainedAsES
        // Eldritch Battery: "Convert 100% of maximum Energy Shield to maximum Mana". Only the
        // energy shield on items (and flat amounts) converts, before any increases to energy
        // shield (user); it becomes base mana. Blood Magic: no mana at all.
        let esShare = min 100.0 (Mods.sumBase c [ Named "ESConvertedToMana" ] db) / 100.0
        let esToMana = esShare * (itemValue MaxEnergyShield (fun _ -> true) + esFromBodyEvasion + Mods.sumBase c [ MaxEnergyShield ] db)
        let bloodMagic = flag "BloodMagic"
        let helmetMana = itemValue Armour ((=) "Helmet") * Mods.sumBase c [ Named "ManaPerHelmetArmour" ] db
        let mana = if bloodMagic then 0.0 else pool MaxMana (baseMana + manaPerLevel * level + 2.0 * int' + esToMana + floor helmetMana)
        let es = esFull * (1.0 - esShare)
        let onlyLeech = flag "OnlyLeechRecoversLife"
        let lifeRecharges = flag "LifeRechargesInsteadOfES"
        // "while you have Energy Shield"
        let cES = if es > 0.0 then { c with Buffs = Map.add "HasEnergyShield" 1 c.Buffs } else c
        // Character.ot base_evasion_rating = 7
        let evasionFlat = 7.0 + itemValue MaxEnergyShield ((=) "Helmet") * Mods.sumBase c [ Named "EvasionPerHelmetES" ] db
        // "Gain N% of Evasion Rating as extra Armour", Iron Reflexes (all of it): the evasion the
        // items show and the flat amounts become armour before either is increased, so armour's
        // increases apply to it, not evasion's (PoB CalcDefence, resource conversion)
        let ironReflexes = Mods.flag c (Named "EvasionToArmour") db
        let evasionAsArmour =
            (itemValue Evasion (fun _ -> true) + evasionFlat + Mods.sumBase c [ Evasion ] db)
            * (if ironReflexes then 1.0 else Mods.sumBase c [ Named "EvasionGainedAsArmour" ] db / 100.0)
        let armour =
            itemPoolIn cES Armour (itemValue MaxEnergyShield ((=) "Boots") * Mods.sumBase c [ Named "ArmourPerBootsES" ] db + evasionAsArmour)
            + mana * Mods.sumBase c [ Named "ManaGainedAsArmour" ] db / 100.0
        let evasion = if ironReflexes then 0.0 else itemPoolIn cES Evasion evasionFlat
        let named n = Mods.sumBase c [ Named n ] db
        let ailments = [ "Ignite"; "Chill"; "Freeze"; "Shock"; "Bleed"; "Poison" ]
        // stun threshold: maximum life (before Chaos Inoculation), ailment threshold: half of it
        let stunThreshold = (lifeFull + named "StunThreshold" + es * named "StunThresholdFromES" / 100.0) * Mods.mult c [ Named "StunThreshold" ] db
        let ailmentThreshold =
            (lifeFull * Map.find "PlayerAilmentThresholdLifeFactor" Tables.gameConstants + named "PlayerAilmentThreshold" + es * named "AilmentThresholdFromES" / 100.0
             + evasion * named "AilmentThresholdFromEvasion" / 100.0)
            * Mods.mult c [ Named "PlayerAilmentThreshold" ] db
        let lifeRecovery = Mods.mult c [ Named "LifeRecoveryRate" ] db
        let manaRecovery = Mods.mult c [ Named "ManaRecoveryRate" ] db
        let esRecovery = Mods.mult c [ Named "ESRecoveryRate" ] db
        let regenToES = Mods.flag c (Named "RegenESInsteadOfLife") db
        let lifeRegen = (Mods.sumBase c [ LifeRegen ] db + life * Mods.sumBase c [ LifeRegenPercent ] db / 100.0) * Mods.mult c [ LifeRegen ] db
        let deflection =
            evasion * Mods.sumBase c [ Named "DeflectionPercentOfEvasion" ] db / 100.0
            + armour * Mods.sumBase c [ Named "DeflectionPercentOfArmour" ] db / 100.0
            + Mods.sumBase c [ Deflection ] db
            |> fun d -> d * Mods.mult c [ Deflection ] db
        let resist t =
            // Chaos Inoculation: immune to chaos damage
            if t = Chaos && ci then t, 100.0
            else
                let mirrored = t = Cold || t = Lightning
                let cap =
                    match Mods.override' c (Named "MaxResistOverride") db with
                    | Some v -> v
                    | None -> min playerResistCap (playerMaxResist + Mods.sumBase c [ MaxResist t ] db + (if mirrored && Mods.flag c (Named "MaxFireResToColdLightning") db then Mods.sumBase c [ MaxResist Fire ] db else 0.0))
                t, min cap (uncappedResist c db t)
        { Str = str
          Dex = dex
          Int = int'
          Life = life
          Mana = mana
          EnergyShield = es
          Armour = armour
          Evasion = evasion
          Deflection = deflection
          Resist = DamageType.all |> List.filter ((<>) Physical) |> List.map resist |> Map.ofList
          LifeRegen = if ci || regenToES || onlyLeech then 0.0 else lifeRegen * lifeRecovery
          ManaRegen =
            ((if flag "NoInherentManaRegen" then 0.0 else mana * manaRegenPercent / 100.0) + life * named "ManaRegenPercentOfLife" / 100.0)
            * Mods.mult c [ ManaRegen ] db * manaRecovery
          EnergyShieldRecharge =
            if flag "NoESRecharge" || lifeRecharges then 0.0
            else es * esRechargePercent / 100.0 * Mods.mult c [ Named "ESRechargeRate" ] db * esRecovery
          // shield block (shown on the item) + flat block, x increased block, capped at 50 + max block
          Block =
            let baseBlock = fromItems (Named "ShieldBlock") + Mods.sumBase c [ Named "Block" ] db
            let cap = min 90.0 (50.0 + Mods.sumBase c [ Named "MaxBlock" ] db)
            min cap (baseBlock * Mods.inc c [ Named "Block" ] db)
          LuckyBlock = Mods.flag c (Named "LuckyBlock") db
          BlockedDamageTaken = Mods.sumBase c [ Named "BlockedDamageTaken" ] db
          // ward: item values (shield) + flat, x increased; regenerates 5% per second x increased rate
          Ward = pool (Named "Ward") (fromItems (Named "Ward"))
          WardRegen = pool (Named "Ward") (fromItems (Named "Ward")) * wardRegenPercent / 100.0 * Mods.inc c [ Named "WardRegen" ] db
          WardOnBlock = Mods.sumBase c [ Named "WardOnBlock" ] db
          CostsInfernalFlame = Mods.flag c (Named "Keystone:Pyromantic Pact") db
          TakenAs =
            [ for from in DamageType.all do
                for into in DamageType.all do
                    let v = Mods.sumBase c [ Named(sprintf "TakenAs:%s:%s" (DamageType.name from) (DamageType.name into)) ] db
                    if v > 0.0 then yield from, into, v ]
          // "X% faster start of Energy Shield Recharge"
          ESRechargeDelay = max 0.0 (Constants.energyShieldRechargeDelay + named "ESRechargeDelayFlat") / max 0.1 (Mods.inc c [ Named "ESRechargeFaster" ] db)
          ESRegen = (es * named "ESRegenPercent" / 100.0 + (if regenToES then lifeRegen else 0.0)) * esRecovery
          ManaBeforeLife = named "DamageRemovedFromManaFirst"
          BypassES = named "BypassES"
          ArmourAppliesTo = DamageType.all |> List.map (fun t -> t, named ("ArmourAppliesTo:" + DamageType.name t)) |> List.filter (fun (_, v) -> v > 0.0) |> Map.ofList
          TakenMult = DamageType.all |> List.map (fun t -> t, max 0.0 (Mods.mult c [ Named "Taken:All"; Named("Taken:" + DamageType.name t) ] db)) |> Map.ofList
          FlatPhysicalTaken = named "FlatPhysicalAttackTaken"
          LessCritDamageTaken = named "LessCritDamageTaken"
          EnemyCritChanceMult = max 0.0 (Mods.inc c [ Named "EnemyCritChanceAgainstSelf" ] db)
          ArmourVsProjectiles = (match named "DefendWithArmourVsProjectiles" with 0.0 -> 1.0 | v -> v / 100.0)
          ArmourVsCrits = (match named "DefendWithArmourVsCrits" with 0.0 -> 1.0 | v -> v / 100.0)
          StunThreshold = stunThreshold
          AilmentThreshold = ailmentThreshold
          FreezeThreshold = (ailmentThreshold + es * named "FreezeThresholdFromES" / 100.0) * Mods.mult c [ Named "FreezeThreshold" ] db
          AvoidStun = named "AvoidStun"
          Avoid = ailments |> List.map (fun a -> a, named ("Avoid:" + a)) |> Map.ofList
          SelfEffect = ailments |> List.map (fun a -> a, max 0.0 (Mods.mult c [ Named("SelfEffect:" + a) ] db)) |> Map.ofList
          SelfDuration = ailments |> List.map (fun a -> a, max 0.0 (Mods.mult c [ Named("SelfDuration:" + a) ] db)) |> Map.ofList
          LifeOnKill = if ci || onlyLeech then 0.0 else (named "LifeOnKill" + life * named "LifePercentOnKill" / 100.0) * lifeRecovery
          ESOnKill = es * named "ESPercentOnKill" / 100.0 * esRecovery
          LifeOnBlock = if ci || onlyLeech then 0.0 else named "LifeOnBlock" * lifeRecovery
          ManaOnBlock = named "ManaOnBlock" * manaRecovery
          Recoup =
            [ "Life", DamageType.all, named "RecoupLife"
              "Life", [ Physical ], named "RecoupLife:Physical"
              "Mana", DamageType.all, named "RecoupMana"
              "ES", DamageType.elemental, named "RecoupES:Elemental" ]
            |> List.filter (fun (pool, _, v) -> v > 0.0 && not ((ci || onlyLeech) && pool = "Life"))
          RecoupSeconds = recoupSeconds / max 0.1 (Mods.inc c [ Named "RecoupSpeed" ] db)
          LifeRecoveryRate = lifeRecovery
          ManaRecoveryRate = manaRecovery
          ESRecoveryRate = esRecovery
          LifeFlask = if ci || onlyLeech then None else flask "Life" db sheet.LifeFlask
          CostsLife = bloodMagic
          LifeRecharge = if lifeRecharges && not ci then life * esRechargePercent / 100.0 * lifeRecovery else 0.0
          UnluckyEvade = flag "UnluckyEvade"
          LuckyDeflect = flag "LuckyDeflect"
          LifeLeechRate = max 0.05 (Mods.mult c [ Named "LifeLeechRate" ] db)
          DeflectEffect = min 100.0 (Vitals.deflectEffect + named "DeflectEffect") }

    let compute bases sheet = computeWith Map.empty bases sheet

    /// Damage taken from one unblocked, unevaded, non-critical hit of `raw` damage of one type.
    let taken (v: PlayerStats) (t: DamageType) (melee: bool) (raw: float) =
        let d =
            match t with
            | Physical ->
                let d = if melee then max 0.0 (raw - v.FlatPhysicalTaken) else raw
                d * (1.0 - Vitals.armourReduction v.Armour d / 100.0)
            | t ->
                let d = raw * (1.0 - (Map.tryFind t v.Resist |> Option.defaultValue 0.0) / 100.0)
                match Map.tryFind t v.ArmourAppliesTo with
                | Some pct -> d * (1.0 - Vitals.armourReduction (v.Armour * pct / 100.0) d / 100.0)
                | None -> d
        d * (Map.tryFind t v.TakenMult |> Option.defaultValue 1.0)

    /// Damage of one type the pools absorb from full: energy shield (half as much against
    /// chaos, less what bypasses it), life with the mana share before it, Runic Ward.
    let pool (v: PlayerStats) (t: DamageType) =
        let es = (if t = Chaos then v.EnergyShield / 2.0 else v.EnergyShield)
        let es = if v.BypassES >= 100.0 then 0.0 else es
        let m = min 0.99 (v.ManaBeforeLife / 100.0)
        let lifeAndMana = if m <= 0.0 then v.Life else (if v.Mana >= m * v.Life / (1.0 - m) then v.Life / (1.0 - m) else v.Life + v.Mana)
        es + lifeAndMana + v.Ward

    /// The largest single hit of this type survived from full pools (before the
    /// character's mitigation), by bisection: damage taken grows with the hit.
    let maxHit (v: PlayerStats) (t: DamageType) =
        let p = pool v t
        if taken v t false 1e9 < p then infinity
        else
            let rec go lo hi n = if n = 0 then lo else (let mid = (lo + hi) / 2.0 in if taken v t false mid < p then go mid hi (n - 1) else go lo mid (n - 1))
            go 0.0 1e9 60
