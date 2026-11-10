namespace PoEsie

module Defence =
    /// Class base attributes from tree.json ("classes": name, base_str, base_dex, base_int).
    let classBases (treeJson: string) =
        Json.list (Json.field "classes" (Json.parse treeJson))
        |> List.choose (fun c ->
            match Json.strField "name" c with
            | Some n ->
                let g k = Json.numField k c |> Option.defaultValue 0.0
                Some(n, (g "base_str", g "base_dex", g "base_int"))
            | None -> None)
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
    let playerMaxResist = 75.0
    let playerResistCap = 90.0

    let compute (bases: Map<string, float * float * float>) (sheet: CharacterSheet) : PlayerStats =
        let db = ModDb.ofList sheet.Mods
        let c = Ctx.empty
        let bStr, bDex, bInt = Map.tryFind (baseClass sheet.ClassName) bases |> Option.defaultValue (0.0, 0.0, 0.0)
        let attr k b = b + Mods.sumBase c [ Attribute k ] db
        let str, dex, int' = attr "Str" bStr, attr "Dex" bDex, attr "Int" bInt
        let level = float sheet.Level
        let fromItems stat = sheet.ItemDefences |> List.sumBy (fun (s, v) -> if s = stat then v else 0.0)
        let pool stat baseValue = (baseValue + Mods.sumBase c [ stat ] db) * Mods.mult c [ stat ] db
        let life = pool MaxLife (baseLife + lifePerLevel * level + 2.0 * str)
        let mana = pool MaxMana (baseMana + manaPerLevel * level + 2.0 * int')
        let es = pool MaxEnergyShield (fromItems MaxEnergyShield)
        let armour = pool Armour (fromItems Armour)
        let evasion = pool Evasion (fromItems Evasion)
        let deflection = evasion * Mods.sumBase c [ Named "DeflectionPercentOfEvasion" ] db / 100.0 + Mods.sumBase c [ Deflection ] db
        let resist t =
            let cap = min playerResistCap (playerMaxResist + Mods.sumBase c [ MaxResist t ] db)
            t, min cap (Mods.sumBase c [ Resist t ] db)
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
          LifeRegen = (Mods.sumBase c [ LifeRegen ] db + life * Mods.sumBase c [ LifeRegenPercent ] db / 100.0) * Mods.mult c [ LifeRegen ] db
          ManaRegen = mana * manaRegenPercent / 100.0 * Mods.mult c [ ManaRegen ] db
          EnergyShieldRecharge = es * esRechargePercent / 100.0 }
