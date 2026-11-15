# Game file extraction

Reads tables straight from the PoE2 install (Steam) with `pathofexile-dat`
and the community dat schema. Used for data PoB does not export.

```bash
cd tools/gamefiles
npm install
npx pathofexile-dat                       # reads config.json, writes tables/English/*.json
python3 gen_rarity.py tables/English > ../../src/PoEtry/RarityData.fs
python3 gen_rare_mods.py tables/English > ../../src/PoEtry/RareModsData.fs
```

Update the game in Steam first to read the current patch. `config.json` points at the install.
