namespace PoEsie

/// SVG drawings of a skill use's geometry: enemies, player, impact point, projectiles.
module Picture =
    let private f (x: float) = System.Math.Round(x, 1).ToString(System.Globalization.CultureInfo.InvariantCulture)

    /// One use: player at the origin, enemies, the impact point, projectile paths.
    /// Hits are drawn from the start to where the projectile reaches the enemy it hits.
    let svg (title: string) (reach: float) (p: ProjectileShape) (primary: EnemyDef) (enemies: EnemyDef list) =
        let paths = Geometry.paths p reach primary enemies
        let impact = if p.FromImpact then Geometry.impactPoint reach primary else Vec.zero
        let pts = Vec.zero :: impact :: (enemies |> List.collect (fun e -> [ Vec.add e.Position (Vec.make e.Size e.Size); Vec.sub e.Position (Vec.make e.Size e.Size) ]))
        let pad = 25.0
        let minX = (pts |> List.map (fun v -> v.X) |> List.min) - pad
        let maxX = (pts |> List.map (fun v -> v.X) |> List.max) + pad
        let minY = (pts |> List.map (fun v -> v.Y) |> List.min) - pad
        let maxY = (pts |> List.map (fun v -> v.Y) |> List.max) + pad
        let w, h = maxX - minX, maxY - minY
        let hits = paths |> List.filter (fun p -> not p.Hit.IsEmpty) |> List.length
        let line (a: Vec) (b: Vec) cls = sprintf "<line x1='%s' y1='%s' x2='%s' y2='%s' class='%s'/>" (f a.X) (f -a.Y) (f b.X) (f -b.Y) cls
        let body =
            [ for e in enemies -> sprintf "<circle cx='%s' cy='%s' r='%s' class='enemy'/>" (f e.Position.X) (f -e.Position.Y) (f e.Size)
              for path in paths do
                  if path.Hit.IsEmpty then yield line path.From path.To "miss"
                  else
                      // draw a hit up to where the projectile leaves the enemy it reached
                      let e = enemies |> List.find (fun e -> e.Name = path.Hit.Head)
                      let dir = Vec.sub path.To path.From
                      let dirN = Vec.scale (1.0 / Vec.length dir) dir
                      let toE = Vec.sub e.Position path.From
                      let t = toE.X * dirN.X + toE.Y * dirN.Y
                      let c2 = max 0.0 ((Vec.length toE) ** 2.0 - t * t)
                      let exit = t + sqrt (max 0.0 (e.Size * e.Size - c2))
                      yield line path.From (Vec.add path.From (Vec.scale exit dirN)) "hit"
              yield sprintf "<circle cx='0' cy='0' r='2.2' class='player'/>"
              yield sprintf "<path d='M %s %s l 3 3 m -6 0 l 3 -3 l 3 -3 m -6 0 l 3 3' class='impact'/>" (f impact.X) (f -impact.Y) ]
            |> String.concat ""
        sprintf "<figure><svg viewBox='%s %s %s %s' role='img' aria-label='%s'>%s</svg><figcaption>%s: %d of %d projectiles hit</figcaption></figure>"
            (f minX) (f -maxY) (f w) (f h) title body title hits paths.Length
