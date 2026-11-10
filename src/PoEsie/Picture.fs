namespace PoEsie

/// SVG drawings of a skill use's geometry: walls, enemies, player, impact point and
/// projectile flights (bounces included).
module Picture =
    let mutable private figureId = 0

    let private f (x: float) = System.Math.Round(x, 1).ToString(System.Globalization.CultureInfo.InvariantCulture)

    /// One use as an SVG figure. `seed` drives the ricochet rolls of the drawn use.
    let svg (title: string) (seed: uint32) (walls: Geometry.Wall list) (reach: float) (p: ProjectileShape) (primary: EnemyDef) (enemies: EnemyDef list) =
        let paths = Geometry.paths (Rng seed) walls p reach primary enemies
        let impact = if p.FromImpact then Geometry.impactPoint reach primary else Vec.zero
        // frame: player, impact and enemies, plus walls that are near
        let core =
            Vec.zero :: impact
            :: (enemies |> List.collect (fun e -> [ Vec.add e.Position (Vec.make e.Size e.Size); Vec.sub e.Position (Vec.make e.Size e.Size) ]))
        let cx, cy = (core |> List.averageBy (fun v -> v.X)), (core |> List.averageBy (fun v -> v.Y))
        let span = core |> List.map (fun v -> max (abs (v.X - cx)) (abs (v.Y - cy))) |> List.max
        let half = max 60.0 (span + 45.0)
        let minX, maxX, minY, maxY = cx - half, cx + half, cy - half, cy + half
        let line (a: Vec) (b: Vec) cls = sprintf "<line x1='%s' y1='%s' x2='%s' y2='%s' class='%s'/>" (f a.X) (f -a.Y) (f b.X) (f -b.Y) cls
        let poly (pts: Vec list) cls =
            sprintf "<polyline points='%s' class='%s'/>" (pts |> List.map (fun v -> f v.X + "," + f -v.Y) |> String.concat " ") cls
        figureId <- figureId + 1
        let clipId = figureId
        let hits = paths |> List.sumBy (fun p -> p.Hit.Length)
        let bounced = paths |> List.filter (fun p -> p.Points.Length > 2) |> List.length
        let body =
            [ for (a, b) in walls -> line a b "wall"
              for e in enemies -> sprintf "<circle cx='%s' cy='%s' r='%s' class='enemy'/>" (f e.Position.X) (f -e.Position.Y) (f e.Size)
              for path in paths -> poly path.Points (if path.Hit.IsEmpty then "miss" else "hit")
              yield "<circle cx='0' cy='0' r='2.2' class='player'/>"
              yield sprintf "<path d='M %s %s l 3 3 m -6 0 l 3 -3 l 3 -3 m -6 0 l 3 3' class='impact'/>" (f impact.X) (f -impact.Y) ]
            |> String.concat ""
        sprintf "<figure><svg viewBox='%s %s %s %s' role='img' aria-label='%s'><clipPath id='c%d'><rect x='%s' y='%s' width='%s' height='%s'/></clipPath><g clip-path='url(#c%d)'>%s</g></svg><figcaption>%s<br>%d projectiles, %d hits, %d chained from a wall</figcaption></figure>"
            (f minX) (f -maxY) (f (2.0 * half)) (f (2.0 * half)) title clipId (f minX) (f -maxY) (f (2.0 * half)) (f (2.0 * half)) clipId body title paths.Length hits bounced
