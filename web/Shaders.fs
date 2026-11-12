/// The tree's shaders (FShade syntax, float32 throughout): sprites out of an atlas, and
/// plain coloured geometry.
module Shaders

open Aardvark.Portable
open Aardvark.Portable.Shader

type Vertex =
    { [<Position>] Positions : V4f
      [<Semantic("DiffuseColorCoordinates")>] Uv : V2f }

type Textured = { [<Semantic("DiffuseColorCoordinates")>] Uv : V2f }
type Shaded = { [<Color>] Color : V4f }

let private atlas =
    sampler2d {
        texture uniform?Atlas
        filter Filter.MinMagLinear
        addressU WrapMode.Clamp
        addressV WrapMode.Clamp
    }

/// The model's place on screen; the quad's own 0..1 coordinates moved into the sprite's
/// rectangle of the atlas (Rect: u, v, width, height).
[<ShaderEffect>]
let spriteVertex (v : Vertex) =
    vertex {
        let model : M44f = uniform?ModelTrafo
        let r : V4f = uniform?Rect
        return { Positions = (uniform?ViewProjTrafo : M44f) * model * v.Positions
                 Uv = V2f (r.X + v.Uv.X * r.Z, r.Y + v.Uv.Y * r.W) }
    }

/// The sprite's colour, dimmed by Tint; what is more transparent in the art than Tint's
/// fourth part says is not drawn. With a low cut (a background picture) the colour fades
/// with the art's transparency towards the ground's colour, in place of blending.
[<ShaderEffect>]
let spriteFragment (f : Textured) =
    fragment {
        let c = atlas.Sample f.Uv
        let t : V4f = uniform?Tint
        if c.W < t.W then discard ()
        // (towards the ground's own colour: there is no blending to do it properly)
        let fade = if t.W < 0.1f then c.W else 1.0f
        // the lowest cut is the ascendancy's portrait: it fades out towards the ring around it
        let r : V4f = uniform?Rect
        let dx = (f.Uv.X - r.X) / r.Z - 0.5f
        let dy = (f.Uv.Y - r.Y) / r.W - 0.5f
        let d = 2.0f * sqrt (dx * dx + dy * dy)
        let fade = if t.W < 0.015f then fade * max 0.0f (min 1.0f ((0.66f - d) / 0.15f)) else fade
        // (beyond the fade the portrait's square must not cover the tree's connections)
        if t.W < 0.015f && d > 0.655f then discard ()
        return { Color = V4f (0.047f + (c.X * t.X - 0.047f) * fade, 0.047f + (c.Y * t.Y - 0.047f) * fade, 0.055f + (c.Z * t.Z - 0.055f) * fade, 1.0f) }
    }

[<ShaderEffect>]
let flatVertex (v : Vertex) =
    vertex {
        let model : M44f = uniform?ModelTrafo
        return { Positions = (uniform?ViewProjTrafo : M44f) * model * v.Positions; Uv = v.Uv }
    }

[<ShaderEffect>]
let flatFragment (f : Textured) =
    fragment {
        let c : V4f = uniform?Color
        return { Color = V4f (c.X, c.Y, c.Z, 1.0f) }
    }

/// A sprite of the atlas; plain colour.
let sprite = Effect.compose [ Effect.ofFunction spriteVertex; Effect.ofFunction spriteFragment ]
let flat = Effect.compose [ Effect.ofFunction flatVertex; Effect.ofFunction flatFragment ]
