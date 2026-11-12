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

/// The sprite's colour, dimmed by Tint; what is transparent in the art is not drawn.
[<ShaderEffect>]
let spriteFragment (f : Textured) =
    fragment {
        let c = atlas.Sample f.Uv
        let t : V4f = uniform?Tint
        if c.W < 0.35f then discard ()
        return { Color = V4f (c.X * t.X, c.Y * t.Y, c.Z * t.Z, 1.0f) }
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
