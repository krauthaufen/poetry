namespace PoEtry

/// Deterministic, portable PRNG (xoshiro128**). Only 32-bit xor/shift/rotate and
/// multiplications by 5 and 9, so it produces identical streams on .NET and in
/// JavaScript (Fable). This is the single piece of mutable state in the engine:
/// every random decision draws from an Rng passed in explicitly.
type Rng(seed: uint32) =
    let mutable s0 = 0u
    let mutable s1 = 0u
    let mutable s2 = 0u
    let mutable s3 = 0u

    let rotl (x: uint32) (k: int) = (x <<< k) ||| (x >>> (32 - k))

    do
        // Expand the seed with xorshift32 so nearby seeds give unrelated streams.
        let mutable x = if seed = 0u then 0x9E3779B9u else seed
        let step () =
            x <- x ^^^ (x <<< 13)
            x <- x ^^^ (x >>> 17)
            x <- x ^^^ (x <<< 5)
            x
        s0 <- step ()
        s1 <- step ()
        s2 <- step ()
        s3 <- step ()

    member _.NextUInt() : uint32 =
        // The products are masked to 32 bits by hand: JavaScript multiplies as floating point and
        // does not wrap, which left the result up to nine times too large there.
        let result = (rotl ((s1 * 5u) &&& 0xFFFFFFFFu) 7 * 9u) &&& 0xFFFFFFFFu
        let t = s1 <<< 9
        s2 <- s2 ^^^ s0
        s3 <- s3 ^^^ s1
        s1 <- s1 ^^^ s2
        s0 <- s0 ^^^ s3
        s2 <- s2 ^^^ t
        s3 <- rotl s3 11
        result

    /// Uniform in [0, 1).
    member this.Float() = float (this.NextUInt()) / 4294967296.0

    /// Uniform in [lo, hi].
    member this.Between(lo: float, hi: float) = lo + (hi - lo) * this.Float()

    /// True with the given probability in percent (0..100).
    member this.Chance(percent: float) =
        percent >= 100.0 || (percent > 0.0 && this.Float() * 100.0 < percent)
