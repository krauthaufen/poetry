namespace PoEtry

/// Minimal XML reader (elements, attributes, text), enough for PoB build files.
/// Hand-written for the same reason as Json: identical on .NET and Fable.
type XmlNode =
    { Name: string
      Attrs: Map<string, string>
      Children: XmlNode list
      Text: string }

module Xml =
    let private decode (s: string) =
        if not (s.Contains "&") then
            s
        else
            let sb = System.Text.StringBuilder()
            let mutable i = 0
            while i < s.Length do
                if s.[i] = '&' then
                    let semi = s.IndexOf(';', i)
                    let entity = s.Substring(i + 1, semi - i - 1)
                    let text =
                        match entity with
                        | "amp" -> "&"
                        | "lt" -> "<"
                        | "gt" -> ">"
                        | "quot" -> "\""
                        | "apos" -> "'"
                        | e when e.StartsWith "#x" -> string (char (System.Convert.ToInt32(e.Substring 2, 16)))
                        | e when e.StartsWith "#" -> string (char (int (e.Substring 1)))
                        | e -> "&" + e + ";"
                    sb.Append(text) |> ignore
                    i <- semi + 1
                else
                    sb.Append(s.[i]) |> ignore
                    i <- i + 1
            sb.ToString()

    let parse (text: string) : XmlNode =
        let n = text.Length
        let mutable i = 0
        let startsAt (p: string) =
            i + p.Length <= n && (let mutable ok = true in (for k in 0 .. p.Length - 1 do if text.[i + k] <> p.[k] then ok <- false); ok)
        let isNameChar c = System.Char.IsLetterOrDigit c || c = '_' || c = '-' || c = ':' || c = '.'
        let skipWs () =
            while i < n && System.Char.IsWhiteSpace text.[i] do
                i <- i + 1
        let readName () =
            let start = i
            while i < n && isNameChar text.[i] do
                i <- i + 1
            text.Substring(start, i - start)
        /// Skip <?...?>, <!-- --> and <!DOCTYPE>.
        let rec skipMisc () =
            skipWs ()
            if i + 1 < n && text.[i] = '<' && (text.[i + 1] = '?' || text.[i + 1] = '!') then
                if startsAt "<!--" then i <- text.IndexOf("-->", i) + 3
                else i <- text.IndexOf('>', i) + 1
                skipMisc ()

        let rec element () : XmlNode =
            i <- i + 1 // '<'
            let name = readName ()
            let mutable attrs = Map.empty
            skipWs ()
            while text.[i] <> '>' && text.[i] <> '/' do
                let key = readName ()
                skipWs ()
                i <- i + 1 // '='
                skipWs ()
                let quote = text.[i]
                let close = text.IndexOf(quote, i + 1)
                attrs <- Map.add key (decode (text.Substring(i + 1, close - i - 1))) attrs
                i <- close + 1
                skipWs ()
            if text.[i] = '/' then
                i <- i + 2
                { Name = name; Attrs = attrs; Children = []; Text = "" }
            else
                i <- i + 1
                let children = ResizeArray()
                let sb = System.Text.StringBuilder()
                let mutable fin = false
                while not fin do
                    if startsAt "</" then
                        i <- text.IndexOf('>', i) + 1
                        fin <- true
                    elif startsAt "<!--" then
                        i <- text.IndexOf("-->", i) + 3
                    elif startsAt "<![CDATA[" then
                        let close = text.IndexOf("]]>", i)
                        sb.Append(text.Substring(i + 9, close - i - 9)) |> ignore
                        i <- close + 3
                    elif text.[i] = '<' then
                        children.Add(element ())
                    else
                        let next = text.IndexOf('<', i)
                        sb.Append(decode (text.Substring(i, next - i))) |> ignore
                        i <- next
                { Name = name; Attrs = attrs; Children = List.ofSeq children; Text = sb.ToString() }

        skipMisc ()
        element ()

    let attr key (node: XmlNode) = Map.tryFind key node.Attrs
    let children name (node: XmlNode) = node.Children |> List.filter (fun c -> c.Name = name)
    let child name node = children name node |> List.tryHead

    /// All descendants with the given name, depth first.
    let rec descendants name (node: XmlNode) =
        node.Children |> List.collect (fun c -> (if c.Name = name then [ c ] else []) @ descendants name c)
