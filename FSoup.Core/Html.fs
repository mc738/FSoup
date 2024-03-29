module FSoup.Core.Html


open System
open System.Collections.Generic
open FSoup.Core

open Parsing

type HtmlAttribute =
    | Id of string
    | Class of string // list
    | Height of int
    | Href of string
    | Width of int
    | Custom of string * string

type HtmlElementType =
    | Div of HtmlElement
    | H1 of HtmlElement
    | Img of HtmlElement
    | Li of HtmlElement
    | Ul of HtmlElement
    | P of HtmlElement

and HtmlContent =
    | Elements of HtmlElementType list
    | Text of string

and HtmlElement =
    { Attributes: HtmlAttribute list option
      Content: HtmlContent }

let elementContractor ctor (a, c) = { Attributes = a; Content = c} |> ctor

let unescapedChar quoteChar =
    let label = "char"
    satisfy (fun ch -> ch <> '\\' && ch <> quoteChar) label

let escapedChar =
    [ ("\\\"", '\"')
      ("\\\\", '\\')
      ("\\/", '/')
      ("\\b", '\b')
      ("\\f", '\f')
      ("\\n", '\n')
      ("\\r", '\r')
      ("\\t", '\t') ]
    |> List.map (fun (toMatch, result) -> parseStr toMatch >>% result)
    |> choice
    <?> "escaped char"

let unicodeChar =
    let backslash = parseChar '\\'
    let uChar = parseChar 'u'
    let hexdigit = anyOf ([ '0' .. '9' ] @ [ 'A' .. 'F' ] @ [ 'a' .. 'f' ])

    let convertToChar (((h1, h2), h3), h4) =
        let str = sprintf "%c%c%c%c" h1 h2 h3 h4
        Int32.Parse(str, Globalization.NumberStyles.HexNumber) |> char
    backslash >>. uChar >>. hexdigit .>>. hexdigit .>>. hexdigit .>>. hexdigit |>> convertToChar

let quotedString =
    let singleQuote = parseChar '\'' <?> "single quote"
    let doubleQuote = parseChar '\"' <?> "double quote"
    
    
    //let quote = parseChar '\"' <?> "quote"
    let c q = unescapedChar q <|> escapedChar <|> unicodeChar
    satisfy (fun c -> c = '\'' || c = '\"') "quote"
    >>= (fun q ->
        if q = '\'' then manyChars (c q) .>> singleQuote
        else manyChars (c q) .>> doubleQuote)

    //quote >>. manyChars c .>> quote

let parseAttributeKey (name: string) =
    parseStr (name + "=") <?> "attribute key"

let parseCustomAttributeKey c =
    many1 (satisfy (fun ch -> ch <> '=') "attribute key") |>> fun cl -> [c] @ cl |> charListToStr
    //()

let parseAttribute name =
    parseAttributeKey name >>. quotedString <?> "attribute value"
//parseEndAttribute

let parseCustomAttribute =
    // Test the first character to see if it's `>` (end of attributes).
    // If not this has already been consumed so pass it on to `parseCustomAttributeKey`.
    satisfy (fun ch -> ch <> '>') "attribute key"
    >>= (fun c -> (parseCustomAttributeKey c .>> parseChar '=') .>>. quotedString <?> "attribute value" |>> Custom)

let parseId =
    parseAttribute "id" |>> Id <?> "id"

let parseClass =
    parseAttribute "class" |>> Class <?> "class"

let parseHref =
    parseAttribute "href" |>> Href <?> "href"

let parseHeight =
    parseAttribute "height" |>> int |>> Height <?> "height"

let parseWidth =
    parseAttribute "width" |>> int |>> Width <?> "width"

let parseAttributes =
    let attr = choice [ parseId; parseClass; parseHref; parseHeight; parseWidth; parseCustomAttribute ]
    sepBy1 attr spaces

let parseBeginOpeningTag name =
    parseStr ("<" + name)

let parseEndOpenTag =
    parseChar '>'

let parseOpenTag name =
    parseBeginOpeningTag name >>. spaces  >>. opt (parseAttributes) .>> parseEndOpenTag

let parseCloseTag name =
    parseStr ("</" + name + ">")

let parseText =
    let label = "char"
    many1 (satisfy (fun ch -> ch <> '<') label) |>> charListToStr |>> Text

let (parseElements, parseElementsRef) = createParserForwardedToRef<HtmlContent> ()

let parseElementContent =
    spaces >>. parseText <|> parseElements .>> spaces

/// Helper function for parsing an element.
/// It takes a element type and the element name.
///
/// Example
///
/// let parseDiv = parseElement Div "div"
let parseElement ctor name =
    (parseOpenTag name .>>. parseElementContent) .>> parseCloseTag name 
    |>> elementContractor ctor
    
/// Parse a div element.
///
/// Example
///
/// let result = run parseDiv "<div id=\"test\"></div>"
let parseDiv =
    parseElement Div "div"

let parseH1 =
    parseElement H1 "h1"

let parseP =
    parseElement P "p"
        
parseElementsRef := choice
    [
        parseDiv
        parseH1
        parseP
    ] |> many |>> Elements