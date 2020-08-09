module FSoup.Core.Types

open System
open System.Collections.Generic
open FSoup.Core

open Parsing

type Attribute =
    | Id of string
    | Class of string // list
    | Height of int
    | Href of string
    | Width of int

type Attributes = Attribute list

type Element =
    | Div of HtmlElement
    | H1 of HtmlElement
    | Img of HtmlElement
    | Li of HtmlElement
    | Ul of HtmlElement

and HtmlContent =
    | Elements of Element list
    | Text of string
    
and HtmlElement =
    { Attributes: Attribute list }

let elementContractor ctor a = { Attributes = a } |> ctor

let unescapedChar =
    let label = "char"
    satisfy (fun ch -> ch <> '\\' && ch <> '\"') label

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
    let quote = parseChar '\"' <?> "quote"
    let c = unescapedChar <|> escapedChar <|> unicodeChar
    quote >>. manyChars c .>> quote

let parseAttributeKey (name: string) =
    parseStr (name + "=") <?> "attribute key"

let parseAttribute name =
    parseAttributeKey name >>. quotedString <?> "attribute value"
//parseEndAttribute

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
    let attr = choice [ parseId; parseClass; parseHref; parseHeight; parseWidth ]
    sepBy1 attr spaces

let parseBeginOpeningTag name =
    parseStr ("<" + name)

let parseEndOpenTag =
    parseChar '>'

let parseOpenTag name =
    parseBeginOpeningTag name >>. spaces >>. parseAttributes .>> parseEndOpenTag

let parseCloseTag name =
    parseStr ("</" + name + ">")

// let parseElementContent =
//    spaces >>.

// let attributes = createParserForwardedToRef<Attribute>()

let parseElement ctor name =
    let attributes = parseOpenTag name
    let body = quotedString
    let _ = parseCloseTag
    parseOpenTag name .>> quotedString .>> parseCloseTag name |>> elementContractor ctor
      
    // returnParser ( c)
    
    
    
let parseDiv =
    parseElement Div "div"

//
//    let attributes = parseOpenTag "div"
//    let body = quotedString
//    let _ = parseCloseTag
//
//    attributes |>> Div
