module FSoup.Core.Tests.UnitTests.Html.Elements

open FSoup.Core
open Microsoft.VisualStudio.TestTools.UnitTesting
open Parsing
open Html

[<TestClass>]
type AttributeUnitTests () =

    [<TestMethod>]
    member this.ParseDiv_EmptyNoAttributes_SomeDiv () =
        let text = "<div></div>"
        let expected = Some(Div { Attributes = None; Content = Elements [] })  
        
        let result = run parseDiv text 
        
        let actual =
            match result with
            | Success(att, _) -> Some att
            | _ -> None
        
        Assert.AreEqual(expected, actual)
        
    [<TestMethod>]
    member this.ParseDiv_EmptyWithId_SomeDiv () =
        let text = "<div id=\"id\"></div>"
        let expected = Some(Div { Attributes = Some [ Id "id" ]; Content = Elements [] })
        
        let result = run parseDiv text 
        
        let actual =
            match result with
            | Success(att, _) -> Some att
            | _ -> None
        
        Assert.AreEqual(expected, actual)
        
    
    [<TestMethod>]
    member this.ParseDiv_TextNoAttributes_SomeDiv () =
        let text = "<div>Hello World!</div>"
        let expected = Some(Div { Attributes = None; Content = Text "Hello World!" })
        
        let result = run parseDiv text 
        
        let actual =
            match result with
            | Success(att, _) -> Some att
            | _ -> None
        
        Assert.AreEqual(expected, actual)
        
    [<TestMethod>]
    member this.ParseDiv_TextAndId_SomeDiv () =
        let text = "<div id=\"id\">Hello World!</div>"
        let expected = Some(Div { Attributes = Some [ Id "id" ]; Content = Text "Hello World!" })
        
        let result = run parseDiv text 
        
        let actual =
            match result with
            | Success(att, _) -> Some att
            | _ -> None
        
        Assert.AreEqual(expected, actual)