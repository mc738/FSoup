module FSoup.Core.Tests.UnitTests.Html.Attributes

open FSoup.Core
open Microsoft.VisualStudio.TestTools.UnitTesting
open Parsing
open Html

[<TestClass>]
type AttributeUnitTests () =

    [<TestMethod>]
    member this.ParseId_VaLue_SomeId () =
        let text = "id=\"hello\""
        let expected = Some(Id "hello")
        
        let result = run parseId text 
        
        let actual =
            match result with
            | Success(att, _) -> Some att
            | _ -> None
        
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.ParseId_Invalid_None () =
        let text = "class=\"hello\""
        let expected = None
        
        let result = run parseId text 
        
        let actual =
            match result with
            | Success(att, _) -> Some att
            | _ -> None
        
        Assert.AreEqual(expected, actual)
        
        
    [<TestMethod>]
    member this.ParseClass_Valid_SomeId () =
        let text = "class=\"hello\""
        let expected = Some(Class "hello")
        
        let result = run parseClass text 
        
        let actual =
            match result with
            | Success(att, _) -> Some att
            | _ -> None
        
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.ParseClass_Invalid_None () =
        let text = "id=\"hello\""
        let expected = None
        
        let result = run parseClass text 
        
        let actual =
            match result with
            | Success(att, _) -> Some att
            | _ -> None
        
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.``Parse single quote id`` () =
        let text = "id='hello'"
        let expected = Some (Id "hello")
        
        let result = run parseId text 
        
        let actual =
            match result with
            | Success(att, _) -> Some att
            | _ -> None
        
        Assert.AreEqual(expected, actual)
    
