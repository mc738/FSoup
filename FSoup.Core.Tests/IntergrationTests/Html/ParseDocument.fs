module FSoup.Core.Tests.IntergrationTests.Html.ParseDocument


open FSoup.Core
open Microsoft.VisualStudio.TestTools.UnitTesting
open Parsing
open Html

[<TestClass>]
type AttributeUnitTests () =

    [<TestMethod>]
    member this.ParseDiv_EmptyNoAttributes_SomeDiv () =
        let text = "<div id=\"id\" class=\"class\"><h1 class=\"header\">Header</h1><p class=\"para\">Hello World!</p></div>"
        let expected = Some(Div {
            Attributes = Some [ Id "id"; Class "class" ]
            Content = Elements [
                H1 {
                    Attributes = Some [ Class "header" ]
                    Content = Text "Header"
                }
                P {
                  Attributes = Some [ Class "para" ]
                  Content = Text "Hello World!"
                }
            ]
        })  
        
        // TODO switch this from using parse Div
        let result = run parseDiv text 
        
        let actual =
            match result with
            | Success(att, _) -> Some att
            | _ -> None
        
        Assert.AreEqual(expected, actual)