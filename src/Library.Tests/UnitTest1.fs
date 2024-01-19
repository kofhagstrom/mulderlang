namespace Library.Tests


open Library
open Library.Common
open Library.Lexer
open Library.Parsec
open NUnit.Framework


[<TestFixture>]
type TestClass() =
    let testData = [ ' '; '1'; '2'; '2'; ' '; 'h'; 'e'; 'j' ]

    let expectSuccess =
        function
        | Success e -> printf $"Output: {e.Output}"
        | Failure e -> failwith $"{e}"

    [<Test>]
    member this.runLexer() =
        testData |> runParser Lexer.runLexer |> expectSuccess
