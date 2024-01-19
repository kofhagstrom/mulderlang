module Library.Lexer

open Library.Parsec
open Library.Common

type Literal =
    | IntL of int
    | StringL of char list
    | IdentifierL of char list

type Token =
    | OpenBraceT
    | LiteralT of Literal

type LexError = UnexpectedError of string

module Lexer =

    let private stringToToken = [ ("{", OpenBraceT) ]

    let private ignore p =
        parser {
            let! _ = p
            return ()
        }

    let private spanL p =
        Parser
        <| (fun input ->
            let str, rest = Common.span p input
            Success { Output = str; Rest = rest })

    let private ws = spanL ((=) ' ')

    let private lexChar t =
        Parser
        <| (function
        | [] ->
            Failure
                { Error = UnexpectedError "EOI"
                  Rest = [] }
        | c :: rest ->
            if c = t then
                Success { Output = c; Rest = rest }
            else
                Failure
                    { Error = UnexpectedError "Unexpected character"
                      Rest = c :: rest })

    let private lexString (str: string) =
        let parsers = List.map lexChar (Seq.toList str)

        let rec traverseParsers parsers outputs =
            fun input ->
                match parsers with
                | p :: ps ->
                    match runParser p input with
                    | Success { Output = output; Rest = rest } -> traverseParsers ps (outputs @ [ output ]) rest
                    | Failure e -> Failure e
                | [] -> Success { Output = outputs; Rest = [] }

        Parser <| traverseParsers parsers []

    let private lexStringLiteral =
        let isAllowedLiteralChar c = System.Char.IsLetter c || c = ' '

        Parser
        <| fun input ->
            match Common.span isAllowedLiteralChar input with
            | [], rest ->
                Failure
                    { Error = UnexpectedError "Expected an alphanumeric"
                      Rest = rest }
            | str, rest ->
                Success
                    { Output = LiteralT(IdentifierL str)
                      Rest = rest }

    let private lexIntLiteral =
        Parser
        <| fun input ->
            match Common.span System.Char.IsDigit input with
            | [], rest ->
                Failure
                    { Error = UnexpectedError "Expected an integer"
                      Rest = rest }
            | str, rest ->
                let integer = int (System.String.Concat(Array.ofList str))

                Success
                    { Output = LiteralT(IntL integer)
                      Rest = rest }

    let private lexLiteral =
        parser {
            do! ignore ws
            let! o = lexStringLiteral <|> lexIntLiteral
            do! ignore ws
            return o
        }

    let rec private lexNonLiteral inp =
        match inp with
        | (str, t) :: rest ->
            parser {
                do! ignore (lexString str)
                return t
            }
            <|> lexNonLiteral rest
        | [] -> errorParser (UnexpectedError "Unexpected") []

    let private lexComment =
        parser {
            do! ignore (lexString "//")
            do! ignore (spanL ((<>) '\n'))
        }

    let private lexMultiLineComment =

        parser {
            do! ignore (lexString "/*")
            do! ignore (spanL ((<>) '*'))
            do! ignore (lexString "*/")
        }

    let private lexNewline = ignore (lexChar '\n')

    let private lexNextToken =

        let rec lexNextToken' () =

            parser {
                do! lexNewline <|> lexComment <|> lexMultiLineComment
                return! (lexNextToken' ())
            }
            <|> parser {
                do! ignore ws
                let! o = lexNonLiteral stringToToken
                do! ignore ws
                return o
            }
            <|> lexLiteral

        lexNextToken' ()

    let runLexer: Parser<Token list, char list, LexError> =

        let many parser =
            Parser
            <| fun input ->
                let rec loop acc rest =
                    match runParser parser rest with
                    | Success { Output = result; Rest = newRest } -> loop (acc @ [ result ]) newRest
                    | Failure { Error = _; Rest = _ } -> Success { Output = acc; Rest = rest }

                loop [] input

        many lexNextToken
