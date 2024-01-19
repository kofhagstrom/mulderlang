module Library.Parsec

type ParseSuccess<'output, 'input> = { Output: 'output; Rest: 'input }
type ParseError<'input, 'error> = { Error: 'error; Rest: 'input }

type ParseResult<'output, 'input, 'error> =
    | Success of ParseSuccess<'output, 'input>
    | Failure of ParseError<'input, 'error>

type Parser<'output, 'input, 'error> = Parser of ('input -> ParseResult<'output, 'input, 'error>)

[<AutoOpen>]
module Parsec =
    let runParser (Parser p) = p

    let map f p =
        Parser
        <| fun input ->
            (match runParser p input with
             | Success { Output = o; Rest = r } -> Success { Output = f o; Rest = r }
             | Failure e -> Failure e)

    let (|>>) p f = map f p

    let (>>=) p f =
        Parser
        <| fun input ->
            match runParser p input with
            | Success { Output = o; Rest = r } -> runParser (f o) r
            | Failure e -> Failure e

    let (<>>>) p1 p2 =
        Parser
        <| fun input ->
            match runParser p1 input with
            | Success { Rest = r } -> runParser p2 r
            | Failure e -> Failure e

    let ret output =
        Parser <| fun input -> Success { Output = output; Rest = input }

    let (<*>) p1 p2 =
        Parser
        <| fun input ->
            match runParser p1 input, runParser p2 input with
            | Success { Output = f }, Success { Output = x; Rest = r } -> Success { Output = f x; Rest = r }
            | Failure e, _ -> Failure e
            | _, Failure e -> Failure e

    let (<|>) p1 p2 =
        Parser
        <| fun input ->
            match runParser p1 input with
            | Success res -> Success res
            | Failure(_) -> runParser p2 input

    let (<*>>) p1 p2 =
        Parser
        <| fun input ->
            match runParser p1 input with
            | Success { Rest = r } -> runParser p2 r
            | Failure e -> Failure e

    let (<<*>) p1 p2 =
        Parser
        <| fun input ->
            match runParser p1 input with
            | Success { Output = o; Rest = r } ->
                match runParser p2 r with
                | Success { Rest = r' } -> Success { Output = o; Rest = r' }
                | Failure e -> Failure e
            | Failure e -> Failure e

    let inline errorParser error rest =
        Parser <| fun _ -> Failure { Error = error; Rest = rest }

type ParserBuilder() =
    member this.Map(x, f) = Parsec.map f x
    member this.Bind(x, f) = Parsec.(>>=) x f
    member this.Return x = Parsec.ret x
    member this.ReturnFrom x = x

let parser = ParserBuilder()
