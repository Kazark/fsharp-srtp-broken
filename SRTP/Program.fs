open System
open SRTP

let private maybeZero : int -> Maybe<int> =
    function
    | 0 -> Right 0
    | _ -> Left ()

// This type clearly does not match the implementation, and yet it type checks
let private whatInTheWorld : list<string>*int -> Either<string,list<string>> =
    traverseBB maybeZero

let private maybeZero2 : Maybe<int> -> Either<string,Maybe<int>> =
    function
    | Right 0 -> Right (Right 0)
    | _ -> Left "no"

// This type clearly does not match the implementation, and yet it type checks
let private whatInTheWorld2 : list<string>*Maybe<int> -> Either<Maybe<int>*Maybe<bool>,list<string>> =
    traverseBB maybeZero2

[<EntryPoint>]
let main _ =
    // If we invoke these jacked up functions, and then immediately print the
    // results, they print the data that's there... which does not match the types
    whatInTheWorld (["foo"; "bar"; "baz"], 2) |> printfn "%A"
    whatInTheWorld ([], 0) |> printfn "%A"
    whatInTheWorld2 (["foo"; "bar"; "baz"], Right 0) |> printfn "%A"
    whatInTheWorld2 ([], Right 2) |> printfn "%A"
    whatInTheWorld2 (["foo"; "bar"; "baz"], Left ()) |> printfn "%A"
    whatInTheWorld2 ([], Left ()) |> printfn "%A"
    // If instead we try to unpack the data that is there (of course according
    // to the declared types) we get a InvalidCastException
    try
        whatInTheWorld2 (["foo"; "bar"; "baz"], Left ())
        |> function
        | Left (Right x, Right b) -> printfn "Foo: %i%b" (x * 5) (not b)
        | Left (Left (), Left ()) -> printfn "Bar"
        | Right xs -> String.concat ";" xs |> printfn "Baz: %s"
        | _ -> printfn "Qux"
    with
    | :? InvalidCastException as exn -> printfn "%A" exn
    0
 
