// Learn more about F# at http://fsharp.org
namespace MixedCompiler

module Core =
    open System
    open NonEmptyList

    type ResultBuilder() =
        member __.Map(f, x) = Result.map f x
        member __.Bind(x, f) = Result.bind f x
        member __.Return(x) = Ok x
        member __.Zero() = Ok []

    let result = ResultBuilder()

    let biggerThan than value =
        if value > than
        then Ok ()
        else Error (sprintf "%i is not bigger than %i" value than)

    type UserActionError =
        | InputNotAvailable
        | Aborted
        | Refuted

    type UserActionSuccess = 
        | Confirmed
        | ConfirmedAndVerify

    type UserOption = {
        shortcut: char
        name: string
        outcome: Result<UserActionSuccess,UserActionError>
    }

    type PromptUserAction = NonEmptyNoDuplicateList<UserOption> -> Result<UserActionSuccess,UserActionError>

    let promptUserAction : PromptUserAction = fun nendOptions ->
        if Console.IsInputRedirected
        then Error InputNotAvailable
        else
            let options = nendOptions |> NonEmptyNoDuplicateList.value
            options |> List.iter (fun opt -> printfn "%c: %s" opt.shortcut opt.name)
            let rec readResponse n =
                if n > 10
                then Error Aborted
                else
                    let char = Console.ReadKey(true).KeyChar
                    match List.tryFind (fun opt -> opt.shortcut = char) options with
                    | None -> readResponse (n + 1)
                    | Some opt -> opt.outcome
            readResponse 0            

    type WorkflowError =
        | BiggerThanError of string
        | CreateOptionsError of NonEmptyNoDuplicateList.CreateError
        | PromptUserActionError of UserActionError

    let workflow _ =
        result {
            do! biggerThan 5 10 |> Result.mapError BiggerThanError
            let! options =
                NonEmptyNoDuplicateList.create [
                    { shortcut = 'a'; name = "Option a"; outcome = Ok Confirmed };
                    { shortcut = 'b'; name = "Option b"; outcome = Ok ConfirmedAndVerify };
                    { shortcut = 'c'; name = "Option c"; outcome = Error Refuted };
                ] |> Result.mapError CreateOptionsError
            let! outcome = promptUserAction options |> Result.mapError PromptUserActionError
            do!
                match outcome with
                | Confirmed ->
                    printfn "Confirmed"
                    Ok ()
                | ConfirmedAndVerify ->
                    printfn "Verifying ..."
                    biggerThan 2 4 |> Result.mapError BiggerThanError
            return ()            
        }

    [<EntryPoint>]
    let main argv =
        printfn "Hello World from F#!"
        match workflow () with
        | Error err ->
            match err with
            | BiggerThanError err -> printfn "%s" err
            | CreateOptionsError err -> printfn "Create options error"
            | PromptUserActionError err ->
                match err with
                | InputNotAvailable -> printfn "Input not available"
                | Aborted -> printfn "Aborted"
                | Refuted -> printfn "Refuted"
        | Ok _ -> printfn "Workflow went ok"
        0 // return an integer exit code
