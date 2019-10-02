namespace MixedCompiler

type NonEmptyList<'a> = private NonEmptyList of 'a list
module NonEmptyList =
    type CreateError =
        | Empty
    let create xs =
        if List.isEmpty xs
        then Error Empty
        else Ok (NonEmptyList xs)
    let value (NonEmptyList neList) = neList

type NonEmptyNoDuplicateList<'a> = private NonEmptyNoDuplicateList of 'a list
module NonEmptyNoDuplicateList =
    type CreateError =
        | Empty
        | Duplicates
    let create xs =
        if List.isEmpty xs
        then Error Empty
        else
            if (xs |> List.distinct |> List.length) <> (xs |> List.length)
            then Error Duplicates
            else Ok (NonEmptyNoDuplicateList xs)
    let value (NonEmptyNoDuplicateList nendList) = nendList