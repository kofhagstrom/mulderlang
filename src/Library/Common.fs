module Library.Common

module Common =
    let rec span p xs =
        let span' x xs =
            let (ys, zs) = span p xs
            (x :: ys, zs)

        match xs with
        | [] -> ([], [])
        | x :: xs -> if p x then span' x xs else ([], x :: xs)
