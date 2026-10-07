letrec zip = λl. λr. case l of { [] -> [] | x :: xs -> case r of { [] -> [] | y :: ys -> (x, y) :: zip xs ys } } in zip
