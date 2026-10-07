letrec zip = λp. case p of { (x :: xs, y :: ys) -> (x, y) :: zip (xs, ys) | _ -> [] } in zip
