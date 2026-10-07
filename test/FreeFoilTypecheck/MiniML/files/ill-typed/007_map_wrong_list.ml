letrec map = λf. λl. case l of { [] -> [] | x :: xs -> f x :: map f xs } in map (λn. n + 1) (true :: [])
