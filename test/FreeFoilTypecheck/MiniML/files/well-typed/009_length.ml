letrec length = λl. case l of { [] -> 0 | x :: xs -> 1 + length xs } in length
