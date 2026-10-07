letrec foldr = λf. λz. λl. case l of { [] -> z | x :: xs -> f x (foldr f z xs) } in foldr
