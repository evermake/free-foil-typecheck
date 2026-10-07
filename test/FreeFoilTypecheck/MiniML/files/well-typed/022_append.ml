letrec append = λl. λr. case l of { [] -> r | x :: xs -> x :: append xs r } in append (1 :: []) (2 :: [])
