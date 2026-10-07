λl. case l of { (x, inl y) :: _ -> x + y | (x, inr b) :: _ -> if b then x else 0 | [] -> 0 }
