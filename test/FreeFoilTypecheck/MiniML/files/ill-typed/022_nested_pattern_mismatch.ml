λl. case l of { (x, y) :: _ -> x + y | inl z :: _ -> z | [] -> 0 }
