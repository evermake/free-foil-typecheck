λf. λl. case l of { [] -> inl 0 | x :: xs -> inr (f x) }
