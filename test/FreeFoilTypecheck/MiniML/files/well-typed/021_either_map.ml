let mapSum = λf. λg. λs. case s of { inl a -> inl (f a) | inr b -> inr (g b) } in mapSum (λn. iszero n) (λb. if b then 1 else 0)
