λs. case s of { inl (inl x) -> x | inl (inr y) -> y + 1 | inr _ -> 0 }
