letrec (even, odd) = (λn. if iszero n then true else odd (n - 1), λn. if iszero n then false else even (n - 1)) in even
