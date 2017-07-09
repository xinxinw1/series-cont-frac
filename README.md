# Series to Cont Frac

```
$ ./repl
> (gen (/ 1 (expt 10 (expt n 2))) 10)
> (gen (fact n) 10)
> (gen (elt *primes* n) 8)
> (gen (elt *divisors* n) 10)
> (gen (poch n n) 20)
> (gen (fact-k (+ (* 2 n) 1) 2) 20)
> (gen-fn (conv (fn (n) (/ 1 (fact n))) (fn (n) (/ 1 (fact n)))) 20)
```
