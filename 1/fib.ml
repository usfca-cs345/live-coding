let fib n =
  let rec helper n f1 f2 = if n = 1 then f1
                           else helper (n - 1) (f1 + f2) f1
  in helper n 1 0
