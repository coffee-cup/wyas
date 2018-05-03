(begin
  (define y 1)
  (define add (lambda (x) (+ x y)))
  (let (z 10) (add z)))
