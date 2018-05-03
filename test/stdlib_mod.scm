(define caar (lambda (pair) (car (car pair))))
(define cadr (lambda (pair) (car (cdr pair))))
(define cdar (lambda (pair) (cdr (car pair))))
(define cddr (lambda (pair) (cdr (cdr pair))))
(define caaar (lambda (pair) (car (car (car pair)))))
(define caadr (lambda (pair) (car (car (cdr pair)))))
(define cadar (lambda (pair) (car (cdr (car pair)))))
(define caddr (lambda (pair) (car (cdr (cdr pair)))))
(define cdaar (lambda (pair) (cdr (car (car pair)))))
(define cdadr (lambda (pair) (cdr (car (cdr pair)))))
(define cddar (lambda (pair) (cdr (cdr (car pair)))))
(define cdddr (lambda (pair) (cdr (cdr (cdr pair)))))
(define caaaar (lambda (pair) (car (car (car (car pair))))))
(define caaadr (lambda (pair) (car (car (car (cdr pair))))))
(define caadar (lambda (pair) (car (car (cdr (car pair))))))
(define caaddr (lambda (pair) (car (car (cdr (cdr pair))))))
(define cadaar (lambda (pair) (car (cdr (car (car pair))))))
(define cadadr (lambda (pair) (car (cdr (car (cdr pair))))))
(define caddar (lambda (pair) (car (cdr (cdr (car pair))))))
(define cadddr (lambda (pair) (car (cdr (cdr (cdr pair))))))
(define cdaaar (lambda (pair) (cdr (car (car (car pair))))))
(define cdaadr (lambda (pair) (cdr (car (car (cdr pair))))))
(define cdadar (lambda (pair) (cdr (car (cdr (car pair))))))
(define cdaddr (lambda (pair) (cdr (car (cdr (cdr pair))))))
(define cddaar (lambda (pair) (cdr (cdr (car (car pair))))))
(define cddadr (lambda (pair) (cdr (cdr (car (cdr pair))))))
(define cdddar (lambda (pair) (cdr (cdr (cdr (car pair))))))
(define cddddr (lambda (pair) (cdr (cdr (cdr (cdr pair))))))

;; (define (not x)            (if x #f #t))
;; (define (null? obj)        (if (eqv? obj '()) #t #f))
(define id (lambda (obj) obj))
(define flip (lambda (func) (lambda (arg1 arg2) (func arg2 arg1))))
(define curry (lambda (func arg1) (lambda (arg) (func arg1 arg))))
(define compose (lambda (f g) (lambda (arg) (f (g arg)))))
(define foldl (lambda (func accum lst)
  (if (null? lst)
      accum
      (foldl func (func accum (car lst)) (cdr lst)))))
(define foldr (lambda (func accum lst)
  (if (null? lst)
      accum
      (func (car lst) (foldr func accum (cdr lst))))))
(define unfold (lambda (func init pred)
  (if (pred init)
      (cons init '())
      (cons init (unfold func (func init) pred)))))
(define fold (lambda (f a l) (foldl f a l)))
(define reduce (lambda (f a l) (fold f a l)))

(define max (lambda (x  num-list) (fold (lambda (y z) (if (> y z) y z)) x (cons 0 num-list))))
(define min (lambda (x  num-list) (fold (lambda (y z) (if (< y z) y z)) x (cons 536870911 num-list))))
(define length (lambda (lst)        (fold (lambda (x y) (+ x 1)) 0 lst)))
(define append (lambda (lst  lsts)  (foldr (flip (curry foldr cons)) lst lsts)))
(define reverse (lambda (lst)       (fold (flip cons) '() lst)))
(define mem-helper (lambda (pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc))))
(define memq (lambda (obj lst)       (fold (mem-helper (curry eq? obj) id) #f lst)))
(define memv (lambda (obj lst)       (fold (mem-helper (curry eqv? obj) id) #f lst)))
(define member (lambda (obj lst)     (fold (mem-helper (curry equal? obj) id) #f lst)))
(define assq (lambda (obj alist)     (fold (mem-helper (curry eq? obj) car) #f alist)))
(define assv (lambda (obj alist)     (fold (mem-helper (curry eqv? obj) car) #f alist)))
(define assoc (lambda (obj alist)    (fold (mem-helper (curry equal? obj) car) #f alist)))
(define map (lambda (func lst)      (foldr (lambda (x y) (cons (func x) y)) '() lst)))
(define filter (lambda (pred lst)   (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst)))
