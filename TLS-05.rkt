#lang racket
; The Little Schemer
; 05-*Oh My Gawd*: It's Full of Stars

; atom? tests if something is an atom
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; lat? tests if something is a lat-list of atoms
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

; member? tests if an atom is part of a lat
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

; rember finds the first instance of the atom a in a lat and removes it
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) `())
      ; (else (cond
              ((eq? (car lat) a) (cdr lat))
              (else (cons (car lat)
                          (rember a
                            (cdr lat)))))))
; ))

; firsts takes a list and returns the first S-expression of all the internal lists
(define firsts
  (lambda (l)
    (cond
      ((null? l) `())
      (else (cons
             (car (car l)) (firsts (cdr l)))))))

; insertR finds an atom (old) in a lat then inserts new to the right of old
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) `())
      (else (cond
         ((eq? (car lat) old)
          (cons old
                (cons new (cdr lat))))
         (else (cons
                (car lat) (insertR new old (cdr lat)))))))))

; insertR finds an atom (old) in a lat then inserts new to the left of old
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) `())
      (else (cond
         ((eq? (car lat) old)
          (cons new lat))
         (else (cons
                (car lat) (insertL new old (cdr lat)))))))))

; subst finds an atom (old) in a lat then replaces the first instance of old with new
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) `())
      (else (cond
              ((eq? (car lat) old)
               (cons new (cdr lat)))
               (else (cons (car lat)
                           (subst new old
                                  (cdr lat)))))))))

; subst2 does the same but for o1 or o2 (two options for old)
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) `())
      (else (cond
              ((or (eq? (car lat) o1) (eq? (car lat) o2))
               (cons new (cdr lat)))
              (else (cons (car lat)
                          (subst2 new o1 o2 (cdr lat)))))))))

; multirember removes all occurances of a in the lat
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) `())
      (else (cond
              ((eq? (car lat) a) (multirember a (cdr lat)))
              (else (cons (car lat) (multirember a (cdr lat)))))))))

; multiinsertR inserts new to the right of any occurance of old in the lat
(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) `())
      (else (cond
              ((eq? (car lat) old) (cons old (cons new
                                                   (multiinsertR new old (cdr lat)))))
              (else (cons (car lat) (multiinsertR new old (cdr lat)))))))))

; multiinsertL inserts new to the left of any occurance of old in the lat
(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) `())
      (else (cond
              ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
              (else (cons (car lat) (multiinsertL new old (cdr lat)))))))))

; multisubst substitutes all occurances of old with new
(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) `())
      (else
       (cond
         ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
         (else (cons (car lat) (multisubst new old (cdr lat)))))))))

; numbers
; only consider nonnegative integers

; add1 adds 1 to the number n
(define add1
  (lambda (n)
    (+ n 1)))

; sub1 subtracts 1 from the number n
(define sub1
  (lambda (n)
    (- n 1)))

; o+ adds the number m to the number n
(define o+
  (lambda (n m)
    (cond
      ((zero? n) m)
      (else (add1 (o+ (sub1 n) m))))))

; zero? asks if a number is empty and null? asks if a list is empty

; o- subtracts number m from number n
(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))

; tup is short for tuple, which is a list of numbers

; addtup adds all the numbers in a tup
(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))

; x multiplies numbers n and m
(define x
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (x n (sub1 m)))))))

; tup+ adds each number in tup1 to the corresponding number in tup2 to produce a tup with these sums
(define tup+
  (lambda (tup1 tup2)
    (cond
      ; ((and (null? tup1) (null? tup2)) `())
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
       (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

; greaterThan compares n and m to test if n is greater than m
(define greaterThan
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (greaterThan (sub1 n) (sub1 m))))))

; lessThan compares n and m to test if n is less than m
(define lessThan
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (lessThan (sub1 n) (sub1 m))))))

; equalTo tests if n is equal to m
(define equalTo
  (lambda (n m)
    (cond
      ((greaterThan n m) #f)
      ((lessThan n m) #f)
      (else #t))))

; exponent finds the value of n^m
(define exponent
  (lambda (n m)
    (cond
      ((zero? m) 1)
      ; ((equalTo 1 m) n)
      (else (x n (exponent n (sub1 m)))))))

; divide
; divide gives the integer found when doing n/m-how many times m goes into n
(define divide
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (divide (o- n m) m))))))

; length finds the number of elements in a lat
(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

; pick
; pick gives the nth element in a lat
(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

; no-nums gives the lat with the number elements removed
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) `())
      (else (cond
              ((number? (car lat)) (no-nums (cdr lat)))
              (else (cons (car lat) (no-nums (cdr lat)))))))))

; all-nums gives the lat with only the number elements, so everything else is removed
(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) `())
      (else (cond
              ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
              (else (all-nums (cdr lat))))))))

; eqan? tests if two atoms are equal/the same using eq? for atoms and = for numbers
(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ; ((and (atom? a1) (atom? a2)) (eq? a1 a2))
      ; (else #f))))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

; occur finds the number of times an atom occurs/appears/exists in a lat
(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else (cond
              ; ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
              ((eq? (car lat) a) (add1 (occur a (cdr lat))))
              (else (occur a (cdr lat))))))))

; one? tells whether a number is equal to one
(define one?
  (lambda (n)
    (= 1 n)))

; rempick removes the nth element of a lat and returns the new lat
(define rempick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

; 05

; -*-functions recur on the car and the cdr when the car is a list--they work on lists that are empty, an atom consed onto a list, or a list consed onto a list

; rember*
; rember* removes all occurances of a from an S-expression
(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) a) (rember* a (cdr l)))
         (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

; insertR* inserts the new atom to the right of the old atom  after all occurances of old
(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
         (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

; occur* finds how many times an atom appears in a list
(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l)) (cond
                         ((eq? (car l) a) (add1 (occur* a (cdr l))))
                         (else (occur* a (cdr l)))))
      (else (+ (occur* a (car l)) (occur* a (cdr l)))))))

; subst* substitutes new for all occurances of old in the list
(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (cond
                         ((eq? (car l) old) (cons new (subst* new old (cdr l))))
                         (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

; insertL* inserts new to the left of all occurances of old in the list
(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (cond
                         ((eq? (car l) old) (cons new (cons old (insertL* new old (cdr l)))))
                         (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

; member* finds whether an element (a) is part of a list (l)
(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l)) (or (eq? (car l) a) (member* a (cdr l))))
      (else (or (member* a (car l)) (member* a (cdr l)))))))

; leftmost
; leftmost gives the leftmost atom in a non-empty list of S-expressions that does not contain the empty list
(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

; eqlist?

#| (define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2))) (cond
                                               ((eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))
                                               (else #f)))
      ((or (atom? (car l1)) (atom? (car l2))) #f)
      (else (cond
              ((eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))
              (else #f))))))
|#
; eqlist? determines whether two lists are equal/the same list
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2))) (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
      ((or (atom? (car l1)) (atom? (car l2))) #f)
      (else
       (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

; equal? determines whether two S-expressions are equal/the same
(define equal?
  (lambda (s1 s2)
    (cond
      ; ((and (null? s1) (null? s2)) #t)
      ; ((or (null? s1) (null? s2)) #f)
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      ; (else (and (eqlist? (car s1) (car s2)) (equal? (cdr s1) (cdr s2)))))))
      (else (eqlist? s1 s2)))))

; eqlist2
; eqlist2 does the same as eqlist, it's just written differently
(define eqlist2?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ; (else (and (equal? (car l1) (car l2)) (equal? (cdr l1) (cdr l2)))))))
      (else (and (equal? (car l1) (car l2)) (eqlist2? (cdr l1) (cdr l2)))))))

; rember2 does the same thing as rember, it's just written differently
(define rember2
  (lambda (s l)
    (cond
      ((null? l) '())
      ((equal? (car l) s) (cdr l))
      (else (cons (car l) (rember2 s (cdr l)))))))