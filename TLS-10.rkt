#lang racket
; The Little Schemer
; 10-What Is the Value of All of This?

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
      (else (or (equal? (car lat) a)
                (member? a (cdr lat)))))))

; rember finds the first instance of the atom a in a lat and removes it
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) `())
              ((equal? (car lat) a) (cdr lat))
              (else (cons (car lat)
                          (rember a
                            (cdr lat)))))))

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
         ((equal? (car lat) old)
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
              ((equal? (car lat) a) (multirember a (cdr lat)))
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
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

; occur finds the number of times an atom occurs/appears/exists in a lat
(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else (cond
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

; list recursion

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
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))

; eqlist2
; eqlist2 does the same as eqlist, it's just written differently
(define eqlist2?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else (and (equal? (car l1) (car l2)) (eqlist2? (cdr l1) (cdr l2)))))))

; rember2 does the same thing as rember, it's just written differently
(define rember2
  (lambda (s l)
    (cond
      ((null? l) '())
      ((equal? (car l) s) (cdr l))
      (else (cons (car l) (rember2 s (cdr l)))))))

; arithmetic expressions

; an arithmetic expression, for this chapter's purposes, is an atom or two arithmetic expressions combined by +, x, or ^

; numbered
; numbered is a function that determines whether a representation of an arithmetic expression contains only numbers besides the +, x, and ^ (+, *, and exp)
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))

; value returns the natural value of a numbered arithmetic expression
(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) '+) (o+ (value (car nexp)) (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) '*) (x (value (car nexp)) (value (car (cdr (cdr nexp))))))
      (else (exponent (value (car nexp)) (value (car (cdr (cdr nexp)))))))))

; value-prefix finds the value of an arithmetic expression written in prefix notation
(define value-prefix
  (lambda (nexp)
    (cond
      ((atom? nexp) (nexp))
      ((eq? (car nexp) '+) (o+ (value (car (cdr nexp))) (value (car (cdr (cdr nexp))))))
      ((eq? (car nexp) '*) (x (value (car (cdr nexp))) (value (car (cdr (cdr nexp))))))
      ((eq? (car nexp) 'exp) (exponent (value (car (cdr nexp))) (value (car (cdr (cdr nexp)))))))))

; 1st-sub-exp returns the first sub-expression of an arithmetic expression
(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

; 2nd-sub-exp returns the second sub-expression of an arithmetic expression
(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

; operator will be useful in prefix notation when we're looking to deal with the operator, which is first (the car) in the expression--using this makes it clearer what we're doing
(define operator
  (lambda (aexp)
    (car aexp)))

; value-prefix-helper finds the value of the arithmetic expression with prefix notation and the helper functions
(define value-prefix-helper
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) '+) (o+ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
      ((eq? (operator nexp) '*) (x (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
      (else (exponent (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))))))

; primitives for numbers: number?, zero?, add1, sub1

; sero? also tests for zero, but at this time we're using a new representation for numbers:
; zero: (), one: (()), two: (() ()), three: (() () ()), ...
(define sero?
  (lambda (n)
    (null? n)))

; edd1 is like add1, just using our new representations for numbers
(define edd1
  (lambda (n)
    (cons '() n)))

; zub1 is like sub1, just using our new representations for numbers
(define zub1
  (lambda (n)
    (cdr n)))

; .+ is like o+, just using our new representations for numbers and our new helper functions
(define .+
  (lambda (n m)
    (cond
      ((sero? m) n)
      (else (edd1 (.+ n (zub1 m)))))))

; lat? doesn't work right with this new () representation of numbers
; Beware of shadows

; sets

; set-no atom appears more than once
; set? determines if a lat is a set
(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      (else
       (cond
         ((one? (occur (car lat) lat)) (set? (cdr lat)))
         (else #f))))))

; set1? does the same as set?
(define set1?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      (else
       (cond
         ((member? (car lat) (cdr lat)) #f)
         (else (set1? (cdr lat))))))))

; set2? does the same as set?
(define set2?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      (else
       (and (one? (occur (car lat) lat)) (set2? (cdr lat)))))))

; set3? does the same as set?
(define set3?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set3? (cdr lat))))))

; makeset creates a set out of a lat-making it so that each atom only appears once and removing repeat occurances of atoms-removes all occurances of an atom except the final appearance of that atom
(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))

; makeset2 serves the same purpose as makeset but keeps the first occurance of every atom and removes subsequent appearances
(define makeset2
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (car lat) (makeset2 (multirember (car lat) (cdr lat))))))))

; subset? returns #t true if each atom in set1 also appears in set2
(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      ((member? (car set1) set2) (subset? (cdr set1) set2))
      (else #f))))

; subset2? does the same as subset? but it's more concise-uses and
(define subset2?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2) (subset2? (cdr set1) set2))))))

; eqset? determines whether two sets are equal-they have to have the same atoms (each appearing only once) but the order in which they appear may be different
(define eqset?
  (lambda (set1 set2)
    (cond
      ((and (null? set1) (null? set2)) #t)
      ((or (null? set1) (null? set2)) #f)
      ((subset? set1 set2) (eqset? (cdr set1) (rember (car set1) set2)))
      (else #f))))

; eqset2? does the same as eqset? but it's more concise
(define eqset2?
  (lambda (set1 set2)
    (cond
      (else (and (subset? set1 set2) (subset? set2 set1))))))

; eqset3? does the same as eqset? and eqset2? but it's even more concise
(define eqset3?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

; intersect? tests whether at least one atom in set1 appears in set2
(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      ((member? (car set1) set2) #t)
      (else (intersect? (cdr set1) set2)))))

; intersect2? does the same as intersect?, just using or
(define intersect2?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2) (intersect2? (cdr set1) set2))))))

; intersect returns a list of atoms that appear in both set1 and set2
(define intersect
  (lambda (set1 set2)
    (cond
      ((intersect? set1 set2) (cond
                                ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
                                (else (intersect (cdr set1) set2))
                                ))
      (else '()))))

; intersect1 does the same as intersect
(define intersect1
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2) (cons (car set1) (intersect1 (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

; union finds the union of two sets--elements in set1, set2, or both
(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))

; set-difference returns the atoms that are in set1 but not in set2
(define set-difference
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (set-difference (cdr set1) set2))
      (else (cons (car set1) (set-difference (cdr set1) set2))))))

; intersectall--*base case with null*
; intersectall finds the atoms that are part of the intersection of all sets within the l-set
(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set) (intersectall (cdr l-set)))))))

; a pair is a different but related object--list with only two S-expressions

; a-pair? tests whether something is a pair
(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

; first refers to the first S-expression of a pair
(define first
  (lambda (p)
    (car p)))

; second refers to the second S-expression of a pair
(define second
  (lambda (p)
    (car (cdr p))))

; build builds/makes a representation of a pair with two S-expressions, s1 and s2
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

; third is how you'd get the third S-expression in a list
(define third
  (lambda (l)
    (car (cdr (cdr l)))))

; rel stands for relation
; l is a rel when it is a set of pairs
; fun stands for function

; fun? determines if rel is a function--it is if (firsts rel) is a set--firsts is from chapter 03
(define fun?
  (lambda (rel)
    (set? (firsts rel))))

; here, for us, a finite function is a list of pairs in which no first element of any pair is the same as any other first element

; revrel reverses the order of the contents of each pair within the rel
(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (build (second (car rel)) (first (car rel))) (revrel (cdr rel)))))))

; revpair reverses the two components of a pair
(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

; revrel1 does the same as revrel, but this time using the new revpair help function
(define revrel1
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (revpair (car rel)) (revrel (cdr rel)))))))

; seconds is like firsts, but instead of getting the first element of all the pairs, it gets the second
(define seconds
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (second (car l)) (seconds (cdr l)))))))

; fullfun? tests if a function is full--no first element of any pair is the same as any other first element of any of the other pairs, and no second element of any pair is the same as any other second element of any other pairs
; since the parameter is fun, we're going along with this assuming we pass in a fun, which we know is a fun and not just a rel
 (define fullfun?
   (lambda (fun)
    ; (and (set? (firsts fun)) (set? (seconds fun)))))
     (set? (seconds fun))))

; one-to-one? is the same as fullfun?, and here we'll just write it differently
(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))

; currying

; rember-f is like rember but it uses either eq? or equal?
(define rember-f0
  (lambda (test? a l)
    (cond
      ((null? l) l)
         ((test? (car l) a) (cdr l))
         (else (cons (car l) (rember-f0 test? a (cdr l)))))))

; eq?-c is like eq? but this time with Curry-ing
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

; eq?-salad is what happens if you'd do (eq?-c k) where k is salad--its value is a function that takes x as an argument and tests whether it is eq? to salad
(define eq?-salad (eq?-c 'salad))

; rember-f takes one argument test? and returns an argument like rember with eq? replaced by test?
(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? (car l) a) (cdr l))
        (else (cons (car l) ((rember-f test?) a (cdr l))))))))

; rember-eq? is the function returned by (rember-f0 test?) where test? is eq?
(define rember-eq? (rember-f eq?))

; insertL-f is insertL from chapter 03, but just curried
(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? old (car l)) (cons new (cons old (cdr l))))
        (else (cons (car l) ((insertL-f test?) new old (cdr l))))))))

; insertR-f is insertR from chapter 03, just curried
(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? (car l) old) (cons old (cons new (cdr l))))
        (else (cons (car l) ((insertR-f test?) new old (cdr l))))))))

; seqL takes three arguments and conses the first argument onto the result of consing the second argument onto the third argument
(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

; seqR is a function that takes three arguments and conses the second argument onto the result of consing the first argument onto the third argument
(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

; insert-g takes in one argument, seq, and returns insertL if seq is seqL and insertR if seq is seqR
(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((eq? (car l) old) (seq new old (cdr l)))
        (else (cons (car l) ((insert-g seq) new old (cdr l))))))))

; insertL0 is just insertL defined with insert-g
(define insertL0 (insert-g seqL))

; insertR0 is just insertR defined with insert-g
(define insertR0 (insert-g seqR))

; insertL1 is insertL defined with insert-g but not passing in seqL
(define insertL1 (insert-g (lambda (new old l)
                  (cons new (cons old l)))))

; seqS
; seqS is a function like seqL or seqR for subst
(define seqS
  (lambda (new old l)
    (cons new l)))

; subst0
; subst0 is like subst just using insert-g
(define subst0 (insert-g seqS))

; seqrem is another one of our seq functions, but for rember
(define seqrem
  (lambda (new old l) l))

; #f just since there's no new value-?
; yyy, or rember0, is rember, now using what we just figured out
(define rember0
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

; atom-to-function takes one argument x and returns the function o+ if (eq? x '+), returns the function x if (eq? x 'x), and returns the function exponent otherwise--abstraction-value from chapter 06
(define atom-to-function
  (lambda (px)
    (cond
      ((eq? px '+) o+)
      ((eq? px 'x) x)
      (else exponent))))

; value0 is value rewritten with atom-to-function
(define value0
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else ((atom-to-function (operator nexp)) (value0 (1st-sub-exp nexp)) (value0 (2nd-sub-exp nexp)))))))

; multirember-f is multirember but rewritten with currying
(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? (car lat) a) ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat) ((multirember-f test?) a (cdr lat))))))))

; multirember-eq? is multirember-f where the test? is eq?
(define multirember-eq? (multirember-f eq?))

; eq?-tuna is like eq?-c only this time it compares the argument to tuna
(define eq?-tuna (eq?-c 'tuna))

; multiremberT is like multirember-f but instead of taking test? and returning a function, it takes a function like eq?-tuna and a lat and then does its work
(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) '())
      ((test? (car lat)) (multiremberT test? (cdr lat)))
      (else (cons (car lat) (multiremberT test? (cdr lat)))))))

; multirember&co makes use of collector functions/continuation! It separates occurances of the atom a in the list of atoms lat from the occurances of the other atoms in lat and forms two lists, one for each of these. Then, it executes the original col that was passed in using these two new lists as arguments. It collects elements to be removed in a list and elements that aren't removed in another list. Then, the collector is called, which is a function that uses those lists in some way.
(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat)
       (col '() '()))
      ((eq? (car lat) a) (multirember&co a
                                         (cdr lat)
                                         (lambda (newlat seen)
                                           (col newlat
                                                (cons (car lat) seen)))))
      (else
       (multirember&co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col (cons (car lat) newlat)
                              seen)))))))

; multiinsertLR inserts new into the left of oldL and to the right of oldR in lat if oldL and oldR are different.
(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) oldL) (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR) (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
      (else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))

; multiinsertLRs does what multiinsertLR does, but inserts new on both sides if oldL and oldR are the same
(define multiinsertLRs
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((and (eq? oldL oldR) (eq? (car lat) oldL)) (cons new (cons oldL (cons new (multiinsertLRs new oldL oldR (cdr lat))))))
      ((eq? (car lat) oldL) (cons new (cons oldL (multiinsertLRs new oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR) (cons oldR (cons new (multiinsertLRs new oldL oldR (cdr lat)))))
      (else (cons (car lat) (multiinsertLRs new oldL oldR (cdr lat)))))))

; multiinsertLR&co is to multiinsertLR as multirember&co is to multirember. When multiinsertLR&co is done, it will use col on the new lat, on the number of left insertions, and the number of right insertions.
(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat)
       (col '() 0 0))
      ((eq? (car lat) oldL) (multiinsertLR&co new oldL oldR (cdr lat) (lambda (newlat L R)
                                                                        (col (cons new (cons oldL newlat)) (add1 L) R))))
      ((eq? (car lat) oldR) (multiinsertLR&co new oldL oldR (cdr lat) (lambda (newlat L R)
                                                                        (col (cons oldR (cons new newlat)) L (add1 R)))))
      (else (multiinsertLR&co new oldL oldR (cdr lat) (lambda (newlat L R)
                                                        (col (cons (car lat) newlat) L R)))))))

; even? tests whether a number is even
(define even?
  (lambda (n)
    (= (x (divide n 2) 2) n)))

; evens-only* removes all odd numbers from a list of nested lists
(define evens-only*
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
         (else (evens-only* (cdr l)))))
      (else (cons (evens-only* (car l)) (evens-only* (cdr l)))))))

; evens-only*&co builds a nested list of even numbers by removing the odd ones from its argument and simultaneously multiplies the even numbers and sums up the odd numbers that occur in its argument
(define evens-only*&co
  (lambda (l col)
    (cond
      ((null? l)
       (col '() 1 0))
      ((atom? (car l))
       (cond
         ((even? (car l))
          (evens-only*&co (cdr l)
                          (lambda (newl p s)
                            (col (cons (car l) newl)
                                 (x (car l) p) s))))
         (else (evens-only*&co (cdr l)
                               (lambda (newl p s)
                                 (col newl
                                      p (o+ (car l) s)))))))
      (else (evens-only*&co (car l)
                            (lambda (al ap as)
                              (evens-only*&co (cdr l)
                                              (lambda (dl dp ds)
                                                (col (cons al dl)
                                                     (x ap dp)
                                                     (o+ as ds))))))))))

; partial functions and Y combinator

; looking and functions like it are partial functions (the other functions we've seen so far are called total functions)
; looking takes the number, finds the atom at that index, if a number, finds the atom at that index... until it reaches a symbol and tests that for equality with the given atom by using keep-looking
(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

; sorn stands for symbol or number
; keep-looking goes through the numbers as described for looking
; keep-looking does not recur on part of lat, so it is unnatural recursion
; keep-looking may never terminate; functions like this are partial functions
; in keep-looking, if sorn is a number, recursion continues with sorn as the sorn-th element in lat, but if sorn is an atom, it checks if it is eq? to a
(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn) (keep-looking a (pick sorn lat) lat))
      (else (eq? a sorn)))))

; shift takes a pair whose first component is a pair and builds a pair by shifting the second part of the first component into the second component
(define shift
  (lambda (pair)
    (build (first (first pair)) (build (second (first pair)) (second pair)))))

; align is not a partial function because it yields a value for every argument
; align is kind of like shift, but it resembles keep-looking since both align and keep-looking change their arguments for their recursive uses but in neither case is the change guaranteed to get us closer to the goal
(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (align (shift pora)))
      (else (build (first pora) (align (second pora)))))))

; length* counts the number of atoms in align's arguments
(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (o+ (length* (first pora)) (length* (second pora)))))))

; weight* is like length* but pays more attention (at least twice as much) to the first component
(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
       (o+ (x (weight* (first pora)) 2)
           (weight* (second pora)))))))

; shuffle is like align but it uses revpair from chapter 07 instead of shift
; shuffle and revpair swap components of pairs when the first component is a pair
(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (shuffle (revpair pora)))
      (else (build (first pora) (shuffle (second pora)))))))

; C--Lothar Collatz
(define C
  (lambda (n)
    (cond
      ((one? n) 1)
      (else
       (cond
         ((even? n) (C (divide n 2)))
         (else (C (add1 (x 3 n)))))))))

; A--Wilhelm Ackermann
(define A
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n) (A n (sub1 m)))))))

; eternity is the most partial function
; eternity reaches its goal for none of its arguments, and this is the most unnatural recursion possible
(define eternity
  (lambda (x)
    (eternity x)))

; lengthL finds the length of a list
(define lengthL
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (lengthL (cdr l)))))))

; length0 can find the length of an empty list and nothing else
; (define length0
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (eternity (cdr l)))))); )

; length <= 1
(lambda (l)
  (cond
    ((null? l) 0)
    (else
     (add1
      ((lambda (l)
         (cond
           ((null? l) 0)
           (else
            (add1 (eternity (cdr l))))))
       (cdr l))))))

; length <= 2
(lambda (l)
  (cond
    ((null? l) 0)
    (else
     (add1
      ((lambda (l)
         (cond
           ((null? l) 0)
           (else
            (add1
             ((lambda (l)
                (cond
                  ((null? l) 0)
                  (else
                   (add1
                    (eternity
                     (cdr l))))))
              (cdr l))))))
       (cdr l))))))

; this lambda function creates length0
((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 eternity)

; length <= 1 in the same style as above
((lambda (f)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (f (cdr l)))))))
 ((lambda (g)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (g (cdr l)))))))
  eternity))

; length <= 2
((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 ((lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l)))))))
  ((lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))
   eternity)))

; mk-length0 takes length as an argument and returns a function that looks like length
((lambda (mk-length0)
   (mk-length0 eternity))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

; mk-length1 is like mk-length0 but for length <= 1
((lambda (mk-length1)
   (mk-length1
    (mk-length1 eternity)))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

; mk-length2 is like mk-length1 but for length <= 2
((lambda (mk-length2)
   (mk-length2
    (mk-length2
     (mk-length2 eternity))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

; mk-length3 for length <= 3 in this style
((lambda (mk-length3)
   (mk-length3
    (mk-length3
     (mk-length3
      (mk-length3 eternity)))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

((lambda (le)
   ((lambda (f) (f f))
    (lambda (f)
      (le (lambda (x) ((f f) x))))))
 (lambda (length)
   (lambda (list)
           (cond
             ((null? list) 0)
             (else
              (add1 (length (cdr list))))))))

; applicative-order Y combinator
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

; 10-What Is the Value of All of This?
; an entry is a pair of lists whose first list is a set. Also, the two lists must be of equal length.

; new-entry builds an entry from a set of names and a list of values
(define new-entry build)

; lookup-in-entry-help is used as a helper function in lookup-in-entry
(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f names))
      ((eq? (car names) name) (car values))
      (else (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))

; lookup-in-entry searches an entry for a name and returns the corresponding element. If the name can't be found, the entry-f function is called.
(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))

; a table (also called an environment) is a list of entries.

; extend-table takes an entry and a table (possibly the empty one) and creates a new table by putting the new entry in front of the old table
(define extend-table cons)

; lookup-in-table searches the list of entries in order
(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (lookup-in-entry name (car table) (lambda (name)
                                                (lookup-in-table name (cdr table) table-f)))))))

; types: *const, *quote, *identifier, *lambda, *cond, *application
; types are represented by actions, which are functions that "do the right thing" when applied to the appropriate type of S-expression

; atom-to-action produces the correct action for the atom. It is a helper function for expression-to-action.
(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote number?)) *const)
      (else *identifier))))

; list-to-action is a help-function for expression-to-action. It's like atom-to-action but for dealing with lists.
(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
         ((eq? (car e) (quote quote)) *quote)
         ((eq? (car e) (quote lambda)) *lambda)
         ((eq? (car e) (quote cond)) *cond)
         (else *application)))
      (else *application))))

; expression-to-action produces the correct action (or function) for each possible S-expression
(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

; val, together with the functions it uses, is an interpreter. It is like eval in Scheme.
(define val
  (lambda (e)
    (meaning e (quote ()))))

; meaning is used in val
(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

; *const is the action for constants
(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e)))))

; *quote is the action for *quote
(define *quote
  (lambda (e table)
    (text-of e)))

; text-of is a help function used in *quote
(define text-of second)

; *identifier contains the values of identifiers
(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

; initial-table is the function passed in for table-f in lookup-in-table
(define initial-table
  (lambda (name)
    (car (quote ()))))

; *lambda is the action for lambda
; *lambda gives us a list like whose contents are like this: non-primitive table formals body
(define *lambda
  (lambda (e table)
    (build (quote non-primitive) (cons table (cdr e)))))

; table-of gives us the table part of the list given in *lambda
(define table-of first)

; formals-of gives us the formal arguments part of the list given in *lambda
(define formals-of second)

; body-of gives us the body part of the list given in *lambda
(define body-of third)

; evcon does what (cond...) does. (cond...) takes any number of cond-lines and considers each line in turn. If the question part on the left is false, it looks at the rest of the lines. Otherwise it proceeds to answer the right part. If it sees an else-line, it treats that cond-line as if its question part were true.
(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines))
                table))
      ((meaning (question-of (car lines))
                table)
       (meaning (answer-of (car lines))
                table))
      (else (evcon (cdr lines) table)))))

; else? finds out if a cond line's question is else
(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? x (quote else)))
      (else #f))))

; question-of finds the question of a cond line
(define question-of first)

; answer-of finds the answer part of a cond line
(define answer-of second)

; *cond is the action for cond
(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

; cond-lines-of is a help function used in *cond
(define cond-lines-of cdr)

; evlis takes a list of (representations of) arguments and a table, and returns a list composed of the meaning of each argument
(define evlis
  (lambda (args table)
    (cond
      ((null? args) (quote ()))
      (else
       (cons (meaning (car args) table) (evlis (cdr args) table))))))

; *application applies its function meaning to the meaning of the arguments
(define *application
  (lambda (e table)
  (apply1
   (meaning (function-of e) table)
   (evlis (arguments-of e) table))))

; function-of finds the function of an application
(define function-of car)

; arguments-of finds the arguments of an application
(define arguments-of cdr)

; the two representations of functions are (primitive primitive-name) and (non-primitive (table formals body))
; The list (table formals body) is called a closure record.

; primitive? determines if a function is a primitive
(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

; non-primitive? determines if a function is a non-primitive
(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))

; apply1 uses non-primitive? and primitive?
; apply1 approximates the function apply in Scheme
(define apply1
  (lambda (fun vals)
    (cond
      ((primitive? fun)
       (apply-primitive
        (second fun) vals))
      ((non-primitive? fun)
       (apply-closure
        (second fun) vals)))))

; apply-primitive is used in apply1
(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name (quote cons))
       (cons (first vals) (second vals)))
      ((eq? name (quote car))
       (car (first vals)))
      ((eq? name (quote cdr))
       (cdr (first vals)))
      ((eq? name (quote null?))
       (null? (first vals)))
      ((eq? name (quote eq?))
       (eq? (first vals) (second vals)))
      ((eq? name (quote atom?))
       (:atom? (first vals)))
      ((eq? name (quote zero?))
       (zero? (first vals)))
      ((eq? name (quote add1))
       (add1 (first vals)))
      ((eq? name (quote sub1))
       (sub1 (first vals)))
      ((eq? name (quote number?))
       (number? (first vals))))))

; :atom? is used in apply-primitive
(define :atom?
  (lambda (x)
    (cond
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) (quote primitive))
       #t)
      ((eq? (car x) (quote non-primitive)) #t)
      (else #f))))

; apply-closure extends the table
; applying a non-primitive function-a closure-to a list of values is the same as finding the meaning of the closure's body with its table extended by an entry of the form (formals values). In this entry, formals is the formals of the closure and values is the result of evlis.
(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry
               (formals-of closure)
               vals)
              (table-of closure)))))