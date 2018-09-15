#lang racket
; The Little Schemer
; 07-Friends and Relations

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
              ((eq? (car lat) a) (cdr lat))
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

; 07

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
      ; ((null? set2) #f)
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
      ; ((null? set2) #f)
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