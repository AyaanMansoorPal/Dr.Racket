;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname a07q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;**********************************************
;; Ayaan Mansoor Pal 
;;**********************************************
;;

;; (order-list nat alist)consumes a natural number
;; and a list nlist, and outputs a list with all the
;; values less than or equal to the natural number in alist
;; order-list: Nat (listof Nat)-> (listof Nat)
;; Examples:
(check-expect(order-list 5 empty) empty)
(check-expect(order-list 0 empty) empty)
(check-expect(order-list 0 (list 0))(list 0))

(define (order-list nat nlist)
  (cond
    [(empty? nlist) empty]
    [(and (<= (first nlist) nat) (>= (first nlist) 0))
     (cons (first nlist) (order-list nat (rest nlist)))]
    [else
     (order-list nat (rest nlist))]))

;; Tests:
(check-expect(order-list 0 (list -1 -2 3)) empty)
(check-expect(order-list 4 (list 2 3 4 5))(list 2 3 4))


;; (values-less num b) consumes a num and an upper
;; bound b and outputs the list of numbers from num
;; to b
;; values-less: Nat Nat-> (listof Nat)
;; Examples:
(check-expect(values-less 0 0) (list 0))
(check-expect(values-less -1 0) empty)
(check-expect(values-less  3 0)(list 0 1 2 3))

(define (values-less num b)
  (cond
    [(> b num ) empty]
    [else
     (cons b (values-less num (add1 b)))]))

;; Tests:
(check-expect(values-less 5 0)(list 0 1 2 3 4 5))
(check-expect(values-less 2 0)(list 0 1 2))

;; (insert number alon) goes through the list
;; alon and inserts the number according to
;; ascending order of the alon (using sort function)
;; insert: Num (listof Num)-> (listof Num)
;; Examples:
(check-expect(insert 0 empty)(list 0))

(define (insert number alon)
  (cond
    [(empty? alon)(cons number empty)]
    [(<= number (first alon)) (cons number alon)]
     [else (cons (first alon) (insert number (rest alon)))]))

;; Tests:
(check-expect(insert 4 (list 0 1 2 5)) (list 0 1 2 4 5))
(check-expect(insert 3(list 4 5 6)) (list 3 4 5 6))

;; (sort alons) consumes a list and sorts it according
;; to ascending order
;; sort: (listof Num)-> (listof Num)
;;Examples:
(check-expect(sort (list 5 4 2 2 0)) (list 0 2 2 4 5))
(check-expect(sort (list 3 2 5 3 1)) (list 1 2 3 3 5))

(define (sort alons)
  (cond
    [(empty? alons) empty]
    [else (insert (first alons) (sort (rest alons)))]))

;; Tests:
(check-expect(sort (list 0.2 0 2.2 -2)) (list -2 0 0.2 2.2))
(check-expect(sort (list 3 3 3 3))(list 3 3 3 3))

;; (compare-list slist olist) consumes two lists
;; and compares the values in both the list
;; and outputs true if the lists are exactly the same
;; otherwise it outputs false
;; compare-list: (listof Nat) (listof Nat)-> Bool
;; Examples:
(check-expect (compare-list empty empty) false)
(check-expect(compare-list empty (list 2))true)
(check-expect(compare-list (list 2) empty)false)

(define (compare-list slist olist)
  (cond
    [(and (empty? slist) (empty? olist)) false]
    [(empty? olist) false]
    [(empty? slist) true]
    [(= (first slist) (first olist)) (compare-list (rest slist) olist)]
    [else
     (compare-list slist (rest olist))]))

;; Tests:
(check-expect(compare-list (list 1 2 3) (list 1 2 3)) true)
(check-expect(compare-list(list 1 2 3) (list 2 3 1)) false)

;; (check-nat-in-list n nlist) checks if the natural numbers till n
;; is in nlist
;; check-nat-in-list: Nat (listof Nat) -> Bool
;; Examples:
(check-expect(check-nat-in-list 0 empty) false)
(check-expect(check-nat-in-list 0 (list 1)) false)
(check-expect(check-nat-in-list 5 (list 6 10 4 8 5 4 5 6 0 1 6 3 2)) true)
(check-expect(check-nat-in-list 3 (list 1 10 0)) false)

(define (check-nat-in-list n nlist)
  (cond
    [(empty? nlist) false]
    [else
     (compare-list  (values-less n 0) (sort(order-list n nlist)))]))

;; Tests:
(check-expect(check-nat-in-list 0 (list 0 1 3 2 4))true)
(check-expect(check-nat-in-list 4 (list 0 2 3 1 4)) true)
(check-expect(check-nat-in-list 2 (list 0 1 3 3 4)) false)
(check-expect(check-nat-in-list 4 (list 2 3 0 1 4 5 2)) true)
(check-expect(check-nat-in-list 4 (list 10 5 5 11 12)) false)
(check-expect(check-nat-in-list 0 (list 12 2 3 14 2 4)) false)



