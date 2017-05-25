;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname a07q4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;**********************************************
;; Ayaan Mansoor Pal 
;;**********************************************



;; (compare list1 list2) consumes two lists, list1 list2
;; and compares the values in list1 to that of list2
;; outputs true when all values match and false otherwise
;; compare: (listof Num) (listof Num)-> Bool
;; Examples:
(check-expect(compare empty empty) true)
(check-expect(compare empty (list 1)) true)
(check-expect(compare (list 1) empty) false)

(define (compare list1 list2)
  (cond
    [(empty? list1) true]
    [(and (empty? list1) (empty? list2)) true]
    [(empty? list2) false]
    [(equal? (first list1) (first list2))
    (compare (rest list1) (rest list2))]
    [else
     false]))

;; Tests:
(check-expect(compare(list 1 2 3)(list 2 3 4)) false)
(check-expect(compare(list 1 2 3)(list 1 2 4 5)) false)
(check-expect(compare(list 5) (list 5 5 5 5)) true)
(check-expect(compare(list 4 4 4)(list 4 4 3 5))false)
(check-expect(compare(list 5 5)(list 2 3 5 4 5)) false)
(check-expect(compare(list 2 3 3)(list 1 2 3 4 5 3))false)
(check-expect(compare(list 5 4 2 3)(list 5 4 2 3))true)

;; (make-new-list alist slist) makes a new list by comparing the values of alist
;; to slist and outputs a list containing the similar values in slist
;; make-new-list: (listof Num) (listof Num)-> (listof Num)
;; Examples:
(check-expect(make-new-list empty empty) empty)
(check-expect(make-new-list empty (list 0)) empty)
(check-expect(make-new-list (list 0) empty) empty)

(define (make-new-list alist slist)
  (cond
    [(or(empty? alist) (empty? slist)) empty]
    [(= (first slist) (first alist)) (cons (first alist)(make-new-list(rest alist)(rest slist)))]
    [else
     (make-new-list  alist (rest slist))]))

;; Tests
(check-expect(make-new-list(list 1 2 3) (list 2 3 4 2)) empty)
(check-expect(make-new-list(list 0 -2 -3) (list 0 -2 -3 4))(list 0 -2 -3))
(check-expect(make-new-list(list -3 2 3) (list 1 2 3 -3 2 3)) (list -3 2 3))

  

;; (sublist? lst1 lst2) checks if lst1 is the subset of
;; lst2 by comparing the consecutive values.
;; sublist?: (listof Num) (listof Num)-> Bool
;; Examples:
(check-expect(sublist? empty empty) true)
(check-expect(sublist? empty (list 2 3 4)) true)
(check-expect(sublist? (list 2 3) empty) false)

(define (sublist? lst1 lst2)
  (cond
    [(and (empty? lst1) (empty? lst2)) true]
    [else
     (compare lst1 (make-new-list lst1 lst2))]))

;; Tests:
(check-expect(sublist? (list 5) (list 2 3 5 23 3)) true)
(check-expect(sublist? (list 4) (list 2 4 5 2 4 5)) true)
(check-expect(sublist?(list -1 -2)(list -1 -2 -3 1 2)) true)
(check-expect(sublist?(list 1.22 1.3 32) (list 1.32 1.3 1.22))false)
(check-expect(sublist?(list -1 -1 -1)(list -1))false)
(check-expect(sublist?(list 3 4 2 4)(list 1 3 4 3 4 2 4)) true)
(check-expect(sublist? (list 2 1 4 5) (list 3 2 1 4 2 2 1 4 5)) true)
(check-expect(sublist?(list 0 -1 3 2 10) (list 10 2 3 -1 0)) false)



