;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname a06q4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;**********************************************
;; Ayaan Mansoor Pal 
;;**********************************************
;;


;; Useful Posns for testing and examples:
(define point0 (make-posn 0 0))
(define point1 (make-posn 0 1))
(define point2 (make-posn 1 0))
(define point3 (make-posn -1 0))
(define point4 (make-posn 0 -1))
(define point5 (make-posn -1 -1))
(define point6 (make-posn 2 2))
(define point7 (make-posn 2 -2))
(define point8 (make-posn -2 2))
(define point9 (make-posn -2 -2))
(define point10(make-posn 3 4))
(define point11(make-posn -3 4))
(define point12(make-posn 3 -4))
(define point13(make-posn -3 -4))

;; Useful list of Posns for testing and examples:
(define alist0 empty)
(define alist1 (list point0))
(define alist2 (list point0 point1))
(define alist3 (list point0 point2 point3))
(define alist4 (list point12 point13 point0))
(define alist5 (list point7 point10 point1 point2))
(define alist6 (list point11 point12 point10 point13))
(define alist7 (list point4 point5 point3 point2))
(define alist8 (list point6 point9 point8 point10 point12))



;; (distance-posns a-posn) calculates the square distance
;; between the origin and a-posn
;; distance-posns: Posn -> Nat
;; Examples:
(check-expect (distance-posns point0) 0)
(check-expect (distance-posns point1) 1)
(check-expect (distance-posns point2) 1)
(check-expect (distance-posns point3) 1)
(check-expect (distance-posns point4) 1)

(define(distance-posns a-posn)
   (+ (sqr (- (posn-x a-posn) 0))(sqr (- (posn-y a-posn) 0))))

;; Tests:
(check-expect (distance-posns point5)2)
(check-expect (distance-posns point6)8)
(check-expect (distance-posns point7)8)
(check-expect (distance-posns point8)8)
(check-expect (distance-posns point9)8)
(check-expect (distance-posns point10)25)
(check-expect (distance-posns point11)25)
(check-expect (distance-posns point12)25)
(check-expect (distance-posns point13)25)

;; (posn-insert p slop) evaluates the position at which the
;; posn p must be inserted in the list of posns slop
;; according to a set of rules
;; posn-insert: Posn (listof Posn)-> (listof Posn)
;; requires: sorted list of slop
;; Examples:
(check-expect (posn-insert point0 alist0) (list (make-posn 0 0)))
(check-expect (posn-insert point1 alist0) (list (make-posn 0 1)))
(check-expect (posn-insert point2 alist8) (list(make-posn 1 0)(make-posn 2 2)(make-posn -2 -2)(make-posn -2 2)
                                                                             (make-posn 3 4)(make-posn 3 -4)))



(define (posn-insert p slop)
  (cond
    [(empty? slop) (cons p empty)]
    [(< (distance-posns p) (distance-posns (first slop))) (cons p slop)]
    [(= (distance-posns p) (distance-posns (first slop)))
     (cond
       [(< (posn-x p) (posn-x (first slop))) (cons p slop)]
       [(= (posn-x p) (posn-x (first slop)))
        (cond
          [(< (posn-y p) (posn-y (first slop))) (cons p slop)]
          [else
           (cons (first slop) (posn-insert p (rest slop)))])]
          [else
           (cons (first slop) (posn-insert p (rest slop)))])]
          [else
           (cons (first slop) (posn-insert p (rest slop)))]))

;; Tests:
(check-expect (posn-insert point1 alist1) (list (make-posn 0 0) (make-posn 0 1)))
(check-expect (posn-insert point2 alist2) (list (make-posn 0 0) (make-posn 0 1) (make-posn 1 0)))
(check-expect (posn-insert point3 alist3) (list (make-posn 0 0) (make-posn -1 0) (make-posn 1 0) (make-posn -1 0)))
(check-expect (posn-insert point4 alist2) (list (make-posn 0 0) (make-posn 0 -1) (make-posn 0 1)))
(check-expect (posn-insert point11 alist6)(list (make-posn -3 4) (make-posn -3 4) (make-posn 3 -4) (make-posn 3 4) (make-posn -3 -4)))

;; (posn-sort alop) consumes an unsorted list alop and outputs
;; an ordered list according to ascending order respective to
;; the rules which apply
;; posn-sort: (listof Posn)-> (listof Posn)
;; Examples:
(check-expect (posn-sort alist0) empty)
(check-expect (posn-sort alist1) (list(make-posn 0 0)))
(check-expect (posn-sort alist2) (list (make-posn 0 0) (make-posn 0 1)))
(check-expect (posn-sort alist3) (list (make-posn 0 0) (make-posn -1 0) (make-posn 1 0)))  

(define (posn-sort alop)
  (cond
    [(empty? alop) empty]
    [else
     (posn-insert(first alop) (posn-sort (rest alop)))]))

;; Tests:
(check-expect (posn-sort alist4) (list (make-posn 0 0) (make-posn -3 -4) (make-posn 3 -4)))
(check-expect (posn-sort alist5) (list (make-posn 0 1) (make-posn 1 0) (make-posn 2 -2) (make-posn 3 4)))
(check-expect (posn-sort alist6) (list (make-posn -3 -4) (make-posn -3 4) (make-posn 3 -4) (make-posn 3 4)))
(check-expect (posn-sort alist7)(list (make-posn -1 0) (make-posn 0 -1) (make-posn 1 0) (make-posn -1 -1)))
(check-expect (posn-sort alist8) (list (make-posn -2 -2) (make-posn -2 2) (make-posn 2 2) (make-posn 3 -4) (make-posn 3 4)))



