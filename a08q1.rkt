;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname a08q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;**********************************************
;; Ayaan Mansoor Pal 
;;**********************************************
;;










(define-struct node (key val left right))
;; A Node is a (make-node Num Str BT BT)

;; A binary tree (BT) is either 
;; empty  
;; (make-node Num Str BT BT)

;; Some useful constants for a BT:

(define tree1 (make-node 5 "" (make-node 10 "" empty empty) (make-node 8 "" empty
(make-node 12 "" empty empty))))

(define tree2 (make-node 1 "" empty empty))

(define tree3 (make-node 5 "" (make-node 10 "" (make-node 3 "" empty empty)
              (make-node 11 "" (make-node 12 "" empty empty) (make-node 13 "" empty empty)))
                               (make-node 6 "" empty empty)))

(define tree4 (make-node 6 "" (make-node 10 "" empty (make-node 13 "" empty
               (make-node 14 "" (make-node 16 "" empty(make-node 17 "" empty empty))
                               (make-node 15 "" empty empty)))) empty))

(define tree5 (make-node 5 "" (make-node 14 "" (make-node 23 "" empty empty) (make-node 54 ""
               (make-node 3 "" (make-node 6 "" empty empty) empty) empty)) (make-node 13 "" empty empty)))

(define tree6 (make-node 12 "" (make-node 4 "" empty empty) empty))

(define tree7 (make-node 54 "" empty (make-node 5 "" empty empty)))

(define tree8 (make-node 10 ""
                         (make-node 11 ""
                                    (make-node 12 "" empty empty)
                                    (make-node 13 ""
                                               (make-node 14 "" empty empty)
                                               (make-node 15 "" empty
                                                          (make-node 16 "" empty empty)))) empty))
(define tree9 (make-node 10 "" empty
                         (make-node 11 "" empty
                                    (make-node 13 ""
                                               (make-node 14 ""
                                                          (make-node 15 ""
                                                                     (make-node 16 "" empty empty) empty) empty) empty))))

(define tree10 (make-node 5 ""
                          (make-node 11 "" empty empty)
                          (make-node 10 ""
                                     (make-node 13 ""
                                                (make-node 14 ""
                                                           (make-node 16 "" empty empty)
                                                           (make-node 17 ""
                                                                      (make-node 18 "" empty empty)
                                                                      (make-node 19 "" empty
                                                                                 (make-node 20 "" empty empty))))
                                                (make-node 15 "" empty empty))
                                     (make-node 12 "" empty empty))))


;; (count a) consumes a BT a, and counts the longest
;;   path of the BT
;; count: BT-> Nat
;; Examples:
(check-expect (count tree5) 4)
(check-expect (count tree4) 5)


(define (count a)
  (cond
    [(and (empty? (node-right a))(empty? (node-left a)))0]
    [(empty? (node-right a))(+ 1 (count (node-left a)))]
    [(empty? (node-left a)) (+ 1 (count (node-right a)))]
    [else
     (+ 1 (max(count (node-right a)) (count (node-left a))))]))

;; Tests:
(check-expect (count tree1) 2)
(check-expect (count tree2) 0)
(check-expect (count tree3) 3)
(check-expect (count tree4) 5)
(check-expect (count tree6) 1)
(check-expect (count tree7) 1)
(check-expect (count tree8) 4)
(check-expect (count tree9) 5)
(check-expect (count tree10) 6)

;; (height bt) consumes a BT and output
;;   the height of the tree by counting
;;     the branches of the BT
;; height: BT-> Nat
;; Examples and Tests: See the
;; Examples and Tests for the function above

(define (height bt)
  (count bt))