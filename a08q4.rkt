;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname a08q4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;**********************************************
;; Ayaan Mansoor Pal 
;;**********************************************
;;











(define-struct cnode (name amt left right))
;; a Cnode is a (make-cnode Str Nat CBST CBST)
;; requires: amt is measured in dollars


;; A customer binary search tree (CBST) is either
;; empty
;; (make-cnode Str Nat CBST CBST)
;; requires: every name in left is less than current name (via string comparison)
;;           every name in right is greater than current name (via string comparison)

(define bank-database
  (make-cnode "Maggie" 2180
              (make-cnode "Hilary" 546
                          (make-cnode "Adam" 1230 empty empty)
                          (make-cnode "Lora" 3000
                                      (make-cnode "Jean" 250 empty empty)
                                      empty))
              (make-cnode "Tom" 1540
                          (make-cnode "Roger" 501 empty empty)
                          (make-cnode "Vivian" 350 empty empty))))


(define bank (make-cnode "Henry" 123 empty empty))


(define bank1(make-cnode "Henry" 230
                         (make-cnode "Annie" 54 empty empty) empty))


(define bank2(make-cnode "Boon" 543
                         (make-cnode "Annie" 422 empty empty)
                         (make-cnode "Mark" 32 empty empty)))

(define bank3(make-cnode "Henry" 1000
                         (make-cnode "Annie" 5000 empty
                                     (make-cnode "Cody" 2 empty empty))
                         (make-cnode "Mark" 43 empty empty)))


(define bank4(make-cnode "Jacob" 4000
                         (make-cnode "Ean" 23444
                                     (make-cnode "Annie" 13 empty empty)
                                     (make-cnode "Fred" 54 empty empty))
                         (make-cnode "Kyle" 4324 empty empty)))


(define bank5(make-cnode "Jacob" 4000
                         (make-cnode "Ean" 2
                                     (make-cnode "Annie" 13 empty empty)
                                     (make-cnode "Fred" 54 empty empty))
                         (make-cnode "Kyle" 4324 empty empty)))


(define bank6(make-cnode "Jacob" 4000
                         (make-cnode "Ean" 23444
                                     (make-cnode "Annie" 13 empty empty)
                                     (make-cnode "Fred" 4 empty empty))
                         (make-cnode "Kyle" 4324 empty empty)))


(define bank7(make-cnode "Jacob" 4000
                         (make-cnode "Ean" 23444
                                     (make-cnode "Annie" 13 empty empty) 
                                     (make-cnode "Fred" 54 empty empty))
                                     (make-cnode "Kyle" 2 empty empty)))
  

;; (min-bt t) consumes a Binary Search Tree t and
;;   compares the amount of each client and checks which is the minimum value
;; min-bt: CBST-> Nat
;; requires: non-empty Binary Search Tree
;; Examples:
(check-expect (min-bt bank-database) 250)
(check-expect (min-bt bank) 123)
(check-expect (min-bt bank1) 54)
(check-expect (min-bt bank2) 32)

(define (min-bt t)
(cond
  [(and (empty? (cnode-left t))(empty? (cnode-right t)))
                                         (cnode-amt t)]
[(empty? (cnode-left t))
(min (cnode-amt t) (min-bt (cnode-right t)))]
[(empty? (cnode-right t))
(min (cnode-amt t) (min-bt (cnode-left t)))]
[else
(min (min-bt (cnode-right t)) (min-bt(cnode-left t))
     (cnode-amt (cnode-left t)) (cnode-amt(cnode-right t)))]))

;; Tests:
(check-expect (min-bt bank3) 2)
(check-expect (min-bt bank4) 13)
(check-expect (min-bt bank5) 2)
(check-expect (min-bt bank6) 4)
(check-expect (min-bt bank7) 2)

    
;; (all-keys bst ost) consumes a Binary Search Tree and an amount ost
;;   compares the amount in the Binary Search Tree and ost to find the name associated with the value
;; all-keys: CBST Nat-> Str
;; requires: non-empty BST
;; Examples:
(check-expect (all-keys bank-database 250) "Jean")
(check-expect (all-keys bank 123) "Henry")
(check-expect (all-keys bank1 54) "Annie")
               
(define (all-keys bst ost)
(cond 
[(empty? bst) ""]
[(= ost (cnode-amt bst)) (cnode-name bst)]
[else
 (string-append (all-keys (cnode-right bst) ost)(all-keys(cnode-left bst) ost))]))

;; Tests:
(check-expect (all-keys bank2 32) "Mark")
(check-expect (all-keys bank3 2) "Cody")
(check-expect (all-keys bank4 13) "Annie")
(check-expect (all-keys bank5 2) "Ean")
(check-expect (all-keys bank6 4) "Fred")
(check-expect (all-keys bank7 2) "Kyle")



;; (low-client bank-db) consumes a Binary Search Tree and
;;   outputs the name of the client with the minimum amount in the database
;; low-client: CBST-> Str
;; requires: non-empty BST
;; See examples and tests for (all-keys bst ost) for examples and tests

(define (low-client bank-db)
  (all-keys bank-db (min-bt bank-db)))
