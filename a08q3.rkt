;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a08q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

;; Some useful constants and examples of a BST:

(define bank-database1
  (make-cnode "Maggie" 2180
              (make-cnode "Hilary" 546
                          (make-cnode "Adam" 1230 empty empty)
                          (make-cnode "Lora" 3000
                                      (make-cnode "Jean" 250 empty empty)
                                      empty))
              (make-cnode "Tom" 1540
                          (make-cnode "Roger" 501 empty empty)
                          (make-cnode "Vivian" 350 empty empty))))

(define bank-database2
   (make-cnode "Ernie" 2000 empty empty))

(define bank-database3
   (make-cnode "Adam" 0 empty empty))

(define bank-database4
   (make-cnode "Tom" 1000
               (make-cnode "Hermione" 10 empty empty) empty))

(define bank-database5
   (make-cnode "Tom" 0 empty
               (make-cnode "Hermione" 10 empty empty)))

(define bank-database6 empty)

;; (withdrawl-amount amount original) consumes the savings
;;    in the bank, original and the withdrawl amount, amount and
;;      calculates the remaining savings in the bank account after the withdrawl
;; withdrawl-amount: Nat Nat-> Nat
;; Examples:
(check-expect (withdrawl-amount 12 13) 1)
(check-expect (withdrawl-amount 10 100) 90)
(check-expect (withdrawl-amount 20 200) 180)

(define (withdrawl-amount amount original)
    (cond
      [(negative? (- original amount)) 0]
      [else
       (- original amount)]))

;; Tests:
(check-expect (withdrawl-amount 1000 100) 0)
(check-expect (withdrawl-amount 300 1300) 1000)



;; (find-name name money bst) consumes a name, money and
;;   a binary search tree bst, and searches through the tree to find the name
;;    and the amount of money withdrawn in assigned to the name in the binary search tree
;; find-name: Str Nat CBST-> Str
;; Examples:
(check-expect (find-name "Maggie" 80 bank-database1) "Maggie has a balance of $2100")
(check-expect (find-name "Hilary" 46 bank-database1)"Hilary has a balance of $500")

(define (find-name name money bst)
  (cond
    [(empty? bst) empty]
    [(string=? (cnode-name bst) name)(string-append(cnode-name bst) " has a balance of $"
                               (number->string(withdrawl-amount money (cnode-amt bst))))]
    [(string>? name (cnode-name bst))(find-name name money (cnode-right bst))]
    [else
     (find-name name money (cnode-left bst))]))

;; Tests:
(check-expect (find-name "Mags" 1230 bank-database1) empty)
(check-expect (find-name "Hilary" 546 bank-database1) "Hilary has a balance of $0")
(check-expect (find-name "Adam" 1230 bank-database1) "Adam has a balance of $0")
(check-expect (find-name "Lora" 3500 bank-database1) "Lora has a balance of $0")
(check-expect (find-name "Jean" 300 bank-database1) "Jean has a balance of $0")
(check-expect (find-name "Tom" 5000 bank-database1) "Tom has a balance of $0")
(check-expect (find-name "Roger" 2000 bank-database1) "Roger has a balance of $0")
(check-expect (find-name "Vivian" 40000 bank-database1) "Vivian has a balance of $0")
(check-expect (find-name "Sam" 0 bank-database1) empty)
(check-expect (find-name "Ernie" 0 bank-database2) "Ernie has a balance of $2000")
(check-expect (find-name "Korra" 29 bank-database2) empty)
(check-expect (find-name "Adam" 3000 bank-database3) "Adam has a balance of $0")
(check-expect (find-name "Emma" 0 bank-database3) empty)
(check-expect (find-name "Adam" 230 bank-database1) "Adam has a balance of $1000")
(check-expect (find-name "Lora" 2700 bank-database1) "Lora has a balance of $300")
(check-expect (find-name "Jean" 200 bank-database1) "Jean has a balance of $50")
(check-expect (find-name "Tom" 500 bank-database1) "Tom has a balance of $1040")
(check-expect (find-name "Roger" 1 bank-database1) "Roger has a balance of $500")
(check-expect (find-name "Vivian" 50 bank-database1) "Vivian has a balance of $300")
(check-expect (find-name "Helga" 0 bank-database6) empty)


;; (withdraw amt client bank-db) consumes an amount amt, a name
;;   client, and a Binary Search Tree, bank-db and outputs the new
;;    balance and updates it in the bank-db after the amount is withdrawn
;; withdraw: Nat Str CBST-> Str
;; Examples:
(check-expect (withdraw 80 "Maggie"  bank-database1) "Maggie has a balance of $2100")
(check-expect (withdraw 46 "Hilary"  bank-database1)"Hilary has a balance of $500")
(check-expect (withdraw 175 "Jean" bank-database1) "Jean has a balance of $75")
(check-expect (withdraw 2000 "Tom" bank-database1) "Tom has a balance of $0")
   

(define (withdraw amt client bank-db)
  (cond
    [(empty? (find-name client amt bank-db)) (string-append
                                  "Client (" client ") does not exist")]
    [else
     (find-name client amt bank-db)]))

;; Tests:
(check-expect (withdraw 20 "Groot"  bank-database1) "Client (Groot) does not exist")
(check-expect (withdraw 1230 "Mags" bank-database1) "Client (Mags) does not exist")
(check-expect (withdraw 546 "Hilary"bank-database1) "Hilary has a balance of $0")
(check-expect (withdraw 1230 "Adam" bank-database1) "Adam has a balance of $0")
(check-expect (withdraw  3500 "Lora"bank-database1) "Lora has a balance of $0")
(check-expect (withdraw  300 "Jean" bank-database1) "Jean has a balance of $0")
(check-expect (withdraw 5000 "Tom"  bank-database1) "Tom has a balance of $0")
(check-expect (withdraw 2000 "Roger"bank-database1) "Roger has a balance of $0")
(check-expect (withdraw 40000"Vivian" bank-database1) "Vivian has a balance of $0")
(check-expect (withdraw 0 "Sam" bank-database1)"Client (Sam) does not exist")
(check-expect (withdraw 0 "Ernie" bank-database2) "Ernie has a balance of $2000")
(check-expect (withdraw 29 "Korra" bank-database2) "Client (Korra) does not exist")
(check-expect (withdraw 3000 "Adam"bank-database3) "Adam has a balance of $0")
(check-expect (withdraw 0 "Emma"  bank-database3) "Client (Emma) does not exist")
(check-expect (withdraw 230 "Adam"bank-database1) "Adam has a balance of $1000")
(check-expect (withdraw 2700 "Lora"bank-database1) "Lora has a balance of $300")
(check-expect (withdraw 200 "Jean"bank-database1) "Jean has a balance of $50")
(check-expect (withdraw 500 "Tom" bank-database1) "Tom has a balance of $1040")
(check-expect (withdraw 1 "Roger"  bank-database1) "Roger has a balance of $500")
(check-expect (withdraw 50 "Vivian"bank-database1) "Vivian has a balance of $300")
(check-expect (withdraw 0 "Helga" bank-database6) "Client (Helga) does not exist")





