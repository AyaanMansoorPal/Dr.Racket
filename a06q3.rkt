;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname a06q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;**********************************************
;; Ayaan Mansoor Pal 
;;**********************************************
;;




(define-struct drink-price (s-size m-size l-size))
;; A Drink-Price is a (make-drink-price Price Price Price)
;; where s-size, m-size, l-size are the prices of the drink if
;; it is available in the respective size, and false otherwise
;; A Price is (anyof Num false)
;; where it is a number >= 0 if the drink is available in the
;; associated size and false otherwise

;; Useful examples and constants for drink-price:
(define price1(make-drink-price false false false))
(define price2(make-drink-price false false 0))
(define price3(make-drink-price false 0 0))
(define price4(make-drink-price false 0 false))
(define price5(make-drink-price 0 false 0))
(define price6(make-drink-price 0 0 0))
(define price7(make-drink-price false false 1))
(define price8(make-drink-price false 1 1))
(define price9(make-drink-price 1 false 1))
(define price10(make-drink-price false 1 false))
(define price11(make-drink-price false 3 false))
(define price12(make-drink-price 12 2 13))


(define-struct drink (name brand prices))
;; A Drink is a (make-drink Str Str Drink-Price)
;; where
;; * name represents the name of the drink
;; * brand represents the brand of the drink
;; * prices provides the prices for the drink in S, M and L size
;; if they are available

;; Useful examples and constants for drink:
(define drink1(make-drink "peach cider" "Aco" price1))
(define drink2 (make-drink "diet apple" "BDrink" price2))
(define drink3 (make-drink "peach cider" "Aco" price3))
(define drink4 (make-drink "diet apple" "BDrink" price4))
(define drink5 (make-drink "peach cider" "Aco" price5))
(define drink6 (make-drink "diet apple" "BDrink" price6))
(define drink7 (make-drink "peach cider" "Aco" price7))
(define drink8 (make-drink "diet apple" "BDrink" price8))
(define drink9 (make-drink "peach cider" "Aco" price9))
(define drink10(make-drink "diet apple" "BDrink" price10))
(define drink11(make-drink "peach cider" "Aco" price11))
(define drink12(make-drink "diet apple" "BDrink" price12))
                           
;; Useful List of Drink:
(define alist0 empty)
(define alist1 (list drink1))
(define alist2 (list drink1 drink2))
(define alist3 (list drink1 drink2 drink3))
(define alist4 (list drink6))
(define alist5 (list drink12))
(define alist6 (list drink1 drink2 drink9))
(define alist7 (list drink10 drink11 drink12))
(define alist8 (list drink7 drink8 drink9))
(define alist9 (list drink4 drink4 drink4))
(define alist10 (list drink9 drink9 drink9 drink9))
(define alist11 (list drink5 drink6 drink12))

;; (length-list alist) calculates the length of the list
;; alist by counting the number of items in the list
;; length-list (listof Drink)-> Nat
;; Examples:
(check-expect (length-list alist0) 0)
(check-expect (length-list alist1) 0)
(check-expect (length-list alist2) 0)

(define (length-list alist)
  (cond
   [(empty? alist) 0]
   [else
     (cond
     [(equal? (drink-price-m-size(drink-prices(first alist))) false) 0]
     [else
      (+ 1 (length-list (rest alist)))])]))

;; Tests:
(check-expect (length-list alist3) 0)
(check-expect (length-list alist4) 1)
(check-expect (length-list alist5) 1)
(check-expect (length-list alist6) 0)
(check-expect (length-list alist7) 3)
(check-expect (length-list alist8) 0)
(check-expect (length-list alist9) 3)
(check-expect (length-list alist10)0)
(check-expect (length-list alist11)0)

;; (sum-of-M lod) calculates the sum of the
;; m-size drink prices in the list lod
;; sum-of-M: (listof Drink)-> Num
;; Examples:
(check-expect (sum-of-M alist0) 0)
(check-expect (sum-of-M alist1) 0)
(check-expect (sum-of-M alist2) 0)

(define (sum-of-M lod)
  (cond
    [(empty? lod) 0]
    [(equal? false (drink-price-m-size(drink-prices (first lod)))) 0]
    [else
     (+ (drink-price-m-size (drink-prices (first lod)))(sum-of-M (rest lod)))]))

;; Tests:
(check-expect (sum-of-M alist3) 0)
(check-expect (sum-of-M alist4) 0)
(check-expect (sum-of-M alist5) 2)
(check-expect (sum-of-M alist6) 0)
(check-expect (sum-of-M alist7) 6)
(check-expect (sum-of-M alist8) 0)
(check-expect (sum-of-M alist9) 0)
(check-expect (sum-of-M alist10)0)
(check-expect (sum-of-M alist11)0)

;; (average-M drinks) consumes a list of drinks,drinks 
;; and calculates the average price of the m-size drinks
;; average-M (listof Drink)-> (anyof Num Sym)
;; Examples:
(check-expect (average-M alist0) 0)
(check-expect (average-M alist1) 'no-data)
(check-expect (average-M alist2) 'no-data)

(define (average-M drinks)
  (cond
    [(empty? drinks) 0]
    [(equal? (sum-of-M drinks) 0) 'no-data]
    [else
     (/ (sum-of-M drinks) (length-list drinks))]))

;; Tests:
(check-expect (average-M alist3) 'no-data)
(check-expect (average-M alist4) 'no-data)
(check-expect (average-M alist5) 2)
(check-expect (average-M alist6) 'no-data)
(check-expect (average-M alist7) 2)
(check-expect (average-M alist8) 'no-data)
(check-expect (average-M alist9) 'no-data)
(check-expect (average-M alist10) 'no-data)
(check-expect (average-M alist11) 'no-data)


