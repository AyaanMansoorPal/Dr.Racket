;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a09q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

;; (condition-for-drink the-drink other) consumes a drink, the-drink
;; and another argument other and evaluates the total price for the
;; medium size drink
;; condition-for-drink: drink (listof drink)-> Nat
;; Check Tests and Examples for (total-price-m-size m-drinks)

(define (condition-for-drink the-drink other)
  (cond
    [(equal? false (drink-price-m-size (drink-prices the-drink))) (+ other 0)]
    [else
     (+ (drink-price-m-size  (drink-prices the-drink)) other)]))

;; (total-price-m-size m-drinks) consumes m-drinks and outputs
;; the total price of the m-size drinks
;; total-price-m-size: (listof drink)-> Num

(define (total-price-m-size m-drinks)
 (foldr condition-for-drink 0 m-drinks))

;; Tests and Examples:
(check-expect (total-price-m-size alist0) 0)
(check-expect (total-price-m-size alist1) 0)
(check-expect(total-price-m-size alist2)0)
(check-expect(total-price-m-size alist3)0)
(check-expect(total-price-m-size alist4)0)
(check-expect(total-price-m-size alist5)2)
(check-expect(total-price-m-size alist6)0)
(check-expect(total-price-m-size alist7)6)
(check-expect(total-price-m-size alist8)1)
(check-expect(total-price-m-size alist9)0)
(check-expect(total-price-m-size alist10)0)
(check-expect(total-price-m-size alist11)2)


;; (condition-for-no-drinks drink other) consumes a drink
;; and other, works as a condition for the foldr function to
;; output the number of m-size drinks
;; condition-for-no-drinks: drink (listof drink) -> Num
;; For tests and examples see function (total-m-size mdrinks)

(define (condition-for-no-drinks drink other)
  (cond
    [(equal? false (drink-price-m-size (drink-prices drink))) (+ other 0)]
    [else
     (+ 1 other)]))

;; (total-m-size mdrinks) consumes a list of drinks and
;; outputs the total number of m-size drinks
;; total-m-size: (listof drink) -> Num

(define (total-m-size mdrinks)
  (foldr condition-for-no-drinks 0 mdrinks))

;; Tests and Examples:
(check-expect(total-m-size alist0)0)
(check-expect(total-m-size alist1)0)
(check-expect(total-m-size alist2)0)
(check-expect(total-m-size alist3)1)
(check-expect(total-m-size alist4)1)
(check-expect(total-m-size alist5)1)
(check-expect(total-m-size alist6)0)
(check-expect(total-m-size alist7)3)
(check-expect(total-m-size alist8)1)
(check-expect(total-m-size alist9)3)
(check-expect(total-m-size alist10)0)
(check-expect(total-m-size alist11)2)
             



;; (average-M drinks) consumes a drink and evaluates
;; the average price of the m-size drinks.
;; average-M: (listof drink)-> Num

(define (average-M drinks)
  (cond
    [(equal? (total-m-size drinks) 0) 'no-data]
    [else
 (/ (total-price-m-size drinks) (total-m-size drinks))]))

;; Examples and Tests:
(check-expect(average-M alist0) 'no-data)
(check-expect(average-M alist1) 'no-data)
(check-expect(average-M alist2) 'no-data)

(check-expect(average-M
 (list
  (make-drink "peach cider" "Aco" (make-drink-price false 1.1 1.2))
  (make-drink "diet apple" "BDrink" (make-drink-price 0.8 0.9 1.2))
  (make-drink "orange mango" "Cbev" (make-drink-price 0.8 0.7 1.2))
  (make-drink "blue pop" "Dcome" (make-drink-price 0.8 false 1.2)))) 0.9)


(check-expect (average-M
 (list
  (make-drink "peach cider" "Aco" (make-drink-price 1.1 false 1.2))
  (make-drink "blue pop" "Dcom" (make-drink-price 0.8 false 1.2)))) 'no-data)
(check-expect (average-M
               (list
                (make-drink "peach" "aco" (make-drink-price 23 43 11))
                (make-drink "hing" "bco"  (make-drink-price false 32 33)))) 37.5)
                            
(check-expect (average-M alist3) 0)
(check-expect (average-M alist4) 0)
(check-expect (average-M alist5) 2)
(check-expect (average-M alist6) 'no-data)
(check-expect (average-M alist7) 2)
(check-expect (average-M alist8) 1)
(check-expect (average-M alist9) 0)
(check-expect (average-M alist10) 'no-data)
(check-expect (average-M alist11) 1)


