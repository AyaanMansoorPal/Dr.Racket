;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a08q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;**********************************************
;; Ayaan Mansoor Pal 
;;**********************************************
;;










(define-struct binode (op arg1 arg2))

;; A binary arithmetic expression Internal Node (BINode) 
;;   is a (make-binode (anyof '*, '+, '/, '-) BinExp BinExp)


;; A Binary arithmetic expression (BinExp) is one of:
;; Num
;; (make-binode (anyof '* '+ '- '/) BinExp BinExp)

;; Some useful examples and constants of Binodes

(define a (make-binode '* 2 6))
(define b (make-binode '+ 2 (make-binode '* 5 3)))
(define c (make-binode '/ (make-binode '+ (make-binode '* 2 6)
                               (make-binode '* 5 2))(make-binode '- 5 3)))
(define d (make-binode '- 5 3))
(define f (make-binode '+ d 6))
(define g (make-binode '- (make-binode '- 5 9) 10))
(define h (make-binode '* (make-binode '* 4 3) 3))
(define i (make-binode '+ d f))
(define j (make-binode '+ 2.3 3))
(define k (make-binode '/ -4 2))
(define l (make-binode '+ (make-binode '+ f 3) 4))



;; (the-function bin) consumes a binode, bin and
;;   to outputs the evaluated and calculated form of bin
;;    with the '+ and '- are swapped during the evaluation process
;; the-function: Binode-> Num
;; Examples:
(check-expect (the-function a) 12)
(check-expect (the-function d) 8)
(check-expect (the-function j) -0.7)


(define (the-function bin)
  (cond
    [(number? bin) bin]
    
    [(symbol=? '* (binode-op bin))
     (cond
   [(and (number? (binode-arg1 bin))(number? (binode-arg2 bin)))
                                (* (binode-arg1 bin) (binode-arg2 bin))]
   [(number? (binode-arg1 bin)) (* (binode-arg1 bin)
                                        (the-function (binode-arg2 bin)))]
   [(number? (binode-arg2 bin)) (* (the-function (binode-arg1 bin))
                                              (binode-arg2 bin))]
   [else
    (* (the-function (binode-arg1 bin))(the-function (binode-arg2 bin)))])]
    
    
    [(symbol=? '- (binode-op bin))
     (cond
   [(and (number? (binode-arg1 bin)) (number? (binode-arg2 bin)))
                                (+ (binode-arg1 bin) (binode-arg2 bin))]
   [(number? (binode-arg1 bin)) (+ (binode-arg1 bin)
                                         (the-function (binode-arg2 bin)))]
   [(number? (binode-arg2 bin)) (+ (the-function (binode-arg1 bin))
                                               (binode-arg2 bin))]
   [else
    (+ (the-function (binode-arg1 bin)) (the-function (binode-arg2 bin)))])]
    
    
    [(symbol=? '+ (binode-op bin))
     (cond
   [(and (number? (binode-arg1 bin)) (number? (binode-arg2 bin)))
                                (- (binode-arg1 bin) (binode-arg2 bin))]
   [(number? (binode-arg1 bin)) (- (binode-arg1 bin)
                                           (the-function (binode-arg2 bin)))]
   [(number? (binode-arg2 bin)) (- (the-function (binode-arg1 bin))
                                                 (binode-arg2 bin))]
   [else
    (- (the-function (binode-arg1 bin))(the-function (binode-arg2 bin)))])]
    
    [(symbol=? '/ (binode-op bin))
     (cond
   [(and (number? (binode-arg1 bin)) (number? (binode-arg2 bin)))
                                 (/ (binode-arg1 bin) (binode-arg2 bin))]
   [(number? (binode-arg1 bin)) (/ (binode-arg1 bin)
                                            (the-function (binode-arg2 bin)))]
   [(number? (binode-arg2 bin)) (/ (the-function (binode-arg1 bin))
                                                  (binode-arg2 bin))]
   [else
    (/ (the-function (binode-arg1 bin))(the-function (binode-arg2 bin)))])]))

;; Tests:
(check-expect(the-function b)-13)
(check-expect(the-function c)0.25)
(check-expect(the-function f)2)
(check-expect(the-function g)24)
(check-expect(the-function h)36)
(check-expect(the-function i)6)
(check-expect(the-function k)-2)
(check-expect(the-function l)-5)


;; (string-form bn) consumes a Binode bn and outputs
;;   the binode in a mathematical string expression form 
;;    with the '+ and '- operator with the opposite operator.
;; string-form: Binode-> Str
;; Examples:
(check-expect (string-form a) "(2*6)")
(check-expect (string-form b) "(2-(5*3))")
(check-expect (string-form c) "(((2*6)-(5*2))/(5+3))")

(define (string-form bn)
  (cond
    [(number? bn) (number->string bn)]
    [else
     (string-append "("  (string-form(binode-arg1 bn))
                    (cond
                      [(symbol=? '+ (binode-op bn)) "-"]
                      [(symbol=? '- (binode-op bn)) "+"]
                      [else
                       (symbol->string (binode-op bn))])(string-form(binode-arg2 bn)) ")")]))

;; Tests:
(check-expect (string-form d) "(5+3)")
(check-expect (string-form f) "((5+3)-6)")
(check-expect (string-form g) "((5+9)+10)")
(check-expect (string-form h) "((4*3)*3)")
(check-expect (string-form i) "((5+3)-((5+3)-6))")
(check-expect (string-form k) "(-4/2)")
(check-expect (string-form l) "((((5+3)-6)-3)-4)")

;; (swap-eval ex) consumes a binode ex and
;;   outputs a mathematial expression with the
;;    '+ and '- swapped and evaluated in that fashion
;; swap-eval: Binode-> Str
;; Examples:
(check-expect(swap-eval a)"(2*6)=12")
(check-expect(swap-eval b)"(2-(5*3))=-13")
(check-expect(swap-eval c)"(((2*6)-(5*2))/(5+3))=1/4")



(define (swap-eval ex)
(string-append (string-form ex) "=" (number->string(the-function ex))))

(check-expect(swap-eval d)"(5+3)=8")
(check-expect(swap-eval f)"((5+3)-6)=2")
(check-expect(swap-eval g)"((5+9)+10)=24")
(check-expect(swap-eval h)"((4*3)*3)=36")
(check-expect(swap-eval i)"((5+3)-((5+3)-6))=6")
(check-expect(swap-eval k)"(-4/2)=-2")
(check-expect(swap-eval l)"((((5+3)-6)-3)-4)=-5")
(check-expect(swap-eval j)"(23/10-3)=-7/10")


