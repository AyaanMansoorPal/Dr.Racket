;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a09q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;**********************************************
;; Ayaan Mansoor Pal 
;;**********************************************
;;

;; (substitute oldst newst slst) consumes two strings
;; oldst and newst,and outputs a list, slst with all
;; the occurrences of oldst replaced with newst
;; substitute: Str Str (listof Str)-> (listof Str)
;; Examples:
(check-expect (substitute "Hell" "o" (list "Hell")) (list "o"))
(check-expect (substitute "count" "down" (list "heads" "count"))
              (list "heads" "down"))
(check-expect (substitute "two" "three" (list "one" "three" "three"))
              (list "one" "three" "three"))

(define (substitute oldst newst slst)
  (build-list (length slst) (lambda (x)
            (cond
            [(equal? (list-ref slst x) oldst) newst]
            [else
            (list-ref slst x)]))))

;; Tests:
(check-expect (substitute "hey" "yo" empty) empty)
(check-expect (substitute "" "alkali" (list "alkali" "alkali"))
              (list "alkali" "alkali"))
(check-expect (substitute "" "" (list "")) (list ""))
(check-expect (substitute "" "hey" (list "" ""))
              (list "hey" "hey"))
(check-expect (substitute "" "" (list "Bogdon" "bogdon"))
              (list "Bogdon" "bogdon"))

(check-expect (substitute "old" "new" (list "this" "old" "new" "wow" "old" "cold"))
              (list "this" "new" "new" "wow" "new" "cold"))
(check-expect(substitute "hold" "outta" (list "hold" "hold" "hold"))
             (list "outta" "outta" "outta"))
(check-expect (substitute "you" "sicken me" (list "you")) (list "sicken me"))
(check-expect (substitute "point" "topoint" (list "point" "point"  "point")) (list "topoint" "topoint" "topoint"))
(check-expect (substitute "is" "not" (list "time""is""time""to""is"))(list "time""not""time""to""not"))
(check-expect (substitute "hello" "you" empty) empty)
                               
