;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a06q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;**********************************************
;; Ayaan Mansoor Pal (20655883)
;; CS 115 Fall 2016
;; Assignment 06, Problem 1
;;**********************************************
;;

;; (string-repetition strs n) a recursive function which outputs a string
;; strs, n number of times
;; string-repetition: Str Nat-> Str
;; Examples:
(check-expect (string-repetition "cat" 0) "")
(check-expect (string-repetition "a" 1) "a")
(check-expect (string-repetition "war" 2) "warwar")
(check-expect (string-repetition "quiet" 5) "quietquietquietquietquiet")

(define (string-repetition strs n)
  (cond
    [(= n 0) ""]
    [else
       (string-append strs (string-repetition strs (sub1 n)))]))

;; Tests:
(check-expect (string-repetition "" 0) "")
(check-expect (string-repetition "" 1) "")
(check-expect (string-repetition "hello" 0) "")
(check-expect (string-repetition "hello" 1) "hello")
(check-expect (string-repetition "hello" 2) "hellohello")
(check-expect (string-repetition "All is lost" 0) "")
(check-expect (string-repetition "All is lost" 1) "All is lost")
(check-expect (string-repetition "All is lost" 2) "All is lostAll is lost")


;; (length-of-list word number) creates a non-empty list with at least
;; one string value, word or a list with the word repeated number times
;; and the length of the list to be equivalent to number
;; length-of-list: Str Nat -> (listof Str)
;; requires: n > 0
;; Examples:
(check-expect (length-of-list "hell" 1) (cons "hell" empty))
(check-expect (length-of-list "Hello" 2)(cons "HelloHello" (cons "Hello" empty)))
(check-expect (length-of-list "darkness" 3) (cons "darknessdarknessdarkness"
                              (cons "darknessdarkness" (cons "darkness" empty))))


(define (length-of-list word number)
  (cond
  [(= 1 number) (cons word empty)]
  [else
   (cons (string-repetition word number)(length-of-list word (sub1 number)))]))

;; Tests:
(check-expect (length-of-list "a" 1) (cons "a" empty))
(check-expect (length-of-list "rules" 2) (cons "rulesrules" (cons "rules" empty)))
(check-expect (length-of-list "playing games" 3) (cons "playing gamesplaying gamesplaying games"
                 (cons "playing gamesplaying games" (cons "playing games" empty))))


;; (repeat-str s) uses the two above recursive functions to output
;; a list of consisting of s which is as long as string length of s
;; and each item is according to certain condition
;; repeat-str: Str -> (listof Str)
;; Examples:
(check-expect (repeat-str "cat") (list "catcatcat" "catcat" "cat"))
(check-expect (repeat-str "a") (list "a"))
 
(define (repeat-str s)
  (cond
  [(equal? "" s) empty]
   [else
    (length-of-list s (string-length s))]))

;; Tests:
(check-expect (repeat-str "") empty)
(check-expect (repeat-str "apple") (list "appleappleappleappleapple" "appleappleappleapple"
                                                    "appleappleapple" "appleapple" "apple"))
      
