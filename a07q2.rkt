;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname a07q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;**********************************************
;; Ayaan Mansoor Pal 
;;**********************************************
;;

;; (check-list f-list s-list) compares the items in f-list with
;; the items in s-list and outputs the numbers of items which
;; are the similar
;; check-list: (listof Sym) (listof Sym)-> Nat
;; requires: f-list s-list to be the same length
;; Examples:
(check-expect (check-list empty empty) 0)
(check-expect (check-list (list 'A) empty) 0)
(check-expect (check-list empty (list 'A)) 0)

(define (check-list f-list s-list)
  (cond
    [(or (empty? f-list) (empty? s-list)) 0]
    [(symbol=?(first f-list)(first s-list))(+ 1(check-list(rest f-list)(rest s-list)))]
    [else
     (check-list (rest f-list) (rest s-list))]))

;; Tests:
(check-expect (check-list (list 'A) (list 'A)) 1)
(check-expect (check-list (list 'A) (list 'B)) 0)
(check-expect (check-list (list 'A 'A) (list 'A 'A)) 2)
(check-expect (check-list (list 'A 'B 'C) (list 'A 'C 'B)) 1)
(check-expect (check-list (list 'A 'B 'C) (list 'A 'B 'C)) 3)
(check-expect (check-list (list 'A 'A 'C 'D 'B 'D) (list 'A 'C 'C 'D 'B 'D)) 5)


;; (pass-mark-compare correct-ans student-ans passing-marks) takes the lists correct-ans
;; student-ans and compares them returns true if the number of similar items
;; surpasses or is equal to passing-marks otherwise false
;; pass-mark-compare (listof Sym) (listof Sym) Nat-> Bool
;; requires: correct-ans and student-ans to be the same length
;; Examples:
(check-expect (pass-mark-compare empty empty 0) true)
(check-expect (pass-mark-compare empty (list 'A) 0) true)
(check-expect (pass-mark-compare (list 'A) empty 0) true)


(define (pass-mark-compare correct-ans student-ans passing-marks)
  (cond
    [(>= (check-list correct-ans student-ans) passing-marks) true]
    [else
     false]))

;; Tests:
(check-expect (pass-mark-compare (list 'A) (list 'A) 0) true)
(check-expect (pass-mark-compare (list 'A) (list 'B) 0) true)
(check-expect (pass-mark-compare (list 'A 'B) (list 'C 'D) 1) false)
(check-expect (pass-mark-compare (list 'A 'B) (list 'A 'B) 2) true)
(check-expect (pass-mark-compare (list 'A 'B 'C) (list 'A 'C 'B) 2) false)


;; (student-pass? corr-ans stud-ans pass-mark) uses functions above
;; and outputs whether the student passed the exam or not
;; student-pass?: (listof Sym) (listof Sym) Nat-> Bool
;; requires: non-empty lists
;;           corr-ans and stud-ans to be the same length
;;           pass-mark to not exceed length of the corr-ans and stud-ans
;; Examples:
(check-expect(student-pass? (list 'A 'A 'C 'D 'B 'D) (list 'A 'C 'C 'D 'B 'D) 4) true)
(check-expect(student-pass? (list 'C 'B 'A 'B) (list 'D 'B 'A 'B) 3) true)
(check-expect(student-pass? (list 'D 'A 'D 'D 'C) (list 'D 'B 'C 'B 'A) 3) false)


(define (student-pass? corr-ans stud-ans pass-mark)
  (cond
    [(= 0 pass-mark) true]
    [else
     (pass-mark-compare corr-ans stud-ans pass-mark)]))

;; Tests:
(check-expect (student-pass? (list 'A 'B) (list 'B 'A) 0) true)
(check-expect (student-pass? (list 'A 'B) (list 'A 'B) 0) true)
(check-expect (student-pass? (list 'D 'C 'D) (list 'A 'D 'D) 1) true)