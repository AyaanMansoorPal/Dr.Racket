;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a06q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;**********************************************
;; Ayaan Mansoor Pal 
;;**********************************************
;;


;; A Class is (list Str Sym Nat)
;; where
;; * the first item is the lecturer
;; * the second item is the class code
;; * the third item is the section number

;; Useful examples of a Class:
(define alist0 empty)
(define alist1 (list (list "Joe" 'cs115 11) (list "Emma" 'cs105 1)
                     (list "Emma" 'cs115 10) (list "Liam" 'cs135 3)))
(define alist2 (list (list "Emma" 'cs115 11) (list "Emma" 'cs144 13)
                     (list "Emma" 'cs115 12) (list "Emma" 'cs133 12)))
(define alist3 (list (list "Joe" 'cs115 12) (list "Emma" 'cs114 12)
                     (list "Liam" 'cs123 13) (list "Niboon" 'cs122 12)))
(define alist4 (list "Joe" 'cs155 11))


;; (lookup-name aloc lec) searches a Class aloc, and compares the
;; lecturers with lec, and outputs the class codes of the lecturer
;; in the order the Class list is consumed
;; lookup-name: (listof Class) Str -> (listof Sym)
;; Examples:
(check-expect (lookup-name alist1 "Joe") (list 'cs115))
(check-expect (lookup-name alist1 "Alex") empty)
(check-expect (lookup-name alist2 "Niboon") empty)

(define (lookup-name aloc lec)
  (cond
    [(empty? aloc) empty]
    [(equal? lec (first aloc)) (cons (second aloc) empty)]
    [else
     (cond
    [(equal? lec (first (first aloc))) (cons (second (first aloc)) (lookup-name (rest aloc) lec))]
    [else
     (lookup-name (rest aloc) lec)])]))

;; Tests:
(check-expect (lookup-name alist0 "") empty)
(check-expect (lookup-name alist0 "Joe") empty)
(check-expect (lookup-name alist2 "Joe") empty)
(check-expect (lookup-name alist1 "Joe") (list 'cs115))
(check-expect (lookup-name alist1 "Liam") (list 'cs135))
(check-expect (lookup-name alist2 "Emma") (list 'cs115 'cs144 'cs115 'cs133))
(check-expect (lookup-name alist4 "Joe") (list 'cs155))
(check-expect (lookup-name alist1 "Emma") (list 'cs105 'cs115))

