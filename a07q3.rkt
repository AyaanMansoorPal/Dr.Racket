;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname a07q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;**********************************************
;; Ayaan Mansoor Pal 
;;**********************************************
;;

;; (compare alist words) takes alist and compares the
;; items in alist and in the list words replacing every _ 
;; in words with the item in alist producing a new list
;; with every _ filled
;; compare: (listof Char) (listof Char)-> (listof Char)
;; Examples:
(check-expect(compare empty empty) empty)
(check-expect(compare (list #\a) empty) empty)
(check-expect(compare empty (list #\a)) (list #\a))

(define (compare alist words)
  (cond
    [(and (empty? words)(empty? alist)) empty]
    [(empty? words) empty]
    [(char=? (first words) #\_) (cons (first alist)
                  (compare (rest alist)(rest words)))]
    [else
    (cons (first words)(compare alist (rest words)))]))

;; Tests:
(check-expect (compare (list #\a) (list #\_)) (list #\a))
(check-expect (compare (list #\a) (list #\T #\h #\_ #\t))
                       (list #\T #\h #\a #\t))
(check-expect (compare (list #\e #\b) (list #\T #\h #\_ #\a #\p #\p #\l #\e))
                       (list #\T #\h #\e #\a #\p #\p #\l #\e))
(check-expect (compare (list #\t #\n #\s) (list #\i #\_)) (list #\i #\t))

;; (fill-in-blanks sentence aloc) consumes a list, aloc and a sentence,
;; it compares the values and outputs a string with the blanks
;; filled out
;; fill-in-blanks: Str (listof Char)-> Str
;; Examples:
(check-expect(fill-in-blanks "" empty) "")
(check-expect(fill-in-blanks "t_de" empty) "t_de")
(check-expect(fill-in-blanks "Th_s is _ _uz_l_" (list #\i #\a #\p #\z #\e))
             "This is a puzzle")

(define (fill-in-blanks sentence aloc)
  (cond
    [(empty? aloc) sentence]
    [else
     (list->string(compare aloc (string->list sentence)))]))

;; Tests:
(check-expect(fill-in-blanks "D_ n't    g_" (list #\o #\o)) "Do n't    go")
(check-expect(fill-in-blanks "_____" (list #\u #\n #\d #\e #\r))"under")
(check-expect(fill-in-blanks"    " empty) "    ")
(check-expect(fill-in-blanks"    "(list #\a #\b))"    ")
(check-expect(fill-in-blanks"_" (list #\a))"a")
(check-expect(fill-in-blanks"_" empty)"_")
(check-expect(fill-in-blanks "He ll _" (list #\o))"He ll o")
(check-expect(fill-in-blanks"D_d y_u fo_get"(list #\i #\o #\r))"Did you forget")
(check-expect(fill-in-blanks "_"(list #\a #\b)) "a")



