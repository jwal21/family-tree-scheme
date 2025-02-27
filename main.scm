#lang racket
(require lang/plt-pretty-big-text)

;;Your full name: 
;;Student ID: 
;;Date of birth (day/month/year): 

;;Data format: Name, Mother, Father, Date of birth, Date of death.
;;An empty list means Unknown.

;;Maternal branch
(define Mb
'(((Mary Blake) ((Ana Ali) (Theo Blake)) ((17 9 2022) ()))
((Ana Ali) ((Ada West) (Md Ali)) ((4 10 1995) ()))
((Theo Blake) ((Mary Jones) (Tom Blake)) ((9 5 1997) ()))
((Greta Blake) ((Mary Jones) (Tom Blake)) ((16 3 1999) ()))
((Mary Jones) (() ())((12 5 1967) (19 5 2024)))
((Tom Blake) (() ()) ((17 1 1964) ()))
((Ada West) (() ()) ((22 8 1973) ()))
((Md Ali) (() ()) ((14 2 1972) (2 5 2023)))
((Ned Bloom) (() ()) ((23 04 2001)()))
((John Bloom) ((Greta Blake) (Ned Bloom)) ((5 12 2023) ()))))

;,Paternal branch
(define Pb
'(((John Smith) ((Jane Doe) (Fred Smith)) ((1 12 1956) (3 3 2021))) 
((Ana Smith) ((Jane Doe) (Fred Smith)) ((6 10 1958) ()))
((Jane Doe) ((Eve Talis) (John Doe)) ((2 6 1930) (4 12 1992)))
((Fred Smith) ((Lisa Brown) (Tom Smith)) ((17 2 1928) (13 9 2016)))
((Eve Talis) (() ()) ((15 5 1900) (19 7 1978)))
((John Doe) (() ()) ((18 2 1899)(7 7 1970)))
((Lisa Brown) (() ())((31 6 1904) (6 3 1980)))
((Tom Smith) (() ()) ((2 8 1897) (26 11 1987)))
((Alan Doe) ((Eve Talis) (John Doe)) ((8 9 1932) (23 12 2000)))
((Mary Doe) (() (Alan Doe)) ((14 4 1964) ()))))

;;define lst-mb
;;define lst-pb
;;define lst-all

;; C1
; Print the first and last name of for each person (maternal branch)
(define (lst-mb mb)
  (if (null? mb)
    ()
    (cons (caar mb) (lst-mb (cdr mb)))))
 
;; C2
; Print the first and last name of for each person (paternal branch)
(define (lst-pb pb)
  (if (null? pb)
  ()
  (cons (caar pb) (lst-pb (cdr pb)))))
  
;; C3
; Print the names for the both maternal and paternal branches
(define (append-lst list1 list2)
  (if (null? list1)
    list2
    (cons (lst-mb list1) (lst-pb list2)))
  )
			
(define (lst-all mb pb)
  (append-lst mb pb))

;; A1
(define (parents lst)
  ())
  
;; A2
(define (living-members lst)
  ())
  
;; A3
(define (current-age lst)
  ())
  
;; A4
(define (same-birthday-month lst month)
  ())
  
;; A5
(define (sort-by-last lst)
  ())
  
;; A6
(define (change-name-to-Juan lst old-name new-name)
  ())
 


;; B1
(define (children lst)
  ()
)
  
;; B2
(define (oldest-living-member lst)
 () 
)
  
;; B3
(define (average-age-on-death lst)
  ())
  
;; B4 - Find all people in the list with their birthday on X month
(define (birthday-month-same lst month)
  ; Use filter to remove all people who do not match the provided month
  (filter (lambda (person)
            ; Check birthday is provided month
            (if (not (null? (caaddr person)))
                (equal? (cadr (caaddr person)) month)
                #f))
          lst))
  
;; B5 - Return a list of all the people sorted by their first name alphabetically
(define (sort-by-first lst)
  (define (first-name person)
    ; Convert the symbol to a string (comparison only works on strings)
    (symbol->string (car (car person))))

  ; Sort by which comes first alphabetically
  (sort lst (lambda (person1 person2)
              (string<? (first-name person1) (first-name person2)))))
  
;; B6 - Update the first name of all people called X to Y
(define (change-name-to-Maria lst old-name new-name)
; Apply the name substitution to each item in the list
(map (lambda (item)
       ; If the name is X, create a new person in their place with the name Y
       ; Else returns unmodified person
       (if (equal? (car (car item)) old-name)
           (list (list new-name (cdr (car item))) (cadr item) (caddr item))
           item))
     lst))

;;You should include code to execute each of your functions below.
(define ESC #\033)
(define CSI (list->string (list ESC #\[ )))
(define CLEAR (string-append CSI "2J"))
(display CLEAR)

; C1
(display "Mother's side:\n")
(lst-mb Mb)

; C2
(display "Father's side:\n")
(lst-pb Pb)

; C3
(display "Combined mother's and father's:\n")
(lst-all Mb Pb)

; B1
(display "Children:\n")
(children Mb)

; B2
(display "Oldest living member:\n")
(oldest-living-member Mb)

; B3
(display "Average age of death:\n")

; B4
(display "People born on X month:\n")
(birthday-month-same Mb 5)

; B5
(display "Sorted by first name:\n")
(sort-by-first Mb)

; B6
(display "Changed name to Maria from Mary:\n")
(change-name-to-Maria Mb 'Mary 'Maria)