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
  ((John Bloom) ((Greta Blake) (Ned Bloom)) ((5 12 2023) ())))
)

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
    ((Mary Doe) (() (Alan Doe)) ((14 4 1964) ())))
)

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

;; HELPER FUNCTIONS:

; Duplicate Deletion
(define (delete-dup x)
    (if (null? x) '()
        (cons (car x) (delete-dup (filter (lambda (y) (not (equal? y (car x))))
                                          (cdr x))))))

;; A1
; This function returns all the living people in a branch
(define (parents lst)   
  (delete-dup   ; Calling helper function
   (apply append                          
           (map (lambda (person)   ; Applies lambda func to each sublist
                  (let ((parent-pair (cadr person)))   ; Accesses parents in the sublist
                    (filter (lambda (x) (not (null? x))) parent-pair)))   ; Removes empty lists if present in parents list
                lst))))




;; A2
; This function returns all living members of a branch
(define (living-members lst)
  (define (is-alive? entry)   ; Helper function to check if person is alive
    (null? (cadr (caddr entry))))   ; Checks that there is no death date

  (map car (filter is-alive? lst)))   ; Filter extracts only the living people's entry, then mapping car to each entry returns the names
  
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
 
;;
;; B1
(define (children lst)
  ())
  
;; B2
(define (oldest-living-member lst)
  ())
  
;; B3
(define (average-age-on-death lst)
  ())
  
;; B4
(define (birthday-month-same lst month)
  ())
  
;; B5
(define (sort-by-first lst)
  ())
  
;; B6
(define (change-name-to-Maria lst old-name new-name)
  ())
  
;;
;;You should include code to execute each of your functions below.
(display "Mother's side:\n")
(lst-mb Mb)
(display "Father's side:\n")
(lst-pb Pb)
(display "All members of the tree:\n")
(lst-all Mb Pb)
(display "A1:\n") 
(display "The parents in the given branch:\n")
(parents Mb)
(newline)
(display "A2:\n")
(display "The living people in the given branch:\n")
(living-members Mb)
	 
