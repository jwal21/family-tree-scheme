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

; Paternal branch
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

;; C1
; Returns the first and last names of each person in the maternal branch
(define (lst-mb mb)
  (if (null? mb)
    ()
    (cons (caar mb) (lst-mb (cdr mb)))))
 
;; C2
; Returns the first and last name of for each person in the paternal branch
(define (lst-pb pb)
  (if (null? pb)
  ()
  (cons (caar pb) (lst-pb (cdr pb)))))
  
;; C3
; Return the names for both the maternal and paternal branches
(define (append-lst lst1 lst2)
  (if (null? lst1)
    lst2
    (cons (lst-mb lst1) (lst-pb lst2))))
			
; Returns the list's combined
(define (lst-all mb pb)
  (append-lst mb pb))

;; HELPER FUNCTIONS:

; Duplicate Deletion
(define (delete-dup x)
    (if (null? x) '()
        (cons (car x) (delete-dup (filter (lambda (y) (not (equal? y (car x))))
                                          (cdr x))))))

; Function to check if person is alive
(define (is-alive? profile)   
  (null? (cadr (caddr profile))))   ; Checks that there is no death date

;; A1
; This function returns all the parents in a branch
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
  ; Filter extracts only the living people's profile, then mapping car to each profile returns the list of names
  (map car (filter is-alive? lst)))   

;; A3
; This function returns the age of all living members in a branch
(define (current-age lst)
  (define (calc-age profile)
    (- 2025 (caddr (car (caddr profile)))))  ; Calculates age by subtracting current year by Year of Birth	

  (map calc-age (filter is-alive? lst)))  ; Performs the age calculation on the filtered list of living people
  
;; A4
; This function returns all people with the same given birth month 
(define (same-birthday-month lst month)
  (map car (filter (lambda (profile)  ; Takes the names of the desired profiles  
            (equal? month (cadr (car (caddr profile))))) ;Checks if profile has matching birth month
	lst)))

  
;; A5
; This function returns all people alphabetically sorted by their last name
(define (sort-by-last lst)
  ; Converts Last name to a string to allow for comparison
  (define (name-to-string profile)
    (symbol->string (cadar profile)))   

  ; Uses sort and helper function to sort the names in ascending order
  (map car (sort lst (lambda (name1 name2)
            (string<? (name-to-string name1) (name-to-string name2))))))

;; A6
; This function returns the given branch, switching the name 'John' with 'Juan' if it appears
(define (change-name-to-Juan lst old-name new-name)
  ; Helper function that either replaces the given name, or preserves the profile
  (define (replace-name name)
    (if (and (not (null? name ))(equal? (car name) old-name))  ; If the list is filled and the first name is the given old-name 
        (cons new-name (cdr name))  ; Concatenate the new first name and the second name
        name))  ; Else, leave unchanged

  ; Function that calls the helper function on a child profile
  (define (update-profile profile)
    (list (replace-name (car profile))   ; Call replace-name on child list
          (map replace-name (cadr profile))  ; Call replace-name on parents list
            (caddr profile)))  ; Add the dates back in

  ; Maps the update function to each profile in the branch
  (map update-profile lst)) 
 

; B1 - Get all children (non-orphaned people from the list)
(define (children people-list)
  (filter (λ (person)
    ; Retrieve the parents from the list
    (let ((parent-list (cadr person)))
      ; Check at least one parent is given
      (or (not (null? (car parent-list ))) (not (null? (cadr parent-list )))))) people-list))

;; B2 - Get the oldest still living family member from the list
(define (oldest-living-member lst)
  ; Filter out all deceased people
  (define (only-living-people lst) (filter (λ (person) (null? (list-ref (list-ref person 2) 1))) lst))

  (define (sort-list-by-DOB person-list)
  (sort person-list
        (λ (person-1 person-2)
          ; Day, month, year for for the two people to compare
          (let ((day-1 (list-ref (list-ref (list-ref person-1 2) 0) 0))
                (month-1 (list-ref (list-ref (list-ref person-1 2) 0) 1))
                (year-1 (list-ref (list-ref (list-ref person-1 2) 0) 2))
                (day-2 (list-ref (list-ref (list-ref person-2 2) 0) 0))
                (month-2 (list-ref (list-ref (list-ref person-2 2) 0) 1))
                (year-2 (list-ref (list-ref (list-ref person-2 2) 0) 2)))
            ; Check which year is first, month and then day
                (cond
                  ((< year-1 year-2) #t)
                  ((> year-1 year-2) #f)
                  ((< month-1 month-2) #t)
                  ((> month-1 month-2) #f)
                  ((< day-1 day-2) #t)
                  ((> day-1 day-2) #f))))))

  ; Return the first element in the list
  (car (sort-list-by-DOB (only-living-people lst)))
)
  
;; B3 - Calculate the average age at which someone dies (calculated from deceased only)
(define (average-age-on-death lst)
; Filter out non-deceased people
(define (only-deceased-people lst)
  (filter (λ (person) (not (null? (list-ref (list-ref person 2) 1)))) lst))

  ; Calculate average age for all deceased people
  (let ((deceased (only-deceased-people lst)))
         ; Adds everyone's age together
        (let ((combined-age
                (apply + (map (λ (person)
                                (- (list-ref (list-ref (list-ref person 2) 1) 2) ; Death year
                                  (list-ref (list-ref (list-ref person 2) 0) 2))) ; Birth year
                            deceased))))
          ; Divide by number of people
          (/ combined-age (length deceased)))))
  
;; B4 - Find all people in the list with their birthday on X month
(define (birthday-month-same lst month)
  ; Use filter to remove all people who do not match the provided month
  (filter (λ (person)
            ; Check the month against the month to filter for
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
  (sort lst (λ (person1 person2)
              (string<? (first-name person1) (first-name person2)))))
  
;; B6 - Update the first name of all people called X to Y
(define (change-name-to-Maria lst old-name new-name)
  ; Iterate over all the family members in the list
  (map (λ (entry)
         ; Get all elements of each entry
         (let ((person-name (car entry)) ; Name of the person
               (parent-names (cadr entry)) ; Parents names
               (dates (caddr entry))) ; DOB and DOD
            ; Check if the name of the person is the old-name, replace with new-name
           (list (if (and (not (null? person-name)) (equal? (car person-name) old-name))
                     (cons new-name (cdr person-name))
                     person-name)
                  ; Do the same for the parents
                 (map (λ (names-of-parents)
                        (if (and (not (null? names-of-parents)) (equal? (car names-of-parents) old-name))
                            (cons new-name (cdr names-of-parents))
                            names-of-parents))
                      parent-names)
                 dates)))
       lst))

;; Display
; C1
(display "C1:\n")
(display "Mother's side:\n")
(lst-mb Mb)
(newline)

; C2
(display "C2:\n")
(display "Father's side:\n")
(lst-pb Pb)
(newline)

; C3
(display "C3:\n")
(display "Combined mother's and father's:\n")
(lst-all Mb Pb)
(newline)

; Section A
(display "Section A using Mb for testing:\n")

; A1
(display "A1:\n") 
(display "Parents:\n")
(parents Mb)
(newline)

; A2
(display "A2:\n")
(display "Living members:\n")
(living-members Mb)
(newline)

; A3
(display "A3:\n")
(display "Current age of living family members:\n")
(current-age Mb)
(newline)

; A4
(display "A4:\n")
(display "People born on X month:\n")
(same-birthday-month Mb 5)
(newline)

; A5
(display "A5:\n")
(display "Sorted by last name:\n")
(sort-by-last Mb)
(newline)

; A6
(display "A6:\n")
(display "Changed name to Juan from John:\n")
(change-name-to-Juan Mb 'John 'Juan)
(newline)

; Section B
(display "Section B using Pb for testing:\n")

; B1
(display "B1:\n")
(display "Children:\n")
(map car (children Pb))
(newline)

; B2
(display "B2:\n")
(display "Oldest living member:\n")
(car (oldest-living-member Pb))
(newline)

; B3
(display "B3:\n")
(display "Average age of death:\n")
(average-age-on-death Pb)
(newline)

; B4
(display "B4:\n")
(display "People born on the fifth month:\n")
(map car (birthday-month-same Pb 5))
(newline)

; B5
(display "B5:\n")
(display "Sorted by first name:\n")
(map car (sort-by-first Pb))
(newline)

; B6
(display "B6:\n")
(display "Changed name to Mary from Maria:\n")
(change-name-to-Maria Pb 'Mary 'Maria) ; Do not use (map car) since testing requires making sure parent names are updated too
(newline)
