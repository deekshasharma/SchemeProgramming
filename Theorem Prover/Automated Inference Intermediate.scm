#lang racket

(define KB '())


; Tell function
(define (tell statement)
  (set! KB (append KB (list statement)))
  'OK
  )

; Ask function
(define (ask statement)
  (let* ((copy-of-KB KB)
          (updated-KB (add-negation statement copy-of-KB)))
    (display updated-KB)
    (resolution-algo updated-KB)))
  
; Add negation of the statement to KB  
(define (add-negation statement copy-of-KB)
  (if (equal? (length statement) 1)
      (append copy-of-KB (list (list (cons 'NOT statement))))
      (append copy-of-KB (list (cdr statement)))
      ))



; Returns true for Contradiction and Unkown otherwise
(define (resolution-algo updated-KB)  
  (let*  ((resolvents (clean-resolvents (resolve-all-pairs updated-KB '()))))
          (if (contains resolvents "CONTRADICTION") #t
             (if(subset resolvents updated-KB) "UNKNOWN"
                 (run-resolution-again (get-new-resolvents resolvents updated-KB '()) updated-KB)))))             


; Update KB and calls resolution-algo if neither Contradiction nor Unknown
 (define (run-resolution-again new-resolvents updated-KB)
  (resolution-algo (append updated-KB new-resolvents)))     
  
                


; Returns a list of resolvents that are new to KB
(define (get-new-resolvents clean-resolvents updated-KB output)
  (if (equal? (length clean-resolvents) 0) output
      (if (contains updated-KB (car clean-resolvents))
          (get-new-resolvents (cdr clean-resolvents) updated-KB output)
          (get-new-resolvents (cdr clean-resolvents) updated-KB (append output (list (car clean-resolvents)))))))


; Return true if the clean-resolvents are the subset of KB
(define (subset clean-resolvents updated-KB)
  (if (equal? (length clean-resolvents) 0) #t
      (if (contains updated-KB (car clean-resolvents))
          (subset (cdr clean-resolvents) updated-KB)
          #f)))

; Return a clean list of all resolvents containing resolved clauses and CONTRADICTION if any
(define (clean-resolvents all-resolvents)
  (remove-false (remove-duplicates all-resolvents '()) '()))
  
; Return a list of all resolvents after removing #f symbol for not unresolved clauses
(define (remove-false all-resolvents output)
  (if (equal? (length all-resolvents) 0) output
      (if (equal? (car all-resolvents) '#f)
          (remove-false (cdr all-resolvents) output)
          (remove-false (cdr all-resolvents) (cons (car all-resolvents) output)))))

; Returns a list of resolvents for all pairs in KB
(define (resolve-all-pairs updated-KB output)
  (if (equal? (length updated-KB) 0) output
      (let* ((resolvent-list (resolve-clause (car updated-KB) (cdr updated-KB) '()) ))
        (if (equal? (length resolvent-list) 0)
            (resolve-all-pairs (cdr updated-KB) output)
            (resolve-all-pairs (cdr updated-KB) (append output resolvent-list))))))
                            

; Return the list of resolvents of given clause with KB
            
(define (resolve-clause  clause1 updated-KB output)
  (if (equal? (length updated-KB) 0) output
      (let* ((clause2 (car updated-KB)))                
            (let* ((resolvent (resolve clause1 clause2)))              
              (resolve-clause clause1 (cdr updated-KB) (append output (list resolvent))))         
        )))              
      
                                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Logic for resolve

; Main function run
(define (resolve statementA statementB)
 (let* ((statement1 (remove-duplicates statementA '())) (statement2 (remove-duplicates statementB '()))) 
  (if (or (check-complimentary-statement statement1) (check-complimentary-statement statement2)) #f
      (if (check-contradiction statement1 statement2) "CONTRADICTION"
          (if (> (count-complimentary statement1 statement2 0) 1) #f
              (if (equal? (count-complimentary statement1 statement2 0) 0) #f              
                  (remove-duplicates (get-result statement1 statement2) '())))))))      


; Returns true if the statement contain the complimentary pair of literals 
(define (check-complimentary-statement statement)
  (if (equal? (length statement) 0) #f
      (let* ((literal (car statement)))
        (if (complimentary-exist (cdr statement) literal) #t
            (check-complimentary-statement (cdr statement))))))
    

 ;Returns true if contradiction is found and false otherwise
(define (check-contradiction statement1 statement2)
  (if (and (equal? (length statement1) 1) (equal? (length statement2) 1))
      (if (complimentary-exist statement2 (car statement1))
      #t
      #f)
  #f))                  
 

; Returns true if complimentary for the given literal exist in statement
(define (complimentary-exist statement literal)
      (if (contains statement (get-complimentary-literal literal))
          #t
          #f))          


; Returns the complimentary of the given literal
(define (get-complimentary-literal literal)
  (if (list? literal) (get-literal literal)
      (list 'NOT literal)))


          
; Returns the count of the complimentary literals between 2 given statements      
 (define (count-complimentary statement1 statement2 count)
    (if (equal? (length statement1) 0) count
        (if (complimentary-exist statement2 (car statement1)) 
            (count-complimentary (cdr statement1) statement2 (+ count 1))
            (count-complimentary (cdr statement1) statement2 count))))
                                  
  

; Returns Contradiction or the resulting clause after resolving 2 statements           
(define (get-result statement1 statement2)
  (let* ((result (my-resolve1 statement1 statement2 '())))
              (if (equal? (length result) 0) "Contradiction"
                  result)))

; Returns the result with all unique literals
(define (remove-duplicates result output)
  (if (equal? (length result) 0) output
      (if (contains output (car result))
          (remove-duplicates (cdr result) output)
          (remove-duplicates (cdr result) (append output (list (car result)))))))


  
 ; Resolve helper function            
(define (my-resolve1 statement1 statement2 result)
  (if (equal? (length statement1) 0) (append result statement2)
      (if (equal? (length statement2) 0) (append result statement1)
          (let* ((current (car statement1)))                                
            (if (complimentary-exist statement2 current)
                        (my-resolve1 (cdr statement1)
                                    (update-statement statement2 (get-complimentary-literal current) '())
                                    result)
                        (my-resolve1 (cdr statement1)
                                    statement2
                                    (append result (list current)))) ))))                                                                          
            


; Returns the updated statement after removing the given element
(define (update-statement statement element result)
  (if (equal? (length statement) 0) result
      (if (equal? (car statement) element) (append result (cdr statement))                              
              (update-statement (cdr statement) 
                                element
                                (append result (list (car statement)))))))                           




; Returns True if the statement contains the given element
(define (contains statement element)
  (if (equal? (length statement) 0) #f
     (if (equal? (car statement) element) #t
                  (contains (cdr statement) element))))


; Return the literal at position 2 from the negation element
(define (get-literal element)
  (car (cdr element)))
 



