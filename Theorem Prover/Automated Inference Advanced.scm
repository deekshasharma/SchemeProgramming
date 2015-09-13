#lang racket
; Returns true if the statement is in CNF
(define (check-cnf statement)
  (if (equal? (length statement) 1) #t
      (if (equal? (car statement) 'IMPLIES) #f
          (if (equal? (car statement) 'BICONDITIONAL) #f
              (if (and (equal? (car statement) 'AND) 
                   (check-cnf-AND (cdr statement)))
                  #t
                  (if (and (equal? (car statement) 'OR)
                       (check-cnf-OR (cdr statement)))
                      #t
                      (if (and (equal? (car statement) 'NOT)
                               (check-cnf-NOT (cdr statement)))
                          #t
                  #f)))))))
                  
                          
              
; Returns true if all the operands of AND are in cnf                                                    
(define (check-cnf-AND operands)                                
  (if (equal? (length operands) 0) #t
      (if (all-literals operands) #t
          (if (check-cnf (car operands))
              (check-cnf-AND (cdr operands))
              #f))))
              
          
; Returns true if all the operands of OR are in cnf                                                          
(define (check-cnf-OR operands)  
      (if (all-literals operands) #t                     
              #f))


; Returns true if all the operands of NOT are in cnf                                                    
(define (check-cnf-NOT operands)
  (if (list? (car operands)) #f
      #t))

; Returns true if all the operands are single literals                                                    
(define (all-literals operands)
  (if (equal? (length operands) 0) #t
      (let* ((first-operand (car operands)))
        (if (list? first-operand)
            (if (and (equal? (car first-operand) 'NOT)
                     (check-cnf-NOT (cdr first-operand)))     
                (all-literals (cdr operands))
                #f)
          (all-literals (cdr operands))  ))))

;Converts a statement to cnf
(define (convert-to-cnf statement)
  (if (equal? (car statement) 'IMPLIES)
      (let* ((result1 (implication-eliminate (cdr statement))))
        (if (check-cnf result1) result1
            (convert-to-cnf result1)))
        (if (equal? (car statement) 'BICONDITIONAL)
            (let* ((result2 (bicon-elimination (cdr statement))))
              (convert-to-cnf result2))
            (if (equal? (car statement) 'NOT)
               (let* ((result3 (handle-NOT (cdr statement))))
                 (if (check-cnf result3) result3
                    (convert-to-cnf result3))) 
               (if (equal? (car statement) 'OR)
                   (let* ((result4 (handle-OR (cdr statement))))
                     (if (check-cnf result4) result4
                        (convert-to-cnf result4))) 
                   statement)))))
                                                                                                                                                                                                                                          
  

; Returns the result of implication elimination
(define (implication-eliminate operands)
  (let* ((operand1 (car operands))
         (operand2 (car (cdr operands))))
    (list 'OR (list 'NOT operand1) operand2)))


; Returns the result of biconditional elimination
(define (bicon-elimination operands)
    (let* ((operand1 (car operands))
         (operand2 (car (cdr operands))))
      (list 'AND (list 'IMPLIES operand1 operand2)
            (list 'IMPLIES operand2 operand1))))


; Returns the result of 
 (define (handle-NOT operand)
   (if (check-double-negation operand) 
       (car (cdr operand))
       (if (check-Demorgans operand)
           (if (equal? (car operand) 'AND)
               (apply-Demorgans-AND (cdr operand))
               (apply-Demorgans-OR (cdr operand)))       
             (handle-NOT (convert-to-cnf operand)))))
           
       
   

; Returns true if double-negation can be applied
(define (check-double-negation operand) 
  (if (equal? (length operand) 2)
      (if (and (equal? (car operand) 'NOT)
               (not (list? (car (cdr operand)))))
               #t
          #f)
      #f))

; Returns true if the Demorgans Law can be applied to the given operand
(define (check-Demorgans operand)
  (if (or (equal? (car operand) 'AND)
          (equal? (car operand) 'OR))
      #t
      #f))

; Returns the result after applying Demorgans-OR  
(define (apply-Demorgans-AND operands)  
      (let ((operand1 (car operands))
            (operand2 (car (cdr operands))))
        (list 'OR (list 'NOT operand1)
              (list 'NOT operand2)))
        )       

; Returns the result after applying Demorgans-OR
(define (apply-Demorgans-OR operands)  
      (let ((operand1 (car operands))
            (operand2 (car (cdr operands))))
        (list 'AND (list 'NOT operand1)
              (list 'NOT operand2))))

; Returns the result after simplyfying OR operands         
(define (handle-OR operands)
  (let* ((operand1 (car operands))
         (operand2 (car (cdr operands))))
    (if (and (check-cnf operand1) 
             (check-cnf operand2)
             (or (equal? (car operand1) 'AND) (equal? (car operand2) 'AND)))
        (distributivity-OR operands)
        (handle-OR (convert-to-cnf operands)))))
  
 ; Apply distribituvity   
(define (distributivity-OR operands)
  (let* ((operand1 (car operands))
         (operand2 (car (cdr operands))))
    (if (equal? (car operand1) 'AND) 
        (list 'AND (list 'OR operand2 (car (cdr operand1)))
              (list 'OR operand2 (car (cdr (cdr operand1)))))
        (list 'AND (list 'OR operand1 (car (cdr operand2)))
              (list 'OR operand2 (car (cdr (cdr operand2))))))))
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Resolution Algorithm 



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
 






    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Test for check-cnf
;(check-cnf '(IMPLIES hasDog hasMess))
;(check-cnf '(OR (NOT (OR (NOT P) Q)) Q))
;(check-cnf '(OR (OR (IMPLIES D C) A) B))
;(check-cnf '(AND (OR a b) (OR a c)))
;(check-cnf '(NOT a))
;(check-cnf '(OR (AND A B) (AND A C)))
;(check-cnf '(OR (NOT A) (NOT C)))


;Test implication elimination
;(implication-eliminate '((OR p q) s))
;(implication-eliminate '((NOT (OR Q T)) P))

; Test bicon-elimination
;(bicon-elimination '((OR A C) (AND B D)))

;(check-double-negation '(NOT (NOT a)))
;(check-Demorgans '(AND (NOT a) b))
;(check-Demorgans '(OR (NOT a) (NOT b)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
