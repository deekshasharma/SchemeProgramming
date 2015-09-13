#lang racket

; Main function to run
(define (search-ga my-eval-fn)
  (extract-member (my-search my-eval-fn 
                  (generate-population 10 '())
                  50000 '()) 
                  '()))
    
; Returns the best individual solution along with its fitness value.
; This function keeps track of the best solution so far for each generation.
(define (my-search fitness-fn population fitness-calls global-max)
  (if (= fitness-calls 10) (car global-max)
      (let* ((evaluated (evaluate-population population fitness-fn '()))
             (best-fitness-now (max-fitness evaluated))
             (best-sol-now (find-member best-fitness-now evaluated)))
                
        ;(display best-fitness-now)(newline) // Display the best fitness for each generation
        ;(display (length evaluated))(newline)
      
        (if (= (length global-max) 0) 
            (my-search fitness-fn 
                   (mutate-population (mate-population (choose-individual evaluated 3 '()) '()) '())
                   (- fitness-calls 10)
                   (cons best-sol-now global-max))
            
            (if (<= (get-nth 7 (car global-max)) (get-nth 7 best-sol-now))
                (my-search fitness-fn 
                   (mutate-population (mate-population (choose-individual evaluated 3 '()) '()) '())
                   (- fitness-calls 10)
                   (cons best-sol-now (cdr global-max)))
                
                (my-search fitness-fn 
                   (mutate-population (mate-population (choose-individual evaluated 3 '()) '()) '())
                   (- fitness-calls 10)
                   global-max))))))


; Removes the fitness value and return the best individual- member
(define (extract-member best-individual output)
  (if (= (length output) 6) output
      (extract-member (cdr best-individual)
                      (append output (list (car best-individual))))))
      
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Stats on Max fitness of the generation
(define (max-fitness generation)
  (let ((fitnesses (get-fitness generation '())))
    (get-nth 1 (sort fitnesses >))
    )
  )

; Return the average fitness of the generation
(define (avg-fitness generation)
  (let ((sum (sum-fitness (get-fitness generation '()) 0))
        (num (length generation)))
    (/ sum num)))
    
  

; Return the sum given a list of all fitness values
(define (sum-fitness list1 result)
(if (= (length list1) 0)
result
(sum-fitness (cdr list1) (+ result (car list1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Population generation


;Random number generator
(define (random-num-gen)
    (+ 1 (random 20)))


;Generate the population as a list of all members.Limit is the size of population
(define (generate-population limit output)
  (if (= limit 0)
      output
      (generate-population (- limit 1) 
                           (cons (generate-member '()) output))))
   
  
;Generate and return each member of the population
(define (generate-member member-list)
  (if (= (length member-list) 6)
      member-list
      (generate-member (cons (random-num-gen) member-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Fitness function and Evaluation

;Return nth element from the list
(define (get-nth index alist)
(cond ((= 1 index) (car alist))
(#t (get-nth (- index 1) (cdr alist)))))

;fitness function
(define (my-eval-fn alist)
(let ((a (get-nth 1 alist))
(b (get-nth 2 alist))
(c (get-nth 3 alist))
(d (get-nth 4 alist))
(e (get-nth 5 alist))
(f (get-nth 6 alist)))
(+ (* 6 a b)
   (* 4 c d d)
   (* -3 a a)
   (* 46 e f)
   (* -5 f c c))))



; Return the population along with their fitness values. This is a list of lists
(define (evaluate-population population my-eval-fn output)
  (if (= (length population) 0) output
          (evaluate-population (cdr population)
                               my-eval-fn
                               (cons (assign-fitness my-eval-fn (car population)) output))))      

;Return a member with a fitness value appended to the end
(define (assign-fitness my-eval-fn member)
  (append member (list (my-eval-fn member))) )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Tournament Selection

; Returns the population ready for mating
(define (choose-individual population k output)
  (if (= (length output) (length population)) output
      (choose-individual population k 
                         (cons (get-best-member population k) output))))
      
   
; Return the best member in the tournament
(define (get-best-member population k)
  (let ((tournament (select-tournament k population '())))
        (find-member (get-nth 1 (sort (get-fitness tournament '()) >))
                     tournament)))
      

; Return a list of k-sized tournament
(define (select-tournament k population output)
  (if (= k 0) output
      (select-tournament (- k 1) 
                         population
                         (cons (get-nth (+ 1 (random (length population))) population) output) )))
   
                           
         
; Return the list of fitness values for all members of population
(define (get-fitness tournament output)
  (if (= (length tournament) 0) output
         (get-fitness (cdr tournament)
                      (append (list (get-nth 7 (car tournament))) output))))


; Find member from the population given the fitness value
(define (find-member fitness-val tournament)
 (if (= (length tournament) 0) (display "population empty")
      (if (= (get-nth 7 (car tournament)) fitness-val) (car tournament)                 
           (find-member fitness-val (cdr tournament)))))

        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; Mating

; Return the list of offsprings after mating. There are no fitness values yet
(define (mate-population selected-pop output)
  (if (= (length selected-pop) 0) output
      (let ((member1 (car selected-pop)) (member2 (car (cdr selected-pop))))
        (mate-population (cdr (cdr selected-pop))
                         (append output (mate-individual member1 member2)))
       )
      )
  )


; Return a list of 2 new offsprings produced from member1 and member2
(define (mate-individual member1 member2)
  (let ((crossover (random-crossover)))
    (list (append (get-firstn crossover member1 '()) 
                  (get-lastm crossover member2 '()))
          (append (get-firstn crossover member2 '())
                  (get-lastm crossover member1 '())))))
        

;Generate a random crossover point from 1 to 5
(define (random-crossover)
    (+ 1 (random 5)))

; Return the first n values of member
(define (get-firstn crossover member output)
  (if (= crossover 0) output
      (get-firstn (- crossover 1) (cdr member) (append output (list (car member))))))

; Return the last 6-n values of a member
(define (get-lastm crossover member output)
  (if (= crossover 6) output
      (get-lastm (+ crossover 1) member (append output (list (get-nth (+ crossover 1) member))))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Mutation

; Return the offsprings after mutation. This does not have fitness values
(define (mutate-population population output)
  (if (= (length population) 0) output
      (mutate-population (cdr population) 
                         (append output (list (mutate (+ 1 (random 6))
                                                      (+ 1 (random 20))
                                                      (car population)
                                                      '() ))))))
        

; Return a mutated member given a random index and random value      
(define (mutate index value member output)
  (if (= index 1)
      (append output (cons value (cdr member)))
      (mutate (- index 1)
              value
              (cdr member)
              (append output (list (car member))))))

(search-ga my-eval-fn)
