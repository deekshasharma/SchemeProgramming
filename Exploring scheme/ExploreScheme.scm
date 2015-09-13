#lang racket


(define (swap-adjacent inlist)
  (swap inlist '())
 )

(define (swap inlist outlist)
 (
     if (
          equal? (length inlist) 0
        )
      outlist
     (
          if(equal? (cdr inlist) '()) 
               (append outlist inlist)
               (
                let (
                     (x (car inlist)) (y (car (cdr inlist))) 
                    )
                     
                     (swap (cdr (cdr inlist)) (append outlist (list y x)))                   
               )     
     )
  )
)







(define (rotate-left position input)
  (if (equal? (length input) 0)
      input
      
  (
   if (<= position 0)
      input
      (rotate-left (- position 1) (append (cdr input) (list (car input))))
  ))
  )







(define (palindrome? input)
  (if (equal? (length input) 0)
      #t
      (compare input (reverse-list input '()))
   )
  )


(define (compare input reversed)
  (if (equal? (length input) 0)
      #t
      (if (equal? (car input) (car reversed))
          (compare (cdr input) (cdr reversed))
          #f
       )
   )
  )

(define (reverse-list input output)
(if (equal? (length input) 0)
output
(reverse-list (cdr input) (cons (car input) output))
)
)

