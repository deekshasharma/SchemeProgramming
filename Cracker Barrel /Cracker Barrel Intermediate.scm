


(define h (make-hash-table 12))
(hashq-create-handle! h 1 '((1 3 6)))
(hashq-create-handle! h 2 '((2 4 7)))
(hashq-create-handle! h 3 '((3 6 9)))
(hashq-create-handle! h 4 '((4 7 10)))
(hashq-create-handle! h 5 '((5 6 7)))

(hashq-create-handle! h 6 '((6 9 11) (6 7 8) (6 3 1)))
(hashq-create-handle! h 7 '((7 10 12) (7 6 5) (7 4 2)))
(hashq-create-handle! h 8 '((8 7 6)))
(hashq-create-handle! h 9 '((9 6 3)))
(hashq-create-handle! h 10 '((10 7 4)))

(hashq-create-handle! h 11 '((11 9 6)))
(hashq-create-handle! h 12 '((12 10 7)))



; Main method to run
(define (dfs initial-state goal-state)
  (if (equal? (search initial-state 
                      goal-state 
                      (all-valid-moves initial-state)
                      '())
       '()) "False"
            (reverse (search initial-state 
                 goal-state 
                 (all-valid-moves initial-state)
                 '()))))                        

      
;Returns the moves if a goal-state exists starting from a given initial state
(define (search state-now goal-state moves output-moves)
  (if (> (length goal-state) (length state-now)) '()                      ; if goal-state is bigger than state-now
     (if (null? state-now) output-moves                                   ; if state-now is empty
          (if (equal? (if-same state-now goal-state) #t) output-moves     ; if state-now is the goal state
              (if (null? moves) '()                                       ; if no moves left
                  (if (equal? (search (apply-move (car moves) state-now) 
                           goal-state
                           (all-valid-moves (apply-move (car moves) state-now))
                           (cons (car moves) output-moves))
                      '() )
                  (search state-now goal-state (cdr moves) output-moves)
                  (search (apply-move (car moves) state-now) 
                           goal-state
                           (all-valid-moves (apply-move (car moves) state-now))
                           (cons (car moves) output-moves))))))))
               

  


; Check if the goal-state and current-state are same even if they are unsorted.
(define (if-same state-now goal-state)
  (equal? (sort state-now <) (sort goal-state <))) 
        


; This method returns a new state after applying given a move and current-state
(define (apply-move move current-state)
       (if (null? current-state) current-state
        (let ((start (car move)) (mid (car (cdr move))) (end (car (cdr (cdr move)))))
            (cons end (remove mid (remove start current-state))))))

;This method removes element x from current-state as part of the move
(define (remove x current-state)
  (if (null? current-state) current-state
      (if (equal? x (car current-state))
          (cdr current-state)
          (cons (car current-state) (remove x (cdr current-state))))))

;A wrapper procedure to take just the current-state and return list of valid moves
(define (all-valid-moves current-state)
        (filter-valid-move (get-all-moves current-state '()) current-state '()))



;Only return valid moves possible from the current-state
(define (filter-valid-move moves-list current-state output-list)
     
          (if (null? moves-list) output-list
             (if (is-valid (car (car moves-list)) 
                           (car (cdr (car moves-list))) 
                           (car (cdr (cdr (car moves-list)))) 
                           current-state)
                 (filter-valid-move (cdr moves-list) current-state (cons (car moves-list) output-list))
                 (filter-valid-move (cdr moves-list) current-state output-list))))


;Get the list of all the moves from a current-state. This include valid and invalid moves
(define (get-all-moves current-state output-list)
         (if (null? current-state) output-list
             (get-all-moves (cdr current-state) 
                            (append (hashq-ref h (car current-state)) output-list ))))
          
   

;Check if the move is valid
(define (is-valid start mid end current-state)
  (if (equal? (member start current-state) #f)
      #f
      (if (equal? (member mid current-state) #f)
          #f
          (if (equal? (member end current-state) #f)
              #t
              #f))))















