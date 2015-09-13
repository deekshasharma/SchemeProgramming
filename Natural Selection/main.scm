;;#lang racket


(define action-list '())

; Initialize
(define (initialize-agent)
  (set! action-list '())
  "OK"
  )

; choose-action function 
(define (choose-action energy event-list env-list)
  (if (square1-empty? (car env-list))
      (front-blocked-no event-list env-list)
      (front-blocked-yes env-list)))
      

; Handles the case when the front is blocked
(define (front-blocked-yes env-list)
  (let* ((row1 (car env-list)))
  (if (and (veg-infront row1) (veg-energy (car (cdr row1))))
      "EAT-PASSIVE"
      (let* ((random-action (turn-or-stay)))
             (set! action-list (list "RANDOM"))               
        random-action))))
        
; Handles the case when the front is not blocked
(define (front-blocked-no event-list env-list)
  (if (equal? (length action-list) 0)
      (actionlist-empty-yes env-list)
      (actionlist-empty-no event-list env-list)
      )
  )


; Control agent moves when the action-list is empty
; Assumes that front is not blocked
(define (actionlist-empty-yes env-list)
  (let* ((veg-loc (get-veg-loc env-list 1 '())))
    (if (equal? (length veg-loc) 0)
        (let* ((random-action (turn-random)))
          (set! action-list (list "RANDOM"))
          random-action)
        (update-action-list veg-loc env-list))))
        

; Wrapper function on top of update-map
(define (update-action-list veg-loc env-list)
  (update-map veg-loc env-list)
  (car action-list))
  

; Update map to reach vegetatation. 
(define (update-map veg-loc env-list)
  (let* ((rownum (car veg-loc))
         (colnum (car (cdr veg-loc))))
    (cond ((equal? rownum 1) (update-map-row1 colnum))
          ((equal? rownum 2) (update-map-row2 colnum env-list))
          ((equal? rownum 3) (update-map-row3 colnum env-list))
          ((equal? rownum 4) (update-map-row4 colnum env-list))
          ((equal? rownum 5) (update-map-row5 colnum env-list)))))



 ;Control agent moves when the action-list is not empty
(define (actionlist-empty-no event-list env-list )
 (if (equal? (car action-list) "RANDOM")
      (actionlist-empty-yes env-list)
      (if (moved-missing? event-list) (re-execute-action env-list)
          (if (full-move-executed? event-list) (complete-plan)  ; move executed? now turn
              (execute-remaining-moves event-list env-list)))))                              
              

; Returns true if (moved spaces) is missing
(define (moved-missing? event-list)
  (if (equal? (length event-list) 0) #f
      (let* ((event (car event-list)))
        (if (equal? (car event) 'moved) #t
            (moved-missing? (cdr event-list))))))
                      

; Re execute Agent moves when earlier moves did not happen     
(define (re-execute-action env-list)
  (let* ((first-action (car action-list)))
    (cond ((equal? first-action "MOVE-PASSIVE-1") (check-move1 env-list)) 
          ((equal? first-action "MOVE-PASSIVE-2") (check-move2 env-list))
          ((equal? first-action "MOVE-PASSIVE-3") (check-move3 env-list)) 
          )))
          
; Try move1 again else random turn
(define (check-move1 env-list)
(if (square1-empty? (car env-list)) (car action-list)
(let* ((random-action (turn-random)))
(set! action-list (list "RANDOM"))
random-action))) 
 
  
; Try move2 again else random turn
(define (check-move2 env-list)
(let* ((row1 (car env-list))
       (row2 (car (cdr env-list))))
  (if (and (square1-empty? row1) (square2-empty? row2)) (car action-list)
      (let* ((random-action (turn-random)))
        (set! action-list (list "RANDOM"))
        random-action))))
 
; Try move3 again else random turn
(define (check-move3 env-list)
(let* ((row1 (car env-list))
       (row2 (car (cdr env-list)))
       (row3 (car (cdr (cdr env-list)))))
  (if (and (square1-empty? row1) (square2-empty? row2) (square3-empty? row3)) (car action-list)
      (let* ((random-action (turn-random)))
        (set! action-list (list "RANDOM"))
        random-action))))

  
; Returns true if the previous planned move was executed
(define (full-move-executed? event-list)
  (let* ((first-action (car action-list))         
         (moved-list (get-moved-list event-list))
         (moved-spaces (get-moved-spaces moved-list)))
    (if (and (equal? first-action "MOVE-PASSIVE-1") (equal? moved-spaces 1)) #t
        (if (and (equal? first-action "MOVE-PASSIVE-2") (= moved-spaces 2)) #t
            (if (and (equal? first-action "MOVE-PASSIVE-3") (equal? moved-spaces 3)) #t
                #f)))))
        
; Returns the last action and empty action-list  
 (define (complete-plan)
   (let* ((last-turn (car (cdr action-list))))
     (set! action-list '())
     last-turn))
   
  

; Returns the moved -list from the event-list
(define (get-moved-list event-list)
  (if (equal? (length event-list) 0) '() 
      (let* ((event (car event-list)))
        (if (equal? (car event) 'moved) event
            (get-moved-list (cdr event-list))))))
            
    
; Return the number of spaces moved
(define (get-moved-spaces moved-list)
  (if (equal? (length moved-list) 0) 0
  (car (cdr moved-list))))
  
  
; Execute the remaining move from the last turn
(define (execute-remaining-moves event-list env-list)
  (let* ((first-action (car action-list)))         
    (if (equal? first-action "MOVE-PASSIVE-2") (execute-remain-move2 env-list)
        (execute-remain-move3 env-list))))
        

; Re calculate and executes the remaining MOVE-PASSIVE-2
(define (execute-remain-move2 env-list)  
  (if (square1-empty? (car env-list))
      (let* ((turn (car (cdr action-list))))
        (set! action-list (list "MOVE_PASSIVE_1" turn))
        (car action-list))
      
      (let* ((random-action (turn-or-stay)))
             (set! action-list (list "RANDOM"))               
        random-action)))
        
      
   
 ; Re calculate and execute the remaining MOVE-PASSIVE-3 
 (define (execute-remain-move3 event-list env-list)
   (let* ((moved-spaces (get-moved-spaces (get-moved-list event-list)))
          (row1 (car env-list))
          (row2 (car (cdr env-list))))
     (if (equal? moved-spaces 2) 
         (if (square1-empty? row1)
            (let* ((turn (car (cdr action-list))))
              (set! action-list (list "MOVE_PASSIVE_1" turn))
              (car action-list))
            (let* ((random-action (turn-or-stay)))
              (set! action-list (list "RANDOM"))               
              random-action))
         
         (if (and (square1-empty? row1) (square2-empty? row2))
             (let* ((turn (car (cdr action-list))))
               (set! action-list (list "MOVE_PASSIVE_2" turn))
               (car action-list))             
             (let* ((random-action (turn-or-stay)))
              (set! action-list (list "RANDOM"))               
              random-action))             
             )))
              

; Return the xy coordinates of the closest vegetation 
(define (get-veg-loc env-list rownum output)
  (if (or (> (length output) 1) (equal? (length env-list) 0)) output
      (let* ((column (get-veg-col (car env-list) 1)))
        (if (equal? column 0)
            (get-veg-loc (cdr env-list) (+ rownum 1) output)
            (get-veg-loc (cdr env-list) (+ rownum 1) (append output (list rownum column)))))))
            
        
      
  
  
; Return the index in a row that contain vegetation with energy
(define (get-veg-col row col)
  (if (equal? (length row) 0) 0
      (if (list? (car row))
          (if (and (equal? (car (car row)) 'vegetation) (veg-energy (car row))) col              
                            (get-veg-col (cdr row) (+ col 1)))                                           
          (get-veg-col (cdr row) (+ col 1)))))
  


; Returns true if there is a vegetation in front of the agent
(define (veg-infront row1)
  (if (list? (car (cdr row1))) 
      (if (equal? (car (car (cdr row1))) 'vegetation) #t
          #f)
  #f))


; Return true if the given vegetation has energy
(define (veg-energy veg-list)
  (if (> (car (cdr (cdr veg-list))) 0) #t
      #f)) 


; Return either a random turn or stay orientation
(define (turn-or-stay)
  (if (equal? (random-num) 1) "TURN-LEFT"
      (if (equal? (random-num) 2) "TURN-RIGHT"
          (if (equal? (random-num) 3) "STAY"
          "TURN-AROUND"))))
          
            
;Randomize turns vs stay 
(define (random-num)
    (+ 1 (random 4)))


; Return a random turn orientation
(define (turn-random)
  (if (equal? (random-num-gen) 1) "TURN-LEFT"
      (if (equal? (random-num-gen) 2) "TURN-RIGHT"
          "TURN-AROUND")))
 
; Randomize the turns
(define (random-num-gen)
    (+ 1 (random 3)))

          


; Updates the map in action list when the vegetation is found in row1 infront of the agent
; Assumes that front is not blocked.
; Assumes that no vegetation in front
(define (update-map-row1 col)
  (cond ((> col 2) (set! action-list (list "MOVE-PASSIVE-1" "TURN-RIGHT")))
        ((< col 2) (set! action-list (list "MOVE-PASSIVE-1" "TURN-LEFT")))))
        
  


; Updates the map in action list when the vegetation is found in row2 infront of the agent
; Assuming that row1 (front of the agent) is not blocked
(define (update-map-row2 col env-list)
  (let* ((row2 (car (cdr env-list))))
         (cond ((and (equal? col 3)(not (square2-empty? row2))) (set! action-list (list "MOVE-PASSIVE-1")))               
               ((and (> col 3)(not (square2-empty? row2))) (set! action-list (list "MOVE-PASSIVE-1" "TURN-RIGHT")))
               ((and (< col 3)(not (square2-empty? row2))) (set! action-list (list "MOVE-PASSIVE-1" "TURN-LEFT")))
               
               ((and (equal? col 3) (square2-empty? row2)) (set! action-list (list "MOVE-PASSIVE-1")))
               ((and (> col 3) (square2-empty? row2)) (set! action-list (list "MOVE-PASSIVE-2" "TURN-RIGHT")))
               ((and (< col 3) (square2-empty? row2)) (set! action-list (list "MOVE-PASSIVE-2" "TURN-LEFT"))))))
               
               
    

; Updates the map in action list when the vegetation is found in row3 infront of the agent.
; Assuming that row1 (front of the agent) is not blocked
(define (update-map-row3 col env-list)
  (let* ((row2 (car (cdr env-list)))
         (row3 (car (cdr (cdr env-list)))))
    
    (cond ((not (square2-empty? row2)) (set! action-list (list "MOVE-PASSIVE-1")))
          
          ((equal? col 4) (set! action-list (list "MOVE-PASSIVE-2")))                          
          
          ((and (square3-empty? row3) (> col 4))                
                (set! action-list (list "MOVE-PASSIVE-3" "TURN-RIGHT")))
          
          ((and (square3-empty? row3) (< col 4))                
                (set! action-list (list "MOVE-PASSIVE-3" "TURN-LEFT")))         
          
          ((and (not (square3-empty? row3)) (> col 4))                
                (set! action-list (list "MOVE-PASSIVE-2" "TURN-RIGHT")))         
          
          ((and (not (square3-empty? row3)) (< col 4))                
                (set! action-list (list "MOVE-PASSIVE-2" "TURN-LEFT"))))))         


; Updates the map in action list when the vegetation is found in row4 infront of the agent 
; Assuming that row1 (front of the agent) is not blocked
(define (update-map-row4 col env-list)
  (let* ((row2 (car (cdr env-list)))
         (row3 (car (cdr (cdr env-list)))))
    (cond ((not (square2-empty? row2)) (set! action-list (list "MOVE-PASSIVE-1")))
          ((not (square3-empty? row3)) (set! action-list (list "MOVE-PASSIVE-2")))
          ((and (square3-empty? row3) (equal? col 5)) (set! action-list (list "MOVE-PASSIVE-3")))
          ((and (square3-empty? row3) (> col 5)) (set! action-list (list "MOVE-PASSIVE-3" "TURN-RIGHT")))
          ((and (square3-empty? row3) (< col 5)) (set! action-list  (list "MOVE-PASSIVE-3" "TURN-LEFT"))))))
                


; Updates the map in action list when the vegetation is found in row5 infront of the agent 
; Assuming that row1 (front of the agent) is not blocked    
(define (update-map-row5 col env-list)
  (let* ((row2 (car (cdr env-list)))
         (row3 (car (cdr (cdr env-list)))))
    (cond ((not (square2-empty? row2)) (set! action-list (list "MOVE-PASSIVE-1")))
          ((not (square3-empty? row3)) (set! action-list (list "MOVE-PASSIVE-2")))
          (square3-empty? row3 (set! action-list (list "MOVE-PASSIVE-3"))))))
          
           

; Returns true if square1 is empty
(define (square1-empty? row1)
  (if (equal? (car (cdr row1)) 'empty) #t
      #f
      ))
; Returns true if square2 is empty
(define (square2-empty? row2)
  (if (equal? (car (cdr (cdr row2))) 'empty) #t
      #f))

; Returns true if square3 is empty
(define (square3-empty? row3)
  (if (equal? (car (cdr (cdr (cdr row3)))) 'empty) #t
      #f))





;(choose-action 10000 '() '(((vegetation 1 200) empty empty)((vegetation 1 400) empty empty empty empty)(empty empty empty empty empty empty (vegetation 1 400))(empty empty empty empty empty empty empty empty empty)(empty empty empty (vegetation 1 0) empty empty empty empty empty empty empty)))
;action-list




;(choose-action 10000 '() '((empty empty empty)(empty empty empty empty barrier)(empty empty empty empty empty barrier barrier)(empty empty empty empty (vegetation 0 140) empty barrier barrier barrier)(empty empty empty empty empty empty empty barrier barrier barrier barrier)))
;(initialize-agent)





