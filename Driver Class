; Tyler Fung
; Date programmed: 5 October 2018
; Desciprtion: Voter and Candidates best matching 
; 
;-------------------------------------------------------
;every_score
;All scores with the true matches are added onto the list. 

(define (every_score vote score)
   (if
    (null? score)
    ()
    (cons (cons(caar score) (cons(cand_score vote (cdar score)) '())) (every_score vote (cdr score)))
  )                   
     
)

;-------------------------------------------------------
;cand_score
;three predicate conditions where if two 1's add up to 2 it will be added to candidate's tally, added up ;to -2 it will also be added to the tally, and 0 representing a non-decision so it is taken off the 
;total. The negative and positives would be added togther to determine the total votes of one candidate.

(define (cand_score voter candadites)
 (cond ((null? candadites) 0)
        (else
         (cond ((and (not (eqv? (car voter) 0)) (not (eqv? (car candadites) 0)) (eqv? (+ (car voter) (car candadites)) 0))            (+ -1 (cand_score (cdr voter) (cdr candadites))))

               ((and (not (eqv? (car voter) 0)) (not (eqv? (car candadites) 0))(eqv? (+ (car voter) (car candadites)) 2))       (+ 1 (cand_score (cdr voter) (cdr candadites))))

               ((and (not (eqv? (car voter) 0)) (not (eqv? (car candadites) 0))(eqv? (+ (car voter) (car candadites)) -2))      (+ 1 (cand_score (cdr voter) (cdr candadites))))

               (else (cand_score (cdr voter) (cdr candadites)))))
  )        
)

;-------------------------------------------------------------------
;best_match
;This funcion matches the best pairs using the "eqv" function to match the caar numbers. Checks through
;each with cadar If there is a match with "best_match" then it will print out there names which is the 
;cdr.

(define (best_match highest every_cand)
(cond ((null? every_cand) ())
        (else
         (cond  ((eqv? highest (cadar every_cand)) (cons(caar every_cand)(best_match highest (cdr every_cand))))
               (else (best_match highest (cdr every_cand))))))  
)

;------------------------------------------------------------------
;maximum
;Determines the maximum score from the list of candidates. using "<" if the comparision of a voter's     ;number is less than the one next to it, then that numnber will become the hightest cand's score. 

(define (maximum L)
     (cond ((null? (cdr L)) (cadar L))
        (else
         (cond ((< (cadar L) (cadadr L)) (maximum (cdr L)))
               (else (maximum (cons (car L) (cddr L))))))) 
)

;--------------------------------------------------------------------
;best_candidates 
;uses a write function to print out all the functions used to determine the results of the candidates    ;and voters

(define (best_candidates voter candidates)
    write (best_match (maximum (every_score voter candidates)) (every_score voter candidates))

)


;-------------------------------------------------------------------
;(cand_score '(1 1 1 1 1 1 1 1 1 -1) '(1 1 1 1 1 1 1 1 -1 -1)
;(everybody_score '(0 0 0 1 1 1 -1 -1 -1 1) '((Adams 1 1 1 1 1 1 1 1 1 1) (Grant -1 -1 -1 -1 -1 -1 -1 -1 -1 -1) (Polk 1 -1 1 -1 1 -1 1 -1 1 -1) (Jackson 1 0 1 0 1 0 1 0 1 0) (Taft 0 -1 0 -1 0 -1 0 -1 0 -1) (Ford 1 1 1 1 0 0 0 0 0 0) (Madison 0 0 0 1 -1 0 0 -1 1 1)))
;(cand_score '(Adam 1 1 1 1))
;(maximum '((Adam 1) (Jackson 3) (Lincoln 2) (Clay 4)))

;(best_score '(1 -1 1 1 1) '((Adam 1 1 1 1) (Jackson 0 -1 0 0 0)) '((Adam 1 1 1 1) (Jackson 0 -1 0 0 0)))

;(best_match -1 '((Adam 2) (Grant -1) (Ford -1)))

;The two test cases
(best_candidates '(0 0 0 1 1 1 -1 -1 -1 1) '((Adams 1 1 1 1 1 1 1 1 1 1) (Grant -1 -1 -1 -1 -1 -1 -1 -1 -1 -1) (Polk 1 -1 1 -1 1 -1 1 -1 1 -1) (Jackson 1 0 1 0 1 0 1 0 1 0) (Taft 0 -1 0 -1 0 -1 0 -1 0 -1) (Ford 1 1 1 1 0 0 0 0 0 0) (Madison 0 0 0 1 -1 0 0 -1 1 1)))

;(best_candidates '(-1 -1 -1 -1 1 1 1 1 1 1) '((Wilson -1 -1 -1 -1 -1 -1 -1 -1 -1 -1) (taylor -1 -1 -1 1 1 1 1 1 1 1) (monroe -1 -1 -1 1 1 -1 -1 1 1 1) (Madison 0 0 0 1 -1 0 0 -1 1 1)))

;Additional test cases for testing boundaries
;(best_candidates '(1 -1 1 -1 1 -1 1 -1 1 -1) '((Peter -1 1 -1 1 -1 1 -1 1 -1 1) (Devin -1 -1 -1 -1 -1 -1 -1 -1 -1 -1) (Henry 1 -1 1 -1 1 -1 1 -1 1 -1) (Jacob 1 0 1 0 1 0 1 0 1 0)))

;(best_candidates '(1 1 1 1 1 -1 1 -1 1 -1) '((Randy 0 0 0 0 0 1 0 0 0 0) (Jospeh 0 0 0 0 0 0 -1 0 0 -1) (Michael 0 0 0 0 1 0 0 0 0 0) (Larry 0 0 0 0  0 0 0 1 0)))

;(best_candidates '(1 0 1 0 1 1 -1 1 -1 1) '((William 1 -1 1 0 1 1 1 1 1 0) (Andrew 0 -1 1 -1 0 -1 0 -1 -1 0) (Polk 1 -1 1 -1 1 -1 1 -1 1 -1) (Washington 1 -1 1 0 1 1 -1 0 1 -1) (Monroe 0 -1 1 -1 0 1 0 0 0 -1) (Abraham 1 -1 1 1 1 0 -1 -1 1 1) (Steven 1 0 1 1 1 0 1 -1 -1 -1)))


;(best_candidates '(1 1 1 1 1 1 1 1 1 -1) '((Manny -1 -1 -1 -1 -1 -1 -1 -1 -1 1) (George 1 1 1 1 1 1 -1 1 1 1) (Michael 1 1 1 1 1 1 1 -1 -1 1) (Robert 0 1 0 0  0 0 0 0 1)))
