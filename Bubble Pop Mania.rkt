#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require racket/list)

#|Bubble-Pop Mania:
 click bubbles in groups of three or more of the same color to win points. The bubbles will disappear and the rest of the bubbles will consolidate.
 Seven or more bubbles clicked will increase the score a whole lot more than if just 3 bubbles are popped. One wins by reaching the goal score; if
 if the board runs out of groups of three bubbles, you lose. Click the spacebar to start over|#

;Universal Constants
;Board and Scores
(define width 600)
(define height 600)
(define ball-r 15)
(define board (rectangle width height 'solid 'white))
(define beginner-score 2500)
(define intermediate-score 1500)
(define advanced-score 300)
(define radvanced 300)

; -> Struct (list num)
; Defines the WS structure, which holds a List of ball structs and a Score and a number of clicks
(define-struct WS (lob score))

; -> Struct (string num num boolean)
; Defines the Ball struct, which holds a color (string), ball-x, ball-y, and clicked? (boolean)
(define-struct Ball (color x y clicked?))

; ball, ball -> bool
; ball equal function
(define (Ball=? ball1 ball2)
  (if (and (equal? (Ball-x ball1) (Ball-x ball2))
           (equal? (Ball-y ball1) (Ball-y ball2)))
      #t #f))

; -> string
;random color for the balls
(define (rnd-C level)
  (list-ref (list "mediumturquoise" "darkgreen" "crimson" "darkmagenta" "darkslateblue" "orange")
            (cond [(equal? level "beginner") (random 3)]
                  [(equal? level "intermediate") (random 4)]
                  [(equal? level "advanced") (random 5)]
                  [(equal? level "rachelAdvanced") (random 6)])))   

; -> list of balls
; creates all the balls that will be on the board at once
(define (make-lob level)
  (flatten
   (build-list (- (/ width (* 2 ball-r)) 1)
               (λ (y) (build-list
                       (- (/ width (* 2 ball-r)) 1)
                       (λ (x) (make-Ball (rnd-C level) (* (+ x 1) (* 2 ball-r)) (* (+ y 1) (* 2 ball-r)) #f)))))))

; String->Function
   ; λ WS -> boolean
; sets how one wins based on the level
(define (make-win level)
 (λ (ws) (cond [(string=? level "beginner") (if (>= (WS-score ws) beginner-score) #t #f)]
               [(string=? level "intermediate") (if (>= (WS-score ws) intermediate-score) #t #f)]
               [(string=? level "advanced") (if (>= (WS-score ws) advanced-score) #t #f)]
               [(string=? level "rachelAdvanced") (if (>= (WS-score ws) radvanced) #t #f)])))


; lob, lob -> lob
; takes two lists of balls, lob1 and lob2. lob1 will only have one item. Checks to see if the ball from lob1
; is touching any of the balls in lob2, then checks to see if the balls touching lob1 are touching anything
; else the same color.
(define (click-helper lob0 lob1 lob2)
  (if (empty? lob1) lob0 
      (local [(define new-lob0 (append lob0 (list (first lob1))))
              (define new-lob1 (append (rest lob1)
                                       (filter (λ (b) (touching? b (first lob1))) lob2)))
              (define new-lob2 (filter (λ (b) (not (touching? b (first lob1)))) lob2))]
        (click-helper new-lob0 new-lob1 new-lob2))))

; Ball, Ball -> Boolean
; Requirement balls have to pass to move 1 at a time from lob1 to lob0
; Checks whether balls are touching and same color
(define (touching? b b2)
  (and (or
        (and (= (Ball-y b) (Ball-y b2))
             (= (abs (- (Ball-x b) (Ball-x b2))) (* 2 ball-r)))
        (and (= (Ball-x b) (Ball-x b2))
             (= (abs (- (Ball-y b) (Ball-y b2))) (* 2 ball-r))))
       (equal? (Ball-color b) (Ball-color b2))))


; Number, Number, WS -> lob
; computes the list of balls that are touching and same color of the ball that was clicked
(define (click-help x y ws)
  (local
    [(define clicked-changer
       (map (λ (ball) (if (< (sqrt (+ (sqr (- x (Ball-x ball))) (sqr (- y (Ball-y ball))))) ball-r)
                          (make-Ball (Ball-color ball) (Ball-x ball) (Ball-y ball) #t)
                          ball))
            (WS-lob ws)))]
    (click-helper empty
                  (filter (λ (ball) (equal? #t (Ball-clicked? ball))) clicked-changer)
                  (filter (λ (ball) (equal? #f (Ball-clicked? ball))) clicked-changer))))

; Ball, Ball -> Boolean
; checks if two balls are different
(define (differentBall? b b2)
  (or (not (equal? (Ball-x b) (Ball-x b2)))
      (not (equal? (Ball-y b) (Ball-y b2)))))


;lob, lob ->lob
  ;λ ball->boolean
;Produce a list with everything in lob2 that is not in lob1, aka the balls that
;won't disappear and will move down in the new WS
(define (unclicked-list lob1 lob2)
  (filter (λ (ball) (foldl
                     (λ (b1 bool) (and bool (differentBall? b1 ball)))
                     #t
                     lob1))
          lob2))

; ball, list -> num
; checks where ball is and then returns a new y coordinate for ball that will move it down
(define (new-y ball l)
  (local [(define ylist (filter (λ (b2) (and (= (Ball-x b2) (Ball-x ball))
                                             (> (Ball-y b2) (Ball-y ball)))) l))]
    (cond [(empty? ylist) 570]
          [else (- 570 (* (* 2 ball-r) (length ylist)))])))

;ball, list ->num
;checks where ball is and then returns a new x coordinate for ball that will move it right
(define (new-x ball l)
  (local [(define xlist (filter (λ (b2) (and (= (Ball-y b2) (Ball-y ball))
                                             (> (Ball-x b2) (Ball-x ball)))) l))]
    (cond [(empty? xlist) 570]
          [else (- 570 (* (* 2 ball-r) (length xlist)))])))
  
;List->list
;returns a new list of balls with y coordinates farther to the right, whole list consolidated sideways
(define (sidewaysBall l)
  (map (λ (ball) (make-Ball (Ball-color ball) (new-x ball l) (Ball-y ball) #false)) l))

; List -> List 
;returns a new list of balls with y coordinates lower, whole list moved down
(define (move-ball l)
  (map (λ (ball) (make-Ball (Ball-color ball) (Ball-x ball) (new-y ball l) #false)) l))

; Number, Number, WS -> WS
 ;once something is clicked, finalizes the list to be re-rendered
 (define (click-handler x y ws)
   (local [(define loc (click-help x y ws))]
     (cond [(empty? loc) ws]
           [(empty? (rest loc)) ws]
           [(empty? (rest (rest loc))) ws]
           [else (make-WS (sidewaysBall (move-ball (unclicked-list loc (WS-lob ws))))
                     (cond [(<= (length loc) 7) (+ (WS-score ws) (length loc))]
                           [else (+ (WS-score ws) (sqr (length loc)))]))])))

  
; WS -> WS
;mouse-handler, manages the coordinates and evts of the mouse 
(define (mouse-handler ws x y evt)
         (if (mouse=? evt "button-up")
      (click-handler x y ws)
      ws))

; Ball -> Image
; Places one ball on the field
(define (place-ball ball img)
  (place-image (circle ball-r 'solid (Ball-color ball))
               (Ball-x ball) (Ball-y ball) img))

; String-> Function
   ; λ WS -> Image
;render, draws different fields depending on the level
(define (make-render level)
  (λ (ws)
    (cond [(string=? level "beginner")
           (above (foldl place-ball board (WS-lob ws))
                  (text (string-append "Score: " (number->string (WS-score ws))
                                       " Goal: " (number->string beginner-score)) 40 "darkslateblue"))]
          [(string=? level "intermediate")
           (above (foldl place-ball board (WS-lob ws))
                  (text (string-append "Score: " (number->string (WS-score ws))
                                       " Goal: " (number->string intermediate-score)) 40 "darkslateblue"))]
          [(string=? level "advanced")
           (above (foldl place-ball board (WS-lob ws))
                  (text (string-append "Score: " (number->string (WS-score ws))
                                       " Goal: " (number->string advanced-score)) 40 "darkslateblue"))]
          [(string=? level "rachelAdvanced")
           (above (foldl place-ball board (WS-lob ws))
                  (text (string-append "Score: " (number->string (WS-score ws))
                                       " Goal: " (number->string radvanced)) 40 "darkslateblue"))])))


;WS-> Image
;Shows blank screen with Winning Score displayed after one wins
(define (final-image ws)
  (overlay (above (text "You won!" 60 "darkslateblue")
                  (text (string-append "Score: " (number->string (WS-score ws))) 50 "darkslateblue"))
           board))

;WS, string-> WS
;cheat keys
;g takes out green
;b takes out all blue
;r takes out all crimson
;p takes out all magenta
;t takes out turquoise
;o takes out all orange
(define (cheat ws key)
  (local [(define cheatList (filter (λ (ball)
                                      (not (equal? (Ball-color ball)
                                                   (cond [(string=? key "g") "darkgreen"]
                                                         [(string=? key "b") "darkslateblue"]
                                                         [(string=? key "r") "crimson"]
                                                         [(string=? key "p") "darkmagenta"]
                                                         [(string=? key "t") "mediumturqiose"]
                                                         [(string=? key "o") "orange"]))))
                                    (WS-lob ws)))]
    (make-WS (move-ball cheatList) (+ (WS-score ws) (length cheatList)))))
; String -> Function
; Key-Handler adjusted by level
   ;resets game with spacebar
   ;applies cheats if in advanced or rachel's advanced levels
(define (make-kh level) (λ (ws key) (if (string=? key " ")
                                        (make-WS (make-lob level) 0)
                                        (if (or (string=? level "advanced")
                                                (string=? level "rachelAdvanced"))
                                            (cheat ws key)
                                            ws))))

; -> WorldState
;Initial WS
(define (initial-WS level) (make-WS (make-lob level) 0))

(define (main level)
  (big-bang (initial-WS level)
            (on-key (make-kh level))
            (on-mouse mouse-handler)
            (to-draw (make-render level))
            (stop-when (make-win level) final-image)))