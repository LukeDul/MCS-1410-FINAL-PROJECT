;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname main) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; the width of the game window 
(define WINDOW-WIDTH 500)

; the height of the game window 
(define WINDOW-HEIGHT 500)

; size of the player's hitbox 
(define PLAYER-HITBOX 7)

; player contains the heading, position & speed of the player character.
(define-struct player [heading position speed])

; holds the state of each usuable key 
(define-struct keys [w a s d])

; contains the width, height, and position of a hitbox 
(define-struct hit-box [width height x y])

; worldstate 
(define-struct world-state [player keyboard hit-box])



;******************************************************* keyboard handling *******************************************************


; World State, Key -> World State
; sets key to true when pressed 
(define (press-handler struct input-key)
 (local [(define w (keys-w (world-state-keyboard struct)))
         (define a (keys-a (world-state-keyboard struct)))
         (define s (keys-s (world-state-keyboard struct)))
         (define d (keys-d (world-state-keyboard struct)))]
   
   (cond [(key=? input-key "w") (update-keys struct (make-keys #t a s d))]
         [(key=? input-key "a") (update-keys struct (make-keys w #t s d))]
         [(key=? input-key "s") (update-keys struct (make-keys w a #t d))]   
         [(key=? input-key "d") (update-keys struct (make-keys w a s #t))]
         [else struct])))


; World State, Key -> World State 
; sets key to false when released
(define (release-handler struct input-key)
 (local [(define w (keys-w (world-state-keyboard struct)))
         (define a (keys-a (world-state-keyboard struct)))
         (define s (keys-s (world-state-keyboard struct)))
         (define d (keys-d (world-state-keyboard struct)))]
   
   (cond [(key=? input-key "w") (update-keys struct(make-keys #f a s d))]
         [(key=? input-key "a") (update-keys struct(make-keys w #f s d))]
         [(key=? input-key "s") (update-keys struct(make-keys w a #f d))]   
         [(key=? input-key "d") (update-keys struct(make-keys w a s #f))]
         [else struct]))) 


; World State, Keys -> World State 
(define (update-keys struct new-keys) (make-world-state (world-state-player struct) new-keys (world-state-hit-box struct)))


; (make-player (make-world-state (make-player "NORTH" (make-posn 250 450) 4) (make-keys #false #false #false #false)) (make-posn 250 446) 4)


;******************************************************* on-tick / movement *******************************************************


; World State -> World State 
; updates heading and position of the player 
(define (tock struct)
 (local [(define heading (player-heading (world-state-player struct)))
         (define new-position (change-position (world-state-player struct)))
         (define new-heading (heading-handler struct))
         (define speed (player-speed (world-state-player struct)))
         (define current-position (player-position (world-state-player struct)))]
   
   (cond [(square-collision? current-position PLAYER-HITBOX (world-state-hit-box struct)); if player is colliding w/ hitbox
          (make-world-state (make-player heading current-position speed) ; freeze player 
                            (world-state-keyboard struct)
                            (world-state-hit-box struct))] 
         
         [else (make-world-state (make-player new-heading new-position speed) ; updates player
                                 (world-state-keyboard struct)
                                 (world-state-hit-box struct))]))) ; retains keyboard
                                 


; World State -> String
; changes the heading based upon which keys are pressed. 
(define (heading-handler struct)
  (local [(define w (keys-w (world-state-keyboard struct)))
          (define a (keys-a (world-state-keyboard struct)))
          (define s (keys-s (world-state-keyboard struct)))
          (define d (keys-d (world-state-keyboard struct)))
          (define heading (player-heading (world-state-player struct)))]
    
    (cond [(and w d) "NE"]
          [(and d s) "SE"]
          [(and s a) "SW"]
          [(and a w) "NW"]  
          [w "NORTH"]
          [a "WEST"]
          [s "SOUTH"]
          [d "EAST"]
          [else heading])))  


; Player Structure -> Posn
; changes the position of the player based upon the current heading and speed
(define (change-position struct)
  (local [(define heading (player-heading struct))
          (define x (posn-x (player-position struct)))
          (define y (posn-y (player-position struct)))
          (define speed (player-speed struct))
          (define diagonal-speed (ceiling (percentage 65 speed)))] ; 70 percent of speed in each direction approximates
                                                                ; a diagonal speed equal to speed 
    
    (cond [(equal? heading "NORTH") (make-posn x (- y speed))]
          [(equal? heading "SOUTH") (make-posn x (+ y speed))]
          [(equal? heading "WEST")  (make-posn (- x speed) y)]
          [(equal? heading "EAST")  (make-posn (+ x speed) y)]
          
          [(equal? heading "NE") (make-posn (+ x diagonal-speed) (- y diagonal-speed))]
          [(equal? heading "NW") (make-posn (- x diagonal-speed) (- y diagonal-speed))]
          [(equal? heading "SE") (make-posn (+ x diagonal-speed) (+ y diagonal-speed))]
          [(equal? heading "SW") (make-posn (- x diagonal-speed) (+ y diagonal-speed))]
          [else (print struct)])))


; Number, Number -> Number
; returns the given percentage of a number 
(define (percentage percent number)
  (* (/ percent 10) (/ number 10)))


;**************************************************** collision ****************************************************


; Posn, Hit-box -> Boolean
; checks if a 4 points surrounding a point is colliding with a point 
(define (square-collision? point size hit-box)
  (if (or
       (point-collision? (make-posn (+ size (posn-x point)) (posn-y point)) hit-box) ; left 
       (point-collision? (make-posn (- (posn-x point) size) (posn-y point)) hit-box) ; right
       (point-collision? (make-posn (posn-x point) (+ size (posn-y point))) hit-box) ; top 
       (point-collision? (make-posn (posn-x point) (- (posn-y point) size)) hit-box) ; bottom 

       (point-collision? (make-posn (+ (posn-x point) size) (+ (posn-y point) size)) hit-box) ; top right 
       (point-collision? (make-posn (- (posn-x point) size) (+ (posn-y point) size)) hit-box) ; top left 
       (point-collision? (make-posn (- (posn-x point) size) (- (posn-y point) size)) hit-box) ; bottom left 
       (point-collision? (make-posn (+ (posn-x point) size) (- (posn-y point) size)) hit-box) ; bottom right 
       )
      #t
      #f)) 


; Posn, hit-box, -> Boolean
; Given a point and a rectangular hit-box returns whether the point the hitbox contains the point
(define (point-collision? point hit-box)
  (if (and
       (<= (posn-x point) (+ (hit-box-x hit-box) (/ (hit-box-width hit-box) 2)))
       (>= (posn-x point) (- (hit-box-x hit-box) (/ (hit-box-width hit-box) 2)))
       (<= (posn-y point) (+ (hit-box-y hit-box) (/ (hit-box-height hit-box) 2)))
       (>= (posn-y point) (- (hit-box-y hit-box) (/ (hit-box-height hit-box) 2))))
      #t
      #f)) 


;******************************************************* rendering ****************************************************
 
; World State -> Image
; Given the Player Structure, struct, returns an image of the playerable character at the Player Structure's coordinates. 
(define (draw struct)
  (local [(define x (posn-x (player-position (world-state-player struct))))
          (define y (posn-y (player-position (world-state-player struct))))
          (define heading (player-heading (world-state-player struct)))
          (define player-image (square (* 2 PLAYER-HITBOX) 'solid 'red))  ; (* PLAYER-HITBOX (sqrt 2))
          (define background (rectangle WINDOW-WIDTH WINDOW-HEIGHT 'solid 'gray))
          (define w (keys-w (world-state-keyboard struct)))
          (define a (keys-a (world-state-keyboard struct)))
          (define s (keys-s (world-state-keyboard struct)))
          (define d (keys-d (world-state-keyboard struct)))]
    
    (place-image (above (text (if w "#t" "#f") 12 "olive") 
                        (text (if a "#t" "#f") 12 "olive")
                        (text (if s "#t" "#f") 12 "olive")
                        (text (if d "#t" "#f") 12 "olive") 
                        (text heading 12 "olive")) 
                 100 100
                 (place-image player-image x y (overlay (square 10 'solid 'black) background))))) 


;******************************************************* initial states ************************************************

; the initial state of the playable character. 
(define initial-player (make-player "NORTH"
                                    (make-posn (/ WINDOW-WIDTH 2) ; aligns player on the middle of x-axis
                                               (ceiling (percentage 90 WINDOW-HEIGHT))) ; aligns player on 90% of y-axis
                                    1)) ; initial speed

(define test-dummy (make-hit-box 10 10 (/ WINDOW-HEIGHT 2) (/ WINDOW-WIDTH 2)))
 
(define initial-keys (make-keys #f #f #f #f)) 

(define initial-world-state (make-world-state initial-player initial-keys test-dummy)) 


;******************************************************* big bang ***********************************************

(big-bang initial-world-state 
  (on-tick tock 0.001)  
  (to-draw draw)
  (state #f)
  (on-key press-handler)
  (on-release release-handler))
