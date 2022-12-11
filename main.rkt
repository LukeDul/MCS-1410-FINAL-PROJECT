;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname main) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; the width of the game window 
(define WINDOW-WIDTH 1000)

; the height of the game window 
(define WINDOW-HEIGHT 500)

; size of the player's hitbox 
(define PLAYER-HITBOX 7)

; player contains the heading, position & speed of the player character.
(define-struct player [heading position speed desired-heading]) 

; holds the state of each usuable key 
(define-struct keys [w a s d])

; contains the width, height, and position of a hitbox 
(define-struct hit-box [width height x y])

;
(define-struct menu [buttons])

; worldstate 
(define-struct world-state [player keyboard level ticks])





(define NORTH 1)
(define NE 2)
(define EAST 3)
(define SE 4)
(define SOUTH 5)
(define SW 6)
(define WEST 7)
(define NW 8)

;(define-struct world-state [menu game-play])

;(define-struct game-play [player keyboard hit-box]) 

; buttons is a list of all buttons on given screen
; screen is the current screen
; (define-struct menu [screen buttons]


#| TO DO



Menus
 - Title Menu
   - Start Button -> Level 1
   - Continue Button -> Last Save Point 
   - Level Select Button
 
 - Level Selector Menu
   - Level 1
   - Level 2
   - Level 3 (Only Accessible if 2 is beaten etc)
 
 - Escape Menu
   - Exit -> Title
   - Exit -> Level Select
 
 - Win Screen / Credits 


Save State
  - Holds Last Save Point
  - Completed Levels 

Level State 
  - Save Point Hit-box Position
  - Start Position
  - End Hit-box position



|#
  
 

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
   
   (cond [(key=? input-key "w") (update-keys struct (make-keys #f a s d))]
         [(key=? input-key "a") (update-keys struct (make-keys w #f s d))]
         [(key=? input-key "s") (update-keys struct (make-keys w a #f d))]   
         [(key=? input-key "d") (update-keys struct (make-keys w a s #f))]
         [else struct]))) 


; World State, Keys -> World State
; makes a new world state with the given keyboard state 
(define (update-keys struct new-keys)
  (make-world-state (world-state-player struct)
                    new-keys
                    (world-state-level struct)
                    (world-state-ticks struct)))


;******************************************************* on-tick / movement *******************************************************



; World State -> World State 
; updates heading and position of the player 
(define (tock struct)

         ; shorthand 
 (local [(define keyboard-state (world-state-keyboard struct))
         (define current-heading (player-heading (world-state-player struct)))
         (define current-desired-heading (player-desired-heading (world-state-player struct)))
         (define current-player-position (player-position (world-state-player struct)))
         (define speed (player-speed (world-state-player struct)))
         (define hit-boxes (level-state-hit-boxes (world-state-level struct)))
         (define end-box (level-state-end-box (world-state-level struct)))
         (define save-box (level-state-save-box (world-state-level struct)))

         ; increments ticks 
         (define ticks (+ 1 (world-state-ticks struct)))

         ; turns the heading towards the desired heading 
         (define new-heading (turn-heading current-heading
                                           current-desired-heading
                                           NW
                                           NORTH))

         ; Changes desired-heading based upon which keys are pressed.
         (define new-desired-heading (desired-heading-handler keyboard-state
                                                              current-desired-heading))

         ; Changes the players position based upon the current heading, position and speed.
         (define new-player-position (change-position current-heading
                                                      current-player-position
                                                      speed))]
  
         ; if player is colliding w/ a hitbox 
   (cond [(colliding? current-player-position PLAYER-HITBOX hit-boxes)  

          ; freeze player
          (update-player-and-ticks struct
                                   ticks
                                   (make-player current-heading
                                                current-player-position
                                                speed
                                                current-heading))]
         

         ; if player is colliding w/ the end box 
         [(colliding? current-player-position PLAYER-HITBOX (list end-box))

                   ; freeze player
          (update-player-and-ticks struct
                                   ticks
                                   (make-player current-heading
                                                current-player-position
                                                speed
                                                current-heading))]
         

         ; if player is colliding w/ the save box 
         [(colliding? current-player-position PLAYER-HITBOX (list save-box))

                   ; freeze player 
          (update-player-and-ticks struct
                                   ticks
                                   (make-player current-heading
                                                current-player-position
                                                speed
                                                current-heading))]
                     
         ; otherwise continue as normal  
         [else (cond [(= 3 ticks)
                      (update-player-and-ticks struct 0 (make-player new-heading new-player-position speed new-desired-heading))]

                     [else
                      (update-player-and-ticks struct ticks  (make-player current-heading new-player-position speed new-desired-heading))])])))
            
 
; Number, Number, Number -> Number, Number 
; if a number, ticks, is large enough, turns the player, otherwise, leaves it as is.


; World State, Number, Player State -> World State
; short-hand function for updating the Player State and Ticks within the World State
(define (update-player-and-ticks struct ticks new-player-state)
  (make-world-state new-player-state
                    (world-state-keyboard struct)
                    (world-state-level struct)
                    ticks))


; Number, Number, Number -> Number
; moves the heading towards the desired heading on the shortest path  
(define (turn-heading current-heading desired-heading max-heading min-heading)
  (cond [(= current-heading desired-heading) current-heading]
        [(turn-left? current-heading desired-heading max-heading min-heading) (turn-left current-heading max-heading min-heading)]
        [else (turn-right current-heading max-heading min-heading)]))
  

; Keyboard State, Number -> Number 
; Changes the heading based upon which keys are pressed. If none are pressed returns the last desired-heading. 
(define (desired-heading-handler keyboard-state current-desired-heading)
  (local [(define w (keys-w keyboard-state))
          (define a (keys-a keyboard-state)) 
          (define s (keys-s keyboard-state))
          (define d (keys-d keyboard-state))] 
    
    (cond [(and w d) NE]
          [(and d s) SE] 
          [(and s a) SW]
          [(and a w) NW]  
          [w NORTH]
          [a WEST]
          [s SOUTH]
          [d EAST]
          [else current-desired-heading]))) 


; Number, Number, Number, Number Number -> Boolean
; returns whether turning left is the shortest path to the desired heading 
(define (turn-left? current-heading desired-heading max-heading min-heading)
  (cond [(< (count-left-turns current-heading desired-heading max-heading min-heading)
            (count-right-turns current-heading desired-heading max-heading min-heading))
         #true]
        [else #false]))


; Number, Number, Number -> Number 
; Counts the number of left turns required to reach a desried heading.
(define (count-left-turns current-heading desired-heading max-heading min-heading)
  (cond [(= current-heading desired-heading) 0]
        [else (+ 1 (count-left-turns (turn-left current-heading max-heading min-heading)
                                     desired-heading
                                     max-heading
                                     min-heading))])) 
 
   
; Number, Number, Number -> Number
; Counts the number of right turns required to reach a desired heading. 
(define (count-right-turns current-heading desired-heading max-heading min-heading) 
  (cond [(= current-heading desired-heading) 0]
        [else (+ 1 (count-right-turns (turn-right current-heading max-heading min-heading)
                                      desired-heading
                                      max-heading
                                      min-heading))])) 


; Number, Number -> Number
; moves the heading to the right  
(define (turn-right current-heading max-heading min-heading)
  (cond [(= current-heading max-heading) min-heading]
        [else (+ 1 current-heading)]))
 

; Number, Number -> Number
; moves the heading to the left 
(define (turn-left current-heading max-heading min-heading)
  (cond [(= current-heading min-heading) max-heading]
        [else (- current-heading 1)]))


; Player Structure -> Posn
; changes the position of the player based upon the current heading, position, and  speed
(define (change-position current-heading position speed)
  (local [(define x (posn-x position))
          (define y (posn-y position))
          (define diagonal-speed (ceiling (percentage 70 speed)))] ; 70 percent of speed in each direction approximates
                                                                ; a diagonal speed equal to speed

    (cond [(equal? current-heading NORTH) (make-posn x (- y speed))]
          [(equal? current-heading SOUTH) (make-posn x (+ y speed))]
          [(equal? current-heading WEST)  (make-posn (- x speed) y)]
          [(equal? current-heading EAST)  (make-posn (+ x speed) y)]
          
          [(equal? current-heading NE) (make-posn (+ x diagonal-speed) (- y diagonal-speed))]
          [(equal? current-heading NW) (make-posn (- x diagonal-speed) (- y diagonal-speed))]
          [(equal? current-heading SE) (make-posn (+ x diagonal-speed) (+ y diagonal-speed))]
          [(equal? current-heading SW) (make-posn (- x diagonal-speed) (+ y diagonal-speed))]
          [else (print "bitchbitchbitch")]))) 


; Number, Number -> Number
; returns the given percentage of a number 
(define (percentage percent number)
  (* (/ percent 10) (/ number 10)))


;**************************************************** collision ****************************************************


; List of hit-boxes, Posn, Number  -> Boolean
; checks if a square surrounding a point with width & height size is colliding w/ any hit-box in a list, hit-boxes
(define (colliding? point size hit-boxes)
  (cond [(empty? hit-boxes) #false]
        [(square-collision? point size (first hit-boxes)) #true]
        [else (colliding? point size (rest hit-boxes))]))  
 

; Posn, Hit-box, Number -> Boolean 
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

; List, Posn, hit-box, hit-box 
(define-struct level-state [hit-boxes start-point-position end-box save-box])

; World State -> Image
; Given the Player Structure, struct, returns an image of the playerable character at the Player Structure's coordinates. 
(define (draw struct)
  (local [(define x (posn-x (player-position (world-state-player struct)))) 
          (define y (posn-y (player-position (world-state-player struct))))
          (define player-image (square (* 2 PLAYER-HITBOX) "solid" "red"))  ; (* PLAYER-HITBOX (sqrt 2))
          (define background (rectangle WINDOW-WIDTH WINDOW-HEIGHT "solid" 'gray))
          (define hit-boxes (level-state-hit-boxes (world-state-level struct)))
          
          (define end-box (level-state-end-box (world-state-level struct)))
          (define end-box-image (rectangle (hit-box-width end-box) (hit-box-height end-box) 'outline 'red))

          
          (define save-box (level-state-save-box (world-state-level struct)))
          (define save-box-image (rectangle (hit-box-width save-box) (hit-box-height save-box) 'outline 'green))] 
    
    (place-image (debugger struct)
                 80 100 
                 (place-image player-image
                              x y
                              (place-images (list end-box-image save-box-image)
                                            (list (make-posn (hit-box-x end-box) (hit-box-y end-box))
                                                  (make-posn (hit-box-x save-box) (hit-box-y save-box)))
                                            (place-images (generate-rectangles hit-boxes) (generate-posns hit-boxes) background)))))) ; final placement 


; list of hit-boxes -> list of posns
; generates a list of posns based upon the list of hit-boxes
(define (generate-posns hit-boxes)
  (map (lambda (hit-box) (make-posn (hit-box-x hit-box) (hit-box-y hit-box))) hit-boxes))
                    
                    
; list of hit-boxes -> list of rectangles  
; generates a list of rectangles based upon the list of hit-boxes 
(define (generate-rectangles hit-boxes)
  (map (lambda (hit-box) (rectangle (hit-box-width hit-box) (hit-box-height hit-box) 'solid 'blue)) hit-boxes))


; World State -> Image
; displays various values from the World State for debugging 
(define (debugger struct)
  (local [(define heading (player-heading (world-state-player struct)))
          (define desired-heading (player-desired-heading (world-state-player struct))) 
          (define y (posn-y (player-position (world-state-player struct))))
          (define x (posn-x (player-position (world-state-player struct))))
          (define ticks (world-state-ticks struct))
          (define w (keys-w (world-state-keyboard struct)))  
          (define a (keys-a (world-state-keyboard struct)))
          (define s (keys-s (world-state-keyboard struct)))
          (define d (keys-d (world-state-keyboard struct)))]
    
    (above (text (if w "W: #T" "W: #F") 12 "red")
           (text (if a "A: #T" "A: #F") 12 "red") 
           (text (if s "S: #T" "S: #F") 12 "red")
           (text (if d "D: #T" "D: #F") 12 "red") 
           (text (number->string x) 12 "orange")
           (text (number->string y) 12 "orange")
           
           (text (cond [(= heading 1)"Current Heading: N"]
                       [(= heading 2)"Current Heading: NE"]
                       [(= heading 3)"Current Heading: E"]
                       [(= heading 4)"Current Heading: SE"] 
                       [(= heading 5)"Current Heading: S"]  
                       [(= heading 6)"Current Heading: SW"]
                       [(= heading 7)"Current Heading: W"]
                       [(= heading 8)"Current Heading: NW"]) 12 "red")
           
           (text (cond [(= desired-heading 1)"Desired Heading: N"]
                       [(= desired-heading 2)"Desired Heading: NE"] 
                       [(= desired-heading 3)"Desired Heading: E"]
                       [(= desired-heading 4)"Desired Heading: SE"] 
                       [(= desired-heading 5)"Desired Heading: S"]
                       [(= desired-heading 6)"Desired Heading: SW"]
                       [(= desired-heading 7)"Desired Heading: W"]
                       [(= desired-heading 8)"Desired Heading: NW"]) 12 "yellow")
           
           (text (number->string ticks) 12 "green")))) 


; ******************************************************* initial states ************************************************
(define-struct level [hit-boxes start-position end-hitbox])

(define empty-level
  (list (make-hit-box 0 0 0 0)))

(define level0-hit-boxes
  (list (make-hit-box 100 500 50 250)
        (make-hit-box 800 100 400 50)    
        (make-hit-box 800 100 600 250)))
  
;contains each hit-box  
(define level1
  (list (make-hit-box 25 500 0 250) ; width height x y 
        (make-hit-box 800 25 400 0)  
        (make-hit-box 25 25 300 300)))  
 

(define level0-end-box (make-hit-box 80 40 900 40)) 

(define level0-save-box (make-hit-box 80 80 150 240))

(define level0-start-posn (make-posn 950 450))

(define level0 (make-level-state level0-hit-boxes level0-start-posn level0-end-box level0-save-box))

; the initial state of the playable character. 
(define initial-player (make-player WEST
                                    (make-posn 950 ; aligns player on the middle of x-axis
                                               450) ; aligns player on 90% of y-axis
                                    8  
                                    WEST)) ; initial speed
 
(define initial-keys (make-keys #f #f #f #f))    

(define initial-world-state (make-world-state initial-player initial-keys level0 0)) 

; ******************************************************* big bang ***********************************************

(big-bang initial-world-state  
  (on-tick tock)  
  (to-draw draw)
  (name "walmart celeste") 
  (state #f)
  (on-key press-handler)
  (on-release release-handler)) 
