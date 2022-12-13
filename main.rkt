;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname main) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require racket/format)

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

; open? is a boolean
(define-struct menu [button-list open?])

(define-struct button [width height x y])

; List, Posn, hit-box, hit-box 
(define-struct level-state [hit-boxes start-posn end-box save-box running?])

; worldstate 
(define-struct world-state [player keyboard level menu ticks]) 

(define NORTH 1)
(define NE 2)
(define EAST 3) 
(define SE 4)
(define SOUTH 5)
(define SW 6)
(define WEST 7)
(define NW 8) 
 
;******************************************************* MOUSE-HANDLING *******************************************************


; World-State, Number, Number, Mouse-Event -> World-State
(define (mouse-handler struct mouse-x mouse-y mouse-event)
  (local [(define start-button (first (menu-button-list (world-state-menu struct))))
          (define level0-button (second (menu-button-list (world-state-menu struct))))
          (define level1-button (third (menu-button-list (world-state-menu struct))))
          (define level2-button (fourth (menu-button-list (world-state-menu struct))))
          (define start-posn (level-state-start-posn (world-state-level struct)))
          (define speed (player-speed (world-state-player struct)))] 
    
    (cond [(and (point-button-collision? (make-posn mouse-x mouse-y) start-button)
                (mouse=? mouse-event "button-down"))
           (toggle-gameplay/menu struct #true #false)]  

          ;
          [(and (point-button-collision? (make-posn mouse-x mouse-y) level0-button)
                (mouse=? mouse-event "button-down"))
           (toggle-gameplay/menu (switch-level (update-player-and-ticks struct (world-state-ticks struct) (make-player NORTH start-posn speed NORTH)) level0) #true #false)]
          
          [(and (point-button-collision? (make-posn mouse-x mouse-y) level1-button)
                (mouse=? mouse-event "button-down"))
           (toggle-gameplay/menu (switch-level (update-player-and-ticks struct (world-state-ticks struct) (make-player NORTH start-posn speed NORTH)) level1) #true #false)]

          [(and (point-button-collision? (make-posn mouse-x mouse-y) level2-button)
                (mouse=? mouse-event "button-down"))
           (toggle-gameplay/menu (switch-level (update-player-and-ticks struct (world-state-ticks struct) (make-player NORTH start-posn speed NORTH)) level2) #true #false)] 
          [else struct])))


;******************************************************* MENU-HANDLING *******************************************************

(define (switch-level struct level)
  (make-world-state (world-state-player struct)
                    (world-state-keyboard struct)
                    level
                    (world-state-menu struct)
                    (world-state-ticks struct))) 


; World-State, Boolean, Boolean -> World-State
; start/stops gameplay and opens/closes menu 
(define (toggle-gameplay/menu struct gameplay-toggle menu-toggle)
  (make-world-state (world-state-player struct)
                    (world-state-keyboard struct)
                    (toggle-gameplay (world-state-level struct) gameplay-toggle)
                    (toggle-menu (world-state-menu struct) menu-toggle) 
                    (world-state-ticks struct))) 


; Menu, Boolean -> Menu
; opens or closes a menu  
(define (toggle-menu struct open/close)
  (make-menu (menu-button-list struct)
             open/close))


; Level-State, Boolean -> Level-State
; starts or stops gameplay
(define (toggle-gameplay struct start/stop)
  (make-level-state (level-state-hit-boxes struct)
                    (level-state-start-posn struct)
                    (level-state-end-box struct) 
                    (level-state-save-box struct)
                    start/stop))


; Posn, Button, -> Boolean
; Given a point and a button returns whether button contains the point using inequalities 
(define (point-button-collision? point button)
  (if (and
       (<= (posn-x point) (+ (button-x button) (/ (button-width button) 2)))
       (>= (posn-x point) (- (button-x button) (/ (button-width button) 2)))
       (<= (posn-y point) (+ (button-y button) (/ (button-height button) 2)))
       (>= (posn-y point) (- (button-y button) (/ (button-height button) 2))))
      #t 
      #f))  

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
        ; [(key=? input-key "escape") ] ; open menu 
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
        [(key=? input-key "escape") (toggle-gameplay/menu struct #false #true )] ; open menu  
         [else struct]))) 


; World State, Keys -> World State
; makes a new world state with the given keyboard state 
(define (update-keys struct new-keys)
  (make-world-state (world-state-player struct)
                    new-keys
                    (world-state-level struct)
                    (world-state-menu struct)
                    (world-state-ticks struct)))




;******************************************************* TOCK *******************************************************

(define (ender struct)
   (local [(define current-player-position (player-position (world-state-player struct)))
           (define end-box (level-state-end-box (world-state-level struct)))]
  (if (colliding? current-player-position PLAYER-HITBOX (list end-box)) #t #f)))

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
         (define start-posn (level-state-start-posn (world-state-level struct)))
         (define running? (level-state-running? (world-state-level struct)))
         
         (define button (menu-button-list (world-state-menu struct)))
         (define open? (menu-open? (world-state-menu struct)))
 

         ; increments ticks 
         (define ticks (+ 1 (world-state-ticks struct)))

         ; turns the heading towards the desired heading 
         (define new-heading (turn-heading current-heading current-desired-heading NW NORTH))

         ; Changes desired-heading based upon which keys are pressed.
         (define new-desired-heading (desired-heading-handler keyboard-state current-desired-heading))

         ; Changes the players position based upon the current heading, position and speed.
         (define new-player-position (change-position current-heading current-player-position speed))]

 
   (cond [(false? running?) struct]
         
         ;---------------------------------------- Game-play ----------------------------------
         
               ; if player is colliding w/ a hitbox move player to start coordinates  
         [else (cond [(colliding? current-player-position PLAYER-HITBOX hit-boxes)
                      (update-player-and-ticks struct ticks (make-player NORTH start-posn speed NORTH))] 

                ; if player is colliding w/ the end box open level select
                [(colliding? current-player-position PLAYER-HITBOX (list end-box))
                 (toggle-gameplay/menu (update-player-and-ticks struct ticks (make-player NORTH start-posn speed NORTH)) #false #true )]
                
                 ;(update-player-and-ticks struct ticks (make-player current-heading current-player-position speed current-heading))]

                ; if player is colliding w/ the save box set start coordinates to save-box coordinates & continue movement 
                [(colliding? current-player-position PLAYER-HITBOX (list save-box))
                 (make-world-state   (cond [(<= TURN-CONSTANT ticks)
                                            (make-player new-heading new-player-position speed new-desired-heading)]
                                           [else
                                            (make-player current-heading new-player-position speed new-desired-heading)])
                                     
                                     (world-state-keyboard struct)
                                     (make-level-state hit-boxes (make-posn (hit-box-x save-box) (hit-box-y save-box)) end-box save-box running?)
                                     (world-state-menu struct)
                                     (if (<= TURN-CONSTANT ticks) 0 ticks))] ; reset ticks  
          
                ; otherwise continue movement  
                [else (cond [(>= ticks TURN-CONSTANT)
                             (update-player-and-ticks struct 0 (make-player new-heading new-player-position speed new-desired-heading))]
                            [else
                             (update-player-and-ticks struct ticks (make-player current-heading new-player-position speed new-desired-heading))])])])))


(define TURN-CONSTANT 2)    


; World State, Number, Player State -> World State
; short-hand function for updating the Player State and Ticks within the World State
(define (update-player-and-ticks struct ticks new-player-state)
  (make-world-state new-player-state
                    (world-state-keyboard struct)
                    (world-state-level struct)
                    (world-state-menu struct)
                    ticks))


;******************************************************* AHHHHHHH *******************************************************


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
; Counts the number of left turns required to reach a desired heading.
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
          [else (print "uh oh")]))) 


; Number, Number -> Number
; returns the given percentage of a number 
(define (percentage percent number)
  (* (/ percent 10) (/ number 10))) 



;======================================================== |COLLISION HELPERS| ============================================

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

;========================================================================================================================
;======================================================== |RENDERING| ===================================================
;========================================================================================================================

; World-State -> Image
; Given the Player Structure, struct, returns an image of the playerable character at the Player Structure's coordinates. 
(define (draw struct)
         ; SHORT-HAND ---------------------------------------------------------------------------------------------------
  (local [(define x (posn-x (player-position (world-state-player struct)))) 
          (define y (posn-y (player-position (world-state-player struct))))
          (define hit-boxes (level-state-hit-boxes (world-state-level struct)))
          (define end-box (level-state-end-box (world-state-level struct)))
          (define save-box (level-state-save-box (world-state-level struct)))
          (define gaming? (level-state-running? (world-state-level struct)))
          (define menu-status (menu-open? (world-state-menu struct)))

          ;buttons
         (define start-button (first (menu-button-list (world-state-menu struct))))
          (define level0-button (second (menu-button-list (world-state-menu struct))))
          (define level1-button (third (menu-button-list (world-state-menu struct))))
          (define level2-button (fourth (menu-button-list (world-state-menu struct))))
          
          ; IMAGES ------------------------------------------------------------------------------------------------------
          (define start-button-img (overlay 
                                    (text "START" 18 'black)
                                    (rectangle (button-width start-button) (button-height start-button) 'solid 'gray)))

          (define level0-button-img (overlay
                                       (text "LEVEL 1" 18 'black)
                                       (rectangle (button-width level0-button) (button-height level0-button) 'solid 'darkgray)))
            
          (define level1-button-img (overlay
                                       (text "LEVEL 2" 18 'black)
                                       (rectangle (button-width level1-button) (button-height level1-button) 'solid 'darkgray)))
            
          (define level2-button-img (overlay 
                                       (text "LEVEL 3" 18 'black)
                                       (rectangle (button-width level2-button) (button-height level2-button) 'solid 'darkgray)))
          
          
          (define player-image (overlay  (circle PLAYER-HITBOX 90 'orange) (circle 20 20 'red)))  ; (* PLAYER-HITBOX (sqrt 2))  
          (define background (rectangle WINDOW-WIDTH WINDOW-HEIGHT 'solid 'black))
          (define title (text "WALMART CELESTE" 40 'black))
          (define start-background (place-image title 500 150 (rectangle WINDOW-WIDTH WINDOW-HEIGHT 'solid 'dimgray)))
          (define end-box-image (rectangle (hit-box-width end-box) (hit-box-height end-box) 40 'gold))
          (define save-box-image (rectangle (hit-box-width save-box) (hit-box-height save-box) 30 'green))
          (define TITLE-SCREEN (place-images (list start-button-img
                                                   level0-button-img
                                                   level1-button-img
                                                   level2-button-img)
                                             
                                             (list (make-posn (button-x start-button) (button-y start-button))
                                                   (make-posn (button-x level0-button) (button-y level0-button))
                                                   (make-posn (button-x level1-button) (button-y level1-button))
                                                   (make-posn (button-x level2-button) (button-y level2-button)))
                                             start-background))
          
          (define GAMING-SCREEN (place-image player-image x y (place-images (list end-box-image save-box-image)
                                                                            (list (make-posn (hit-box-x end-box) (hit-box-y end-box))
                                                                                  (make-posn (hit-box-x save-box) (hit-box-y save-box)))
                                                                            (draw-hit-boxes hit-boxes background))))]   
    ; DRAW -------------------------------------------------------------------------------------------------------------- 
    (place-image (debugger struct)
                 80 100
                 (if (false? gaming?) TITLE-SCREEN GAMING-SCREEN))))



; List of Hit-Boxes, Image -> Image
; places an image of each hit-box on a background image 
(define (draw-hit-boxes hit-boxes background)
  (place-images (generate-rectangles hit-boxes) (generate-posns hit-boxes) background))


; List of Hit-Boxes -> List of Posns
; generates a list of posns based upon the list of hit-boxes
(define (generate-posns hit-boxes)
  (map (lambda (hit-box) (make-posn (hit-box-x hit-box) (hit-box-y hit-box))) hit-boxes))
                    
                    
; List of Hit-Boxes -> List of Rectangles   
; generates a list of rectangles based upon the list of hit-boxes 
(define (generate-rectangles hit-boxes)
  (map (lambda (hit-box) (rectangle (hit-box-width hit-box) (hit-box-height hit-box) "solid" "dim gray")) hit-boxes))

;======================================================== |Debugger| ===================================================

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
          (define d (keys-d (world-state-keyboard struct)))
          (define gameplay-status (level-state-running? (world-state-level struct)))
          (define menu-status (menu-open? (world-state-menu struct)))]
    
    (above (text (if w "W: #T" "W: #F") 12 "red")
           (text (if a "A: #T" "A: #F") 12 "red") 
           (text (if s "S: #T" "S: #F") 12 "red")
           (text (if d "D: #T" "D: #F") 12 "red") 
           (text (number->string x) 12 "orange")
           (text (number->string y) 12 "orange")
           (text (if gameplay-status "gameplay running" "gameplay not running") 12 "green")
           (text (if menu-status "menu open" "menu closed") 12 "yellow")
           
           (text (cond [(= heading NORTH) "Current Heading: NORTH"] 
                       [(= heading NE) "Current Heading: NE"]
                       [(= heading EAST) "Current Heading: EAST"]
                       [(= heading SE) "Current Heading: SE"] 
                       [(= heading SOUTH) "Current Heading: SOUTH"]  
                       [(= heading SW) "Current Heading: SW"]
                       [(= heading WEST) "Current Heading: WEST"]
                       [(= heading NW) "Current Heading: NW"]) 12 "red")
           
           (text (cond [(= desired-heading NORTH) "Desired Heading: NORTH"]
                       [(= desired-heading NE) "Desired Heading: NE"] 
                       [(= desired-heading EAST) "Desired Heading: EAST"]
                       [(= desired-heading SE) "Desired Heading: SE"] 
                       [(= desired-heading SOUTH) "Desired Heading: SOUTH"]
                       [(= desired-heading SW) "Desired Heading: SW"]
                       [(= desired-heading WEST) "Desired Heading: WEST"]
                       [(= desired-heading NW) "Desired Heading: NW"]) 12 "yellow")
           
           (text (number->string ticks) 12 "green"))))  

;========================================================================================================================
;======================================================== |INITIAL STATES| ==============================================
;========================================================================================================================

(define empty-level
  (list (make-hit-box 0 0 0 0))) 

(define level0-hit-boxes
  (list (make-hit-box 100 500 50 250)
        (make-hit-box 800 100 400 50)    
        (make-hit-box 800 100 600 250)
        (make-hit-box 1000 40 500 490); width height x y 
        
        (make-hit-box 50 100 700 425)
        (make-hit-box 50 130 500 360)
        (make-hit-box 50 130 400 435)
        (make-hit-box 50 130 300 360)
        (make-hit-box 75 115 180 380) 
        
        (make-hit-box 270 30 360 186)
        (make-hit-box 230 26 360 160)
        (make-hit-box 230 20 360 105)

        (make-hit-box 270 30 660 110)
        (make-hit-box 230 26 660 135)
        (make-hit-box 230 20 660 195)

        (make-hit-box 20 500 1000 250)
        (make-hit-box 500 20 900 0)))    
  

(define level0-end-box (make-hit-box 190 60 895 40))  

(define level0-save-box (make-hit-box 100 45 150 300))

(define level0-start-posn (make-posn 950 450)) 

(define level0 (make-level-state level0-hit-boxes level0-start-posn level0-end-box level0-save-box #false))

;contains each hit-box  
(define level1-hit-boxes
  (list (make-hit-box 25 500 0 250) 
        (make-hit-box 800 25 400 0)  
        (make-hit-box 25 25 300 300)))

(define level1-end-box (make-hit-box 190 60 895 40))  

(define level1-save-box (make-hit-box 100 45 150 300)) 

(define level1-start-posn (make-posn 950 450))  

(define level1 (make-level-state level1-hit-boxes level1-start-posn level1-end-box level1-save-box #false))

;contains each hit-box  
(define level2-hit-boxes
  (list (make-hit-box 25 500 0 250) 
        (make-hit-box 800 25 400 0)  
        (make-hit-box 25 25 300 300)
        (make-hit-box 50 200 900 400)
        ))

(define level2-end-box (make-hit-box 190 60 895 40))   

(define level2-save-box (make-hit-box 100 45 150 300))

(define level2-start-posn (make-posn 950 450)) 

(define level2 (make-level-state level2-hit-boxes level2-start-posn level2-end-box level2-save-box #false))

; the initial state of the playable character. 
(define initial-player (make-player WEST
                                    level0-start-posn
                                    8  
                                    WEST)) ; initial speed
  
(define initial-keys (make-keys #f #f #f #f))


; ------------------------ TITLE MENU ---------------------------

(define start-button (make-button 100 60 500 250))  

(define level-0-button (make-button 100 60 380 320)) 

(define level-1-button (make-button 100 60 500 320))

(define level-2-button (make-button 100 60 620 320)) 

(define title-menu-buttons (list start-button level-0-button level-1-button level-2-button)) 

(define title-menu (make-menu title-menu-buttons #true)) 


(define initial-world-state (make-world-state initial-player initial-keys level0 title-menu 0))  

;======================================================= |THE BIG-BANG| ==================================================

;(define (last-scene struct)
 ; (overlay (above (text "You have won." 50 'GREEN) (text " Congragulations v( ‘.’ )v" 50 'GREEN)) (rectangle 1000 500 'solid 'dimgray)))

(big-bang initial-world-state  
  (on-tick tock)  
  (to-draw draw)
  (on-mouse mouse-handler)
  (name "walmart celeste")  
  (state #f)
 ; (stop-when  ender last-scene)
  (on-key press-handler)
  (on-release release-handler))   
 
#| TO DO

ONLY STARAT MOVING ONCE A KEY IS PRESSED 

Menus
 - Title Menu
   - Start Button -> Level 1
   - Continue Button -> Last Save Point  
   - Level Select Button
 
 - Level Selector Menu
   - Level 1
   - Level 2
   - Level 3
 
 - Escape Menu
   - Exit -> Title
   - Exit -> Level Select
 
 - Win Screen / Credits 


Save State
  - Holds Last Save Point
  - Completed Levels 

LEVEL SELECT


|#
  
