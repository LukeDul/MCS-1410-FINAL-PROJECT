;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname main) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; RATE-OF-CHANGE is rate of change of the heading. 
(define RATE-OF-CHANGE 1)

; MOVE-DISTANCE is how many pixels the PLAYER moves per tick.
(define MOVE-DISTANCE 10)

; Player contains the coordinates and heading of the playable character.
; The heading is the direction & amount that the playable character will move on the x-axis.
; Note: the direction & amount that the player moves on the y-axis is derived from the heading with the function f(x) = MOVE-DISTANCE - |x|.
(define-struct player (x-heading y-heading x y))

; the width of the window 
(define WINDOW-WIDTH 500)

; the height of the window 
(define WINDOW-HEIGHT 500)


; Number -> Number
; Given a Number, num, returns zero if num is greater than MOVE-DISTANCE, otherwise returns num. 
; Ensures the heading never escapes the bounds of MOVE-DISTANCE.
(define (bind num)
  (if (> (abs num) MOVE-DISTANCE)
      0
      num))


; Structure, Key -> Structure 
; Given a key, input-key, and player structure returns the structure with the player's heading altered based upon input-key.
(define (key-handler struct input-key)
 (local ; local definitions for readability  
    [(define x-heading (player-x-heading struct))
     (define y-heading (player-y-heading struct))
     (define x (player-x struct))
     (define y (player-y struct))]
 
    (cond
      [(key=? input-key "a") ; if a is pressed 
       (make-player
        (bind (- x-heading RATE-OF-CHANGE))
        (bind (- MOVE-DISTANCE (abs (- x-heading RATE-OF-CHANGE)))); angles player to the left
        x
        y)] ; note: x and y are unchanged
      
      [(key=? input-key "d") ; if d is pressed 
       (make-player
        (bind (+ x-heading RATE-OF-CHANGE))
        (bind (- MOVE-DISTANCE (abs (+ x-heading RATE-OF-CHANGE)))); angles player to the right
        x
        y)]
      [else struct]))) ; note: x and y are unchanged


; Player Structure -> Player Structure
; Given a Player Structure, struct, returns the Player structure with its coordinates altered, based upon its heading.
; Tock runs every tick (~25 ticks per second)
(define (tock struct)
 (local ; local definitions for readability  
    [(define x-heading (player-x-heading struct))
     (define y-heading (player-y-heading struct))
     (define x (player-x struct))
     (define y (player-y struct))]
   
   (make-player
    x-heading ; note: heading is unchanged
    y-heading
    (+ x x-heading)
    (+ y y-heading)))) ; f(x) = MOVE-DISTANCE - |x| this ensures that the combined movement does not exceed MOVE-DISTANCE


; Player Structure -> Image
; Given the Player Structure, struct, returns an image of the playerable character at the Player Structure's coordinates. 
(define (draw struct)
  (local
    [(define x (player-x struct))
     (define y (player-y struct))
     
     (define player-image (circle 10 'solid 'red))
     (define background (rectangle WINDOW-WIDTH WINDOW-HEIGHT 'solid 'white))]
    
    (place-image player-image x y background)))


; the initial state of the playable character. 
(define initial-player (make-player
                        0
                        -4
                        (/ WINDOW-WIDTH 2)
                        (/ WINDOW-HEIGHT 2)))

(big-bang initial-player
  (on-tick tock)
  (to-draw draw)
  (state #f)
  (on-key key-handler))