;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname main) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

; STEP determines the rate of change of the heading 
(define RATE-OF-CHANGE 1)

; SPEED determines how many pixels the PLAYER moves per tick 
(define SPEED 4)

; player contains the coordinates and heading of the player
; the heading is the amount by which x will change 
(define-struct player (heading x y))


; Number -> Number
; Given a Number, num, returns zero if num is greater than SPEED, otherwise returns num. 
; Ensures the heading never escapes the bounds of SPEED.
(define (bind num)
  (if (> num SPEED)
      0
      num))


; Structure, Key -> Structure 
; Given a key, input-key, and player structure returns the structure with the player's heading altered based upon input-key.
(define (key-handler struct input-key)
 (local
    [(define heading (player-heading struct))
     (define x (player-x struct))
     (define y (player-y struct))]
 
    (cond
      [(key=? input-key "a") ; if a is pressed 
       (make-player
        (bind (- heading RATE-OF-CHANGE)) ; angles player to the left
        x
        y)] ; note x and y are unchanged
      
      [(key=? input-key "d") ; if d is pressed 
       (make-player
        (bind (+ heading RATE-OF-CHANGE)) ; angles player to the right
        x
        y)]))) ; note x and y are unchanged


; Player Structure -> Player Structure
; Given a Player Structure, struct, returns the Player structure with its coordinates altered, based upon its heading.
; Tock runs every tick (~25 ticks per second)
(define (tock struct)...)
  