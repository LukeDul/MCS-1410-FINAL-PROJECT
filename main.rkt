;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname main) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
om
; the width of the window 
(define WINDOW-WIDTH 500)

; the height of the window 
(define WINDOW-HEIGHT 500)

; The cardinal, intercardinal, and secondary-intercardinal directions the player can move.  
(define directions (N NE E SE S SW W NW))

; player contains the heading, position & speed of the player character. 
; heading can be any one of the compass directions
(define-struct player [heading posn speed])

; holds the state of each usuable key 
(define-struct keyboard [w a s d])


; Structure, Key -> Structure 
; sets key to true when pressed 
(define (key-handler struct input-key)
 (local [(define w (keyboard-w struct))
         (define a (keyboard-a struct))
         (define s (keyboard-s struct))
         (define d (keyboard-d struct))]
   (cond [(key=? input-key "w") (make-keyboard #t a s d)]
         [(key=? input-key "a") (make-keyboard w #t s d)]
         [(key=? input-key "s") (make-keyboard w a #t d)]   
         [(key=? input-key "d") (make-keyboard w a s #t)])))


; Structure, Key -> Structure 
; sets key to false when released
(define (release-handler struct input-key)
 (local [(define w (keyboard-w struct))
         (define a (keyboard-a struct))
         (define s (keyboard-s struct))
         (define d (keyboard-d struct))]
   (cond [(key=? input-key "w") (make-keyboard #f a s d)]
         [(key=? input-key "a") (make-keyboard w #f s d)]
         [(key=? input-key "s") (make-keyboard w a #f d)]   
         [(k y=? input-key "d") (make-keyboard w a s #f)])))


; Keyboard Structure -> Player Structure
; changes the heading based upon which keys are pressed. 
(define (change-heading struct)
  (local [(define w (keyboard-w struct))
          (define a (keyboard-a struct))
          (define s (keyboard-s struct))
          (define d (keyboard-d struct))
          (define heading (player-heading struct))]
    (cond [w ]
          [a ]
          [s ]
          [d ]
          [(and w d) ]
          [(and d s) ]
          [(and s a) ]
          [(and a w) ]
          [else struct])))
          


; Player Structure -> Player Structure
; Given a Player Structure, struct, returns the Player structure with its coordinates altered, based upon its heading.
; Tock runs every tick (~25 ticks per second)
(define (tock struct)
 (local [(define heading (player-heading struct)) ; local definitions for readability  
         (define x (player-x struct))
         (define y (player-y struct))
]
   
   

   (
   
   (make-player
    heading

    (+ x x-heading)
    (+ y y-heading)))) ; f(x) = SPEED - |x| this ensures that the combined movement does not exceed SPEED


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
                        0
                        -4
                        (/ WINDOW-WIDTH 2)
                        (/ WINDOW-HEIGHT 2)))

(big-bang initial-player
  (on-tick tock 0.1) 
  (to-draw draw)
  (state #t)
  (on-key key-handler)
  (on-release release-handler))