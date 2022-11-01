;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname main) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

; STEP determines the rate of change of the heading 
(define RATE-OF-CHANGE 1)

; SPEED determines how many pixels the PLAYER moves per tick 
(define SPEED 4)


; heading indicates the direction the PLAYER moves 
(define-struct heading (x y))

; player contains the coordinates and heading of the player
(define-struct player (heading x y))


; Structure, Key -> Structure
; given a key and player, returns the structure with the player's heading altered based upon the key 
(define (key-handler struct input-key)...)