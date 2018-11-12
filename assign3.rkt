;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname assign3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; CMPU-101 
; Fall 2018
; Assign 3
; Kristen Koenig
;
; Description: Uses a list of bouncing balls to animate many balls
; of different sizes and colors, all moving in the same scene at 
; different speeds.

(require 2htdp/image) 
(require 2htdp/universe)

(define RADIUS 25)

; Scene dimensions
(define WIDTH 500)
(define HEIGHT 300) 

; Create the background scene image
(define BACKGROUND
  (place-image (rectangle WIDTH HEIGHT "solid" "lightgray")
               (/ WIDTH 2) (/ HEIGHT 2)
               (empty-scene WIDTH HEIGHT)))

; Data Definitions 
(define-struct ball (im x y dx dy))
; A ball is a (make-ball im p dx dy) where
; im is an image (of the ball), 
; x and y are numbers representing the ball's position, and
; dx and dy are numbers representing the ball's horizontal and 
;   vertical velocity

; Data Definition for a list-of-balls:
; A list-of-balls is either:
; 1. '(), or
; 2. (cons b lob), where b is a ball
;    and lob is a list-of-balls

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define four (4) example ball CONSTANTS:
;   one touching each edge of the scene (top, bottom, left, right)
;   These will help you test bounce conditions.

; here's one of my ball CONSTANTS, which you may use or modify
; if you like to define the rest.
(define BALL-AT-LEFT 
  (make-ball (circle (+ RADIUS 14) "solid" "teal")
             (+ RADIUS 14) (/ HEIGHT 2) -1 -2))

(define BALL-AT-TOP
  (make-ball (circle (+ RADIUS 4) "solid" "red")
             (/ WIDTH 2) (+ RADIUS 4) 4 -2))
              
(define BALL-AT-BOTTOM
  (make-ball (circle (+ RADIUS 24) "solid" "yellow")
             (/ WIDTH 2) (- HEIGHT (+ RADIUS 24)) 3 6))

(define BALL-AT-RIGHT
  (make-ball (circle (- RADIUS 4) "solid" "purple")
             (- WIDTH (+ RADIUS 4)) (/ HEIGHT 2) 2 5))

; Define INIT-LOB to be a list-of-balls:
; You will use this to be the initial state of the world.
; I've defined it to be the empty list, but you should define it
; to contain the four example ball CONSTANTS you just defined. 
(define INIT-LOB
(list BALL-AT-LEFT
      BALL-AT-TOP
      BALL-AT-BOTTOM
      BALL-AT-RIGHT)) 
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Templates for a ball and a list-of-balls.
; Use these to help you get started with the functions below.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ball -> ???
; Template for a function that consumes a ball
;(define (fun-for-ball b) 
;  (...(ball-im b)...
;;   ...(ball-x b)...(ball-y b)...
 ;  ...(ball-dx b)...(ball-dy b)...))

; list-of-balls -> ???
; Template for a function that consumes a list-of-balls
;(define (fun-for-list-of-balls lob) 
;  (cond
 ;   [(empty? lob)...] 
 ;   [else (...(fun-for-ball (first lob))...
  ;         ...(fun-for-lob (rest lob))...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Design the functions below, in order. I've supplied the
; signature, purpose statement, and header for each function.
;
; You provide the check-expect examples, and using the appropriate
; template, complete the function bodies.
;
; I recommend you proceed in order, and complete each function,
; with passing tests, before going on to the next.
;
; The reason for completing the functions in the order they appear
; is earlier functions can be used as helper functions for the
; later functions.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ball -> number
; computes the radius of given ball
(define (ball-radius b)
  (/(image-height (ball-im b))2))



(check-expect (ball-radius (make-ball (circle (+ RADIUS 4) "solid" "teal")
             (+ RADIUS 4) (/ HEIGHT 2) -4 4)) 29)
(check-expect (ball-radius (make-ball (circle (+ RADIUS 15) "solid" "red")
             (/ WIDTH 2) (+ RADIUS 4) -4 4)) 40)
(check-expect (ball-radius (make-ball (circle RADIUS "solid" "red")
             (/ WIDTH 2) (+ RADIUS 4) -4 4)) 25)

; ball -> boolean
; determines whether the ball reached the top edge of scene
(define (top-edge? b)
  (<= (ball-y b) (ball-radius b)))



(check-expect (top-edge? (make-ball (circle (+ RADIUS 4) "solid" "red")
             (/ WIDTH 2) (+ RADIUS 4) -4 4)) #true)
(check-expect (top-edge? (make-ball (circle (+ RADIUS 4) "solid" "red")
             (/ WIDTH 2) (+ RADIUS 20) -4 4)) #false)
(check-expect (top-edge? (make-ball (circle (+ RADIUS 4) "solid" "red")
             (/ WIDTH 2) (+ RADIUS 17) -4 4)) #false)

; ball -> boolean
; determines whether the ball reached the bottom edge of scene
(define (bottom-edge? b)
   (>= (ball-y b) (- HEIGHT (ball-radius b))))



(check-expect (bottom-edge? (make-ball (circle (+ RADIUS 4) "solid" "yellow")
             (/ WIDTH 2) (- HEIGHT (+ RADIUS 4)) -4 4)) #true)
(check-expect (bottom-edge? (make-ball (circle (+ RADIUS 4) "solid" "red")
             (/ WIDTH 2) (+ RADIUS 20) -4 4)) #false)
(check-expect (bottom-edge? (make-ball (circle (+ RADIUS 4) "solid" "red")
             (/ WIDTH 2) (+ RADIUS 20) -4 4)) #false)

; ball -> boolean
; determines whether the ball reached the left edge of scene
(define (left-edge? b)
  (<= (ball-x b) (ball-radius b)))



(check-expect (left-edge? (make-ball (circle (+ RADIUS 4) "solid" "teal")
             (+ RADIUS 4) (/ HEIGHT 2) -4 4)) #true)
(check-expect (left-edge? (make-ball (circle (+ RADIUS 4) "solid" "teal")
             (+ RADIUS 30) (/ HEIGHT 2) -4 4)) #false)
(check-expect (left-edge? (make-ball (circle (+ RADIUS 4) "solid" "teal")
             (+ RADIUS 55) (/ HEIGHT 2) -4 4)) #false) 

; ball -> boolean
; determines whether the ball reached the right edge of scene
(define (right-edge? b)
  (>= (ball-x b) (- WIDTH (ball-radius b))))



(check-expect (right-edge? (make-ball (circle (+ RADIUS 4) "solid" "purple")
             (- WIDTH (+ RADIUS 4)) (/ HEIGHT 2) -4 4)) #true)
(check-expect (right-edge? (make-ball (circle (+ RADIUS 4) "solid" "purple")
             400 (/ HEIGHT 2) -4 4)) #false)
(check-expect (right-edge? (make-ball (circle (+ RADIUS 4) "solid" "purple")
             430 (/ HEIGHT 2) -4 4)) #false)

; ball -> ball
; reverse ball's up-down direction   
(define (reverse-up-down b)
  (make-ball (ball-im b)(ball-x b)(ball-y b)(ball-dx b)(-(ball-dy b))))



(check-expect (reverse-up-down (make-ball (circle (+ RADIUS 4) "solid" "purple")
             400 300 -4 4)) (make-ball (circle (+ RADIUS 4) "solid" "purple")
             400 300 -4 -4))
(check-expect (reverse-up-down (make-ball (circle (+ RADIUS 4) "solid" "purple")
             400 300 -4 16)) (make-ball (circle (+ RADIUS 4) "solid" "purple")
             400 300 -4 -16))
(check-expect (reverse-up-down (make-ball (circle (+ RADIUS 4) "solid" "purple")
             400 300 -4 -8)) (make-ball (circle (+ RADIUS 4) "solid" "purple")
             400 300 -4 8))

; ball -> ball
; reverse ball's left-right direction   
(define (reverse-left-right b)
   (make-ball (ball-im b)(ball-x b)(ball-y b)(-(ball-dx b))(ball-dy b)))



(check-expect (reverse-left-right (make-ball (circle (+ RADIUS 4) "solid" "purple")
             400 300 -4 4)) (make-ball (circle (+ RADIUS 4) "solid" "purple")
             400 300 4 4))
(check-expect (reverse-left-right (make-ball (circle (+ RADIUS 4) "solid" "purple")
             400 300 16 -4)) (make-ball (circle (+ RADIUS 4) "solid" "purple")
             400 300 -16 -4))
(check-expect (reverse-left-right (make-ball (circle (+ RADIUS 4) "solid" "purple")
             400 300 -8 -4)) (make-ball (circle (+ RADIUS 4) "solid" "purple")
             400 300 8 -4)) 

; ball -> ball
; changes direction of given ball if it hit the top or bottom edge
(define (bounce-up-down b)
  (cond
    [(top-edge? b)(reverse-up-down b)]
    [(bottom-edge? b) (reverse-up-down b)]
    [else b]))
                                       

(check-expect (bounce-up-down (make-ball (circle (+ RADIUS 4) "solid" "red")
                                         (/ WIDTH 2) (+ RADIUS 4) -4 4))
              (make-ball (circle (+ RADIUS 4) "solid" "red")(/ WIDTH 2) (+ RADIUS 4) -4 -4))
(check-expect (bounce-up-down (make-ball (circle (+ RADIUS 4) "solid" "yellow")
                                         (/ WIDTH 2) (- HEIGHT (+ RADIUS 4)) -4 4))
              (make-ball (circle (+ RADIUS 4) "solid" "yellow")(/ WIDTH 2) (- HEIGHT (+ RADIUS 4)) -4 -4))
 
; ball -> ball
; changes direction of given ball if it hit the left or right edge
(define (bounce-left-right b)
  (cond
    [(right-edge? b)(reverse-left-right b)]
    [(left-edge? b)(reverse-left-right b)]
    [else b]))


(check-expect (bounce-left-right (make-ball (circle (+ RADIUS 4) "solid" "teal")
             (+ RADIUS 4) (/ HEIGHT 2) -4 4)) (make-ball (circle (+ RADIUS 4) "solid" "teal")
             (+ RADIUS 4) (/ HEIGHT 2) 4 4))

(check-expect (bounce-left-right (make-ball (circle (+ RADIUS 4) "solid" "teal")
             (+ RADIUS 4) (/ HEIGHT 2) -4 4)) (make-ball (circle (+ RADIUS 4) "solid" "teal")
             (+ RADIUS 4) (/ HEIGHT 2) 4 4))                                              

; ball -> ball
; moves the given ball by its dx and dy amounts
(define (move-ball b)
  (make-ball (ball-im b) (+ (ball-x b) (ball-dx b)) (+ (ball-y b) (ball-dy  b))
             (ball-dx b) (ball-dy b)))

(check-expect (move-ball (make-ball (circle (+ RADIUS 4) "solid" "teal")
             (+ RADIUS 4) (/ HEIGHT 2) -4 4)) (make-ball (circle (+ RADIUS 4) "solid" "teal")
             25 154 -4 4))
(check-expect (move-ball (make-ball (circle (+ RADIUS 4) "solid" "teal")
             (+ RADIUS 4) (/ HEIGHT 2) 10 -10)) (make-ball (circle (+ RADIUS 4) "solid" "teal")
             39 140 10 -10)) 

; list-of-balls -> list-of-balls
; moves (and possibly bounces) each ball in given list
(define (move-list-of-balls lob)
  (cond
    [(empty? lob) '()]  
    [else (cons (move-ball (bounce-up-down (bounce-left-right (first lob))))
                (move-list-of-balls (rest lob)))]))    
 

(check-expect (move-list-of-balls (list (make-ball (circle 29 "solid" "teal")(+ RADIUS 4) (/ HEIGHT 2) 10 -10) (make-ball (circle 29 "solid" "teal")(+ RADIUS 4) (/ HEIGHT 2) 10 -10)))
              (list (make-ball (circle 29 "solid" "teal")19 140 -10 -10) (make-ball (circle 29 "solid" "teal")19 140 -10 -10)))
(check-expect (move-list-of-balls '()) '())

; ball image -> image
; renders given ball b on given background bg
(define (render-ball b bg)
  (place-image (ball-im b)(ball-x b)(ball-y b) bg))

(check-expect (render-ball (make-ball (circle 29 "solid" "teal") 29 (/ HEIGHT 2) 10 -10) BACKGROUND)
              (render-ball (make-ball (circle 29 "solid" "teal") 29 (/ HEIGHT 2) 10 -10) BACKGROUND))

  
; list-of-balls -> image 
; produces image of each ball at each given current position on
; background.
; (Yes, I provided this function for you! You shouldn't have to
;  touch it if you've correctly implemented the functions above.)
(define (render-balls lob) 
  (cond [(empty? lob) BACKGROUND]
        [else (render-ball (first lob)
                           (render-balls (rest lob)))]))

(check-expect (render-balls '()) BACKGROUND)
(check-expect (render-balls (list (make-ball (circle 29 "solid" "teal") 29 (/ HEIGHT 2) 10 -10)))
             (render-ball(make-ball (circle 29 "solid" "teal") 29 (/ HEIGHT 2) 10 -10) BACKGROUND))  

; Here's the main function with the big-bang expression!
; Once you've implemented move-list-of-balls, uncomment on-tick below.
(define (main w)
  (big-bang w
            (on-tick move-list-of-balls 1/28) 
            (to-draw render-balls)))

; Run program automatically, or type this in Interactions Pane:
; Use INIT-LOB as the initial state of the world...
(main INIT-LOB)