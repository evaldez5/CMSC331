;#lang racket
(require racket/trace)
;//////////////////////////////////////////////
; Name : Edwin Valdez     Date: 04/24/2018
; /////////////////////////////////////////////
; This is a function that would help to calculate the volumen and area of a cone
; to consider x mean a radious and y means hight 
(define cone-facts
  ( lambda (x y)
   (let
	 (
		(pi 3.14)
   		(b 1/3)
   	)

     ; this formula calculates the volumen of a cone 
   (list  (* b (* pi (* (* x x) y)))
     ; this formula calculates the area of a cone
     (+ (* pi (* x x)) 
               (* (* pi x) 
                  (sqrt (+ (* x x ) (* y y)))))
     )
	)
  )
 )
;this functio rotates the first two numbers and put them at end of the list
; but those values would be swap
(define double-rotate
  (lambda (lst)
    ;(display (cdr lst))
    ; this portion of the code is use to take the first two value and swap them
    (append  (cddr lst) (list (cadr lst)) (list (car lst)))
   ; (append (cdr lst) (car lst) (cadr lst))
    ))
; Helper funtion that will would get the number of items in the list in a recursion form
(define gettingItems
    (lambda (lst num)
        (if (> num 0)
            (if (not(equal? lst null))
            (cons (car lst) (gettingItems (cdr lst) (- num 1)))
            '())
            '()
            )
      )
 )
; this is the main function that would show just the number in the range indicated
(define slice
    (lambda (lst start count)
        (if (> start 0)
            (slice (cdr lst) (- start 1) count)
            (gettingItems lst count))
      )
)
; this function would remove the number indicated from the list if it is there
(define remove-first
    (lambda (lst x)
      ;(display lst)
        (if (null? lst)                     ; If an empty list
            '()                             ; returning an empty list if the list is null 
            (if (equal? x (car lst))        ; else, if first item in list
                (cdr lst)                   ; returning the rest of list, after done
                (cons (car lst) (remove-first (cdr lst) x ))  ;else case 
                )
          )
      )
  )
; this function is used to determine is the number is a perfect square or not
(define perfect-squares
  (lambda (lst)
   (map (lambda(y) (* y y)) (filter integer? (map (lambda(x) (sqrt x)) lst)))
   )
)
; helper function that would help to find the average of the list then it would be used
; by my standar deviation function
(define average
  (lambda (lst)
  (/ (apply + lst) (length lst)))
)
; this is the formula for the standard deviation which would need the help of
; our function average 
(define sd
  (lambda (x)
   ; (map (lambda(y) (* y y))  (- x (average x)))
    (define avg (average x))               ; this is used to set a variable with the value of average
    ;(display "\n")
    (sqrt (/ 
           (apply + 
                  (map (lambda(s) (* s s)) 
                       (map (lambda(r) (- r avg)) x)))     ; calculate (x + mean)^2
           (- (length x) 1))
    )
  )
)
