(load "put-get.scm")
(load "types.scm")
(load "ps5-code.scm")
(define (inc x) (+ x 1))
;;; test equ? on numbers
(define n1 (create-number 2))
(define n2 (create-number 2))
(define n3 (create-number 5))
(display (equ? n1 n2))  ;#t
(newline)
(display (equ? n2 n3))  ;#f
(newline)

;;; Exercise 5.2
(define r5/13 (create-rational (create-number 5) (create-number 13)))
(define r2 (create-rational (create-number 2) (create-number 1)))
(define rsq (square (add r5/13 r2)))

;;; test equ? on rationals
(define r15/39 (create-rational (create-number 15) (create-number 39)))
(display (equ? r5/13 r15/39))  ;#t
(newline)
(display (equ? r5/13 r2))  ;#f
(newline)

;;; test equ?, add, sub on (rational number) and (number rational)
(display (equ? n2 r2))  ;#t
(newline)
(display (equ? (sub (add n2 r5/13) r5/13) n2))  ;#t
(newline)

;;; create-numerical-polynomial
(define p1 (create-numerical-polynomial 'x '(1 5 0 -2)))
(define p1^2 (square p1))
(define p1^4 (square p1^2))
;;; pretty-print a polynomial
(define (print-polynomial poly)
  (define (print-term var term)
    (begin (display (contents (coeff term)))
           (display var)
           (display "^")
           (display (order term))))
  (define (print-termlist var L)
    (if (null? L)
        'done
        (begin
          (print-term var (first-term L))
          (if (not (null? (rest-terms L)))
              (display "+")
              'ok)
          (print-termlist var (rest-terms L)))))
  (let ((var (cadr poly))
        (L (cddr poly)))
    (print-termlist var L))
  (newline))
;;; test multiplication of polynomial
(print-polynomial p1)    ; x^3+5x^2-2
(print-polynomial p1^2)  ; x^6+10*x^5+25*x^4−4*x^3−20*x^2+4
(print-polynomial p1^4)  ; x^12+20*x^11+150*x^10+492*x^9+505*x^8−600*x^7−976*x^6+240*x^5+600*x^4−32*x^3−160*x^2+16

