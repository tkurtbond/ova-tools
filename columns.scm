(import (scheme))

(import (chicken base))
(import (chicken io))
(import (chicken port))
(import (chicken process-context))
(import (chicken sort))
(import (chicken string))
(import (chicken pretty-print))

(import args)
(import bindings)
(import (schemepunk show))
(import loop)
(import matchable)
(import (srfi 1))
(import (srfi 152))
(import yaml)          ; The yaml egg uses libyaml, which is yaml 1.1.

(show #t (columnar "/* " (displayed "abc\ndef\n")
                   " | " (displayed "123\n456\n")
                   " */"))

(show #t (tabular
          "|" (joined displayed '("+1" "+2" "+3" "+4") "\n")
          "|" (joined displayed '("A1" "A2" "A3" "A4") "\n")
          "|" (joined displayed '(#\a #\b #\c #\d #\e) "\n")
          "|"))



(define abilities
  '(("+2" "Companion (Azyrus)")
    ("+2" "Cute!")
    ("+1" "Iron-Willed")
    ("+2" "Knowledge (Shoujo Manga)")
    ("+2" "Quick")
    ("+2" "Shape-Shifter")
    ("+4" "Transformation")))

(define weaknesses
  '(("-2" "Ageism")
    ("-2" "Crybaby")
    ("-1" "Easily Distracted")
    ("-2" "Focus (Powers Require Neko Transformation Locket)")
    ("-1" "Frail")
    ("-1" "Naive")))

(show #t (tabular "|" (joined displayed (map (lambda (x) (car x)) abilities) "\n")
                  "|" (joined displayed (map (lambda (x) (cadr x)) abilities) "\n")
                  "|" (joined displayed (map (lambda (x) (car x)) weaknesses) "\n")
                  "|" (joined displayed (map (lambda (x) (cadr x)) weaknesses) "\n")
                  "|"))

(map (lambda (x) (show #f (with ((width 20)) (wrapped (cadr x))))) weaknesses)


(show #t (tabular "|" (joined displayed (map (lambda (x) (car x)) abilities) "\n")
                  "|" (joined displayed (map (lambda (x) (cadr x)) abilities) "\n")
                  "|" (joined displayed (map (lambda (x) (car x)) weaknesses) "\n")
                  "|" (joined displayed (map (lambda (x) (show #f (with ((width 40)) (wrapped (cadr x))))) weaknesses) "\n")
                  "|"))

(loop for item
      in (map (lambda (x) (show #f (with ((width 40)) (wrapped (cadr x))))) weaknesses) collect (string-count item (lambda (c) (char=? c #\newline))))

(loop with lengths = '()
      with others = '()
      for item in
      (map (lambda (x) (show #f (with ((width 40)) (wrapped (cadr x))))) weaknesses)
      do (loop with n = (string-count item (lambda (c) (char=? c #\newline)))
               collect n into lengths
               do (loop repeat n collect 'x into others)))


(define l (map (lambda (x) (show #f (with ((width 40)) (wrapped (cadr x))))) weaknesses))

l

(define (tryit l1 l2)
  (define (lines s)
    (string-count s (lambda (c) (char=? c #\newline))))
  (define (recurse l1 l2 nl1 nl2)
      (if (null? l1)
          (cons nl1 (cons 
          (cons nl1 nl2)
      (if (= dif 0)
          (recurse (cdr
      ))))))))
(let* ((i1 (car l1))                ; item of the first list
       (i2 (car l2))                ; item of the second list
       (n1 (lines i1))              ; number of lines of first list's item
       (n2 (lines i2))              ; number of lines of second list's item
       (dif (- n1 n2))
       )
           
      
      

      
(loop for i from 1 to 3 collect (loop for j from 1 to 4 collect (cons i j)))

(loop for i from 1 to 3
      for j from 1 to 4 collect (cons i j))


(define (lines s)
  (string-count s (lambda (c) (char=? c #\newline))))


(define l1 '("1" "2" "3" "4"))
(define l2 '("a" "b\nc" "d" "e\nf"))
(define l3 (map (lambda (s) (lines s)) l2))

(loop for i in l1
      for j in l2
      for k in l3
      if (= k 0) collect (list i j j))
