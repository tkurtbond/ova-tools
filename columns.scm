;;; I think this has proved that monadic formatting can't generate the correct HTML tables..
;;;
;;; Specifically, for HTML, its columns divide things into characters
;;; and lines, but recursive OVA things like Magic (Arcane and
;;; Witchcraft), Transformation, and so forth need cells that can
;;; contain tables.  What I got, rather than one <tr> that contains a
;;; table, is one <tr> for each line inn the subtable.  Sigh.

;;; For text output, it has no way to handle subtables, and if you try
;;; to wrap things, the costs get out of sync with the descriptions,
;;; since the wrapping adds extra lines to the descriptions.  Notice
;;; that the +2 from Tough ends up on the first line of the
;;; Tranformation definition, and the -1 from Frail ends up on the
;;; first line of the wrapped Focus entry, while the -2 from Naive
;;; ends up on Frail.

;;; Possibly I could check the attributes and weaknesses for
;;; descriptions that have multiple lines, and add padding empty cells
;;; to the costs.

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



(begin                                  ; HTML
(define transformation
  '(("+2" "Attack")
    ("+3" "Barrier (AREA EFFECT; ELOBORATE GESTURES; 5 END)")
    ("+2" "Combat Expert")
    ("+2" "Healer")
    ("-1" "Bizarre Appearance (Cat Features)")))

(define abilities
  `(("+2" "Companion (Azyrus)")
    ("+2" "Cute!")
    ("+1" "Iron-Willed")
    ("+2" "Knowledge (Shoujo Manga)")
    ("+2" "Quick")
    ("+2" "Shape-Shifter")
    ("+4" ,(string-append "Transformation" "\n"
                          (show #f
                                (with ((width 20))
                                  "<table>\n"
                                  (columnar "<tr><td>" (joined displayed (map (lambda (x) (car x)) transformation) "\n")
                                            "</td><td>" (joined displayed (map (lambda (x) (cadr x)) transformation) "\n")
                                            "</td></tr>")
                                  "</table>\n"))))
    ("+2" "Tough")
    ))

(display (cadr (list-ref abilities 6)))

(define weaknesses
  '(("-2" "Ageism")
    ("-2" "Crybaby")
    ("-1" "Easily Distracted")
    ("-2" "Focus (Powers Require Neko Transformation Locket)")
    ("-1" "Frail")
    ("-2" "Naive")))

;; This outputs a working HTML table.
(show #t
      (with ((width 20))
      "<table>\n"
      "<tr><th>Lvl<th><th>Attributes</th><th>Lvl</th><th>Weaknesses</th></tr>\n"
      (columnar "<tr><td>" (joined displayed (map (lambda (x) (car x)) abilities) "\n")
                "</td><td>" (joined displayed (map (lambda (x) (cadr x)) abilities) "\n")
                "</td><td>" (joined displayed (map (lambda (x) (car x)) weaknesses) "\n")
                "</td><td>" (joined displayed (map (lambda (x) (cadr x)) weaknesses) "\n")
                "</td></tr>")
      "</table>\n"))
)

(begin                                  ; Text
(define transformation
  '(("+2" "Attack")
    ("+3" "Barrier (AREA EFFECT; ELOBORATE GESTURES; 5 END)")
    ("+2" "Combat Expert")
    ("+2" "Healer")
    ("-1" "Bizarre Appearance (Cat Features)")))

(define abilities
  `(("+2" "Companion (Azyrus)")
    ("+2" "Cute!")
    ("+1" "Iron-Willed")
    ("+2" "Knowledge (Shoujo Manga)")
    ("+2" "Quick")
    ("+2" "Shape-Shifter")
    ("+4" ,(string-append "Transformation" "\n"
                          (show #f
                                  (tabular " " (joined displayed (map (lambda (x) (car x)) transformation) "\n")
                                           " " (joined displayed (map (lambda (x) (cadr x)) transformation) "\n")
                                           " "))))
    ("+2" "Tough")
    ))


(pp abilities)

(display (cadr (list-ref abilities 6)))

(define weaknesses
  '(("-2" "Ageism")
    ("-2" "Crybaby")
    ("-1" "Easily Distracted")
    ("-2" "Focus (Powers Require Neko Transformation Locket)")
    ("-1" "Frail")
    ("-2" "Naive")))

(map (lambda (x) (show #f (with ((width 20)) (wrapped (cadr x))))) weaknesses)


(show #t (tabular "|" (joined displayed (map (lambda (x) (car x)) abilities) "\n")
                  "|" (joined displayed (map (lambda (x) (cadr x)) abilities) "\n")
                  "|" (joined displayed (map (lambda (x) (car x)) weaknesses) "\n")
                  "|" (joined displayed (map (lambda (x) (cadr x)) weaknesses) "\n")
                  "|"))

(show #t (tabular "|" (joined displayed (map (lambda (x) (car x)) abilities) "\n")
                  "|" (joined displayed (map (lambda (x) (cadr x)) abilities) "\n")
                  "|" (joined displayed (map (lambda (x) (car x)) weaknesses) "\n")
                  "|" (joined displayed (map (lambda (x) (show #f (with ((width 40)) (wrapped (cadr x))))) weaknesses) "\n")
                  "|"))
)


(loop for item
      in (map (lambda (x) (show #f (with ((width 40)) (wrapped (cadr x))))) weaknesses) collect (string-count item (lambda (c) (char=? c #\newline))))

;; Hangs.
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
