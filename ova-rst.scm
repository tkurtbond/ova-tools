(module ova-rst ()
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
(import yaml)                           ; The yaml egg uses libyaml, which is yaml 1.1.

(define standard-input-port (current-input-port))

(define *debugging* #t)

(define-syntax dbg
  (syntax-rules ()
    ((_ e1 e2 ...)
     (when *debugging*
       e1 e2 ...
       (flush-output (current-error-port))))))

(define (die status . args)
  (show (current-error-port) (program-name) ": ")
  (apply show (cons (current-error-port) args))
  (show (current-error-port) "\n")
  (exit status))

;; (put 'when-in-alist 'scheme-indent-function 1)
(define-syntax when-in-alist
  (syntax-rules ()
    ((_ (var key alist) b1 ...)
     (let ((val (assoc key alist)))
       (when val
	 (let ((var (cdr val)))
	   b1 ...))))))

(define (must-exist item alist)
  (let ((found (assoc item alist)))
    (if found
        (cdr found)
        (die 2 "Unable to find " (written item) " in " (written alist)))))

(define (may-exist item alist)
  (let ((result (assoc item alist)))
    (if result
        (cdr result)
        result)))

(define (rbold text) (string-append "**" text "**")) ; reST bold
(define (ritalic text) (string-append "*" text "*")) ; reST italic
(define (tbold text) (string-append "\\fB" text "\\fP")) ; troff bold
(define (titalic text) (string-append "\\fI" text "\\fP")) ; troff italic

(define (process-entity entity entity-no)
  (dbg (show (current-error-port) nl "entity no: " entity-no nl (pretty entity) nl))
  #f)

(define (process-entity-raw-ms entity entity-no)
  #f)

(define (process-entity-terse entity entity-no)
  ;; Example: Azyrus, p. 37.
  ;; Display Attributes (abilities and then weaknesses, separated by a semicolon.
  ;; Something Like:
  ;;
  ;; **Azyrus**
  ;; Despite his cute and fuzzy exterior, Azyrus is always
  ;; ready with sage advice for Fukiko.  He can often be exasperated
  ;; with her youthful naivete, but his heart is in the right place —
  ;; even when his mouth isn't.
  ;;
  ;; **Attributes:** Flight +2, Quick +1, Smart +2 (Abilities Total +5); Awkward
  ;; Size (Small) -1, Frail -1, Short-Tempered -1 (Weaknesses Total -3).
  ;; **Attribute Total:** +2.
  ;;
  ;; If present:
  ;;
  ;; **Combat:**
  ;; **Defense 3**, **Health 30**, **Endurance 40**.
  ;; **Attacks**
  (define (process-attribute attribute)
    (let ((name (must-exist "name" attribute))
          (level (must-exist "level" attribute)))
      (show #f name " " (numeric level #f #f #t))))

  (let* ((abilities-total 0)
         (abilities (may-exist "abilities" entity))
         (abilities-total (if abilities (loop for ability in abilities sum (must-exist "level" ability)) 0))
         (weaknesses (may-exist "weaknesses" entity))
         (weaknesses-total (if weaknesses (loop for weakness in weaknesses sum (must-exist "level" weakness)) 0))
         (attributes-total (+ abilities-total weaknesses-total))
         (combat (assoc "combat" entity)))
    (when-in-alist (entity-name "name" entity)
      (let ((underline (make-string (string-length entity-name)
                                    (if (and (> entity-no 1)
                                             *subunderliner*)
                                        *subunderliner*
                                        *underliner*))))
        (show #t entity-name nl underline nl nl)))
    (show #t (rbold "Attributes:") nl)
    (when abilities
      (show #t (ritalic "Abilities:") " ")
      (show #t (joined displayed (loop for ability in abilities collect (process-attribute ability)) ", ") " (Abilitites Total " (numeric abilities-total #f #f #t) ")" nl)
      #f)
    (when (and abilities weaknesses) (show #t "; " nl))
    (when weaknesses
      (show #t (ritalic "Weaknesses:") " ")
      (show #t (joined displayed (loop for weakness in weaknesses collect (process-attribute weakness)) ", ") " (Weaknesses Total " (numeric weaknesses-total #f #f #t) ")" nl)
      #f)
    (show #t (rbold "(Attributes Total "  (numeric attributes-total #f #f #t) ")" nl))
    ))

(define (process-file)
  ;;; It is a file of possibly multiple entities.
  (let ((entities (yaml-load (current-input-port))))
    (loop for entity in entities
          for entity-no from 1
          do (*output-formatter* entity entity-no))))

(define (process-filename filename)
  (cond ((string=? filename "-")
         (dbg (show (current-error-port) "Using standard input from filename -" nl))
         (with-input-from-port standard-input-port process-file))
        (else
         (dbg (show (current-error-port) "Using filename " filename nl))
         (with-input-from-file filename process-file))))

;; Command line flags go here.

(define *output-file* #f)
(define *output-formatter* process-entity)
(define *underliner* #\-)
(define *subunderliner* #f)

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "Usage: " (program-name) " [options...] [files...]")
      (newline)
      (print (args:usage +command-line-options+))
      (newline)
      (show #t "Current argv: " (written (argv)) nl)))
  (exit 1))


(define +command-line-options+
  (list (args:make-option
            (h help) #:none "Display this text."
          (usage))
        (args:make-option
            (m raw-ms-tables) #:none "Use groff tbl output in a raw ms block in reST output."
          (set! *output-formatter* process-entity-raw-ms))
        (args:make-option
            (o output) #:required "Output file."
          (set! *output-file* arg))
        (args:make-option
            (t terse) #:none "Use terse output."
          (set! *output-formatter* process-entity-terse))
        (args:make-option
         (U subunderliner) #:required
         "Entities after the first are subentities,
                          and use a different character for
                          underlining the subheader."
         (set! *subunderliner* (string-ref arg 0)))
        (args:make-option
         (u underliner) #:required
         "Character to use for underlining the header."
         (set! *underliner* (string-ref arg 0)))
        ))

(define (main)
  (receive (options operands) (args:parse (command-line-arguments)
                                          +command-line-options+)
    (define (process-operands)
      (cond  ((zero? (length operands))
              (dbg (show (current-error-port) "zero arguments, so using standard input" nl))
              (with-input-from-port standard-input-port process-file))
             (else
              (loop for filename in operands do (process-filename filename)))))

    (if *output-file*
        (with-output-to-file *output-file* process-operands)
        (process-operands))))

;; Only invoke main if this has been compiled.  That way we can load the
;; module into csi and debug it. 
(cond-expand
  ((and chicken-5 compiling)
   (main))
  ((and chicken-5 csi)))
)
