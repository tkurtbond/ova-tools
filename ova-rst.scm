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

(define (process-entity entity entity-no)
  (dbg (show (current-error-port) nl "entity no: " entity-no nl (pretty entity) nl))
  #f)

(define (process-entity-raw-ms entity entity-no)
  #f)

(define (process-entity-terse entity entity-no)
  #f)

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
