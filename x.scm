(import libyaml)
(import (chicken pretty-print))
(import yaml)

(define filename "/home/tkb/current/RPG/Tools/OVA/test-data/Fukiko.yaml")

(define yproc (call-with-input-file filename yaml<-))
;; Gets the first document.
(call-with-output-file "/tmp/0.dat" (lambda (port) (pp (yproc) port)))
;; Gets all the documents in a list.
(call-with-output-file "/tmp/1.dat" (lambda (port) (pp (car (yproc -1)) port)))

(define ytree (call-with-input-file filename yaml-load))
(call-with-output-file "/tmp/3.dat" (lambda (port) (pp ytree port)))

(define test1 "---
- name: Kurt
  job: Programmer
- 2
- 3
...
")
(pp ((yaml<- test1)))
(pp (yaml-load test1))


#(
  ((("name" . "Kurt")
    ("job" . "Programmer")))
  2
  3)

(
 (("name" . "Kurt")
  ("job" . "Programmer"))
 2
 3)

    

(define test2 "---
# document 1
codename: YAML
name: YAML ain't markup language
release: 2001
---
# document 2
uses:
 - configuration language
 - data persistence
 - internet messaging
 - cross-language data sharing
---
# document 3
company: spacelift
domain:
 - devops
 - devsecops
tutorial:
   - name: yaml
   - type: awesome
   - rank: 1
   - born: 2001
author: omkarbirade
published: true
...
")

(pp ((yaml<- test2) 2))
(pp (yaml-load test2))

