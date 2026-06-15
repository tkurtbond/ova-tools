(import yaml)
(import loop)
(import (schemepunk show))
(import (chicken pretty-print))

(define fukiko (with-input-from-file "test-data/Fukiko.yaml" (lambda () (yaml-load (current-input-port)))))

(show #t (with ((width 40)) (wrapped "one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen") nl))

      
