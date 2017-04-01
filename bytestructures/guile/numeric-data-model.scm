(define-module (bytestructures guile numeric-data-model))
(import (system foreign))
(define data-model
  (if (= 4 (sizeof '*))
      (if (= 2 (sizeof int))
          'lp32
          'ilp32)
      (cond
       ((= 8 (sizeof int))  'ilp64)
       ((= 4 (sizeof long)) 'llp64)
       (else                'lp64))))
(cond-expand-provide (current-module) (list data-model))
