(define-module (bytestructures guile numeric-data-model))

(import (system foreign))
(import (system base target))

(define architecture
  (let ((cpu (target-cpu)))
    (cond
     ((member cpu '("i386" "i486" "i586" "i686"))
      'i386)
     ((string=? "x86_64" cpu)
      'x86-64)
     ((string-prefix? "arm" cpu)
      'arm)
     ((string-prefix? "aarch64" cpu)
      'aarch64))))

(define data-model
  (if (= 4 (sizeof '*))
      (if (= 2 (sizeof int))
          'lp32
          'ilp32)
      (cond
       ((= 8 (sizeof int))  'ilp64)
       ((= 4 (sizeof long)) 'llp64)
       (else                'lp64))))

(cond-expand-provide
 (current-module)
 (list architecture data-model))
