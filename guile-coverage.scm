;;; Use this in the REPL.  It produces wrong results when ran as a script.

(use-modules (system vm coverage)
             (system vm vm)
             (srfi srfi-11))

(let ((output-directory
       (string-append
        (getenv "HOME") "/srv/http/htdocs/lcov/scheme-bytestructures")))
  (let-values (((data . values)
                (with-code-coverage (the-vm)
                  (lambda ()
                    (load "run-tests.guile.scm")))))
    (let* ((port (mkstemp! (string-copy "/tmp/bytestructures-coverage-XXXXXX")))
           (file (port-filename port)))
      (coverage-data->lcov data port)
      (close port)
      (when (not (zero? (system* "genhtml" file "-o" output-directory)))
        (error  "genhtml failed"))
      (delete-file file))))
