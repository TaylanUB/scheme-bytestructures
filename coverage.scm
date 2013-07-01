;;; Couldn't get this to work from a script; run it from the REPL.

(let ((genhtml-path
       (string-concatenate
        (list (getenv "HOME") "/src/lcov-1.10/bin/genhtml")))
      (output-directory
       (string-concatenate
        (list (getenv "HOME") "/srv/http/htdocs/lcov/scheme-bytestructures"))))
  (call-with-values
      (lambda () ((@ (system vm coverage) with-code-coverage)
             ((@ (system vm vm) the-vm))
             (@@ (bytestructures test) main)))
    (lambda (data . values)
      (let* ((file (tmpnam))
             (port (open-file file "w")))
        ((@ (system vm coverage) coverage-data->lcov) data port)
        (system* genhtml-path file "-o" output-directory)
        (close port)
        (delete-file file)))))
