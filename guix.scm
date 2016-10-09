(define-module (scheme-bytestructures)
  #:use-module (gnu packages guile)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system trivial))

(define-public scheme-bytestructures
  (package
    (name "scheme-bytestructures")
    (version "20160726.53127f6")
    (source (origin
	      (method git-fetch)
	      (uri (git-reference
		    (url "https://github.com/TaylanUB/scheme-bytestructures")
		    (commit "53127f608caf64b34fa41c389b2743b546fbe9da")))
	      (file-name (string-append name "-" version "-checkout"))
	      (sha256
	       (base32
		"0l4nx1vp9fkrgrgwjiycj7nx6wfjfd39rqamv4pmq7issi8mrywq"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
	 (use-modules (guix build utils)
		      (ice-9 match)
		      (ice-9 popen)
		      (ice-9 rdelim))

	 (let* ((out (assoc-ref %outputs "out"))
		(guile (assoc-ref %build-inputs "guile"))
		(effective (read-line
			    (open-pipe* OPEN_READ
					(string-append guile "/bin/guile")
					"-c" "(display (effective-version))")))
		(module-dir (string-append out "/share/guile/site/"
					   effective))
		(source (assoc-ref %build-inputs "source"))
		(doc (string-append out "/share/doc/scheme-bytestructures"))
		(scm-files (string-split "bytestructures/guile/explicit-endianness.scm
bytestructures/guile/numeric-metadata.scm
bytestructures/guile/ffi.scm
bytestructures/guile/vector.scm
bytestructures/guile/union.scm
bytestructures/guile/numeric-all.scm
bytestructures/guile/utils.scm
bytestructures/guile/pointer.scm
bytestructures/guile/base.scm
bytestructures/guile/numeric.scm
bytestructures/guile/struct.scm
bytestructures/guile/bitfields.scm
bytestructures/r6/bytevectors.scm
bytestructures/body/base.syntactic.scm
bytestructures/body/explicit-endianness.scm
bytestructures/body/vector.scm
bytestructures/body/union.scm
bytestructures/body/utils.scm
bytestructures/body/base.scm
bytestructures/body/numeric.scm
bytestructures/body/struct.scm
bytestructures/body/bitfields.scm
bytestructures/guile.scm"
					 #\newline))
		(guild (string-append (assoc-ref %build-inputs "guile")
				      "/bin/guild")))
	   ;; Make installation directories.
	   (mkdir-p (string-append module-dir "/bytestructures/guile"))
	   (mkdir-p (string-append module-dir "/bytestructures/r6"))
	   (mkdir-p (string-append module-dir "/bytestructures/body")) 
	   (mkdir-p doc)

	   ;; Compile .scm files and install.
	   (chdir source)
	   (setenv "GUILE_AUTO_COMPILE" "0")
	   (for-each (lambda (file)
		       (let* ((dest-file (string-append module-dir "/"
							file))
			      (go-file (string-append module-dir "/"
						      (substring file 0
								 (string-rindex file #\.))
						      ".go")))
			 ;; Install source module.
			 (copy-file file dest-file)
			 ;; Install compiled module.
			 (unless (zero? (system* guild "compile"
						 "-L" source
						 "-o" go-file
						 file))
			   (error (format #f "Failed to compile ~s to ~s!"
					  file go-file)))))
		     scm-files)

	   ;; Also copy over the README.
	   (install-file "README.md" doc)
	   #t))))
    (inputs
     `(("guile" ,guile-2.0)))
    (home-page "https://github.com/TaylanUB/scheme-bytestructures")
    (synopsis "Structured access to bytevector contents for Guile")
    (description
     "Scheme bytestructures offers a system imitating the type system
of the C programming language, to be used on bytevectors.  C's type
system works on raw memory, and Scheme works on bytevectors which are
an abstraction over raw memory.  It's also more powerful than the C
type system, elevating types to first-class status.")
    (license gpl3)))

(define (guile-2.2-package-name name)
  "Return NAME with a \"guile2.2-\" prefix instead of \"guile-\", when
applicable."
  (if (string-prefix? "guile-" name)
      (string-append "guile2.2-"
                     (string-drop name
                                  (string-length "guile-")))
      name))

(define package-for-guile-2.2
  ;; A procedure that rewrites the dependency tree of the given package to use
  ;; GUILE-NEXT instead of GUILE-2.0.
  (package-input-rewriting `((,guile-2.0 . ,guile-next))
			   guile-2.2-package-name))

(define-public guile2.2-scheme-bytestructures
  (package-for-guile-2.2 guile-minikanren))


scheme-bytestructures
