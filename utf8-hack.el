;; -*- mode: emacs-lisp -*-
;; Copyright Leo Butler (leo.butler@member.fsf.org) 2013
;; Released under the terms of GPLv3+

;; ELISP CODE to generate the list of alphabetical characters
;; Change alpha-char-regex to select a different range of characters

(defvar utf8-hack-acr-list
  '(
    "greek .+ letter"						;; all greek letters, including accents
    "greek .+ letter [^ ]+$"					;; all greek letters, unaccented
    "mathematical"						;; all math symbols
    "\\(\\(latin\\|greek\\) .+ letter\\|mathematical\\)"	;; all latin, greek and math
    "\\(letter\\|symbol\\|math\\)"				;; all
    )
  "List of regexes which may be used to select the alphabetical wide characters.")



(defun utf8-hack (&optional alpha-char-regex data-file)
  (let* ((alpha-char-regex (or alpha-char-regex (nth 4 utf8-hack-acr-list)))
	 (selector (lambda (x) (cond (t
				      (string-match alpha-char-regex (car x)))
				     (nil
				      (and (> 120832 (cdr x)) (< 127 (cdr x)))))))
	 (data-file (or data-file "utf8-hack-data.lisp"))
	 (coding-system-for-read 'utf-8-unix)
	 (coding-system-for-write 'utf-8-unix)
	 (ucs-char-list
	  (encode-coding-string
	   (apply #'concat
		  (mapcar
		   (lambda (x)
		     (if (funcall selector x)
			 (format "'(|%c| %S)" (cdr x) (car x))))
		   (ucs-names)))
	   'utf-8-unix)))
    (if (get-buffer data-file) (kill-buffer data-file))
    (if (file-exists-p data-file) (delete-file data-file))
    (save-excursion
      (find-file-literally data-file)
      (insert ";; -*- mode:lisp; coding: utf-8 -*-\n#.(list ")
      (insert ucs-char-list)
      (insert ")")
      (basic-save-buffer)
      (kill-buffer))))

; end of utf8-hack.el 
