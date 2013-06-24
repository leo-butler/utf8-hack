;; -*- mode: lisp; coding: utf-8 -*-
;; Copyright Leo Butler (leo.butler@member.fsf.org) 2013
;; Released under the terms of GPLv3+

#|
 
 >From mailnull  Wed May 22 20:40:46 2013
 Received-SPF: pass (sog-mx-4.v43.ch3.sourceforge.com: domain of math.utexas.edu designates 146.6.25.7 as permitted sender) client-ip=146.6.25.7; envelope-from=maxima-bo\
 unces@math.utexas.edu; helo=ironclad.mail.utexas.edu;
 Date: Wed, 22 May 2013 20:39:26 +0000
 From: Leo Butler <l_butler@users.sourceforge.net>
 To: Robert Dodier <robert.dodier@gmail.com>
 In-Reply-To: <knj30l$a8a$1@ger.gmane.org> (message from Robert Dodier on Wed,
         22 May 2013 18:32:53 +0000)
 CC: <maxima@math.utexas.edu>
 Subject: Re: [Maxima] utf8 + maxima
 Content-Type: text/plain; charset="utf-8"
 
    >From mailnull  Wed May 22 18:34:43 2013
    Received-SPF: pass (sog-mx-4.v43.ch3.sourceforge.com: domain of math.utexas.edu designates 146.6.25.7 as permitted sender) client-ip=146.6.25.7; envelope-from=maxima\
 -bounces@math.utexas.edu; helo=ironclad.mail.utexas.edu;
    From: Robert Dodier <robert.dodier@gmail.com>
    Date: Wed, 22 May 2013 18:32:53 +0000
    Content-Type: text/plain; charset="utf-8"
 
    On 2013-05-22, Leo Butler <l_butler@users.sourceforge.net> wrote:
 
    > But both ecl and gcl choke,
 
    Well, ECL should be able to process UTF-8 characters. How did you launch
    it? I'm pretty sure I've tried it with ECL by launching a UTF-8 xterm
    and then executing Maxima + ECL in that and it works fine. Also
    something like 'LANG=foo.UTF-8 maxima -l ecl'.
 
 That does not work for me with ecl 11.1.1 from the debian testing
 repo.  The issue appears to be with this version of ecl, because, if
 the encoding is set on the command line, ecl barfs.
 
    > Maxima 5.30.0 http://maxima.sourceforge.net
    > using Lisp GNU Common Lisp (GCL) GCL 2.6.7 (a.k.a. GCL)
    > Distributed under the GNU Public License. See the file COPYING.
    > Dedicated to the memory of William Schelter.
    > The function bug_report() provides bug reporting information.
    > (%i1) ρ:1;
    > incorrect syntax: \201 is not an infix operator
    > \317\201
    > ^
 
    Well, this is understandable -- GCL doesn't see the whole UTF-8
    character, instead a sequence of 2 characters \317 and \201. \317 is
    nonalphabetic according to ALPHA-CHAR-P, therefore it's treated as a
    separate token from the next one (\201), then the parser barfs on \201
    since it's not an operator.
 
 Ok, the error message explains as much. The point is that the GCL
 reader happily interns a symbol whose symbol-name consists of 2
 characters \317\201:
 
 >(coerce (symbol-name 'ρ) 'list)
 
 (#\\317 #\\201)
 
 So I could hack a "utf8-enabled" Maxima parser by redefining alphabetp
 or *alphabet*, like so
 
 (%i1) :lisp (setf *alphabet* (append '(#\\317 #\\201) *alphabet*))
 
 (\317 \201 _ %)
 (%i1) ρ : 1;
 (%o1)                                  1
 
 
 
 
 Leo
 _______________________________________________
 Maxima mailing list
 Maxima@math.utexas.edu
 http://www.math.utexas.edu/mailman/listinfo/maxima
 
|#
;; LISP CODE
(defvar *alpha-char-hash* nil "A hash table with keys that are the
character codes from wide characters in the UTF8 encoding, and the
values are the corresponding UTF8 code points as interned
symbols. This is a hack to enable the use of wide characters in
non-UTF8 Lisps.")

(let* ((ach (make-hash-table :test #'eql))
       (data-file ($file_search1 "utf8-hack-data.lisp" '((mlist) $file_search_lisp))))

  (setf *alpha-char-hash*  (make-hash-table :test #'eql))

  (defmfun $set_alpha_char (char-sym description)
    "A user-level function to add a wide character to the hashtable of
known alphabetical characters."
    (let ((char-sym-list (coerce (symbol-name char-sym) 'list))
	  (csd (cons description char-sym)))
      (mapc (lambda (c)
	      (push csd (gethash c ach))
	      (push csd (gethash c *alpha-char-hash*))) char-sym-list))
    '$done)

  (loop for (char-sym description) in (with-open-file (instr data-file :direction :input) (read instr :eof-error-p nil :eof-value))
       do ($set_alpha_char char-sym description))
  
  (defmfun $utf8_hack (&optional regexp)
	   "Select the wide characters via a MAXIMA-NREGEX regexp. If
	   REGEXP is the symbol `all', this is equivalent to \".\"; if
	   REGEXP is NIL, then no matches are made, i.e. the hash
	   table *ALPHA-CHAR-HASH* is emptied. Example:
	   utf8_hack(\"greek .+ letter [^ ]+$\");"
	   (let ((selector (cond ((stringp regexp)
				  (coerce (maxima-nregex:regex-compile regexp :case-sensitive nil) 'function))
				 ((eql regexp '$all)
				  (lambda (x) (declare (ignore x)) t))
				 ((null regexp)
				  (lambda (x) (declare (ignore x)) nil))
				 (t
				  (merror "utf8_hack(regexp), regexp must be a string, the symbol `all' or empty.")))))
	     (setf *alpha-char-hash*  (make-hash-table :test #'eql))
	     (maphash (lambda (k v)
			(mapc (lambda (x)
				(if (and x (funcall selector (car x)))
				    (push x (gethash k *alpha-char-hash*))))
			      v))
		      ach)
	     )
	   '$done))

;; redefine alphabetp from src/nparse.lisp
;; to use the *alpha-char-hash* table
(defmfun alphabetp (n)
  (and (characterp n)
       (or (alpha-char-p n)
	   (member n *alphabet*)
	   (gethash n *alpha-char-hash*))))

(defmfun $print_alpha_char_hash ()
  "Print the hash *alpha-char-hash*."
  (maphash (lambda(k v) (format t "~s = ~a~%" k v)) *alpha-char-hash*)
  '$done)

; end of utf8-hack.lisp 
