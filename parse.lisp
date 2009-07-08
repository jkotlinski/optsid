#| Copyright (c) 2009, Johan Kotlinski
 |
 | Permission is hereby granted, free of charge, to any person
 | obtaining a copy of this software and associated documentation
 | files (the "Software"), to deal in the Software without
 | restriction, including without limitation the rights to use,
 | copy, modify, merge, publish, distribute, sublicense, and/or sell
 | copies of the Software, and to permit persons to whom the
 | Software is furnished to do so, subject to the following
 | conditions:
 |
 | The above copyright notice and this permission notice shall be
 | included in all copies or substantial portions of the Software.
 |
 | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 | EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 | OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 | NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 | HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 | WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 | FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 | OTHER DEALINGS IN THE SOFTWARE.
 |#

(defun skip-siddump-header (in)
  "Skip initial siddump output header."
  (loop for line = (read-line in nil) 
		while (or (string= line "")
				  (not (equal (char line 0) #\+)))))

(defun dots-p (str) "Does str contain dots?" (find #\. str))

(defvar *prev-filter-vol* nil)
(defun filter-to-value (type vol)
  (if (dots-p type) (setf type "Off"))
  (+ (if (dots-p vol) *prev-filter-vol* (setf *prev-filter-vol* (parse-integer vol :radix 16)))
	 (* #x10 (position type '("Off" "Low" "Bnd" "L+B" "Hi " "L+H" "B+H" "LBH") :test #'string=))))

(defun parse-filter (line)
  (let ((type (subseq line 102 105))
		(vol (subseq line 106 107)))
	(if (and (dots-p type) (dots-p vol))
	  nil
	  (filter-to-value type vol))))

(defun parse-hex (line col1 w)
  (let ((txt (subseq line col1 (+ col1 w))))
	(if (dots-p txt) nil (parse-integer txt :radix 16))))

(defun parse-freq (ch line) (parse-hex line (+ 10 (* 28 ch)) 4))

(defun create-op (id val)
  (if (null val) nil
	(cons id val)))

(defvar prev-pu '(10000 10000 10000)) ; 10000 = not initialized.

(defun swap (val)
  "Swap high and low nibble."
  (+ (ash (logand #xf val) 4)
	 (ash (logand #xf0 val) -4)))

(defun create-pu-op (ch val)
  (if (null val) nil
	(create-op (elt '(pu0 pu1 pu2) ch) (swap (round (/ val 16)))))) ; Discard least significant nibble.

(defvar prev-freq '(100000 100000 100000)) ; 100000 = not initialized.

(defun create-fc-op (val)
  (if (null val) nil (create-op 'filt-cut (/ val 256)))) ; Discard least significant nibble.

(defun make-rel-freq (ch diff)
  (setf (elt prev-freq ch)
		(+ (elt prev-freq ch) diff))
  (let ((val 
		  (round (if (< diff 0) 
			(+ 256 (/ diff 2))
			(/ diff 2)))))
	(assert (< val 256))
	(assert (>= val 0))
	(if (> val 0)
	  (create-op (elt '(freq-rel0 freq-rel1 freq-rel2) ch) val))))

(defun make-abs-freq (ch new-val)
  (setf (elt prev-freq ch) new-val)
  (create-op (elt '(freq0 freq1 freq2) ch) new-val))

(defun create-freq-op (ch new-val)
  (if (null new-val) nil
	(let* ((diff (- new-val (elt prev-freq ch)))
		   (rel (and (< diff 256) (>= diff -256))))
	  (if rel 
		(make-rel-freq ch diff)
		(make-abs-freq ch new-val)))))

(defun parse-line (line)
  (delete nil
		  (list 
			(create-freq-op 0 (parse-hex line 10 4))
			(create-op 'wf0 (parse-hex line 24 2))
			(create-op 'adsr0 (parse-hex line 27 4))
			(create-pu-op 0 (parse-hex line 32 3))
			(create-freq-op 1 (parse-hex line 38 4))
			(create-op 'wf1 (parse-hex line 52 2))
			(create-op 'adsr1 (parse-hex line (+ 27 28) 4))
			(create-pu-op 1 (parse-hex line 60 3))
			(create-freq-op 2 (parse-hex line 66 4))
			(create-op 'wf2 (parse-hex line 80 2))
			(create-op 'adsr2 (parse-hex line (+ 27 56) 4))
			(create-pu-op 2 (parse-hex line 88 3))
			(create-fc-op (parse-hex line 94 4))
			(create-op 'filt-rc (parse-hex line 99 2))
			(create-op 'filt-vol (parse-filter line)))))

(defun parse-file (path limit)
  (let ((frames nil))
	(with-open-file (stream path)
	  (skip-siddump-header stream)
	  (loop for line = (read-line stream nil 'foo)
			until (or (eq line 'foo) (< (decf limit) 0))
			do (setf frames (cons (parse-line line) frames))))
	(reverse frames)))

