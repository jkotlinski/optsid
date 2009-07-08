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

(load "fifo.lisp")

(defconstant max-arp-len 64) ; max arpeggiato length
(defconstant arp-count 256) ; maximum amount of arpeggiatos
(defconstant min-arp-gain 1) ; minimum gain (in bytes) for switching to normal sequence to arp

(defvar *arps* nil)

(defun all-substrs-of-len (lst len)
  "Return all substrings of list lst with length len
  that don't start or end with nil. Overlapping substrings
  are not permitted."
  (let ((active-substr (make-fifo))
		(all-substrs (make-hash-table :test #'equal))
		(index 0))
	; populate fifo
	(loop while (< (fifo-size active-substr) len) do
		  (fifo-push (pop lst) active-substr))

	(loop while lst do
		  (if (not (edge-null-p active-substr))
			(let* ((substr (fifo-list active-substr))
				   (indexes (gethash substr all-substrs nil)))
			  (if (or (null indexes)
					  (>= (- index (first indexes)) len))
				(push index (gethash substr all-substrs)))))

		  ; update active-substr...
		  (incf index)
		  (fifo-pop active-substr)
		  (fifo-push (pop lst) active-substr))
	all-substrs))

(defun get-common-substrings-of-len (lst len)
  "Return a list of ((ops) . (indexes)) pairs for all substrings
  of length len that occur more than once in lst. Overlapping
  substrings are not permitted."
  (let ((combinations nil))
	(maphash
	  (lambda (substr indexes)
		(if (> (length indexes) 1)
		  (push (list substr indexes) combinations)))
	  (all-substrs-of-len lst len))
	combinations))

(defun gen-range (len)
  "Generate integer list 0..len-1"
  (let ((lst nil))
	(dotimes (i len)
	  (setf lst (cons i lst)))
	(nreverse lst)))

(defun get-common-substrings (lst)
  "Return a list of (substring . count) pairs for all substrings
  that occur more than once in lst.  Overlapping substrings are not permitted."
  (let ((i 2) 
		(collection nil))
	(loop (if (> i max-arp-len) (return collection))
		  (let ((new-collection (get-common-substrings-of-len lst i)))
			(setf collection (append collection new-collection)))
		  ; (format t "~a~%" i)
		  (incf i))))

(defun filter (op-types frames)
  (let ((new-frames nil))
	(dolist (frame frames)
	  (let ((new-frame nil))
		(dolist (op frame)
		  (if (member (op-type op) op-types)
			(setf new-frame (cons op new-frame))))
		(setf new-frames (cons new-frame new-frames))))
	(nreverse new-frames)))

(defun op-count (ops-list)
  (let ((sum 0))
	(dolist (ops ops-list)
	  (incf sum (length ops)))
	sum))

(defun substitution-gain (substitution)
  "Return gain from performing substitution.
  Assumes that every list can be triggered by a single cmd/arg call."
  (let* ((call-count (length (second substitution)))
		 (ops-list (first substitution))
		 (arp-row-count (length ops-list)))
	(+ (* 2 call-count (op-count ops-list)) ; ops removed! all gain is from here
	   (* -2 call-count) ; new calls that have to be done
	   -2 ; arpeggiato table entry ptr
	   (- arp-row-count) ; arpeggiato list, values
	   -1 ; length of arpeggiato list
	   )))

(defun print-substitution (substitution)
  (format t "~s  <- gain. ~s: " (substitution-gain substitution) (length (first substitution)))
  (dolist (ops (first substitution))
	(dolist (op ops)
	  (format t "~a ~x, " (car op) (cdr op)))
	(format t "; "))
  (format t "~{ ~s~}~%" (second substitution)))

(defvar *substitution-table* (make-hash-table :test #'equal))
(defconstant all-arp-types
			 '((freq0) (wf0) (adsr0)
			   (freq1) (wf1) (adsr1)
			   (freq2) (wf2) (adsr2)
			   (freq-rel0) (freq-rel1) (freq-rel2)
			   (filt-rc) (filt-cut) (filt-vol) (pu0) (pu1) (pu2)))

(defun calc-substitutions (arp-types)
  (dolist (op-types arp-types)
	(setf (gethash op-types *substitution-table*) 
		  (get-common-substrings (filter op-types frames)))))

(defun freqtypep (arp-type)
  (member (car arp-type) '(freq0 freq1 freq2)))

(defun largest-substitution ()
  "Get largest substitution and regenerate the associated substitutions"
  (let ((maxgain -100000)
		(maxsubstitution nil)
		(maxarptype nil))
	(maphash 
	  (lambda (arp-type substitutions)
		(dolist (substitution substitutions)
		  (if (> (substitution-gain substitution) maxgain)
			(progn (setf maxsubstitution substitution)
				   (setf maxgain (substitution-gain substitution))
				   (setf maxarptype arp-type)))))
	  *substitution-table*)
	(if (> maxgain min-arp-gain)
	  (cons maxarptype maxsubstitution)
	  nil)))

(defun get-substitutions ()
  (let ((substitutions nil))
	(maphash 
	  (lambda (types subs)
		(setf types types) ; surpress warning
		(setf substitutions (append substitutions subs)))
	  *substitution-table*)
	substitutions))

(defun add-start-arp-cmd (index arptype)
  (setf (elt frames index)
		(cons 
		  (cons arptype (length (cdr *arps*))) ; arp type+no
		  (elt frames index))))

(defun remove-arp-frame (index op-frames)
  (dolist (ops op-frames)
	(dolist (op ops)
	  (setf (elt frames index)
			(remove-if (lambda (x) (equal op x)) (elt frames index))))
	(incf index)))

(defun apply-substitution (type-sub)
  (if type-sub
	(let ((arptype (car type-sub))
		  (sub (cdr type-sub)))
	  (format t "apply-substitution: ")
	  (print-substitution sub)
	  (dolist (index (second sub))
		(add-start-arp-cmd index arptype)
		(remove-arp-frame index (first sub)))
	  (setf *arps* (append *arps* (list type-sub)))
	  (calc-substitutions (list arptype)))))

(defun generate-arps ()
  (calc-substitutions all-arp-types)
  (dotimes (i arp-count)
	(let ((sub (largest-substitution)))
	  (if (null sub) (return nil)
		(apply-substitution sub)))))

(generate-arps)

; profiling
;(require :sb-sprof)
;(sb-sprof:with-profiling (:max-samples 1000 :report :graph :loop nil :show-progress t) (generate-arps))
