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
(load "arp-tree.lisp")

(defconstant max-arp-len 255) ; max arpeggiato length
(defconstant arp-count 256) ; maximum amount of arpeggiatos
(defconstant min-arp-gain 1) ; minimum gain (in bytes) for switching to normal sequence to arp

(defvar *arps* nil)

(defun prune-tail-nils (lst)
  (let ((last-not-null-item (position-if-not #'null lst :from-end t)))
	(if last-not-null-item
	  (subseq lst 0 (1+ last-not-null-item)))))

(defun build-arp-tree (lst)
  (let ((active-substr (make-fifo))
		(arp-tree (make-arp-tree)))
	(setf lst (fifo-populate active-substr lst max-arp-len))

	(loop while (not (null lst)) do
		  (if (not (null (first (fifo-head active-substr))))
			(add-list (prune-tail-nils (fifo-head active-substr)) arp-tree))
		  (fifo-pop active-substr)
		  (fifo-push (pop lst) active-substr))
	(format t " ~a" (child-node-count arp-tree))
	arp-tree))

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
	(pprint op-types)
	(setf (gethash op-types *substitution-table*)
		  (build-arp-tree (filter op-types frames)))))

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
; (dotimes (i arp-count)
;	(let ((sub (largest-substitution)))
;	  (if (null sub) (return nil)
;		(apply-substitution sub))))
)

(generate-arps)

; profiling
;(require :sb-sprof)
;(sb-sprof:with-profiling (:max-samples 1000 :report :graph :loop nil :show-progress t) (generate-arps))
