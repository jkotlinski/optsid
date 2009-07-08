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

(declaim (optimize speed))

(setf *break-on-signals* 'error)

(defun copy-file (inpathname outpathname &optional (buffersize #.(* 8 1024)))
  "buffered file copy routine"
  (with-open-file (is inpathname :direction :input)
	(with-open-file (os outpathname :direction :output
						:if-exists :supersede
						:if-does-not-exist :create)
	  (let ((buffer (make-array buffersize :element-type (stream-element-type is))))
		(do ((nread (read-sequence buffer is) (read-sequence buffer is))
			 (total 0 (+ total nread)))
		  ((zerop nread) total)
		  (write-sequence buffer os :end nread))))))

; -------------

(load "parse.lisp")

(defvar frames (parse-file 
					  "data/Noisy_Pillars.txt"
					  ; "data/Diffractions.txt"
					  ; "data/Onward.txt"
					  4000
					  ))

(defun op-type (op) (car op))
(defun op-val (op) (cdr op))

(defun freqp (op) (member (op-type op) '(freq0 freq1 freq2 freq0-forget freq1-forget freq2-forget)))
(defun freqrelp (op) (member (op-type op) '(freq-rel0 freq-rel1 freq-rel2)))
(defun freqsym-to-ch (op)
  (let ((op (op-type op)))
	(cond ((eq op 'freq0) 0)
		  ((eq op 'freq1) 1)
		  ((eq op 'freq2) 2)
		  ((eq op 'freq-rel0) 0)
		  ((eq op 'freq-rel1) 1)
		  ((eq op 'freq-rel2) 2)
		  (t 0))))

(defun replace-type (op newtype) 
  (setf (car op) newtype))

(defvar prev-abs-op '(nil nil nil))
(defun make-forget-freqabs-frames ()
  (dolist (frame frames)
	(dolist (op frame)
	  (let* ((ch (freqsym-to-ch op)))
		(cond ((freqp op)
			   (if (elt prev-abs-op ch) 
				 (replace-type (elt prev-abs-op ch) (elt '(freq0-forget freq1-forget freq2-forget) ch)))
			   (setf (elt prev-abs-op ch) op))
			  ((freqrelp op) (setf (elt prev-abs-op ch) nil)))))))

(defun all-ops-of-type (test)
  (let ((ops nil))
	(dolist (frame frames)
	  (dolist (op frame)
		(if (funcall test op) (setf ops (cons op ops)))))
	ops))

(defun all-vals (ops)
  (loop for op in ops collect (op-val op)))

(defun adsrp (op) (member (op-type op) '(adsr0 adsr1 adsr2)))
; (make-forget-freqabs-frames)

(load "subst.lisp")

(load "writer.lisp")
(write-data "asm.a")

(format t "~%cmds and tailnils ~a~%" *written-cmds*)
; (pprint *arps*)

(maphash
  (lambda (str count)
	(format t "~a~t~a; " str count))
  *cmd-count-table*)

(sb-ext:quit)
