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

(defvar *os* nil)

(defun msb (word) "Most significant byte." (ash word -8))
(defun lsb (word) "Least significant byte." (logand word #xff))

(defvar *written-cmds* 0)
(defvar *cmd-count-table* (make-hash-table :test #'equal))
(defun write-op-val (str val)
  (if (or (< val 0) (> val 255))
	(progn
	  (pprint (list str val))
	  (assert nil)))
  (incf *written-cmds*)
  (incf (gethash str *cmd-count-table* 0))
  (format *os* "~a, $~x, " str val))

(defvar *all-freqs* nil)
(defun freq-index (op) 
  (let ((freq (op-val op)))
	(pushnew freq *all-freqs*)
	(assert (< (length *all-freqs*) 255))
	(1+ (position freq *all-freqs*))))
(defvar *adsr-table* nil)
(defun adsr-index (op) 
  (let ((adsr (op-val op)))
	(if (not (member adsr *adsr-table*))
	  (setf *adsr-table* (append *adsr-table* (list adsr))))
	(1+ (position adsr *adsr-table*))))

(defun write-freq-table ()
  (format *os* "FREQS_HI~%!byte ~{~S~^, ~}~%~%" (mapcar #'msb *all-freqs*))
  (format *os* "FREQS_LO~%!byte ~{~S~^, ~}~%~%" (mapcar #'lsb *all-freqs*)))

(defun write-adsr-table ()
  (format *os* "ADSR_HI~%!byte ~{~S~^, ~}~%~%" (mapcar #'msb *adsr-table*))
  (format *os* "ADSR_LO~%!byte ~{~S~^, ~}~%~%" (mapcar #'lsb *adsr-table*)))

(defun write-freq-rel (ch val)
  (cond ((> val 0) (write-op-val (elt '("FREQ_ADD_0" "FREQ_ADD_1" "FREQ_ADD_2") ch) val))
		((< val 0) (write-op-val (elt '("FREQ_SUB_0" "FREQ_SUB_1" "FREQ_SUB_2") ch) (- val)))))

(defun write-op (op)
  (cond 
	((eq (op-type op) 'freq0) (write-op-val "FREQ_ABS_0" (freq-index op)))
	((eq (op-type op) 'freq1) (write-op-val "FREQ_ABS_1" (freq-index op)))
	((eq (op-type op) 'freq2) (write-op-val "FREQ_ABS_2" (freq-index op)))
	((eq (op-type op) 'freq-rel0) (write-freq-rel 0 (op-val op)))
	((eq (op-type op) 'freq-rel1) (write-freq-rel 1 (op-val op)))
	((eq (op-type op) 'freq-rel2) (write-freq-rel 2 (op-val op)))
	((eq (op-type op) 'wf0) (write-op-val "WF0" (op-val op)))
	((eq (op-type op) 'wf1) (write-op-val "WF1" (op-val op)))
	((eq (op-type op) 'wf2) (write-op-val "WF2" (op-val op)))
	((eq (op-type op) 'filt-cut) (write-op-val "FILT_CUT" (op-val op)))
	((eq (op-type op) 'filt-rc) (write-op-val "FILT_RC" (op-val op)))
	((eq (op-type op) 'filt-vol) (write-op-val "FILT_VOL" (op-val op)))
	((eq (op-type op) 'pu0) (write-op-val "PU0" (op-val op)))
	((eq (op-type op) 'pu1) (write-op-val "PU1" (op-val op)))
	((eq (op-type op) 'pu2) (write-op-val "PU2" (op-val op)))
	((eq (op-type op) 'adsr0) (write-op-val "ADSR0" (adsr-index op)))
	((eq (op-type op) 'adsr1) (write-op-val "ADSR1" (adsr-index op)))
	((eq (op-type op) 'adsr2) (write-op-val "ADSR2" (adsr-index op)))
	((equal (op-type op) '(freq0)) (write-op-val "START_ARP_FREQ0" (op-val op)))
	((equal (op-type op) '(freq1)) (write-op-val "START_ARP_FREQ1" (op-val op)))
	((equal (op-type op) '(freq2)) (write-op-val "START_ARP_FREQ2" (op-val op)))
	((equal (op-type op) '(adsr0)) (write-op-val "START_ARP_ADSR0" (op-val op)))
	((equal (op-type op) '(adsr1)) (write-op-val "START_ARP_ADSR1" (op-val op)))
	((equal (op-type op) '(adsr2)) (write-op-val "START_ARP_ADSR2" (op-val op)))
	((equal (op-type op) '(wf0)) (write-op-val "START_ARP_WF0" (op-val op)))
	((equal (op-type op) '(wf1)) (write-op-val "START_ARP_WF1" (op-val op)))
	((equal (op-type op) '(wf2)) (write-op-val "START_ARP_WF2" (op-val op)))
	((equal (op-type op) '(filt-cut)) (write-op-val "START_ARP_FILT_CUT" (op-val op)))
	((equal (op-type op) '(filt-rc)) (write-op-val "START_ARP_FILT_RC" (op-val op)))
	((equal (op-type op) '(filt-vol)) (write-op-val "START_ARP_FILT_VOL" (op-val op)))
	((equal (op-type op) '(pu0)) (write-op-val "START_ARP_PU0" (op-val op)))
	((equal (op-type op) '(pu1)) (write-op-val "START_ARP_PU1" (op-val op)))
	((equal (op-type op) '(pu2)) (write-op-val "START_ARP_PU2" (op-val op)))
	((equal (op-type op) '(freq-rel0)) (write-op-val "START_ARP_FREQ_REL0" (op-val op)))
	((equal (op-type op) '(freq-rel1)) (write-op-val "START_ARP_FREQ_REL1" (op-val op)))
	((equal (op-type op) '(freq-rel2)) (write-op-val "START_ARP_FREQ_REL2" (op-val op)))
	(t (assert nil))
	))
(defvar *write-fc* 0)
(defun write-frame (frame)
  (format *os* "!byte ")
  (mapcar (lambda (x) (write-op x)) frame)
  (format *os* "0 ; ~a~%" *write-fc*)
  (incf *written-cmds*)
  (incf *write-fc*))

(defun write-arp (i)
  (let* ((frame (elt *arps* i))
		 (arptype (first frame))
		 (oplists (second frame)))
	(format *os* "ARP_~a ; ~a~%!byte ~a~%" i (first arptype) (length oplists))
	(dolist (oplist oplists)
	  (dolist (op-type arptype)
		(let* ((op (find op-type oplist :test (lambda (x y) (equal x (car y)))))
			   (val (if op (cdr op) 0)))
		  (cond ((freqp op) (setf val (freq-index op)))
				((adsrp op) (setf val (adsr-index op))))
		  (if (< val 0) (setf val (+ 256 val)))
		  (format *os* "!byte $~x~%" val))))))

#|
(trace write-arp)
(trace freqp)
(trace adsrp)
(trace freq-index)
(trace adsr-index)
|#

(defun write-arps ()
  (format *os* "ARP_TABLE_MSB~%")
  (dotimes (i (length *arps*))
	(format *os* "!byte >ARP_~a~%" i))
  (format *os* "ARP_TABLE_LSB~%")
  (dotimes (i (length *arps*))
	(format *os* "!byte <ARP_~a~%" i))
  (dotimes (i (length *arps*))
	(write-arp i)))

(defun write-data (path)
  (copy-file "header.a" "asm.a")
  (with-open-file (*os* path :direction :output :if-exists :append)
	(format *os* "CMDS~%")
	(dolist (frame frames)
	  (write-frame frame))
	(format *os* "!byte RESET~%~%")
	(write-arps)
	(write-adsr-table)
	(write-freq-table)))

