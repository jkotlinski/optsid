(defmacro while (expression &body body)
  `(tagbody
	 start (if (not ,expression) (go end))
	 ,@body
	 (go start)
	 end))

(defstruct arp-tree
  (value nil)
  (indexes nil)
  (parent nil)
  (children nil)
  (gain nil)) ; computed by calc-arp-node-gain

(defun has-parent (node) (arp-tree-parent node))

(defun print-upwards (node)
  (if (has-parent node)
	(progn
	  (print-upwards (arp-tree-parent node))
	  (format t "~a " (arp-tree-value node)))))

(defun calc-arp-node-gain (node depth)
  (fresh-line)
  (format t "calc-arp-node-gain ~a; " depth)
  (print-upwards node))

(defun calc-arp-tree-gain (tree &optional (depth 0))
  (if (> depth 0) (calc-arp-node-gain tree depth))
  (dolist (child (arp-tree-children tree))
	(calc-arp-tree-gain child (1+ depth))))

(defun pprint-arp-tree (stream tree &optional (tree-depth 0))
  (if (> tree-depth 0) ; Don't print root node...
	(progn
	  (dotimes (i (1- tree-depth)) (format stream "  "))
	  (format stream "~a ~a ~tgain ~a~%" 
			  (arp-tree-value tree)
			  (arp-tree-indexes tree)
			  (arp-tree-gain tree))))
  (dolist (child (arp-tree-children tree))
	(pprint-arp-tree stream child (1+ tree-depth))))

(set-pprint-dispatch 'arp-tree #'pprint-arp-tree)

(defun find-child (value node)
  (find-if 
	(lambda (child) (equal value (arp-tree-value child)))
	(arp-tree-children node)))

(defun add-child (index value node)
  (let ((child (find-child value node)))
	(if child
	  (progn
		(push index (arp-tree-indexes child))
		child)
	  (let ((new-child (make-arp-tree :indexes (list index) :value value :parent node))) 
		(push new-child (arp-tree-children node))
		new-child))))

(defun add-list (index lst node)
  (if (not (null lst))
	(add-list (1+ index) (cdr lst) (add-child index (car lst) node))))

(defun child-node-count (node)
  (let ((sum 0))
	(dolist (child (arp-tree-children node))
	  (incf sum (child-node-count child)))
	(+ (length (arp-tree-children node))
	   sum)))

; ----- testing

(load "fifo.lisp")

(defconstant max-arp-len 5)

(defun prune-tail-nils (lst)
  (let ((last-not-null-item (position-if-not #'null lst :from-end t)))
	(if last-not-null-item
	  (subseq lst 0 (1+ last-not-null-item)))))

(defun build-arp-tree (lst)
  (let ((active-substr (make-fifo))
		(arp-tree (make-arp-tree))
		(index 0))
	(setf lst (fifo-populate active-substr lst max-arp-len))

	(loop while (> (fifo-size active-substr) 0) do
		  (if (not (null (first (fifo-head active-substr))))
			(add-list index (prune-tail-nils (fifo-head active-substr)) arp-tree))
		  (fifo-pop active-substr)
		  (incf index)
		  (if lst (fifo-push (pop lst) active-substr)))
	arp-tree))

(let ((testtree (build-arp-tree '(a a a b c nil nil b c a a a a))))
  (calc-arp-tree-gain testtree)
  ;(pprint testtree)
  (format t "~a~%" (child-node-count testtree)))
