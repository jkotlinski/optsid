(defstruct arp-tree
  (value nil)
  (times 0 :type integer)
  (parent nil)
  (children nil))

(defun find-child (value node)
  (find-if 
	(lambda (child) (equal value (arp-tree-value child)))
	(arp-tree-children node)))

(defun add-child (value node)
  (let ((child (find-child value node)))
	(if child
	  (progn
		(incf (arp-tree-times child))
		child)
	  (let ((new-child (make-arp-tree :times 1 :value value :parent node))) 
		(push new-child (arp-tree-children node))
		new-child))))

(defun add-list (lst node)
  (if (not (null lst))
	(add-list (cdr lst) (add-child (car lst) node))))

(defun child-node-count (node)
  (let ((sum 0))
	(dolist (child (arp-tree-children node))
	  (incf sum (child-node-count child)))
	(+ (length (arp-tree-children node))
	   sum)))

; ----- testing

#|
(let ((testtree (make-arp-tree)))
  (dolist (item
			'(((a . 1) (b . 2) nil (d . 4))
			  ((a . 2) (b . 2) nil (d . 4))
			  ((a . 2) (b . 2) nil (c . 4))
			  (nil (b . 2) nil (c . 4))))
	(add-list item testtree))

  (format t "~a~%" (child-node-count testtree)))
|#
