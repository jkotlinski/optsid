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

#| A simple FIFO.  New nodes are inserted at the tail, and removed
 | from the head.  The HEAD is the head of the list, and the TAIL is
 | the cons cell of the last member of the list.
 |#
(defstruct fifo
  (head nil)
  (tail nil)
  (size 0))

(defmacro fifo-list (f) `(fifo-head ,f))

(defun fifo-push (item f)
  "Append the given ITEM to the end of the fifo F"
  (let ((tmp (list item)))
    (if (= 1 (incf (fifo-size f)))
      (setf (fifo-head f) tmp)
      (setf (rest (fifo-tail f)) tmp))
    (setf (fifo-tail f) tmp)))

(defun fifo-populate (f lst cnt)
  "Move cnt items from lst to f. Return updated lst."
  (loop while (< (fifo-size f) cnt) do
		(fifo-push (pop lst) f))
  lst)

(defun fifo-pop (f)
  "Remove an item from the fifo."
  (assert (> (fifo-size f) 0))
  (decf (fifo-size f))
  (let ((item (pop (fifo-head f))))
	(unless (fifo-head f)
	  (setf (fifo-tail f) nil))
	item))

(defun starts-with-null-p (fifo) (null (first (fifo-head fifo))))
(defun ends-with-null-p (fifo) (null (first (fifo-tail fifo))))
(defun edge-null-p (fifo) (or (starts-with-null-p fifo) (ends-with-null-p fifo)))

