(in-package #:cl-voting)


(defmacro compute-and-print (&body body)
  `(let ((result ,@body))
     (format t "~A~%" result)
     result))


(defun cons-to-plist (key value plist)
  (let ((plist (copy-list plist)))
    (cons key (cons value plist))))


(defun sort-alist-by-value (alist predicate &optional tie-breaking-order)
  "Sort ALIST '((key . value) ...) by value according to PREDICATE, breaking ties according to TIE-BREAKING-ORDER on keys"
  (if tie-breaking-order
      (sort alist (lambda (x y)
                    (destructuring-bind ((xk . xv) (yk . yv)) (list x y)
                      (let ((result (funcall predicate xv yv)))
                        (if (eql result (funcall predicate yv xv))
                            (ordering> xk yk tie-breaking-order)
                            result)))))
      (sort alist predicate :key #'cdr)))


(defun sort-plist-by-value (plist predicate &optional tie-breaking-order)
  "Sort PLIST by value"
  (let ((alist (loop for (key value) on plist by #'cddr
                     collect (cons key value))))
    (sort-alist-by-value alist predicate tie-breaking-order)))


(defun alist->plist (alist)
  (loop for (key . value) in alist
        collect key
        collect value))


(defun all-permutations (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
                 append (mapcar (lambda (l) (cons element l))
                                (all-permutations (remove element list)))))))


(defun select-random (list)
  (nth (random (length list)) list))
