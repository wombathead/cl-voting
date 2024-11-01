(in-package #:cl-voting)


(defun k-approval-vector (m k)
  (loop for j from 1 upto m
        collect (if (<= j k) 1 0)))


(defun borda-vector (m)
  (loop for j from 1 upto m
        collect (- m j)))


(defun plurality-vector (m)
  (k-approval-vector m 1))


(defun golden-ticket-vector (n scoring-vector)
  "Replace first score with N+1 in SCORING-VECTOR"
  (cons (1+ n) (rest scoring-vector)))


(defun golden-ticket-k-approval-vector (m k n)
  (golden-ticket-vector n (k-approval-vector m k)))
