(in-package #:cl-voting)


(defun initialise-scores-alist (candidates)
  (mapcar (lambda (c) (cons c 0)) candidates))


(defun get-candidate-score-alist (candidate scores)
  (cdr (assoc candidate scores)))


(defun add-scores-alists (scores deltas)
  (loop for (candidate . delta) in deltas
        collect (cons candidate (+ delta (get-candidate-score-alist candidate scores)))))


(defun scoring-fn (&rest si)
  "Assign scores s1, s2, ..., sm to ranks 1, 2, ..., m"
  si)


(defun ranking->scores (ranking scoring-rule)
  "Takes a RANKING and assigns points according to SCORING-RULE"
  (loop for candidate in ranking
        for si in scoring-rule
        collect (cons candidate si)))


(defun apply-scoring-rule (scoring-rule rankings)
  (loop with candidates = (first rankings)
        with scores = (initialise-scores-alist candidates)
        for ranking in rankings
        do (setf scores (add-scores-alists scores (ranking->scores ranking scoring-rule)))
        finally (return scores)))


(defun compute-multiwinner-scoring-rule (winners scoring-rule rankings &optional tie-breaking-rule)
  "Apply SCORING-RULE to RANKINGS and select number of winners equal to WINNERS"
  (flet ((top-k-candidates (scores)
           (mapcar #'first (subseq (sort-alist-by-value scores #'> tie-breaking-rule) 0 winners))))
    (top-k-candidates (apply-scoring-rule scoring-rule rankings))))


(defun compute-single-winner-scoring-rule (scoring-rule profiles &optional tie-breaking-rule)
  (first (compute-multiwinner-scoring-rule 1 scoring-rule profiles tie-breaking-rule)))


(defun single-winner-plurality (rankings &optional tie-breaking-rule)
  (let ((plurality (cons 1 (loop repeat (1- (length (first rankings))) collect 0))))
    (compute-single-winner-scoring-rule plurality rankings tie-breaking-rule)))


(defun single-winner-borda (rankings &optional tie-breaking-rule)
  (let ((borda (loop with m = (length (first rankings))
                     for j from 1 upto m
                     collect (- m j))))
    (compute-single-winner-scoring-rule borda rankings tie-breaking-rule)))


(defun multiwinner-k-approval (winners k rankings &optional tie-breaking-rule)
  (let ((k-approval (loop with m = (length (first rankings))
                          for j from 1 upto m
                          collect (if (<= j k) 1 0))))
    (compute-multiwinner-scoring-rule winners k-approval rankings tie-breaking-rule)))


(defun single-winner-k-approval (k rankings &optional tie-breaking-rule)
  "Voters can approve of k candidates"
  (multiwinner-k-approval 1 k rankings tie-breaking-rule))


(defun multiwinner-golden-ticket-k-approval (winners k rankings &optional tie-breaking-rule)
  (let ((scoring-rule (loop with n = (length rankings)
                            with m = (length (first rankings))
                            for j from 1 upto m
                            collect (cond
                                      ((= j 1) (1+ n))
                                      ((<= j k) 1)
                                      (t 0)))))

    (compute-multiwinner-scoring-rule winners scoring-rule rankings tie-breaking-rule)))
