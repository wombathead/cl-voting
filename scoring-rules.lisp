(in-package #:cl-voting)

;;;; scoring-rules.lisp
;;;; Define scoring rules

;;; This group of functions allows us to define tie breaking rules amongst candidates

(defun define-ordering> (elements)
  "Define a total order ELEMENTS = '(A B C D ...) = A > B > C > D > ..."
  (lambda (x y) (< (position x elements) (position y elements))))


(defun define-ordering< (elements)
  (define-ordering> (reverse elements)))


(defun ordering> (x y ordering)
  (funcall ordering x y))


(defun ordering< (x y ordering)
  (ordering> y x ordering))


(defun ordering<= (x y ordering)
  (not (ordering> x y ordering)))


(defun ordering>= (x y ordering)
  (not (ordering< x y ordering)))


;;; This group of functions lets us assign scores to candidates

(defun initialise-scores-alist (candidates)
  (mapcar (lambda (c) (cons c 0)) candidates))


(defun get-candidate-score-alist (candidate scores)
  (cdr (assoc candidate scores)))


(defun add-scores-alists (scores deltas)
  (loop for (candidate . delta) in deltas
        collect (cons candidate (+ delta (get-candidate-score-alist candidate scores)))))


(defun ranking->scores (ranking scoring-rule)
  "Takes a RANKING and assigns points according to SCORING-RULE"
  ;; TODO
  (loop for candidate in ranking
        for si in scoring-rule
        collect (cons candidate si)))


#| OLD
(defun compute-multiwinner-scoring-rule (winners scoring-rule rankings &optional tie-breaking-rule)
  "Apply SCORING-RULE to RANKINGS and select number of winners equal to WINNERS"
  (flet ((top-k-candidates (scores)
           (mapcar #'first (subseq (sort-alist-by-value scores #'> tie-breaking-rule) 0 winners))))
    (top-k-candidates (apply-scoring-rule scoring-rule rankings))))


(defun compute-single-winner-scoring-rule (scoring-rule profiles &optional tie-breaking-rule)
  (first (compute-multiwinner-scoring-rule 1 scoring-rule profiles tie-breaking-rule)))
|#


(defun apply-scoring-rule-to-ranking (scoring-rule ranking)
  "Assign points specified in SCORING-RULE = (s1 s2 ... sm) to candidates in RANKING = '(c1 c2 ... cm)"
  (mapcar (lambda (ri si) (cons ri si)) ranking scoring-rule))


(defun apply-scoring-rule-to-profile (scoring-rule profile)
  (mapcar (lambda (ranking) (apply-scoring-rule-to-ranking scoring-rule ranking)) profile))


(defun scoring-rule-totals (scoring-rule profile)
  (flet ((apply-scoring-rule ()
             (mapcar (lambda (ranking) (apply-scoring-rule-to-ranking scoring-rule ranking)) profile)))
    (reduce #'add-scores-alists (apply-scoring-rule))))


(defun score-sorting-function (predicate ordering)
  (lambda (x y)
    (destructuring-bind ((xk . xv) (yk . yv)) (list x y)
      (let ((result (funcall predicate xv yv)))
        (if (eql result (funcall predicate yv xv))
            (ordering> xk yk ordering)
            result)))))


(defun top-candidates (k alist ordering)
  "Return the K highest scoring candidates in ALIST, breaking ties with ORDERING"
  (subseq (sort alist (score-sorting-function #'> ordering)) 0 k))


(defun define-multiwinner-scoring-rule (winners scoring-vector ordering)
  (let ((ordering (define-ordering> ordering)))
    (lambda (profile)
      (mapcar #'car (top-candidates winners
                                    (scoring-rule-totals scoring-vector profile)
                                    ordering)))))


(defun define-single-winner-scoring-rule (scoring-vector ordering)
  (lambda (profile)
    (first (funcall (define-multiwinner-scoring-rule 1 scoring-vector ordering) profile))))


(defun define-single-winner-k-approval (m k ordering)
  (define-single-winner-scoring-rule (k-approval-vector m k) ordering))


(defun define-single-winner-borda (m ordering)
  (define-single-winner-scoring-rule (borda-vector m) ordering))


(defun define-single-winner-plurality (m ordering)
  (define-single-winner-scoring-rule (plurality-vector m) ordering))


(defun define-golden-ticket-k-approval (m k n winners ordering)
  (unless (< winners n)
    (define-multiwinner-scoring-rule winners (golden-ticket-k-approval-vector m k n) ordering)))


(defun compute-voting-rule (rule profile)
  (funcall rule profile))
