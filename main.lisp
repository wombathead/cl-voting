(in-package #:cl-voting)


(defun define-ordering> (elements)
  "Define a total order ELEMENTS = (A B C D ...) = A > B > C > D > ..."
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


(defun rank-profile->score-profile (rank scoring-rule )

  )


(defun main ()

  )

(loop with partial = '((a b c))
      for manipulation in (all-permutations '(a b c))
      for profile = (append partial (list manipulation))
      for outcome = (multiwinner-golden-ticket-k-approval 2 2 profile)
      do (progn
           (format t "~%Profile: ~A, Outcome: ~A"
                   profile
                   outcome
                   )
           )
      )

;; iterate over fixed votes of the other player
(loop initially (format t "~%~%Starting~%~%")
      for partial in (all-permutations '(a b c))
      do (progn
           (loop with sincere = '(a b c)
                 for report in (all-permutations sincere)
                 for profile = (cons report (list partial))
                 for outcome = (multiwinner-golden-ticket-k-approval 2 2 profile)
                 do (progn
                      (format t "Outcome on ~A: ~A~%" profile outcome))
                    finally (terpri))))


(defun manipulations-leading-to-outcome (partial-profile desired-outcome candidates voting-rule)
  (loop for bid in (all-permutations candidates)
        for profile = (cons bid (list partial-profile)) ;; TODO: make work with partial profiles that are already lists
        for outcome = (funcall voting-rule profile)
        if (equal outcome desired-outcome)
          collect bid))

(defun 2x2-game-as-bimatrix (ordering>-1 ordering>-2 candidates)
  (loop initially (format t "~%~%~A~%~%" (all-permutations candidates))

        with ordering>-fn-1 = (define-ordering> ordering>-1)
        with ordering>-fn-2 = (define-ordering> ordering>-2)
        with rule = (lambda (b) (multiwinner-golden-ticket-k-approval 2 2 b))
        for strategy-1 in (all-permutations candidates)
        do (progn
             (loop for strategy-2 in (all-permutations candidates)
                 for profile = (cons strategy-1 (list strategy-2))
                 for outcome = (funcall rule profile)
                 do (format t "~A " outcome))
             (terpri))

        )

  )
