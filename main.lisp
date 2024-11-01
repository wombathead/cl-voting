(in-package #:cl-voting)



(defun manipulations-leading-to-outcome (partial-profile desired-outcome candidates voting-rule)
  "TODO"
  (loop for bid in (all-permutations candidates)
        for profile = (cons bid (list partial-profile)) ;; TODO: make work with partial profiles that are already lists
        for outcome = (funcall voting-rule profile)
        if (equal outcome desired-outcome)
          collect bid))

(defun 2x2-game-as-bimatrix (ordering>-1 ordering>-2 candidates)
  "TODO"
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


(defun rank-profile->score-profile (rank scoring-rule )
  "TODO"
  )


(defun plot-2x2-rule-outcomes (candidates voting-rule &optional (tie-breaking-rule (define-ordering> candidates)))
  ;; TODO: make the voting rule REQUIRE a tie breaking rule I think.
  (let ((all-rankings (all-permutations candidates)))
    (loop initially (progn
                      (format t "~%~%Starting~%~%")
                      (format t "~10@t~{~A~^~t~}~%" all-rankings))

          for partial in all-rankings
          do (progn
               (loop initially (format t "~8@A~t" partial)
                     with sincere = candidates
                     for report in all-rankings
                     for profile = (cons report (list partial))
                     for outcome = (funcall voting-rule profile tie-breaking-rule)

                     collect outcome into outcomes
                     finally (format t "~{~7@A~t~}~%" outcomes))))))


(defun plot-2x2-outcomes (candidates &optional (tie-breaking-rule (define-ordering> candidates)))
  (let ((all-rankings (all-permutations candidates)))
    (loop initially (progn
                      (format t "~%~%Starting~%~%")
                      (format t "~10@t~{~A~^~t~}~%" all-rankings))

          for partial in all-rankings
          do (progn
               (loop initially (format t "~8@A~t" partial)
                     with sincere = candidates
                     for report in all-rankings
                     for profile = (cons report (list partial))
                     for outcome = (multiwinner-golden-ticket-k-approval 2 2 profile tie-breaking-rule)

                     collect outcome into outcomes
                     finally (format t "~{~7@A~t~}~%" outcomes))))))


(let* ((candidates '(a b c d e))
       (m (length candidates))
       (profile '((a b c d e)
                  (b a d e c)
                  (b c e a d)))
       (n (length profile))
       (rule (define-golden-ticket-k-approval m 2 n 3 candidates)))

  (print (compute-voting-rule rule profile)))
