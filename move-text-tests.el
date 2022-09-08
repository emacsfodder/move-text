;;; move-text-tests.el --- Tests for Move-Text -*- lexical-binding: t; -*-
;;
;; Created: September 07, 2022

;;; Code:

(require 'move-text)
(require 'ert)

(defmacro should-on-temp-buffer (input-string expect-string &rest body)
  "INPUT-STRING processed by BODY forms in a temp buffer should equal EXPECT-STRING."
  (declare (indent 1) (debug t))
  `(should (string= ,expect-string
                    (with-temp-buffer
                    ;; (with-current-buffer-window "*ERT-should-temp-buffer*"
                    ;;   (erase-buffer)
                      (insert ,input-string)
                      (goto-char (point-min))
                      ,@body
                      (buffer-string)))))

(let ((transient-mark-mode t))

  (ert-deftest move-line-down-test ()
    "Move text down by (1) one line, (2) by region."
    (should-on-temp-buffer
        "This is a test
Line 2
Line 3
"
      "Line 2
This is a test
Line 3
"
      (call-interactively #'move-text-down))
    (should-on-temp-buffer
        "This is a test
Line 2
Line 3
Line 4
"
        "This is a test
Line 3
Line 2
Line 4
"
        (forward-line)
        (call-interactively #'move-text-down))
    )

  (ert-deftest move-region-down-test ()
    (should-on-temp-buffer
        "This is a test
Line 2
Line 3
Line 4
Line 5
Line 6
"
        "This is a test
Line 2
Line 5
Line 3
Line 4
Line 6
"
        (let (reg-beg reg-end)
          (forward-line 2)
          (setq reg-beg (point))
          (activate-mark)
          (forward-line 2)
          (setq reg-end (point))
          ;; TODO: (call-interactively #'move-text-down t (vector reg-beg reg-end 1))
          (move-text-region-down reg-beg reg-end 1)
          )
        )
    )

  (ert-deftest move-line-up-test ()
     "Move text up by (1) one line, (2) by region."
     (should-on-temp-buffer
       "This is a test
Line 2
Line 3
"
     "This is a test
Line 3
Line 2
"
     (forward-line 2)
     (call-interactively #'move-text-up)))

  (ert-deftest move-region-up-test ()
     (should-on-temp-buffer
         "This is a test
Line 2
Line 3
Line 4
Line 5
Line 6
"
       "Line 2
Line 3
This is a test
Line 4
Line 5
Line 6
"
       (let (reg-beg reg-end)
         (forward-line)
         (setq reg-beg (point))
         (activate-mark)
         (forward-line 2)
         (setq reg-end (point))
         (move-text-region-up reg-beg reg-end 1)
         )
       )
     )
  )



;;; move-text-tests.el ends here
