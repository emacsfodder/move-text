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

  (ert-deftest move-text-down-test ()
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

;;     (should-on-temp-buffer
;;         "This is a test
;; Line 2
;; Line 3
;; Line 4
;; Line 5
;; Line 6
;; "
;;         "This is a test
;; Line 2
;; Line 4
;; Line 3
;; Line 5
;; Line 6
;; "
;;       ;; (set-mark 16)
;;       ;; (goto-char 27)
;;       ;; (activate-mark)
;;         (let ((beg 16)
;;               (end 27))
;;           (goto-char 0)
;;           (forward-line 3)
;;           (setq beg (point))
;;           (forward-line 1)
;;           (setq end (point))
;;           (move-text-region-down beg end 1)
;;           (message (buffer-string)))

;;         )
    )

  (ert-deftest move-text-up-test ()
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
     (goto-char 24)
     (call-interactively #'move-text-up))

;;      (should-on-temp-buffer
;;          "This is a test
;; Line 2
;; Line 3
;; Line 4
;; Line 5
;; Line 6
;; "
;;        "Line 2
;; Line 3
;; This is a test
;; Line 4
;; Line 5
;; Line 6
;; "
;;        (goto-char 0)
;;        (forward-line 2)
;;        (forward-char 2)
;;        (activate-mark)
;;        (forward-line)
;;        (end-of-line)
;;        (backward-char 2)
;;        (message "Test Region: string \"%s\"" (buffer-substring-no-properties (region-beginning) (region-end)))
;;        (move-text-up))))
     )
  )



;;; move-text-tests.el ends here
