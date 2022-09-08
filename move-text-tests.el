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
      (goto-char 0)
      (call-interactively #'move-text-down)))

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
Line 4
Line 3
Line 5
Line 6
"
        (forward-line 2)
        (push-mark)
        (activate-mark)
        (forward-line)
        (message "Mark at %d - Point at %d" (mark) (point))
        (move-text-down (mark) (point) 1)))

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
       (goto-char 0)
       (forward-line)
       (push-mark)
       (activate-mark)
       (forward-line 2)
       (move-text-up (mark) (point) 1))))

;;; move-text-tests.el ends here
