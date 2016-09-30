;;; move-text.el --- Move current line or region with M-up or M-down.

;; filename: move-text.el
;; Description: Move current line or region with M-up or M-down.
;; Author: Jason M <jasonm23@gmail.com>
;; Keywords: edit
;; Url: https://github.com/emacsfodder/move-text
;; Compatibility: GNU Emacs 25.1
;; Version: 2.0.0
;;
;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; MoveText 2.0.0 is a re-write of the old move-text and compatible with >= Emacs 25.1
;;
;; It allows you to move the current line using M-up / M-down if a
;; region is marked, it will move the region instead.
;;

;;; Installation:
;;
;; Put move-text.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'move-text)
;; (move-text-default-bindings)

;;; Acknowledgements:
;;
;;  Original v1.x was a Feature extracted from basic-edit-toolkit.el - by Andy Stewart (LazyCat)
;;

;;; Code:

;;;###autoload
(defun move-text-at-last-line-p ()
  "Predicate, point at the last line?"
  (equal (count-lines (point-min) (point)) (count-lines (point-min) (point-max))))

(defun move-text-at-first-line-p ()
  "Predicate, point at the first line?"
  (pcase (count-lines (point-min) (+ (point) 1)) ((or 0 1) t)))

;;;###autoload
(defun move-line-up ()
  "Move the current line up."
  (interactive)
  (if (move-text-at-last-line-p)
      (let ((target-point))
        (message "At last line")
        (kill-whole-line)
        (forward-line -1)
        (beginning-of-line)
        (setq target-point (point))
        (yank)
        (unless (looking-at "\n")
          (newline))
        (goto-char target-point))
    (progn (transpose-lines 1)
           (forward-line -2))))

;;;###autoload
(defun move-line-down ()
  "Move the current line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

;;;###autoload
(defun move-region (start end n)
  "Move the current region (START END) up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

;;;###autoload
(defun move-region-up (start end n)
  "Move the current region (START END) up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))

;;;###autoload
(defun move-region-down (start end n)
  "Move the current region (START END) down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))

;;;###autoload
(defun move-text-up (&optional start end n)
  "Move the line or region (START END) up by N lines."
  (interactive "r\np")
  (if (not (move-text-at-first-line-p))
    (if (region-active-p)
        (move-region-up start end n)
      (move-line-up))))

;;;###autoload
(defun move-text-down (&optional start end n)
  "Move the line or region (START END) down by N lines."
  (interactive "r\np")
  (if (region-active-p)
      (move-region-down start end n)
    (move-line-down)))

;;;###autoload
(defun move-text-default-bindings ()
  "Use default bindings for move-text-up and move-text-down (M-up / M-down)."
  (interactive)
  "Bind `move-text-up' and `move-text-down' to M-up and M-down."
  (global-set-key [M-down] 'move-text-down)
  (global-set-key [M-up] 'move-text-up))

(provide 'move-text)

;;; move-text.el ends here
