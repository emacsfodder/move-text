[![Move-Text CI Tests](https://github.com/emacsfodder/move-text/actions/workflows/test.yml/badge.svg)](https://github.com/emacsfodder/move-text/actions/workflows/test.yml)
[![MELPA](https://melpa.org/packages/move-text-badge.svg)](https://melpa.org/#/move-text)
[![MELPA Stable](https://stable.melpa.org/packages/move-text-badge.svg)](https://stable.melpa.org/#/move-text)

# Move Text

MoveText 
allows you to move the current line using M-up / M-down (or any other bindings you choose)
if a region is marked, it will move the region instead.

Using the prefix arg (C-u *number* or META *number*) will predetermine how many lines to move.

Install from MELPA (or MELPA stable)

```
M-x package-install move-text <RETURN>
```

If you want to use the default bindings, add the following to .emacs
anywhere after `(package-initialize)`:

```
(move-text-default-bindings)
```
This sets the keyboard shortcuts:

-  <kbd>Meta</kbd>-<kbd>up</kbd> `move-text-up` (line or active region)
-  <kbd>Meta</kbd>-<kbd>down</kbd> `move-text-down` (line or active region)

## Demonstration

![](move-text.gif)

### Indent after moving...

[@jbreeden](https://github.com/jbreeden) gave us this useful function advice to have Emacs re-indent the text in-and-around a text move.

```lisp
(defun indent-region-advice (&rest ignored)
  (let ((deactivate deactivate-mark))
    (if (region-active-p)
        (indent-region (region-beginning) (region-end))
      (indent-region (line-beginning-position) (line-end-position)))
    (setq deactivate-mark deactivate)))

(advice-add 'move-text-up :after 'indent-region-advice)
(advice-add 'move-text-down :after 'indent-region-advice)
```
