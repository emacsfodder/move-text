[![MELPA](https://melpa.org/packages/move-text-badge.svg)](https://melpa.org/#/move-text)
[![MELPA Stable](https://stable.melpa.org/packages/move-text-badge.svg)](https://stable.melpa.org/#/move-text)

# MoveText (2.0.8)

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
