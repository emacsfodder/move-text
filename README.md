# move-text

MoveText is extracted from Basic edit toolkit.
It allows you to move the current line using M-up / M-down (or any other bindings you choose) 
if a region is marked, it will move the region instead.

Install with `package-install` / `package-install-file`

If you want to use the default bindings, add the following to .emacs anywhere after `(package-initialize)`:

     (move-text-default-bindings)

