(provide 'pw-misc)

(defun pw/prev-frame ()
  "Switch to the previous frame."
  (interactive)
  (other-frame -1))

(defvar pw/font-size-list '(120 140 160 180 280 340)
  "List of font sizes for pw/font-size to iterate through.")

(defvar pw/font-size-index 3
  "Index of current font size for pw/font-size-change to iterate through.")

(defun pw/font-size-decrease()
  "Cycle through pw/font-size-list of fonts"
  (interactive)
  (setq pw/font-size-index (max 0 (- pw/font-size-index 1)))
  (set-face-attribute 'default (selected-frame) :height (nth pw/font-size-index pw/font-size-list)))

(defun pw/font-size-increase()
  "Set the font size to increase"
  (interactive)
  (setq pw/font-size-index (min (- (length pw/font-size-list) 1) (+ pw/font-size-index 1)))
  (set-face-attribute 'default (selected-frame) :height (nth pw/font-size-index pw/font-size-list)))

(defun pw/ediff-current(arg)
  "Run ediff-vc-internal on the current file against it's latest revision.
If prefix arg, use it as the revision number"
  (interactive "P")
  (require 'ediff)
  (ediff-load-version-control t)
  (let ((rev (if arg (format "%d" arg) "")))
    (funcall
     (intern (format "ediff-%S-internal" ediff-version-control-package))
     rev "" nil))
  )

(defun pw/reindent ()
  "Reindent and cleanup whitespace.  If the region is active then
this only applies to the region otherwise the entire buffer.
See `whitespace-style' to customize which whitespace is cleaned."
  (interactive)
  (if (use-region-p)
      (progn
	(indent-region (region-beginning) (region-end))
	(whitespace-cleanup-region (region-beginning) (region-end)))
    ;; else
    (indent-region (point-min) (point-max))
    (whitespace-cleanup)))

(defun pw/no-line-column-number ()
  "Turn off line-number-mode and column-number-mode."
  (interactive)
  (line-number-mode -1)
  (column-number-mode -1))

(defun pw/region-is-active-p()
  "Return if the region is active.  Accounts for difference between
Emacs and XEmacs."
  (if (fboundp 'region-active-p)
      (region-active-p)
    mark-active))

(defun pw/eval-region-or-defun ()
  "Eval the active region (if it's active) otherwise the current defun."
  (interactive)
  (if (pw/region-is-active-p)
      (eval-region (region-beginning) (region-end))
    (eval-defun nil)))
