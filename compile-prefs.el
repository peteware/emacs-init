(provide 'compile-prefs)
(require 'use-package)

(use-package pw-trunc-lines
  :init
  (progn
    (add-hook 'compilation-mode-hook 'pw/trunc-lines)))
  
;; I use `M-x compile' to do builds and I like the *compilation*
;; window to continuously scroll.  Below, I bind `^C-c' to
;; `compile'
;; I also like my compilation window to truncate long lines mostly
;; for the long link lines
(setq compilation-scroll-output 'first-error)
(add-hook 'compilation-mode-hook 'pw/no-line-column-number)

(defun pw/ansi-color (begin end)
  (interactive "r")
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region begin end)))
(defun pw/colorize-compilation-buffer ()
  (pw/ansi-color compilation-filter-start (point-max)))
  
(if (and (boundp 'compilation-fiter-hook) (fboundp 'ansi-color-for-comint-mode-on))
    (add-hook 'compilation-filter-hook 'pw/colorize-compilation-buffer))

