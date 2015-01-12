(provide 'compile-prefs)
(require 'use-package)

;; I use `M-x compile' to do builds and I like the *compilation*
;; window to continuously scroll.  Below, I bind `^C-c' to
;; `compile'
;; I also like my compilation window to truncate long lines mostly
;; for the long link lines
(setq compilation-scroll-output 'first-error)

(use-package pw-misc
  :init
  (add-hook 'compilation-mode-hook 'pw/no-line-column-number))

(use-package ansi-color
  :init
  (if (and (boundp 'compilation-fiter-hook) (fboundp 'ansi-color-apply-on-region))
      (add-hook 'compilation-filter-hook 'pw/colorize-compilation-buffer)))

  

