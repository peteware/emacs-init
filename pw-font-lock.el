(provide 'pw-font-lock)

;; Various ways of delaying fontification.  With these, the file is first
;; displayed and then in the background it is highlighted.  There are
;; different schemes between Emacs (jit-lock) and versions of XEmacs
(cond
 ((fboundp 'jit-lock-mode)
  (setq jit-lock-chunk-size 5000
        jit-lock-contextually 'syntax-driven
	jit-lock-context-time .6
	jit-lock-defer-time .1
	jit-lock-stealth-nice 0.1
	jit-lock-stealth-time 5
	jit-lock-stealth-verbose nil)
  (jit-lock-mode t)
  )
 ((fboundp 'turn-on-lazy-shot)
  (add-hook 'font-lock-mode-hook 'turn-on-lazy-shot))
 ((fboundp 'turn-on-lazy-lock)
  (add-hook 'font-lock-mode-hook 'turn-on-lazy-lock)
  (setq lazy-lock-stealth-time 10)
  (setq lazy-lock-minimum-size 10000)))

