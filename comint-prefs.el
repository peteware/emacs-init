(provide 'comint-prefs)

(defun comint-for-pete ()
  "Set up comint/shell/gud mode as I like.  Turn off all highlighting and generally do what you can to keep things fast.

Setup M-p finds matching input."
  (interactive)
  (font-lock-mode 1)                    ;If you find this slow, change it to 0
  (if (boundp 'font-lock-mode-disable-list)
      (add-to-list 'font-lock-mode-disable-list 'shell-mode))
  (if (fboundp 'ansi-color-for-comint-mode-on)
      (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))
  (make-variable-buffer-local 'line-number-mode)
  (make-variable-buffer-local 'column-number-mode)
  (setq line-number-mode nil)
  (setq column-number-mode nil)
  (setq comint-scroll-show-maximum-output nil)
  (setq comint-input-ignoredups t)
  (local-set-key "\M-p" 'comint-previous-matching-input-from-input)
  )

(defun pw/comint-reset-display (use-sh-syntax)
  "Figure out what emacs uses for X11 DISPLAY and send it in a form suitable to cut&paste.
A prefix arguments is sh syntax, default is csh syntax"
  (interactive "P")
  (let* ((proc (get-buffer-process (current-buffer)))
	 (pmark (process-mark proc))
	 (dpy (frame-parameter nil 'display))
	 (cmd (if use-sh-syntax (format "export DISPLAY=%s" dpy)
		(format "setenv DISPLAY %s" dpy))))
    (goto-char pmark)
    ;; If the process echoes commands, don't insert a fake command in
    ;; the buffer or it will appear twice.
    (unless comint-process-echoes
      (insert cmd) (insert "\n"))
    (sit-for 0)
    (comint-send-string proc cmd)
    (comint-send-string proc "\n")))
      

(defun dbx-for-pete()
  "Set up dbx/gud mode with a working break point."
  (interactive)
  (gud-def gud-break  "stop at \"%f\":%l"
	   "\C-b" "Set breakpoint at current line  --pete.")
  (gud-def gud-up "up %p; echo stopped in $vfunc at line $vlineno in file \\\"$vfile\\\""
	   "<" "Up (numeric arg) stack frames. --pete")
  (gud-def gud-down "down %p; echo stopped in $vfunc at line $vlineno in file \\\"$vfile\\\""
	   ">" "Down (numeric arg) stack frames. --pete")
  (gud-def gud-next "next %p; echo stopped in $vfunc at line $vlineno in file \\\"$vfile\\\""
	   "n" "Next statement (numeric arg). --pete")
  (gud-def gud-step "step %p; echo stopped in $vfunc at line $vlineno in file \\\"$vfile\\\""
	   "s" "Step into (numeric arg). --pete")
  ; I had trouble with "field" properties working with dbx
  (make-local-variable 'comint-use-prompt-regexp)
  (setq comint-use-prompt-regexp t))
  

(defun pw/turn-off-fontlock ()
  (interactive)
  (font-lock-mode 0))

;(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
;(add-hook 'comint-mode-hook		'comint-for-pete)
;;(add-hook 'comint-shell-hook		'turn-off-fontlock)
;(add-hook 'dbx-mode-hook 'dbx-for-pete)

