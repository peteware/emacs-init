(provide 'pw-switch-buffer)

(cond
 ((fboundp 'ido-mode)
  ;; `ido-mode` does for find-file what iswitchb-mode does
  ;; for switch-to-buffer.  It was cool but I found it
  ;; it slowed down on big directories too much and
  ;; some very annoying interaction with tramp
  (setq ido-default-buffer-method 'selected-window)
  (setq ido-default-file-method 'selected-window)
  (setq ido-enable-flex-matching t)
  (setq ido-enable-dot-prefix t)
  (setq ido-enable-tramp-completion nil)
  (setq ido-max-directory-size 30000)
  (setq ido-rotate-file-list-default t)
  (setq ido-enter-matching-directory 'first)
  (setq ido-use-virtual-buffers 'auto)
  (setq ido-separator "|")
  (setq ido-ignore-files (append ido-ignore-files '("\\`00")))
  (setq ido-ignore-buffers
        (list "\\` " ".*Completions.*"))
  (setq ido-work-directory-list-ignore-regexps
        (list "/bb/bin" "/bb/data" "/bb/data/tmp" "/bbsrc/apputil"))
  (ido-mode 1))
 ((fboundp 'iswitchb-mode)
    ;; `iswitchb-mode' provides a nice completion for switching between
    ;; buffers.  The `iswitchb-use-virtual-buffers' and `recentf-mode'
    ;; adds recent files to the match 
    (setq iswitchb-default-method 'samewindow
          iswitchb-max-to-show 5
          iswitchb-use-virtual-buffers t)
    (recentf-mode 1)
    (iswitchb-mode 1))
 ((fboundp 'icomplete-mode)
  (icomplete-mode 1))
  )
