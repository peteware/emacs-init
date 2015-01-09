(provide 'pw-switch-buffer)

(cond
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
 ((fboundp 'ido-mode)
  ;; `ido-mode` does for find-file what iswitchb-mode does
  ;; for switch-to-buffer.  It was cool but I found it
  ;; it slowed down on big directories too much and
  ;; some very annoying interaction with tramp
  (setq ido-default-buffer-method 'samewindow)
  (setq ido-enable-tramp-completion nil)
  (setq ido-ignore-buffers
        (list "\\'" ".*Completions.*"))
  (setq ido-work-directory-list-ignore-regexps
        (list "/bb/bin" "/bb/data" "/bb/data/tmp" "/bbsrc/apputil"))
  (ido-mode 1)
  ))
