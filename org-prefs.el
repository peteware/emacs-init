(provide 'org-prefs)
(require 'org)

;(if (not (fboundp 'assoc-default))
;    (defun assoc-default (key alist &optional test default)
;      (cdr (assoc-if test alist :key key))))

(defun pw/c++-orgstruct ()
  (interactive)
  (make-local-variable 'orgstruct-heading-prefix-regexp)
  (setq orgstruct-heading-prefix-regexp "^// ")
  (turn-on-orgstruct))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cr" 'org-capture)
;(define-key global-map "\C-cr" 'org-capture)
(add-hook 'org-mode-hook 'turn-on-font-lock)  ; Org buffers only
(add-hook 'c++-mode-hook 'pw/c++-orgstruct)
(add-hook 'c-mode-hook 'pw/c++-orgstruct)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(setq calendar-latitude 40.76)
(setq calendar-longitude -73.96)
(setq org-log-done 'time)
(setq org-cycle-include-plain-lists t)
(setq org-agenda-text-search-extra-files '(agenda-archives "~/notes/cmds.org"))
(setq org-agenda-start-with-follow-mode nil)
(setq org-agenda-window-setup 'other-frame)
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-agenda-dim-blocked-tasks t)
(setq org-directory "~/notes/")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-time-grid
      '((daily today)
        #("----------------" 0 16
          (org-heading t))
        (800 1000 1200 1400 1600 1800 2000)))
(setq org-agenda-files '("~/notes/frd.org" "~/notes/xndf.org" "~/notes/fxfa.org" "~/notes/todo.org" "~/notes/drqs.org" "~/notes/events.org"))
(setq org-global-properties
        '((Effort_ALL . "0 0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00")))
(setq org-columns-default-format "%60ITEM(Task) %17Effort(Estimated Effort){:} %CLOCKSUM")
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/notes/todo.org" "Bloomberg Current")
         "** TODO %?%i \n   Added: %U\n")
        ("c" "Code" entry (file+headline "~/notes/code.org" "Recent Code Reviews")
         "** TODO %?%t\n*** %i\n")
        ("n" "Notes" entry (file+datetree "~/notes/notes.org")
         "** %?\n")
        ("d" "Drqs" entry (file+headline "~/notes/drqs.org" "Active DRQS")
         "** TODO %?%i \n   Added: %U\n")
        ("F" "FRD" entry (file+headline "~/notes/frd.org" "ToDo")
         "** TODO %?%i \n   Added: %U\n")
        ("f" "FXFA" entry (file+headline "~/notes/fxfa.org" "ToDo")
         "** TODO %?%i \n   Added: %U\n")
        ("x" "XNDF" entry (file+headline "~/notes/xndf.org" "ToDo")
         "** TODO %?%i\n   Added: %U\n")))

(setq org-todo-keywords
      '((sequence "TODO" "NEXT" "|" "DONE")))
(setq org-agenda-custom-commands
      '(("D" "Daily ActionList"
         ((agenda ""
                  ((org-agenda-span 1)
                   (org-agenda-show-log t)
                   (org-agenda-sorting-strategy '((agenda time-up priority-down tag-up)))
                   (org-deadline-warning-days 0))
                  (org-agenda-log-mode-items '(closed clock state))
                  )
          (todo "NEXT")))
        ("W" "Weekly Plan"
         ((agenda ""
                  ((org-agenda-span 'week)
                   (org-agenda-show-log t)
                   (org-agenda-sorting-strategy '((agenda time-up priority-down tag-up)))
                   (org-deadline-warning-days 0))
                  (org-agenda-log-mode-items '(closed clock state))
                  )
          (todo "NEXT")
          (todo "TODO")
          (tags-todo "DRQS")))
        ))
