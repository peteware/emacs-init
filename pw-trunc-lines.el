(provide 'pw-trunc-lines)

(defun pw/trunc-lines (&optional arg)
  "Toggle `truncate-lines' so long lines stay on a single line instead of wrapping.
If arg is positive, always enable truncate lines.  If arg is negative or zero,
disable truncate lines."
  (interactive "P")
  (cond
   ((and arg (> (prefix-numeric-value arg) 0))
    (setq truncate-lines t))
   ((and arg (<= (prefix-numeric-value arg) 0))
    (setq truncate-lines nil))
   (t
    (setq truncate-lines (not truncate-lines)))))
