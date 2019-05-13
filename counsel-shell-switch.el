(require 'counsel)
(provide 'counsel-shell-switch)
(defun pw/counsel-switch-to-shell-buffer ()
  "Switch to a shell buffer, or create one."
  (interactive)
  (ivy-read "Shell buffer: " (counsel--buffers-with-mode #'shell-mode)
            :action #'pw/counsel--switch-to-shell
            :caller 'pw/counsel-switch-to-shell-buffer))

(defun pw/counsel--switch-to-shell (name)
  "Display shell buffer with NAME and select its window.
Reuse any existing window already displaying the named buffer.
If there is no such buffer, start a new `shell' with NAME."
  (if (get-buffer name)
      (let ((start-buffer (buffer-name)))
        (pop-to-buffer name '((display-buffer-reuse-window
                               display-buffer-same-window)
                              (inhibit-same-window . nil)
                              (reusable-frames . visible)))
        (if (equal name start-buffer)
            (goto-char (point-max))))
    (shell name)))
