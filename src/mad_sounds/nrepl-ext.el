(defun plexus-nrepl-odoc ()
  "Get the doc for the current expression at point."
  (interactive)
  (let ((form (nrepl-sexp-at-point))
        (result-buffer (nrepl-popup-buffer nrepl-result-buffer nil)))
    (nrepl-send-string (format "(odoc %s)" form)
                       (nrepl-popup-eval-out-handler result-buffer)
                       (nrepl-current-ns)
                       (nrepl-current-tooling-session))))

(global-set-key (kbd "C-h o") 'plexus-nrepl-odoc)
