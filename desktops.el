(defvar desktops-configuration-alist nil
  "")

(defun desktops--read-desktop-name (&optional already-exist-desktops allow-new)
  (completing-read "desktop name: " already-exist-desktops nil (not allow-new)))


(defun desktops-save (&optional desktop-name window-configuration)
  (interactive (list (desktops--read-desktop-name desktops-configuration-alist t)))
  (let ((window-configuration (or window-configuration
                                  (current-window-configuration))))
    (if (assoc desktop-name desktops-configuration-alist)
        (setf (cdr (assoc desktop-name desktops-configuration-alist)) window-configuration)
      (push (cons desktop-name window-configuration) desktops-configuration-alist))))

(defun desktops-del (&optional desktop-name)
  (interactive (list (desktops--read-desktop-name desktops-configuration-alist)))
  (setq desktops-configuration-alist (assq-delete-all desktop-name desktops-configuration-alist)))

(defun desktops-restore (&optional desktop-name)
  (interactive (list (desktops--read-desktop-name desktops-configuration-alist)))
  (let ((previous-configuration (current-window-configuration))
        (configuration (cdr (assoc desktop-name desktops-configuration-alist))))
    (set-window-configuration configuration)
    (desktops-save "previous" previous-configuration)))

(provide 'desktops)
