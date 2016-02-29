(defvar desktops-configuration-alist nil
  "")
(defvar desktops-current-desktop-name nil)

(defun desktops--read-desktop-name (&optional already-exist-desktops allow-new)
  (completing-read "desktop name: " already-exist-desktops nil (not allow-new)))


(defun desktops-save (&optional desktop-name window-configuration)
  (interactive (list (desktops--read-desktop-name desktops-configuration-alist t)))
  (let ((window-configuration (or window-configuration
                                  (current-window-configuration))))
    (if (assoc desktop-name desktops-configuration-alist)
        (setf (cdr (assoc desktop-name desktops-configuration-alist)) window-configuration)
      (push (cons desktop-name window-configuration) desktops-configuration-alist))
    (setq desktops-current-desktop-name desktop-name)))

(defun desktops-del (&optional desktop-name)
  (interactive (list (desktops--read-desktop-name desktops-configuration-alist)))
  (setq desktops-configuration-alist (assq-delete-all desktop-name desktops-configuration-alist)))

(defun desktops-restore (&optional desktop-name)
  (interactive (list (desktops--read-desktop-name desktops-configuration-alist)))
  (let ((previous-configuration (current-window-configuration))
        (configuration (cdr (assoc desktop-name desktops-configuration-alist))))
    (if (set-window-configuration configuration)
        (raise-frame (window-configuration-frame configuration)))
    (desktops-save desktops-current-desktop-name previous-configuration)
    (desktops-save "previous" previous-configuration)
    (setq desktops-current-desktop-name desktop-name)))

(defun desktops-popup-restore-menu (&optional configuration-alist)
  (interactive (list desktops-configuration-alist))
  (let* ((desktop-names (mapcar #'car configuration-alist))
         (menu-items (mapcar (lambda (name)
                               (cons name name))
                             desktop-names))
         (menu `("desktop-names" ("panel" ,@menu-items)))
         (selected-desktop-name (x-popup-menu t menu)))
    (desktops-restore selected-desktop-name)))

(provide 'desktops)
