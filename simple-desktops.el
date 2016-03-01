(defvar sd-configuration-alist nil
  "")

(defvar sd-previous-desktop-name nil)

(defun sd--set-current-desktop-name (deskto-name)
  (set-frame-parameter (selected-frame) 'sd-current-desktop-name desktop-name))

(defun sd--get-current-desktop-name ()
  (frame-parameter (selected-frame) 'sd-current-desktop-name))

(defun sd--read-desktop-name (&optional already-exist-sd allow-new)
  (completing-read "desktop name: " already-exist-sd nil (not allow-new)))


(defun sd-save (&optional desktop-name window-configuration)
  (interactive (list (sd--read-desktop-name sd-configuration-alist t)))
  (let ((window-configuration (or window-configuration
                                  (current-window-configuration))))
    (if (assoc desktop-name sd-configuration-alist)
        (setf (cdr (assoc desktop-name sd-configuration-alist)) window-configuration)
      (push (cons desktop-name window-configuration) sd-configuration-alist))
    (sd--set-current-desktop-name desktop-name)))

(defun sd--resave ()
  (sd-save (sd--get-current-desktop-name) (current-window-configuration)))

(defun sd-del (&optional desktop-name)
  (interactive (list (sd--read-desktop-name sd-configuration-alist)))
  (setq sd-configuration-alist (assq-delete-all desktop-name sd-configuration-alist)))

(defun sd-restore (&optional desktop-name)
  (interactive (list (sd--read-desktop-name sd-configuration-alist)))
  (let ((current-configuration (current-window-configuration))
        (configuration (cdr (assoc desktop-name sd-configuration-alist))))
    (if (frame-live-p (window-configuration-frame configuration)) 
        (progn
          (sd-save (sd--get-current-desktop-name) current-configuration)
          (setq sd-previous-desktop-name (sd--get-current-desktop-name))
          (set-window-configuration configuration)
          (raise-frame (window-configuration-frame configuration))
          (sd--set-current-desktop-name desktop-name))
      (sd-del desktop-name)
      (warn "desktop:%s does not exist anymore" desktop-name))))

(defun sd-shift ()
  (interactive)
  (sd-restore sd-previous-desktop-name))

(defun sd-popup-restore-menu (&optional configuration-alist)
  (interactive (list sd-configuration-alist))
  (let* ((desktop-names (mapcar #'car configuration-alist))
         (menu-items (mapcar (lambda (name)
                               (cons name name))
                             desktop-names))
         (menu `("desktop-names" ("panel" ,@menu-items)))
         (selected-desktop-name (x-popup-menu t menu)))
    (sd-restore selected-desktop-name)))

(provide 'simple-desktops)
