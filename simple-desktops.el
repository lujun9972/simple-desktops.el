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
  "Save WINDOW-CONFIGURATION as DESKTOP-NAME"
  (interactive (list (sd--read-desktop-name sd-configuration-alist t)))
  (let ((window-configuration (or window-configuration
                                  (current-window-configuration))))
    (if (assoc desktop-name sd-configuration-alist)
        (setf (cdr (assoc desktop-name sd-configuration-alist)) window-configuration)
      (push (cons desktop-name window-configuration) sd-configuration-alist))
    (sd--set-current-desktop-name desktop-name)))

(defun sd--resave ()
  "Save current-window-configuration as current-desktop-name"
  (sd-save (sd--get-current-desktop-name) (current-window-configuration)))

(defun sd-del (&optional desktop-name)
  "Delete DESKTOP-NAME related window-configuration"
  (interactive (list (sd--read-desktop-name sd-configuration-alist)))
  (setq sd-configuration-alist (assq-delete-all desktop-name sd-configuration-alist)))

(defun sd--valid-window-configuration-p (configuration)
  "Is CONFIGURATION is a valid window-configuration which means the related frame is alive"
  (and (window-configuration-p configuration)
       (frame-live-p (window-configuration-frame configuration))))

(defun sd-restore (&optional desktop-name)
  "Restore DESKTOP-NAME related window-configuration"
  (interactive (list (sd--read-desktop-name sd-configuration-alist)))
  (let ((current-configuration (current-window-configuration))
        (configuration (cdr (assoc desktop-name sd-configuration-alist))))
    (if (sd--valid-window-configuration-p configuration) 
        (progn
          (sd--resave)
          (setq sd-previous-desktop-name (sd--get-current-desktop-name))
          (set-window-configuration configuration)
          (raise-frame (window-configuration-frame configuration))
          (sd--set-current-desktop-name desktop-name))
      (sd-del desktop-name)
      (warn "desktop:%s does not exist anymore" desktop-name))))

(defun sd-shift ()
  "Shift to previous window-configuration"
  (interactive)
  (sd-restore sd-previous-desktop-name))

(defun sd-popup-restore-menu (&optional configuration-alist)
  "Popup a menu to let user select which desktop want to be restored"
  (interactive (list sd-configuration-alist))
  (let* ((desktop-names (mapcar #'car configuration-alist))
         (menu-items (mapcar (lambda (name)
                               (cons name name))
                             desktop-names))
         (menu `("desktop-names" ("panel" ,@menu-items)))
         (selected-desktop-name (x-popup-menu t menu)))
    (sd-restore selected-desktop-name)))

(provide 'simple-desktops)
