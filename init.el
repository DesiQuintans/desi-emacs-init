;; ------------------------------------------------------------
;; # Environment setup
;; ------------------------------------------------------------

(package-initialize)

(require 'package)
;;(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/")
             t)
(package-initialize)



;; ------------------------------------------------------------
;; # Package auto-install
;; ------------------------------------------------------------

; https://camdez.com/blog/2015/04/03/switching-to-melpa-stable/

(defvar my/packages
  '(draft-mode golden-ratio ido-completing-read+ markdown-mode monokai-theme multiple-cursors persp-mode smex org-journal diredfl cm-mode yasnippet move-text wc-goal-mode writegood-mode adaptive-wrap))

(require 'cl-lib)

(defun my/install-packages ()
  "Ensure the packages I use are installed. See `my/packages'."
  (interactive)
  (let ((missing-packages (cl-remove-if #'package-installed-p my/packages)))
    (when missing-packages
      (message "Installing %d missing package(s)" (length missing-packages))
      (package-refresh-contents)
      (mapc #'package-install missing-packages))))

(my/install-packages)



;; ------------------------------------------------------------
;; # Package auto-load
;; ------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (adaptive-wrap visual-fill-column writegood-mode wc-goal-mode cm-mode yasnippet golden-ratio ido-completing-read+ smex persp-mode multiple-cursors monokai-theme markdown-mode draft-mode))))



;; ------------------------------------------------------------
;; # Load my init files
;; ------------------------------------------------------------

(load "~/.emacs.d/init/emacs-env.el")
(load "~/.emacs.d/init/my-keys-mode.el")
(load "~/.emacs.d/init/custom-functions.el")
(load "~/.emacs.d/init/org-settings.el")
(load "~/.emacs.d/init/keybinds.el")
(load "~/.emacs.d/init/enable-packages.el")
(load "~/.emacs.d/init/snippets.el")
(load "~/.emacs.d/init/unicode-insertion.el")



;; ------------------------------------------------------------
;; # Emacs appearance
;; ------------------------------------------------------------

(load-theme 'monokai t)
(set-frame-font "Fira Code 10" nil t)  ; Seems to slow down scrolling?

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



;; ------------------------------------------------------------
;; # Maximise the window (must be done last)
;; ------------------------------------------------------------

(w32-send-sys-command 61488)		; Start maximised
