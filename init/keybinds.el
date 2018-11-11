;; # Define global bindings -----------------------------------

;; (cua-mode t)  ; Standard UI shortcuts. I'll try learning base Emacs.
(define-key global-map (kbd "C-z") 'undo)  ; Stop minimizing the frame

;; org-mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;; recentf (recently opened files)
(global-set-key (kbd "C-x f") 'recentf-open-files)



;; # Custom functions mapped to keys --------------------------

(define-key my-mode-map (kbd "M-Q") #'unfill-paragraph)
;; (define-key global-map (kbd "M-Q") 'unfill-paragraph)



;; # Add keybinds to minor mode -------------------------------

;; Emacs
(define-key my-mode-map (kbd "C-?") #'next-buffer)
