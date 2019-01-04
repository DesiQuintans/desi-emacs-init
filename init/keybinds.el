;; # Add custom keybinds to my minor mode ---------------------

;; Built-in Emacs lisp functions
(define-key my-mode-map (kbd "C-?")           #'next-buffer)


;; Custom functions in init/custom-functions.el
(define-key my-mode-map (kbd "C-d")           #'copy-line)
(define-key my-mode-map (kbd "M-Q")           #'unfill-paragraph)
(define-key my-mode-map (kbd "C-<backspace>") #'aborn/backward-kill-word)
(define-key my-mode-map (kbd "C-\"")          #'zot-get)


;; ivy, counsel, swiper
(define-key my-mode-map (kbd "C-s")           #'swiper)
(define-key my-mode-map (kbd "M-x")           #'counsel-M-x)
(define-key my-mode-map (kbd "M-y")           #'counsel-yank-pop)
(define-key my-mode-map (kbd "C-x C-f")       #'counsel-find-file)
(define-key my-mode-map (kbd "<f1> f")        #'counsel-describe-function)
(define-key my-mode-map (kbd "<f1> v")        #'counsel-describe-variable)
(define-key my-mode-map (kbd "<f1> l")        #'counsel-find-library)
(define-key my-mode-map (kbd "<f2> i")        #'counsel-info-lookup-symbol)
(define-key my-mode-map (kbd "<f2> u")        #'counsel-unicode-char)
(define-key minibuffer-local-map (kbd "C-r")  'counsel-minibuffer-history)


;; undo-tree
;; This is a global key so that it can be overwritten by minor modes (e.g. draft-mode).
(define-key global-map (kbd "C-z")            'undo-tree-undo)
(define-key global-map (kbd "C-S-z")          'undo-tree-redo)
(define-key global-map (kbd "C-S-M-z")        'undo-tree-visualize)


;; org-mode
(define-key my-mode-map (kbd "C-c l")         #'org-store-link)
(define-key my-mode-map (kbd "C-c c")         #'org-capture)
(define-key my-mode-map (kbd "C-c a")         #'org-agenda)


;; recentf (recently opened files)
(define-key my-mode-map (kbd "C-x f")         #'recentf-open-files)


;; bs (fancy buffer list)
(define-key my-mode-map (kbd "C-x C-b")       #'bs-show)


;; multiple-cursors
(define-key my-mode-map (kbd "C->")           #'mc/mark-next-like-this)
(define-key my-mode-map (kbd "C-.")           #'mc/skip-to-next-like-this)
(define-key my-mode-map (kbd "C-<")           #'mc/mark-previous-like-this)
(define-key my-mode-map (kbd "C-,")           #'mc/skip-to-previous-like-this)
(define-key my-mode-map (kbd "C-S-M-m")       #'mc/mark-all-dwim)


;; helpful
(define-key my-mode-map (kbd "C-h f")         #'helpful-callable)
(define-key my-mode-map (kbd "C-h v")         #'helpful-variable)
(define-key my-mode-map (kbd "C-h k")         #'helpful-key)
(define-key my-mode-map (kbd "C-h o")         #'helpful-symbol)


;; expand-region
(define-key my-mode-map (kbd "C-=")           #'er/expand-region)
(define-key my-mode-map (kbd "C-+")           #'er/contract-region)


;; org-journal
(define-key my-mode-map (kbd "C-c C-j")       #'org-journal-new-entry)


;; persp-mode (defined in init/enable-packages.el as part of eval-after-load)
;;(define-key my-mode-map (kbd "C-x b")       #'persp-switch-to-buffer)
;;(define-key my-mode-map (kbd "C-x k")       #'persp-kill-buffer)
;;(define-key my-mode-map (kbd "C-/")         #'persp-next)


;; visual-regex
(define-key my-mode-map (kbd "C-c r")         #'vr/replace)
(define-key my-mode-map (kbd "C-c q")         #'vr/query-replace)
(define-key my-mode-map (kbd "C-M-r")         #'vr/isearch-backward)
(define-key my-mode-map (kbd "C-M-s")         #'vr/isearch-forward)
(define-key my-mode-map (kbd "C-c m")         #'vr/mc-mark)


;; window-jump
(define-key my-mode-map (kbd "C-c <left>")    #'window-jump-left)
(define-key my-mode-map (kbd "C-c <right>")   #'window-jump-right)
(define-key my-mode-map (kbd "C-c <up>")      #'window-jump-up)
(define-key my-mode-map (kbd "C-c <down>")    #'window-jump-down)
