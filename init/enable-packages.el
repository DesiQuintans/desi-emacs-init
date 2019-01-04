;; # display-time ---------------------------------------------

(setq display-time-format "%F %R"
      display-time-interval 10
      display-time-default-load-average nil
      display-time-load-average-threshold 1000)

(display-time-mode 1)



;; # ivy / counsel / swiper -----------------------------------

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-extra-directories nil)
(setq ivy-use-selectable-prompt t)  ; Stop auto-completing by pressing UP to select the prompt line.
(setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))  ; Searches start with ^ by default, enforcing an initial word anyway.

;; # undo-tree ------------------------------------------------

(require 'undo-tree)
(global-undo-tree-mode)  ; Replace undo system with a tree system



;; # persp-mode -----------------------------------------------

(with-eval-after-load "persp-mode-autoloads"
  (setq wg-morph-on nil) ;; switch off animation
  (setq persp-autokill-buffer-on-remove 'kill-weak)
  (setq persp-kill-foreign-buffer-behaviour 'dont-ask-weak)
  (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))
  
(with-eval-after-load "persp-mode"
  (setq persp-interactive-completion-function #'ivy-completing-read)
  (define-key my-mode-map (kbd "C-x C-b")
    #'(lambda (arg)
        (interactive "P")
        (with-persp-buffer-list () (bs-show arg))))
  (define-key my-mode-map (kbd "C-x b") #'persp-switch-to-buffer)
  (define-key my-mode-map (kbd "C-x k") #'persp-kill-buffer)
  (define-key my-mode-map (kbd "C-/") #'persp-next))

;; https://gist.github.com/Bad-ptr/1aca1ec54c3bdb2ee80996eb2b68ad2d#file-persp-ivy-el
(with-eval-after-load "persp-mode"
  (with-eval-after-load "ivy"
    (add-hook 'ivy-ignore-buffers
              #'(lambda (b)
                  (when persp-mode
                    (let ((persp (get-current-persp)))
                      (if persp
                          (not (persp-contain-buffer-p b persp))
                        nil)))))

    (setq ivy-sort-functions-alist
          (append ivy-sort-functions-alist
                  '((persp-kill-buffer   . nil)
                    (persp-remove-buffer . nil)
                    (persp-add-buffer    . nil)
                    (persp-switch        . nil)
                    (persp-window-switch . nil)
                    (persp-frame-switch . nil))))))


;; # hl-line-mode ---------------------------------------------

;; Some modes really benefit from having highlighted lines.
(add-hook 'tabulated-list-mode-hook 'hl-line-mode)  ; e.g. list-packages
(add-hook 'bs-mode-hook 'hl-line-mode)  ; bs-show


;; # smex -----------------------------------------------------

(smex-initialize) ; Autocomplete in M-x
(setq smex-save-file "~/.emacs.d/.smex-items")


;; # recentf --------------------------------------------------

(require 'recentf)
(recentf-mode 1)
(setq recentf-exclude '("\\.emacs\\.d"))  ; Emacs files are always open anyway.


;; # golden-ratio ---------------------------------------------

;; https://truongtx.me/2014/11/15/auto-resize-windows-by-golden-ratio-in-emacs

(require 'golden-ratio)
(golden-ratio-mode 1)  ; Resizes the active window dynamically.


(setq golden-ratio-exclude-modes '("ediff-mode"
                                   "eshell-mode"))

(setq golden-ratio-auto-scale t)  ; Makes golden-ratio aware of the frame size.



;; # window-jump  ---------------------------------------------

(require 'window-jump)



;; # dired and related packages -------------------------------

(require 'dired-x)  ; Extended dired functions, including dired-do-find-marked-files.
(require 'diredfl)
(require 'w32-browser)  ; Open files using default app in Windows

(diredfl-global-mode 1)

(setq dired-dwim-target t)  ; If another dired window is open, use that window as the target for moving/copying/etc.

;; Auto-refresh dired quietly.
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode)

(eval-after-load "w32-browser"
  '(progn
    (define-key dired-mode-map [(control return)] 'dired-w32explore)
    (define-key dired-mode-map [(meta return)] 'dired-w32-browser)
    (define-key dired-mode-map [mouse-2] 'dired-mouse-w32-browser)
    (define-key dired-mode-map (kbd "<C-M-return>") 'dired-multiple-w32-browser)))



;; # yasnippet ------------------------------------------------

;; Snippets are stored in .emacs.d/snippets.
(require 'yasnippet)
(require 'warnings)
(yas-global-mode 1)

;; Allow lisp code to edit the buffer. E.g. delete backwards to replace characters.
(add-to-list 'warning-suppress-types '(yasnippet backquote-change))



;; # helpful (C-h replacements) -------------------------------

(require 'helpful)
(require 'elisp-demos)

; Add examples to Help buffers
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

(setq helpful-max-buffers 5)


;; # expand-region --------------------------------------------

(require 'expand-region)



;; # delight (edit the mode line) -----------------------------

(require 'delight)
(delight '((golden-ratio-mode nil    golden-ratio)
           (ivy-mode          nil    ivy)
           (abbrev-mode       nil    abbrev)
           (eldoc-mode        nil    eldoc)
           (yas-minor-mode    nil    yasnippet)
           (visual-line-mode  " vlm" simple)))



;; # stupid-indent-mode ---------------------------------------

(require 'stupid-indent-mode)
(setq stupid-indent-level 4)



;; # ahk-mode -------------------------------------------------

(defun ahk-modes ()
  "Set up my preferred modes for Autohotkey editing."
  (interactive)
  (ahk-mode)
  (stupid-indent-mode))
  

(add-to-list 'auto-mode-alist '("\\.ahk\\'"  . ahk-modes))



;; # view large files ----------------------- ------------------

(require 'vlf-setup)
;; The package will automatically detect a large file and offer
;; to load it with vlf.



;; # regex packages -------------------------------------------

;; Try to use PCRE syntax everywhere in Emacs (it will announce "PCRE regexp"
;; in the minibuffer). Note that Swiper/Ivy won't use this because they have
;; their own fuzzy searching regex builder.
(require 'pcre2el)
(pcre-mode t)

;; Show regex matches and replacements in real-time inside the buffer.
(require 'visual-regexp)
(require 'visual-regexp-steroids)
(setq vr/engine 'pcre2el)  ; Use pcre2el because way faster than Python.


;; # which-key ------------------------------------------------

(require 'which-key)
(which-key-mode)


;; # magit ----------------------------------------------------

(setq magit-git-executable "C:/Dropbox/Apps/PortableGit/mingw32/bin/git.exe")
