;; # ido ------------------------------------------------------

(ido-mode t)        ; Autocomplete in file/buffer selections
(ido-everywhere 1)  ; In as many places as possible.



;; # ido-completing-read-plus ---------------------------------

(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)  ; ido auto-completion in even more places



;; # multiple-cursors -----------------------------------------

(define-key my-mode-map (kbd "C->") #'mc/mark-next-like-this)
(define-key my-mode-map (kbd "C-<") #'mc/mark-previous-like-this)
(define-key my-mode-map (kbd "C-S-M-m") #'mc/mark-all-dwim)



;; # org-journal ----------------------------------------------

;; I want to add journal entries manually instead of having one made
;; automatically when I first access the day's journal page. This function
;; creates/shows the journal page but does not create a time-stamped heading.
;; New time entries should be added using org-capture (C-c c j).
;; https://stackoverflow.com/a/12829884/5578429
(defun just-show-org-journal ()
  (interactive)
  (let ((current-prefix-arg 'nothing)) ;; emulate C-u
    (call-interactively 'org-journal-new-entry) ;; invoke align-regexp interactively
    )
  )

;;(global-set-key (kbd "C-c j") 'just-show-org-journal)
(global-set-key (kbd "C-c j") 'org-journal-new-entry)  ; Default behaviour



;; # persp-mode -----------------------------------------------

(with-eval-after-load "persp-mode-autoloads"
  ;(setq wg-morph-on nil) ;; switch off animation
  (setq persp-autokill-buffer-on-remove 'kill-weak)
  (setq persp-kill-foreign-buffer-behaviour 'dont-ask-weak)
  (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))
  
(with-eval-after-load "persp-mode"
  (setq persp-interactive-completion-function #'ido-completing-read)
  (define-key my-mode-map (kbd "C-x b") #'persp-switch-to-buffer)
  (define-key my-mode-map (kbd "C-x k") #'persp-kill-buffer)
  (define-key my-mode-map (kbd "C-/") #'persp-next)
  (persp-set-ido-hooks t))  ; ido only shows buffers in perspective
  
  ;; (global-set-key (kbd "C-x b") #'persp-switch-to-buffer)
  ;; (global-set-key (kbd "C-x k") #'persp-kill-buffer))
 


;; # smex -----------------------------------------------------

(smex-initialize) ; Autocomplete in M-x

(define-key my-mode-map (kbd "M-x") #'smex)
(define-key my-mode-map (kbd "M-S-x") #'smex-major-mode-commands)
(define-key my-mode-map (kbd "C-M-S-x") #'execute-extended-command) ;; Old M-x.



;; # recentf --------------------------------------------------

(require 'recentf)
(recentf-mode 1)



;; # golden-ratio ---------------------------------------------

;; https://truongtx.me/2014/11/15/auto-resize-windows-by-golden-ratio-in-emacs

(require 'golden-ratio)
(golden-ratio-mode 1)  ; Resizes the active window so that its contents fit comfortably inside.


(setq golden-ratio-exclude-modes '("ediff-mode"     ; Exclude certain kinds of windows from resizing.
                                   "eshell-mode"))  ; dired-mode

(setq split-width-threshold nil)  ; Apparently stops emacs from making new windows if current one is too big.

(setq golden-ratio-auto-scale t)  ; Makes golden-ratio aware of the frame size.



;; # dired and related packages -------------------------------

(setq dired-dwim-target t)  ; If another dired window is open, use that window as the target for moving/copying/etc.

(require 'dired-x)  ; Extended dired functions, including dired-do-find-marked-files.

(require 'diredfl)
(diredfl-global-mode 1)



;; # yasnippet ------------------------------------------------

;; Snippets are stored in .emacs.d/snippets.
(require 'yasnippet)
(yas-global-mode 1)



;; # markdown-mode and other writing-related packages ---------

(require 'wc-goal-mode)  ; Not auto-loaded, so needs to be required.
(require 'adaptive-wrap)

(require 'markdown-mode)
(setq markdown-coding-system 'utf-8)
(setq markdown-asymmetric-header t)  ; Hashes only on the left side of headers
(setq markdown-indent-on-enter 'indent-and-new-item)  ; List continuation
(setq markdown-italic-underscore t)  ; Use underscores for italics.
(set-face-attribute 'markdown-code-face nil :inherit nil :background "black")  ; Default fontify is bright white bg and changed font.


(require 'visual-fill-column)
(setq-default visual-fill-column-center-text t)  ; Put the text in the middle of the frame, away from the edges.
(setq-default visual-fill-column-width 90)  ; Different visual width from fill-column.
(setq-default visual-fill-column-fringes-outside-margins nil)  ; Keep the rough ends within the max width.


(defun writing-environment ()
  (interactive)
  "Set up my preferred Markdown writing environment. This is called as the 'auto-mode' so that many modes can be loaded at once."
  (markdown-mode)               ; Writing and formatting
  (cm-mode)                     ; CriticMarkup for annotating text
  (move-text-default-bindings)  ; For rearranging paragraphs
  (writegood-mode)              ; Identifying weasel words, passive voice, duplicates
  (wc-goal-mode)                ; Word count and goal tracking
  (visual-fill-column-mode)     ; Visual wrapping of lines
  (adaptive-wrap-prefix-mode)   ; Maintain list indentation with visual wrapping
  )


(add-to-list 'auto-mode-alist '("\\.md\\'" . writing-environment))  ; Auto-open for .md
(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . writing-environment))  ; Auto-open for .md


;; # draft-mode -----------------------------------------------

;; I leave this to be manually toggled with M-x draft-mode.

(with-eval-after-load "draft-mode"
  (define-key draft-mode-map [remap undo] 'end-of-buffer))
