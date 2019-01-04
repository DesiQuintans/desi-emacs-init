;; # Custom writing environment -------------------------------

;; I like writing in Markdown, but markdown-mode by itself is not enough. I've
;; therefore defined a 'writing-environment', which is a function that loads
;; all of the packages that I employ to make Emacs into a really nice writing
;; program.



;; # Variable setup -------------------------------------------

(require 'wc-goal-mode)  ; Not auto-loaded, so needs to be required.
(require 'adaptive-wrap)

(require 'markdown-mode)
(setq markdown-coding-system 'utf-8)
(setq markdown-asymmetric-header t)  ; Hashes only on the left side of headers
(setq markdown-indent-on-enter 'indent-and-new-item)  ; List continuation
(setq markdown-italic-underscore t)  ; Use underscores for italics.

;; Make the formatting colours more like the Merbivore theme in RStudio.
(set-face-attribute 'markdown-code-face nil :inherit nil :foreground "cornflowerblue" :background "gray20")
(set-face-attribute 'markdown-italic-face nil :foreground "yellow")
(set-face-attribute 'markdown-bold-face nil :foreground "darkseagreen")

;; Heading sizes should be easier to differentiate.
(set-face-attribute 'markdown-header-face-1 nil :height 2.0 :slant 'italic)
(set-face-attribute 'markdown-header-face-2 nil :height 2.0)
(set-face-attribute 'markdown-header-face-3 nil :height 1.6)
(set-face-attribute 'markdown-header-face-4 nil :height 1.2)
(set-face-attribute 'markdown-header-face-5 nil :height 1.0)
(set-face-attribute 'markdown-header-face-6 nil :height 1.0 :slant 'italic)

(require 'visual-fill-column)
(setq-default visual-fill-column-center-text t)  ; Center the column in the frame.
(setq-default visual-fill-column-width 80)  ; Different visual width from fill-column.
(setq-default visual-fill-column-fringes-outside-margins nil)  ; Keep the rough ends within the max width.

;; By default, draft-mode still allows undo. I disable both built-in undo and undo-tree.
(with-eval-after-load "draft-mode"
  (define-key draft-mode-map [remap undo] 'end-of-buffer)
  (define-key draft-mode-map [remap undo-tree-undo] 'end-of-buffer)
  (define-key draft-mode-map [remap undo-tree-redo] 'end-of-buffer)
  (define-key draft-mode-map [remap undo-tree-visualize] 'end-of-buffer)
  )

(require 'cm-mode)
(setq-default cm-read-only-annotations nil)


;; # Modes used -----------------------------------------------

(defun writing-environment ()
  (interactive)
  "Set up my preferred Markdown writing environment. This is called as the 'auto-mode' so that many modes and functions can be loaded at once."
  (markdown-mode)                    ; Writing and formatting
  (markdown-toggle-markup-hiding 1)  ; Hide all style characters. Toggle with C-cxm.
  (move-text-default-bindings)       ; For rearranging paragraphs
  (writegood-mode)                   ; Identifying weasel words, passive voice, duplicates
  (visual-fill-column-mode t)        ; Visual wrapping of lines
  (adaptive-wrap-prefix-mode)        ; Maintain list indentation with visual wrapping
  (wc-goal-mode)                     ; Word count and goal tracking
  (smartscan-mode)                   ; Jump to next/prev matching work with M-n or M-p
  (pandoc-mode)                      ; Syntax colours for citations, interactions with Pandoc
  (cm-mode)                          ; CriticMarkup for annotating text
  )

;; Toggle draft-mode per-buffer with M-x draft-mode.



;; # File formats ---------------------------------------------

(add-to-list 'auto-mode-alist '("\\.md\\'" . writing-environment))
(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . writing-environment))
