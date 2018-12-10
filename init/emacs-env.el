;; # Frame setup ----------------------------------------------

(tool-bar-mode 0)                     ; No graphical toolbar
(scroll-bar-mode 0)                   ; No scrollbar (it's a distracting bright white)
(menu-bar-mode 0)                     ; No menu bar (File, Edit, Options, etc.)
(setq inhibit-startup-screen t)       ; No splash on start-up

(column-number-mode t)                ; Column number in info area
(setq-default display-line-numbers t) ; Absolute line numbers



;; # File behaviours ------------------------------------------

(setq make-backup-files nil)	   ; Stop creating backup~ files
(setq auto-save-default nil)	   ; Stop creating #autosave# files
(setq initial-scratch-message nil) ; No scratch buffer header message

(global-auto-revert-mode 1)        ; Reload files if they're edited externally



;; # Encoding -------------------------------------------------

(set-language-environment   "UTF-8")
(set-default-coding-systems 'utf-8)



;; # Text-editing settings ------------------------------------

;(electric-pair-mode 1)                        ; Autotype matching bracket
(show-paren-mode t)                           ; Highlight matching bracket

(global-visual-line-mode 1)                   ; Make word wrapping default in all buffers

(setq-default indent-tabs-mode nil)           ; No tabs, only spaces
(setq-default tab-width 4)                    ; Tabs are 4 spaces wide
(setq-default cua-auto-tabify-rectangles nil) ; Don't convert spaces to tabs

(setq-default transient-mark-mode 1)          ; Hide highlighting if no active selection
(setq-default cua-keep-region-after-copy t)   ; Don't cancel highlighting after copying
(delete-selection-mode 1)                     ; Overwrite or delete selected region

(setq-default fill-column 79)                 ; M-q wraps at word boundaries at 80 chars
(setq-default sentence-end-double-space nil)  ; M-q sees single-spaced full stops
(setq-default require-final-newline t)        ; Always end files with newline



;; # Scrolling behaviour --------------------------------------

;; Line-by-line scrolling with arrow keys, instead of skipping to next page
;; when the bottom of the buffer is reached. https://stackoverflow.com/a/4160949
;; (setq-default
;;   redisplay-dont-pause t
;;   scroll-margin 1
;;   scroll-step 1
;;   scroll-conservatively 10000
;;   scroll-preserve-screen-position 1
;;   scroll-up-aggressively 0.01
;;   scroll-down-aggressively 0.01)
