;; # Startup values -------------------------------------------

(tool-bar-mode 0)                     ; No graphical toolbar
(scroll-bar-mode 0)                   ; No scrollbar (it's a distracting bright white)
(menu-bar-mode 0)                     ; No menu bar (File, Edit, Options, etc.)
(setq inhibit-startup-screen t)       ; No splash on start-up
(setq initial-major-mode 'text-mode)  ; Apparently reduces start-up time.


;; # Frame and window setup -----------------------------------

;; Prefer to split windows vertically (side-by-side)
;; https://stackoverflow.com/a/20514750/5578429
(setq-default split-height-threshold nil)
(setq-default split-width-threshold 40)



(column-number-mode t)                ; Column number in info area
(setq-default display-line-numbers t) ; Absolute line numbers



(setq help-window-select 't)

;; # File behaviours ------------------------------------------

(setq make-backup-files nil)	   ; Stop creating backup~ files
(setq auto-save-default nil)	   ; Stop creating #autosave# files
(setq initial-scratch-message nil) ; No scratch buffer header message

(global-auto-revert-mode 1)        ; Reload files if they're edited externally



;; # Encoding -------------------------------------------------

;; Without doing this, I ran into a problem where AHK would not recognise strings.

(set-default-coding-systems 'utf-8)
(set-language-environment   "UTF-8")
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
   (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))



;; # Text-editing settings ------------------------------------

(electric-pair-mode 1)                       ; Autotype matching bracket
(show-paren-mode t)                          ; Highlight matching bracket

(global-visual-line-mode 1)                  ; Make word wrapping default in all buffers

(setq-default indent-tabs-mode nil)          ; No tabs, only spaces
(setq-default tab-width 4)                   ; Tabs are 4 spaces wide
(setq cua-auto-tabify-rectangles nil)        ; Don't convert spaces to tabs

(setq-default transient-mark-mode 1)         ; Hide highlighting if no active selection
(setq-default cua-keep-region-after-copy t)  ; Don't cancel highlighting after copying
(delete-selection-mode 1)                    ; Overwrite or delete selected region

(setq-default fill-column 79)                ; M-q wraps at word boundaries at 80 chars
(setq sentence-end-double-space nil)         ; M-q sees single-spaced full stops
(setq require-final-newline t)               ; Always end files with newline



;; # Removing slow-downs --------------------------------------

;; Make files open faster in Windows
;; reddit.com/r/emacs/comments/55ork0/is_emacs_251_noticeably_slower_than_245_on_windows/d8cxli2/
(if (>= emacs-major-version 25)
    (remove-hook 'find-file-hooks 'vc-refresh-state)
  (remove-hook 'find-file-hooks 'vc-find-file-hook))

;; # Scrolling behaviour --------------------------------------

;; Line-by-line scrolling with arrow keys, instead of skipping to next page
;; when the bottom of the buffer is reached. https://stackoverflow.com/a/4160949
(setq-default
  redisplay-dont-pause t ; Update screen even when input is detected.
  scroll-margin 10  ; Start scrolling when point is within n lines of top and bottom of window
  scroll-conservatively 10000  ; Recenter point if it moves off-screen
  scroll-preserve-screen-position '"always")  ; Point keeps its screen position



;; # Other behaviours -----------------------------------------

(fset 'yes-or-no-p 'y-or-n-p)  ; Accept 'y' and 'n'.

(setq x-underline-at-descent-line t)  ; Draw underlines lower.

(setq save-interprogram-paste-before-kill t)  ; Don't overwrite clipboard from other apps.
