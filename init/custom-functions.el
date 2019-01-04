;; # Custom functions -----------------------------------------

;; # User feedback --------------------------------------------

(defun beep-and-flash ()
  (setq orig-fg (face-foreground 'mode-line))
  (set-face-foreground 'mode-line "#FFFFFF")
  ;; (set-face-foreground 'mode-line orig-fg))
  (run-with-idle-timer 0.1 nil
                       (lambda (fg) (set-face-foreground 'mode-line fg))
                       orig-fg)
  (play-sound '(sound :file "~/.emacs.d/init/sounds/custom-beep.wav")))

(defun my-bell-function ()
  "Disable the error beep when I scroll beyond the limits of the buffer."
  (unless (memq this-command
        '(isearch-abort abort-recursive-edit exit-minibuffer
              keyboard-quit mwheel-scroll down up next-line previous-line
              backward-char forward-char))
    (beep-and-flash)))

(setq-default ring-bell-function 'my-bell-function)


;; # Zotero ---------------------------------------------------

;; Oh my goodness this was tough to write.
(defun zot-get ()
  "Uses Better BibTeX's API to insert Pandoc citekeys."
  (interactive)
  ;; https://stackoverflow.com/a/16447438
  (with-current-buffer (url-retrieve-synchronously "http://localhost:23119/better-bibtex/cayw?format=pandoc&brackets=yes&clipboard=yes")
    (prog1 (if (s-contains? "[@" (buffer-string)) (setq has_citekey t) (setq has_citekey nil)) (kill-buffer)))
  (if has_citekey (yank)))



;; # Text insertion and manipulation --------------------------

(defun datetime ()
  "Insert current date-time string in full ISO 8601 format.
Example: 2010-11-29T23:23:35-08:00"
  (interactive)
  (insert
   (concat
    (format-time-string "%Y-%m-%dT%T")
    ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
     (format-time-string "%z")))))


;; https://www.emacswiki.org/emacs/UnfillParagraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))


;; https://stackoverflow.com/questions/12132601/duplicating-the-current-line
(defun copy-line ()
  "Duplicate the current line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line 1)
  (yank)
  (yank)
  (previous-line) (end-of-line)
  (setq kill-ring (cdr kill-ring)))


;; https://stackoverflow.com/a/39438119
;; Lets Emacs delete whitespace only instead of having to delete to a word with Ctrl + Bspc.
(defun aborn/backward-kill-word ()
  "Customize/Smart backward-kill-word."
  (interactive)
  (let* ((cp (point))
         (backword)
         (end)
         (space-pos)
         (backword-char (if (bobp)
                            ""           ;; cursor in begin of buffer
                          (buffer-substring cp (- cp 1)))))
    (if (equal (length backword-char) (string-width backword-char))
        (progn
          (save-excursion
            (setq backword (buffer-substring (point) (progn (forward-word -1) (point)))))
          (setq ab/debug backword)
          (save-excursion
            (when (and backword          ;; when backword contains space
                       (s-contains? " " backword))
              (setq space-pos (ignore-errors (search-backward " ")))))
          (save-excursion
            (let* ((pos (ignore-errors (search-backward-regexp "\n")))
                   (substr (when pos (buffer-substring pos cp))))
              (when (or (and substr (s-blank? (s-trim substr)))
                        (s-contains? "\n" backword))
                (setq end pos))))
          (if end
              (kill-region cp end)
            (if space-pos
                (kill-region cp space-pos)
              (backward-kill-word 1))))
      (kill-region cp (- cp 1)))         ;; word is non-english word
    ))



;; # Making window-jump and golden-ratio work together --------

(defadvice window-jump-left (after wj-gr-jump-left activate)
  "Jump left and then rescale with `golden-ratio'."
  (golden-ratio))

(defadvice window-jump-right (after wj-gr-jump-right activate)
  "Jump right and then rescale with `golden-ratio'."
  (golden-ratio))

(defadvice window-jump-up (after wj-gr-jump-up activate)
  "Jump up and then rescale with `golden-ratio'."
  (golden-ratio))

(defadvice window-jump-down (after wj-gr-jump-down activate)
  "Jump down and then rescale with `golden-ratio'."
  (golden-ratio))
