;; # org-mode packages ----------------------------------------

(require 'org-journal)  ; A new org file every day



;; # Auto-opens -----------------------------------------------

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))



;; # Org behaviour --------------------------------------------

(setq org-use-speed-commands t)             ; Removes need for command prefix when mark is at start of heading

(setq org-startup-indented t)               ; Makes structure easier to see
(setq org-indent-indentation-per-level 4)   ; Indent levels by 4 chars

(setq org-startup-folded 'content)          ; Unfold 1 level at startup
(setq org-catch-invisible-edits 'error)     ; Don't edit folded regions
(setq-default org-ellipsis " ←")            ; Replace fold indicator
(setq-default org-cycle-separator-lines 1)  ; Preserve blank trailing lines when folded

(setq org-M-RET-may-split-line t)           ; M-RET can split a line into a new heading or item

(setq org-tags-column -80)                  ; -ve value right-aligns tags.

(setq org-support-shift-select t)     ; Allow shift-select in org. 'always turns off all org-mode shift-contexts. t keeps as many contexts as possible.

(setq org-list-allow-alphabetical t)  ; Allow alphabetical bullets in plain lists.

(setq org-hide-emphasis-markers t)  ; Hide marks for /italic/, *bold*, etc.

;; # org refiling ---------------------------------------------

;; https://dsdshcym.github.io/blog/2018/03/02/my-org-refile-workflow/
;; This lets headings be refiled amongst all open org-journal entries.
(defun +org/opened-buffer-files ()
  "Return the list of files currently opened in emacs"
  (delq nil
        (mapcar (lambda (x)
                  (if (and (buffer-file-name x)
                           (string-match "[0-9]\\{8\\}$"
                                         (buffer-file-name x)))
                      (buffer-file-name x)))
                (buffer-list))))

(setq org-refile-targets '((+org/opened-buffer-files :maxlevel . 4)))

(setq org-outline-path-complete-in-steps t)



;; # Directories and default files ----------------------------

(setq org-agenda-files (quote ("~/.emacs.d/documents/org/important_dates.org")))
(setq org-directory "~/.emacs.d/documents/org")
(setq org-default-notes-file "~.emacs.d/documents/org/misc.org")
(setq org-journal-dir "~/.emacs.d/documents/journal/")



;; # Tags and keywords ----------------------------------------

(setq org-tag-alist '(("home" . ?h) ("outside" . ?o) ("travel" . ?t) 
                      ("librarian" . ?l) ("desiderata" . ?d)
                      ("work" . ?w) ("site" . ?s) ("buy" . ?b) ("PhD" . ?p) 
                      ("DC1" . ?z) ("DC2" . ?x) ("DC3" . ?c) ("DC4" . ?v)
                      ("rough" . ?r) ("coherent" . ?k) ("polish" . ?f)))

(setq org-todo-keywords
    '((sequence "TODO(t)" "WAIT(w)" "LIST(l)" "|" "STOP(s)" "DONE(d)")))
       ;; TODO  A task that needs to be done.
       ;; WAIT  On hold while waiting for more information.
       ;; LIST  Organises a list of items that is on hold.
       ;; STOP  This task doesn't need to be done (e.g. no longer important).
       ;; DONE  The task is finished.

(setq-default org-log-done 'time)  ; 'time = Log the current time when closing a task



;; # Priorities -----------------------------------------------

(setq org-highest-priority ?1)  ; I like numbers over letters.
(setq org-lowest-priority ?5)   ; Lowest priority 5 because I want to define 5 must-do items 
(setq org-default-priority ?3)  ; Analogous to the default A-C with default B.

(setq org-priority-start-cycle-with-default t)  ; Starts at 3. If nil, starts at 2 or 4.


;; # Capture templates ----------------------------------------

;; https://github.com/bastibe/org-journal#journal-capture-template
(defun org-journal-find-location ()
    (org-journal-new-entry t)
    (goto-char (point-min)))

(setq org-capture-templates
      '(("j" "Journal" entry (function org-journal-find-location)
         "\n* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")

        ("c" "Task"    plain (file "~/.emacs.d/documents/org/dump_task.org")
         "\n*** TODO %?\n\n%T")
        
        ("i" "Idea"    entry (file "~/.emacs.d/documents/org/dump_idea.org")
         "* %?\n\n%T")))



;; # Org agenda view ------------------------------------------

(setq org-journal-enable-agenda-integration t)  ; journal to agenda view

(setq org-journal-carryover-items "TODO=\"TODO\"|TODO=\"WAIT\"|TODO=\"LIST\"")  ; Journal items to carry over

(setq org-agenda-start-on-weekday 1)  ; week starts on Monday

(setq org-agenda-skip-scheduled-if-done 1)  ; Don't show DONE items in agenda view.



;; # Org habits -----------------------------------------------

(add-to-list 'org-modules 'org-habit t)
