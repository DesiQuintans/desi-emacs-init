;; # org-mode packages ----------------------------------------

(require 'org-journal)  ; A new org file every day



;; # Auto-opens -----------------------------------------------

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))



;; # Org speed commands ---------------------------------------

;; When the cursor is at the start of a heading line (before any stars),
;; you don't have to use the modifier keys to do stuff.
(setq org-use-speed-commands nil)

;; # Directories and default files ----------------------------

(setq org-directory "~/.emacs.d/documents/org")
(setq org-default-notes-file "~.emacs.d/documents/org/misc.org")
(setq org-journal-dir "~/.emacs.d/documents/journal/")



;; # Tags and keywords ----------------------------------------

(setq org-tag-alist '(("work" . ?w) ("home" . ?h) ("outside" . ?o) ("rpackage" . ?r) ("librarian" . ?l) ("desiderata" . ?d) ("buy" . ?b) ("site" . ?s) ("DC1" . ?z) ("DC2" . ?x) ("DC3" . ?c) ("DC4" . ?v) ("PhD" . ?p) ("travel" . ?t)))

(setq org-todo-keywords
    '((sequence "TODO(t)" "WAIT(w)" "LIST(l)" "|" "STOP(s)" "DONE(d)")))
       ;; TODO  A task that needs to be done.
       ;; WAIT  On hold while waiting for more information.
       ;; LIST  Organises a list of items that is on hold.
       ;; STOP  This task doesn't need to be done (e.g. no longer important).
       ;; DONE  The task is finished.

(setq-default org-log-done 'note)  ; 'time = Log the current time when closing a task



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



;; # Indenting and appearance ---------------------------------

(setq org-startup-indented t)               ; Makes structure easier to see
(setq org-indent-indentation-per-level 4)   ; Indent levels by 4 chars

(setq org-startup-folded 'content)          ; Unfold 1 level at startup
(setq org-catch-invisible-edits 'error)     ; Don't edit folded regions
(setq-default org-ellipsis " ←")            ; Replace fold indicator
(setq-default org-cycle-separator-lines 1)  ; Preserve blank trailing lines when folded

(setq org-M-RET-may-split-line t)           ; M-RET can split a line into a new heading or item

(setq org-tags-column -85)                  ; -ve value right-aligns tags.

(setq org-support-shift-select t)           ; Allow shift-select in org.



;; # Org agenda view ------------------------------------------

(setq org-journal-enable-agenda-integration t)  ; journal to agenda view

(setq org-journal-carryover-items "TODO=\"TODO\"|TODO=\"WAIT\"|TODO=\"LIST\"")  ; Journal items to carry over

(setq org-agenda-start-on-weekday 1)  ; week starts on Monday

(setq org-agenda-skip-scheduled-if-done 1)  ; Don't show DONE items in agenda view.
