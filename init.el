;; ------------------------------------------------------------
;; # Locating .emacs.d outside the User Profile folders
;; ------------------------------------------------------------

;; https://old.reddit.com/r/emacs/comments/a6ka23/change_home_folder_location_windows/

;; I chose the batch file method because it doesn't create an
;; empty .emacs.d folder in my user profile.


;; ------------------------------------------------------------
;; # Package 
;; ------------------------------------------------------------

;;(package-initialize)

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(package-initialize)


;; ------------------------------------------------------------
;; # Package auto-install
;; ------------------------------------------------------------

; https://camdez.com/blog/2015/04/03/switching-to-melpa-stable/

(defvar my/packages
  '(draft-mode markdown-mode monokai-theme multiple-cursors persp-mode smex ido-completing-read+ golden-ratio yasnippet cm-mode wc-goal-mode writegood-mode visual-fill-column adaptive-wrap smartscan ahk-mode undo-tree company ivy swiper counsel rainbow-mode helpful expand-region org-journal diredfl move-text delight zotxt stupid-indent-mode magit elisp-demos w32-browser vlf visual-regexp visual-regexp-steroids pcre2el paredit window-jump which-key))

(require 'cl-lib)

(defun my/install-packages ()
  "Ensure the packages I use are installed. See `my/packages'."
  (interactive)
  (let ((missing-packages (cl-remove-if #'package-installed-p my/packages)))
    (when missing-packages
      (message "Installing %d missing package(s)" (length missing-packages))
      (package-refresh-contents)
      (mapc #'package-install missing-packages))))

(my/install-packages)



;; ------------------------------------------------------------
;; # Package auto-load
;; ------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (which-key window-jump paredit pcre2el visual-regexp-steroids visual-regexp vlf w32-browser elisp-demos yasnippet pandoc-mode markdown-mode+ markdown-mode magit stupid-indent-mode ahk-mode delight move-text diredfl org-journal expand-region helpful rainbow-mode counsel swiper ivy company undo-tree smartscan adaptive-wrap visual-fill-column writegood-mode wc-goal-mode cm-mode golden-ratio ido-completing-read+ smex persp-mode multiple-cursors monokai-theme draft-mode))))



;; ------------------------------------------------------------
;; # Load my init files
;; ------------------------------------------------------------

(load "~/.emacs.d/init/emacs-env.el")
(load "~/.emacs.d/init/my-keys-mode.el")
(load "~/.emacs.d/init/custom-functions.el")
(load "~/.emacs.d/init/org-settings.el")
(load "~/.emacs.d/init/enable-packages.el")
(load "~/.emacs.d/init/write-env.el")
(load "~/.emacs.d/init/snippets.el")
(load "~/.emacs.d/init/unicode-insertion.el")
(load "~/.emacs.d/init/keybinds.el")



;; ------------------------------------------------------------
;; # Emacs appearance
;; ------------------------------------------------------------

;; Use rainbow-mode for easy colour comparison!
(setq default-yellow      "#E6DB74")
(setq clrs-yellow         "#FFDC00")

(setq default-orange      "#FD971F")
(setq clrs-orange         "#FF851B")

(setq default-red         "#F92672")
(setq clrs-red            "#FF4136")

(setq clrs-maroon         "#85144b")
(setq default-magenta     "#FD5FF0")
(setq clrs-fuchsia        "#F012BE")

(setq clrs-blue           "#0074D9")
(setq clrs-aqua           "#7FDBFF")
(setq default-blue        "#66D9EF")
(setq clrs-navy           "#001f3f")

(setq clrs-olive          "#3D9970")
(setq clrs-green          "#2ECC40")
(setq default-green       "#A6E22E")
(setq clrs-lime           "#01FF70")

(setq default-cyan        "#A1EFE4")
(setq clrs-teal           "#39CCCC")

(setq default-violet      "#AE81FF")
(setq clrs-purple         "#B10DC9")

(setq clrs-silver         "#DDDDDD")
(setq default-gray        "#64645E")
(setq clrs-gray           "#AAAAAA")

(setq clrs-white          "#FFFFFF")
(setq default-foreground  "#F8F8F2")
(setq default-background  "#272822")

(setq clrs-black          "#111111")


;; Set the colours that are ultimately used in monokai theme.
(setq monokai-yellow      clrs-yellow)
(setq monokai-orange      clrs-orange)
(setq monokai-red         clrs-red)
(setq monokai-magenta     clrs-fuchsia)
(setq monokai-blue        clrs-aqua)
(setq monokai-green       clrs-lime)
(setq monokai-cyan        clrs-teal)
(setq monokai-violet      clrs-purple)
(setq monokai-gray        default-gray)
(setq monokai-foreground  clrs-white)
(setq monokai-background  default-background)



;; # Theme colours and faces ----------------------------------

(load-theme 'monokai t)

;; Hack has italics and boldface and is nice and narrow.
;; https://sourcefoundry.org/hack/#download
(set-frame-font "Hack 10" nil t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number ((t (:inherit default :background "#272822" :foreground "#404040" :underline nil))))
 '(mode-line ((t (:background "#373737" :foreground "#595959" :box (:line-width 3 :color "#373737" :style unspecified)))))
 '(mode-line-buffer-id ((t (:foreground "#3D9970" :weight bold))))
 '(mode-line-inactive ((t (:background "#272822" :foreground "#111111" :box (:line-width 3 :color "#272822" :style unspecified))))))



;; ------------------------------------------------------------
;; # Maximise the window (must be done last)
;; ------------------------------------------------------------

;; When decorations are hidden and the frame is maximised, Windows sees it as a
;; fullscreen app exactly as it does if (toggle-frame-fullscreen) is used.

;;(set-frame-parameter nil 'undecorated t)  ; Hides the title bar, borders, etc.
;;(w32-send-sys-command 61488)		        ; Start maximised. 

(toggle-frame-fullscreen)  ; True full-screen, hides title bar and taskbar.
