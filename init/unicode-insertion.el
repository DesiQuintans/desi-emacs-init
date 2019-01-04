;; # Unicode characters in Emacs ------------------------------

;; http://ergoemacs.org/emacs/emacs_n_unicode.html
;; http://xahlee.info/comp/unicode_index.html
;; http://ergoemacs.org/emacs/keyboard_shortcuts_examples.html

;; (define-key key-translation-map (kbd "<f8> ") (kbd ""))

;; The internal names (C-x 8 RET) of the characters are given in the comments.
;; I use <f8> as a reference to C-x 8 RET.

(define-key key-translation-map (kbd "<f8> *")   (kbd "°"))  ; degree sign

(define-key key-translation-map (kbd "<f8> SPC") (kbd " "))  ; non-breaking space
(define-key key-translation-map (kbd "<f8> -") (kbd "‑"))  ; non-breaking hyphen

(define-key key-translation-map (kbd "<f8> u")   (kbd "μ"))  ; greek small letter mu
(define-key key-translation-map (kbd "<f8> a")   (kbd "α"))  ; greek small letter alpha
(define-key key-translation-map (kbd "<f8> b")   (kbd "β"))  ; greek small letter beta

(define-key key-translation-map (kbd "<f8> n")   (kbd "–"))  ; en dash
(define-key key-translation-map (kbd "<f8> m")   (kbd "—"))  ; em dash

(define-key key-translation-map (kbd "<f8> <left>")  (kbd "←"))  ; left arrow
(define-key key-translation-map (kbd "<f8> <right>") (kbd "→"))  ; right arrow
(define-key key-translation-map (kbd "<f8> <up>")    (kbd "↑"))  ; up arrow
(define-key key-translation-map (kbd "<f8> <down>")  (kbd "↓"))  ; down arrow

(define-key key-translation-map (kbd "<f8> x")  (kbd "×"))  ; multiplication sign
(define-key key-translation-map (kbd "<f8> /")  (kbd "÷"))  ; division sign
(define-key key-translation-map (kbd "<f8> +")  (kbd "±"))  ; plus-minus sign
