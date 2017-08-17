;;; ==================================================
;;; tweaks
;;; ================================================== 

;; remove startup message
(setq inhibit-startup-message t)

;; remove menu bar
(menu-bar-mode -1)

;; remove scroll bar
(toggle-scroll-bar -1)

;; remove tool bar
(tool-bar-mode -1)

;; ez switching between open windows
(windmove-default-keybindings)

;;; ==================================================
;;; packages 
;;; ==================================================
;; import package
(require 'package)
(package-initialize)

;; set up try package
(use-package try
  :ensure t)

;; set up which-key package
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; set up neotree package
(use-package neotree
  :ensure t)
(global-set-key [f8] 'neotree-toggle)
;; ability to change root in neotree
;; (define-key map (kbd "RET")
;;   (neotree-make-executor
;;    :file-fn 'neo-open-file
;;    :dir-fn 'neo-open-dir)
;; )

;; enable org mode
(require 'org)

;; boot tern
(use-package tern
  :ensure t)

;; boot ac-html
(use-package ac-html
  :ensure t)

;;; ==================================================
;;; repositories
;;; ==================================================

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.milkbox.net/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize) ;; You might already have this line

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#eaeaea" "#d54e53" "DarkOliveGreen3" "#e7c547" "DeepSkyBlue1" "#c397d8" "#70c0b1" "#181a26"))
 '(custom-enabled-themes (quote (paganini)))
 '(custom-safe-themes
   (quote
    ("3b31ebd74082c6a3043dfd8112069163978330e21cfc9e6ff2c9798dfd6d6505" "5d7e1a089a0392827c1a1a626c93f2be5cf1a108a5f86663e9f1eed67fd094ea" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "1e67765ecb4e53df20a96fb708a8601f6d7c8f02edb09d16c838e465ebe7f51b" default)))
 '(fci-rule-color "#3f1a1a")
 '(package-selected-packages
   (quote
    (color-theme-wombat list-packages-ext tern ac-html paganini-theme afternoon-theme yoshi-theme kaolin-theme color-theme neotree emmet-mode which-key try use-package)))
 '(rainbow-delimiters-max-face-count 10)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
     (40 . "goldenrod")
     (60 . "#e7c547")
     (80 . "DarkOliveGreen3")
     (100 . "#70c0b1")
     (120 . "DeepSkyBlue1")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "goldenrod")
     (200 . "#e7c547")
     (220 . "DarkOliveGreen3")
     (240 . "#70c0b1")
     (260 . "DeepSkyBlue1")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "goldenrod")
     (340 . "#e7c547")
     (360 . "DarkOliveGreen3"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
