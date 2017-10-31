;;; ==================================================
;;; repositories
;;; ==================================================

;; import package
(require 'package)
(package-initialize)

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

;;; ==================================================
;;; packages 
;;; ==================================================

;; set up try package
(use-package try
  :ensure t)

;; set up which-key package
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; set up yasnippet
(use-package yasnippet
  :ensure t) 

;; set up web-mode package
(require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))

;; set up emmet-mode package
(defun web-mode-emmet-enable ()
  "Hook to enable emmet-mode when web-mode is enabled"
  (emmet-mode 1))

(add-hook 'web-mode-hook 'web-mode-emmet-enable)
;; set up tabbar package
(require 'tabbar)

(set-face-attribute
 'tabbar-default nil
 :background "#202020"
 :box '(:line-width 1 :color "black" :style nil))
(set-face-attribute
 'tabbar-unselected nil
 :background "gray30"
 :foreground "white"
 :box '(:line-width 5 :color "gray30" :style nil))
(set-face-attribute
 'tabbar-selected nil
 :background "gray75"
 :foreground "black"
 :box '(:line-width 5 :color "gray75" :style nil))
(set-face-attribute
 'tabbar-highlight nil
 :background "white"
 :foreground "black"
 :box '(:line-width 5 :color "white" :style nil))
(set-face-attribute
 'tabbar-button nil
 :box '(:line-width 1 :color "gray72" :style nil))
(set-face-attribute
 'tabbar-separator nil
 :height 1.0)	   

;; change padding of tabs

;;adding spaces
(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB."
  (let ((label (if tabbar--buffer-show-groups
		   (format: "[%s]  " (tabbar-tab-tabset tab))
		 (format "%s  " (tabbar-tab-value tab)))))
    (if tabbar-auto-scroll-flag
	label
      (tabbar-shorten
       label (max 1 (/ (window-width)
		       (length (tabbar-view
				(tabbar-current-tabset)))))))))
;; enable tabbar-mode
(tabbar-mode 1)

;; enable electric-pair-mode
;; (electric-pair-mode 1)

;; enable emmet mode
;; (emmet-mode 1)

;; function to fit neotree face styles to current theme
(defun fit-neotree-styles nil
  "Adjust neotree styling to fit current loaded theme.
Updates upon loading any new theme."
  (interactive)
  (set-face-attribute
   'neo-dir-link-face nil :foreground (face-attribute 'font-lock-builtin-face :foreground))
  (set-face-attribute
   'neo-file-link-face nil :foreground (face-attribute 'font-lock-comment-face :foreground))
  )
(global-set-key [C-f8] 'fit-neotree-styles)

;; set up neotree package
(use-package neotree
  :ensure t)
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)
  
;; enable org mode
(require 'org)

;; enable emmet mode
(use-package emmet-mode
  :ensure t)

;; boot tern
(use-package tern
  :ensure t)

;; boot ac-html
(use-package ac-html
  :ensure t)

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

;; load theme
;; (defvar after-load-theme-hook nil
  ;; "Hook run after a color theme is loaded using load-theme`.")
;; (add-hook 'after-load-theme-hook 'fit-neotree-styles)

;; (defadvice load-theme (after run-after-load-theme-hook-activate)
  ;; "Run `after-load-theme-hook`."
  ;; (run-hooks 'after-load-theme-hook))
(load-theme 'rebecca t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#eaeaea" "#d54e53" "DarkOliveGreen3" "#e7c547" "DeepSkyBlue1" "#c397d8" "#70c0b1" "#181a26"))
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "4e4d9f6e1f5b50805478c5630be80cce40bee4e640077e1a6a7c78490765b03f" "1bd383f15ee7345c270b82c5e41554754b2a56e14c2ddaa2127c3590d0303b95" "6c7db7fdf356cf6bde4236248b17b129624d397a8e662cf1264e41dab87a4a9a" "2016cdf6b11ed21f40a2246ca86d8c97ee77208bf1739467f8b00d1c0d1adc70" "1177fe4645eb8db34ee151ce45518e47cc4595c3e72c55dc07df03ab353ad132" "3b31ebd74082c6a3043dfd8112069163978330e21cfc9e6ff2c9798dfd6d6505" "5d7e1a089a0392827c1a1a626c93f2be5cf1a108a5f86663e9f1eed67fd094ea" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "1e67765ecb4e53df20a96fb708a8601f6d7c8f02edb09d16c838e465ebe7f51b" default)))
 '(debug-on-error t)
 '(fci-rule-color "#3f1a1a")
 '(global-linum-mode t)
 '(line-number-mode nil)
 '(linum-format (quote dynamic))
 '(org-fontify-whole-heading-line t)
 '(package-selected-packages
   (quote
    (postcss-sorting company-tern company solarized-theme yasnippet-snippets yasnippet rebecca-theme inkpot-theme emmet tern-auto-complete less-css-mode web-beautify faces+ multiple-cursors theme-changer sublimity minimap web-mode tabbar color-theme-wombat list-packages-ext tern ac-html paganini-theme afternoon-theme yoshi-theme kaolin-theme color-theme neotree emmet-mode which-key try use-package)))
 '(rainbow-delimiters-max-face-count 10)
 '(tabbar-separator (quote (0.5)))
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
 '(vc-annotate-very-old-color nil)
 '(window-divider-default-right-width 1)
 '(window-divider-mode t))

;; turn on line numbers in margin, except in neotree
(global-linum-mode)
(add-hook 'neotree-mode-hook (lambda ()
			       ;; space between line numbers and text, right-aligned
			       (setq linum-format " %d ")
			       (linum-mode -1)))

;; space between line numbers and text, right-aligned
(setq linum-format " %d ")

;; ez editing files as sudo without opening new emacs
(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
			 (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
(global-set-key (kbd "C-x C-r") 'sudo-edit)

;;; ==================================================
;;; functions
;;; ==================================================

;; (defun what-face (pos)
  ;; (interactive "d")
  ;; (let ((face (or (get-char-property (pos) 'read-face-name)
	      ;; (get-char-property (pos) 'face))))
    ;; (if face (message "Face: %s" face) (message "No face at %d" pos))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
