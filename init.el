;;; TODOS
;;; 1. Welcome screen
;;; 2. Check rainbow parens AND parinfer at the same time
;;; 3. Move buffers
;;; 4. Check folding (tested in JS, doesn't work)

(setq make-backup-files nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(when (eq system-type 'darwin) ;; mac specific settings
  (setq ns-right-option-modifier 'meta))

(setq package-enable-at-startup nil)
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("mepla"     . "https://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(if window-system
  (progn
    (tool-bar-mode -1)
    (toggle-scroll-bar -1)
    (menu-bar-mode -1)))

(require 'use-package)

(use-package which-key
  :ensure t
  :diminish "Ꙍ"
  :init (which-key-mode)
  :config
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-side-window-max-width 0.33
        which-key-idle-delay 0.05))

(use-package evil
  :ensure t
  :diminish undo-tree-mode
  :bind ("C-u" . evil-scroll-up)
  :init (evil-mode))

(use-package evil-surround
  :ensure t
  :init (global-evil-surround-mode))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init (ivy-mode 1)
  :bind (:map ivy-minibuffer-map
          ("C-j" . ivy-next-line)
          ("C-k" . ivy-previous-line)
          ("C-h" . ivy-backward-kill-word)))

(use-package parinfer
  :ensure t
  :diminish "()"
  :config
  (setq parinfer-extensions
    '(defaults       ; should be included.
      pretty-parens  ; different paren styles for different modes.
      evil           ; If you use Evil.
      smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
      smart-yank))  ; Yank behavior depend on mode.
  :init
  (progn
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))

(use-package counsel :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package flycheck
  :ensure t
  :diminish "!"
  :init (global-flycheck-mode))

(use-package flycheck-color-mode-line
  :ensure t
  :init
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(use-package nyan-mode
  :ensure t
  :init  (nyan-mode)
  :config
  (nyan-toggle-wavy-trail)
  (nyan-start-animation))

(use-package spaceline
  :ensure t
  :config
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  :init
  (progn
    (require 'spaceline-config)
    (spaceline-emacs-theme)))

(use-package evil-nerd-commenter
  :ensure t)

(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package pretty-mode
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'pretty-mode))

(use-package auto-complete
  :ensure t
  :init (auto-complete-mode)
  :config
  (ac-config-default)
  :bind (:map ac-completing-map
          ("C-j" . ac-next)
          ("C-k" . ac-previous)))

(use-package tern
  :ensure t
  :config
  (add-to-list 'exec-path "/usr/local/bin")
  :init
  (add-hook 'js-mode-hook (lambda () (tern-mode t))))

(use-package tern-auto-complete
  :ensure t
  :init
  (eval-after-load 'tern
    '(progn
       (require 'tern-auto-complete)
       (tern-ac-setup))))

(use-package nodejs-repl
  :ensure t
  :bind (:map js-mode-map
          ("C-c f" . nodejs-repl-load-file)
          ("C-c e" . nodejs-repl-send-region)
          ("C-c TAB" . nodejs-repl-switch-to-repl)))

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (auto-package-update-maybe))

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(use-package emmet-mode
  :ensure t
  :bind (:map emmet-mode-keymap
          ("TAB" . emmet-expand-line))
  :init
  (progn
    (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
    (add-hook 'css-mode-hook  'emmet-mode))) ;; enable Emmet's css abbreviation.

(use-package ws-butler
  :ensure t
  :init (ws-butler-global-mode))

(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))

(use-package magit
  :ensure t
  :diminish auto-revert-mode)

(use-package evil-magit
  :ensure t)

(use-package dockerfile-mode
  :ensure t
  :bind (:map dockerfile-mode-map
          ("C-c b" . dockerfile-build-buffer)
          ("C-c B" . dockerfile-build-no-cache-buffer))
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package elisp-slime-nav
  :ensure t
  :bind (:map elisp-slime-nav-mode-map
          ("C-c d" . elisp-slime-nav-describe-elisp-thing-at-point)
          ("C-c g" . elisp-slime-nav-find-elisp-thing-at-point))
  :config
  (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode))

(use-package spacemacs-theme
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

(use-package general
  :ensure t
  :config
  (general-evil-setup t)
  (general-define-key
   :states '(normal motion insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "f"   '(:ignore t :which-key "Files")
   "ff"  'counsel-find-file
   "fr"  'counsel-recentf
   "fs"  'save-buffer
   "q"   'save-buffers-kill-terminal
   "w"   '(:ignore t :which-key "Windows")
   "wv"  'split-window-right
   "ws"  'split-window-below
   "wd"  'delete-window
   "wj"  'windmove-down
   "wk"  'windmove-up
   "wh"  'windmove-left
   "wl"  'windmove-right
   "wm"  (lambda ()
           (interactive)
           (if (and (= 1 (length (window-list)))
                    (assoc ?_ register-alist))
             (jump-to-register ?_)
             (progn
               (window-configuration-to-register ?_)
               (delete-other-windows))))
   "b"   '(:ignore t :which-key "Buffers")
   "bd"  'kill-this-buffer
   "bb"  'ivy-switch-buffer
   "p"   '(:ignore t :which-key "Parinfer")
   "pt"  'parinfer-toggle-mode
   "e"   (general-simulate-keys "C-c !" t "Flycheck" Errors)
   "t"   'counsel-load-theme
   "SPC" 'counsel-M-x
   "h"   '(:ignore t :which-key "Help")
   "hf"  'describe-function
   "hv"  'describe-variable
   "hp"  'describe-project
   "hm"  'describe-mode
   "hk"  'counsel-descbinds
   "TAB" 'mode-line-other-buffer
   "c"   '(:ignore t :which-key "Comment")
   "cl"  'evilnc-comment-or-uncomment-lines
   "cp"  'evilnc-comment-or-uncomment-paragraphs
   "U"   'auto-package-update-now
   "g"   'magit-dispatch-popup
   "l"   'linum-mode)

  (general-define-key
   :states '(normal motion emacs visual)
   "," (general-simulate-keys "C-c"))

  (general-define-key
   :states '(normal motion emacs visual)
   "/" 'swiper))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (spacemacs-dark)))
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(package-selected-packages
   (quote
    (elisp-slime-nav markdown-mode dockerfile-mode magit restclient ws-butler emmet-mode web-mode auto-package-update kite-mini mini-kite wooky tern auto-complete angular-mode pretty-mode org-bullets spaceline spaceline-config spacemacs-theme nyan-mode flycheck rainbow-delimiters which-key use-package parinfer general evil-surround counsel))))

(custom-set-faces)
;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
