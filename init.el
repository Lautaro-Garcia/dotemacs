;;; dotemacs -- My own Emacs configuration file
;;; Commentary:
;;; TODOS
;;; 1. Welcome screen
;;; 2. Check rainbow parens AND parinfer at the same time
;;; 3. Check folding (tested in JS, doesn't work)

;;; Code:
(setq make-backup-files nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(when (eq system-type 'darwin) ;; mac specific settings
  (setq ns-option-modifier 'meta)
  (setq ns-right-option-modifier nil))

(setq package-enable-at-startup nil)
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("mepla"     . "https://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless (package-installed-p 'diminish)
  (package-install 'diminish))

(if window-system
  (progn
    (tool-bar-mode -1)
    (toggle-scroll-bar -1)
    (menu-bar-mode -1)))

(eval-when-compile (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

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
          ("C-h" . ivy-backward-delete-char)))

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
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(defun my/use-eslint-from-node-modules ()
  "Use local eslint from projects."
  (let ((root (locate-dominating-file (or (buffer-file-name) default-directory)
               (lambda (dir)
                 (let ((eslint (expand-file-name "node_modules/eslint/bin/eslint.js" dir)))
                   (and eslint (file-executable-p eslint)))))))
    (when root
      (let ((eslint (expand-file-name "node_modules/eslint/bin/eslint.js" root)))
        (setq-local flycheck-javascript-eslint-executable eslint)))))

(use-package flycheck
  :ensure t
  :diminish "!"
  :init
  (progn
    (global-flycheck-mode)
    (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)))

(use-package flycheck-color-mode-line
  :ensure t
  :init
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(use-package darkokai-theme
  :ensure t
  :config (load-theme 'darkokai t))

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
  :init (add-hook 'js-mode-hook (lambda () (tern-mode t)))
  :config
  (add-to-list 'exec-path "/usr/local/bin")
  (dolist (keybinding '("C-c C-c" "C-c C-d" "C-c C-r"))
    (unbind-key keybinding tern-mode-keymap))
  :bind (:map tern-mode-keymap
              ("C-c t" . tern-get-type)
              ("C-c d" . tern-get-docs)
              ("C-c r" . tern-rename-variable)))

(use-package tern-auto-complete
  :ensure t
  :init
  (eval-after-load 'tern
    '(progn
       (require 'tern-auto-complete)
       (tern-ac-setup))))

(use-package js-comint
  :ensure t
  :bind (:map js-comint-mode-map
          ("C-c f" . js-comint-send-buffer)
          ("C-c e" . js-comint-send-region)
          ("C-c TAB" . js-comint-repl)))

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (auto-package-update-maybe))

(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'" . web-mode))

(use-package emmet-mode
  :ensure t
  :bind (:map emmet-mode-keymap
          ("TAB" . emmet-expand-line))
  :init
  (progn
    (add-hook 'web-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
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
  :diminish elisp-slime-nav-mode
  :bind (:map elisp-slime-nav-mode-map
         ("C-c d" . elisp-slime-nav-describe-elisp-thing-at-point)
         ("C-c g" . elisp-slime-nav-find-elisp-thing-at-point))
  :init
  (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode))

(use-package gruvbox-theme
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

(use-package autopair
  :ensure t
  :diminish autopair-mode
  :init (autopair-global-mode))

(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode))

(use-package package-lint
  :ensure t)

(use-package haskell-mode
  :ensure t
  :mode ("\\.hs\\'" . haskell-mode))

(use-package counsel-spotify
  :ensure t)

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init (projectile-mode))

(use-package counsel-projectile
  :ensure t
  :init (counsel-projectile-mode))

(use-package jedi
  :ensure t
  :config
  (setq jedi:tooltip-method nil)
  (add-hook 'python-mode-hook 'jedi:setup))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)))

(use-package clojure-mode
  :ensure t)

(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package vi-tilde-fringe
  :ensure t
  :config
  (global-vi-tilde-fringe-mode))

(use-package origami
  :ensure t
  :config (origami-mode))

(use-package diff-hl
  :ensure t
  :config (diff-hl-margin-mode))

(use-package evil-matchit
  :ensure t
  :config (global-evil-matchit-mode))

(use-package tide
  :ensure t
  :config (setq company-tooltip-align-annotations t)
  :bind (:map tide-mode-map
         ("C-c d" . tide-jump-to-definition)
         ("C-c r" . tide-references)
         ("C-c e" . tide-project-errors)
         ("C-c ?" . tide-documentation-at-point))
  :init
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1))
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(defun toggle-maximize-buffer ()
  "Maximizes buffer."
  (interactive)
  (if (and (= 1 (length (window-list)))
         (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))

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
   "wk"  'evil-window-up
   "wK"  'evil-window-move-very-top
   "wj"  'evil-window-down
   "wJ"  'evil-window-move-very-bottom
   "wl"  'evil-window-right
   "wL"  'evil-window-move-far-right
   "wh"  'evil-window-left
   "wH"  'evil-window-move-far-left
   "wo"  'delete-other-windows
   "wd"  'delete-window
   "ws"  'evil-window-split
   "wv"  'evil-window-vsplit
   "wm"  'toggle-maximize-buffer
   "b"   '(:ignore t :which-key "Buffers")
   "bd"  'kill-this-buffer
   "bb"  'ivy-switch-buffer
   "P"   '(:ignore t :which-key "Parinfer")
   "Pt"  'parinfer-toggle-mode
   "p"   '(projectile-command-map :which-key "Projectile")
   "e"   (general-simulate-keys "C-c !" t nil nil "" Error)
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
   "g"   '(:ignore t :which-key "Magit")
   "gg"  'magit-dispatch-popup
   "gb"  'magit-blame
   "l"   'linum-mode
   "m"   '(:ignore t :which-key "Music")
   "mt"  'counsel-spotify-toggle-play-pause
   "mn"  'counsel-spotify-next
   "mp"  'counsel-spotify-previous
   "ms"  '(:ignore t :which-key "Search")
   "msA" 'counsel-spotify-search-artist
   "msa" 'counsel-spotify-search-album
   "mst" 'counsel-spotify-search-track)

  (general-define-key
   :states '(normal motion emacs visual)
   "," (general-simulate-keys "C-c")
   "/" 'swiper))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(counsel-spotify-client-id "")
 '(counsel-spotify-client-secret "")
 '(custom-enabled-themes (quote (darkokai)))
 '(custom-safe-themes
   (quote
    ("6ee6f99dc6219b65f67e04149c79ea316ca4bcd769a9e904030d38908fd7ccf9" default)))
 '(js-indent-level 2)
 '(package-selected-packages
   (quote
    (darkokai-theme yafolding js2-mode gruvbox-theme grubvox-theme elisp-slime-nav markdown-mode dockerfile-mode magit restclient ws-butler emmet-mode web-mode auto-package-update kite-mini mini-kite wooky tern auto-complete angular-mode pretty-mode org-bullets spaceline spaceline-config spacemacs-theme nyan-mode flycheck rainbow-delimiters which-key use-package parinfer general evil-surround counsel))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
