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

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init (ivy-mode 1)
  :bind ("C-s" . 'swiper))

(use-package parinfer
  :ensure t
  :diminish "()"
  :config
  (setq parinfer-extensions
    '(defaults       ; should be included.
      pretty-parens  ; different paren styles for different modes.
      smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
      smart-yank))  ; Yank behavior depend on mode.
  :hook ((clojure-mode emacs-lisp-mode common-lisp-mode scheme-mode lisp-mode) . parinfer-mode))

(use-package counsel :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

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
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

(use-package darkokai-theme
  :ensure t
  :config (load-theme 'darkokai t))

(use-package org-bullets
  :ensure t
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

(use-package hl-todo
  :ensure t
  :diminish
  :config (global-hl-todo-mode))

(use-package pretty-mode
  :ensure t
  :hook prog-mode)

(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

(use-package js2-mode
  :ensure t
  :config
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (js-do-use-nvm)
  :mode ("\\.js\\'" . js2-mode)
  :bind (:map js2-mode-map
          ("C-x C-e" . js-send-last-sexp)
          ("C-c b" . js-send-buffer)
          ("C-c C-v" . js-send-buffer-and-go)
          ("C-c l" . js-load-file-and-go)
          ("C-c t" . mocha-test-at-point)
          ("C-c T" . mocha-test-project)))

(use-package js2-refactor
  :ensure t
  :hook (js2-mode . js2-refactor-mode)
  :config
  (setq js2-skip-preprocessor-directives t)
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(use-package ac-js2
  :ensure t
  :hook (js2-mode . ac-js2-mode))

(use-package js-comint
  :ensure t)

(use-package mocha
  :ensure t)

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
  :hook (web-mode css-mode))

(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :init (ws-butler-global-mode))

(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))

(use-package magit
  :ensure t
  :diminish auto-revert-mode
  :bind ("C-x g" .'magit-dispatch-popup)
        ("C-x G" . 'magit-status))

(use-package dockerfile-mode
  :ensure t
  :bind (:map dockerfile-mode-map
          ("C-c b" . dockerfile-build-buffer)
          ("C-c B" . dockerfile-build-no-cache-buffer))
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package docker
  :ensure t)

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
  :hook (emacs-lisp-mode . elisp-slime-nav-mode))

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

(defvar counsel-spotify-map (make-sparse-keymap))

(use-package counsel-spotify
  :ensure t
  :bind-keymap ("C-x m" . counsel-spotify-map)
  :bind (:map counsel-spotify-map
          ("SPC" . 'counsel-spotify-toggle-play-pause)
          ("<right>" . 'counsel-spotify-next)
          ("<left>" . 'counsel-spotify-previous)
          ("A" . 'counsel-spotify-search-artist)
          ("a" . 'counsel-spotify-search-album)
          ("t" . 'counsel-spotify-search-track)))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init (projectile-mode))

(use-package counsel-projectile
  :ensure t
  :init (counsel-projectile-mode))

(use-package jedi
  :ensure t
  :hook (python-mode . jedi:setup)
  :config
  (setq jedi:tooltip-method nil))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)))

(use-package clojure-mode
  :ensure t)

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
  :diminish editorconfig-mode
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

(global-set-key (kbd "C-x 1") 'toggle-maximize-buffer)


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
    (emacs-surround darkokai-theme yafolding js2-mode gruvbox-theme grubvox-theme elisp-slime-nav markdown-mode dockerfile-mode magit restclient ws-butler emmet-mode web-mode auto-package-update kite-mini mini-kite wooky tern auto-complete angular-mode pretty-mode org-bullets spaceline spaceline-config spacemacs-theme nyan-mode flycheck rainbow-delimiters which-key use-package parinfer counsel))))

(custom-set-faces)
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
