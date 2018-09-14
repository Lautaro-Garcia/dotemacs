;;; dotemacs -- My own Emacs configuration file
;;; Commentary:
;;; When things go slow:
;;; Emacs -Q -l profile-dotemacs/profile-dotemacs.el --eval "(setq profile-dotemacs-file (setq load-file-name \"init.el full path\"))" -f profile-dotemacs

;;; Code:
(setq make-backup-files nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(when (eq system-type 'darwin) ;; mac specific settings
  (setq ns-option-modifier 'meta)
  (setq ns-right-option-modifier nil))

(setq package-enable-at-startup nil)
(setq package-archives '(("org"           . "http://orgmode.org/elpa/")
                         ("gnu"           . "http://elpa.gnu.org/packages/")
                         ("melpa"         . "https://melpa.org/packages/")
                         ("marmalade"     . "http://marmalade-repo.org/packages/")
                         ("melpa-stable"  . "https://stable.melpa.org/packages/")))

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

(toggle-truncate-lines t)

(use-package which-key
  :ensure t
  :diminish "Ꙍ"
  :config (which-key-mode)
  :custom
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key--side-window-max-dimensions 0.33)
  (which-key-idle-delay 0.05))

(use-package ivy
  :ensure t
  :defer 2
  :diminish ivy-mode
  :config (ivy-mode 1)
  :bind ("C-s" . 'swiper))

(use-package undo-tree
  :ensure t
  :diminish
  :config (global-undo-tree-mode 1)
  :bind
  ("C-_" . 'undo)
  ("C--" . 'redo))

(use-package paredit
  :ensure t
  :diminish "()"
  :hook ((clojure-mode emacs-lisp-mode common-lisp-mode scheme-mode lisp-mode) . paredit-mode))

(use-package paredit-everywhere
  :ensure t
  :hook (prog-mode . paredit-everywhere-mode))

(use-package counsel
  :defer t
  :ensure t)

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
  :diminish " ! "
  :config
  (progn
    (global-flycheck-mode)
    (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)))

(use-package exec-path-from-shell
  :ensure t
  :defer t
  :config
  (exec-path-from-shell-initialize))

(use-package all-the-icons
  :ensure t
  :defer t)

(use-package zerodark-theme
  :defer t
  :ensure t
  :config
  (zerodark-setup-modeline-format))

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
  :diminish
  :hook (after-init . global-company-mode))

(use-package js2-mode
  :ensure t
  :config
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (js-do-use-nvm)
  :mode ("\\.js\\'" . js2-mode)
  :bind (:map js2-mode-map
          ("C-c C-c" . js-send-last-sexp)
          ("C-c b" . js-send-buffer)
          ("C-c C-v" . js-send-buffer-and-go)
          ("C-c l" . js-load-file-and-go)
          ("C-c t" . mocha-test-at-point)
          ("C-c T" . mocha-test-project)))

(use-package js2-refactor
  :ensure t
  :diminish
  :hook (js2-mode . js2-refactor-mode)
  :config
  (setq js2-skip-preprocessor-directives t)
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(use-package lsp-mode
  :ensure t)

(use-package company-lsp
  :ensure t
  :after (lsp-mode company)
  :init (push 'company-lsp company-backends))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :config (lsp-ui-sideline-mode t)
  :bind
  ("M-." . lsp-ui-peek-find-definitions)
  ("M-?" . lsp-ui-peek-find-references))

(use-package lsp-javascript-typescript
  :ensure t
  :hook
  (js2-mode . lsp-javascript-typescript-enable)
  (typescript-mode . lsp-javascript-typescript-enable))

(use-package js-comint
  :defer t
  :ensure t)

(use-package mocha
  :ensure t
  :defer t)

(use-package elpy
  ;; Packages needed:
  ;; 1. jedi
  ;; 2. flake8
  ;; 3. autopep8
  ;; 4. yapf
  ;; 5. rope
  :ensure t
  :defer t
  :config (elpy-enable))

(use-package auto-package-update
  :ensure t
  :defer t
  :custom (auto-package-update-delete-old-versions t)
  :config (auto-package-update-maybe))

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
  :config (ws-butler-global-mode))

(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))

(use-package magit
  :ensure t
  :diminish auto-revert-mode
  :defer 5
  :bind
  ("C-x g" .'magit-dispatch-popup)
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
         ("C-c d" . elisp-slime-nav-describe-elisp-thing-at-point))
  :hook (emacs-lisp-mode . elisp-slime-nav-mode))

(use-package autopair
  :ensure t
  :defer t
  :diminish autopair-mode
  :config (autopair-global-mode))

(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode))

(use-package sass-mode
  :ensure t
  :mode (("\\.sass\\'" . sass-mode)
         ("\\.scss\\'" . sass-mode)))

(use-package package-lint
  :defer t
  :ensure t)

(use-package haskell-mode
  :ensure t
  :mode ("\\.hs\\'" . haskell-mode))

(use-package intero
  :ensure t
  :hook (haskell-mode . intero-mode))

;; (use-package lsp-haskell
;;   :ensure t
;;   :hook (haskell-mode . lsp-haskell-enable))

(defvar counsel-spotify-map (make-sparse-keymap))

(use-package counsel-spotify
  :ensure t
  :defer 5
  :custom
  (counsel-spotify-client-id "")
  (counsel-spotify-client-secret "")
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
  :defer t
  :bind-keymap ("C-c p" . projectile-command-map)
  :diminish projectile-mode
  :config (projectile-mode))

(use-package counsel-projectile
  :ensure t
  :config (counsel-projectile-mode))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)))

(use-package clojure-mode
  :ensure t
  :mode(("\\.clj\\'" . clojure-mode)
        ("\\.cljs'" . clojure-mode)
        ("\\.cljc" . clojure-mode)))

(use-package diff-hl
  :ensure t
  :defer t
  :config (diff-hl-margin-mode))

(use-package tide
  :ensure t
  :diminish
  :hook (typescript-mode . tide-setup)
  :config
  (setq company-tooltip-align-annotations t)
  (setq tide-tsserver-executable "/usr/bin/tsserver") ;; Have tsserver installed globally
  :bind (:map tide-mode-map
         ("C-c R" . tide-references)
         ("C-c r" . tide-rename-symbol)
         ("C-c e" . tide-project-errors)
         ("C-c ?" . tide-documentation-at-point)
         ("C-c C-m" . tide-refactor)))

(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package slime
  :ensure t
  :config
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

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
 '(auto-package-update-delete-old-versions t t)
 '(compilation-message-face (quote default))
 '(counsel-spotify-client-id "")
 '(counsel-spotify-client-secret "")
 '(custom-enabled-themes (quote (zerodark)))
 '(custom-safe-themes
   (quote
    ("ff79b206ad804c41a37b7b782aca44201edfa8141268a6cdf60b1c0916343bd4" "51ba4e2db6df909499cd1d85b6be2e543a315b004c67d6f72e0b35b4eb1ef3de" "6ee6f99dc6219b65f67e04149c79ea316ca4bcd769a9e904030d38908fd7ccf9" default)))
 '(fci-rule-color "#424748")
 '(highlight-changes-colors (quote ("#ff8eff" "#ab7eff")))
 '(highlight-tail-colors
   (quote
    (("#424748" . 0)
     ("#63de5d" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#424748" . 100))))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (paredit-everywhere company-lsp intero lsp-javascript-typescript lsp-ui lsp-mode indium elpy true undo-tree cedit paredit editorconfig tide diff-hl clojure-mode yaml-mode counsel-projectile projectile counsel-spotify haskell-mode package-lint sass-mode autopair elisp-slime-nav markdown-mode docker dockerfile-mode magit restclient ws-butler emmet-mode web-mode auto-package-update mocha js-comint ac-js2 js2-refactor js2-mode company pretty-mode hl-todo org-bullets exec-path-from-shell flycheck rainbow-delimiters counsel parinfer ivy which-key diminish use-package)))
 '(pos-tip-background-color "#E6DB74")
 '(pos-tip-foreground-color "#242728")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#ff0066")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#63de5d")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#53f2dc")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#06d8ff"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#242728" "#424748" "#F70057" "#ff0066" "#86C30D" "#63de5d" "#BEB244" "#E6DB74" "#40CAE4" "#06d8ff" "#FF61FF" "#ff8eff" "#00b2ac" "#53f2dc" "#f8fbfc" "#ffffff"))
 '(which-key--side-window-max-dimensions 0.33 t)
 '(which-key-idle-delay 0.05)
 '(which-key-sort-order (quote which-key-key-order-alpha)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "#DA8548"))))
 '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "#61AFEF"))))
 '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "#C678DD"))))
 '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "#5593C7"))))
 '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "#8EB161"))))
 '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "#DA8548"))))
 '(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground "#61AFEF"))))
 '(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face :foreground "#C678DD"))))
 '(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face :foreground "#5593C7")))))
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
(provide 'init)
;;; init.el ends here
