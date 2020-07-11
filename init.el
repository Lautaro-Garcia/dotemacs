;;; init.el --- Configuración de Emacs

;;; Commentary:

;; Setear repos, cargar ciertos paquetes (delight, use-package) y cargar
;; la config desde el org

;;; Code:


(let ((gc-cons-threshold most-positive-fixnum))

  ;; Setear repositorios
  (require 'package)
  (setq-default
   load-prefer-newer t
   package-enable-at-startup nil)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (package-initialize)

  ;; Instalar dependencias
  (unless (and (package-installed-p 'delight)
               (package-installed-p 'use-package))
    (package-refresh-contents)
    (package-install 'delight t)
    (package-install 'use-package t))
  (setq-default
   use-package-always-defer t
   use-package-always-ensure t)

  ;; Usar el último org
  (use-package org :ensure org-plus-contrib)

  ;; Cargar los secrets
  (load (expand-file-name ".secrets.el" user-emacs-directory))

  ;; Cargar la configuración desde el .org
  (org-babel-load-file (expand-file-name "dotemacs.org" user-emacs-directory))
  (garbage-collect))

;;; init.el ends here
