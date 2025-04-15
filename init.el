;; init.el --- Emacs entry point

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq package-enable-at-startup nil)

(straight-use-package 'org)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(with-eval-after-load 'org
  (require 'org-indent)
  (require 'ox-md)
  (add-hook 'org-mode-hook #'org-indent-mode))

(use-package toc-org
  :after org
  :hook (org-mode . toc-org-enable))

(use-package org-modern
  :after org
  :hook ((org-mode . org-modern-mode)
         (org-agenda-mode . org-modern-agenda)))


(setq desktop-path '("~/.emacs.d/"))

(org-babel-load-file "~/.emacs.d/config.org")
