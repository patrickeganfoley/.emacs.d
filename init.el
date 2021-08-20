;;; init.el --- P Foley emacs configs
;;; Commentary:

;;  rebuilt with [these instructions](https://news.ycombinator.com/item?id=9595396)
(setq mac-command-modifier 'meta ;; I want this to do nothing.
      mac-option-modifier 'meta)

;;  emacs-mac also has slightly different fullscreen support.
;;  https://github.com/joostkremers/writeroom-mode/issues/34
;;  You can get fullscren by calling M-x toggle-frame-fullscreen
(defun set-frame-width-interactive (arg)
  "Interactively set frame width.
ARG
Taken from https://stackoverflow.com/a/644950.
Use like this:  `C-u 80 M-x set-frame-width-interactive`."
   (interactive "p")
   (set-frame-width (selected-frame) arg))

;;; Code:
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(electric-indent-mode -1)

;; Always make tabs into spaces
(setq-default indent-tabs-mode nil)

(set-frame-font "Menlo 24")


;;  Emacs Lisp Code
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;  Basic package setup
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)


;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(use-package google-this
  :ensure t
  )



;;  Shell things
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))
;; Got this stuff from Mickey Peterson
;; https://www.masteringemacs.org/article/running-shells-in-emacs-overview
;; (setq explicit-shell-file-name "/bin/bash")
(setq shell-file-name "bash")
;; Is this necessary if I have exec-path-from-shell??
(setenv "SHELL" shell-file-name)
(setenv "ESHELL" shell-file-name)


(use-package tramp
  ;; C-x C-f <ip-address>:<path_to_file>
  ;; or cd <ip-address>:<path_to_file>
  ;; You can use it to edit files
  ;; inside docker containers too!
  :ensure t)

(use-package osx-browse
  ;; This provides lisp functions to
  ;; open safari.
  ;; It's necessary for things like browse-at-remote.
  :ensure t)


(use-package browse-at-remote
  :ensure t
  :bind (("C-c g g" . 'browse-at-remote))
  :config (progn
	    (setq browse-url-browser-function 'osx-browse-url-safari)
	    (setq browse-url-browser-function 'osx-browse-url-safari)
	    (setq browse-url-browser-function 'osx-browse-url-safari)))


;; TeX
(use-package latex-math-preview
  :ensure t)

;; Custom tries to put auto-generated code in your init.el
;; This will prevent that.
;; https://emacs.stackexchange.com/a/29746
(setq custom-file "~/.emacs.d/custom.el")
(load-file custom-file)

;; This is me trying to learn lisp
(defun load-init ()
  "Reload the init file.  I am learning how to write elisp."
  (interactive) ;; Do I need this for everything??
  (message "Reloading init.el...")
  (load-file "~/.emacs.d/init.el"))

;; Trying to define a function.  I made these two
;; with Macros, but I have no idea what they mean.
;; I'd like to change this so it
;;   1.  Will use single if I put single, double if double
;;   2.  Is toggle-able or can be undone with a similar function
(fset 'quotify
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote (" \"\342\346 \"" 0 "%d")) arg)))
(global-set-key (kbd "M-'") 'quotify)
(global-set-key (kbd "M-\"") 'quotify)
;; Neat!  this worked!!

(fset 'spacify
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("  " 0 "%d")) arg)))
(global-set-key (kbd "M-<SPC>") 'spacify)

;;  Appearance
(defun transparency (value)
  "Set the transparency of the frame window.
VALUE from 0 = transparent, 100 = opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))


;; Themes
(use-package color-theme-sanityinc-solarized
  :ensure t
  :config (progn (load-theme 'sanityinc-solarized-dark t t)
		 (load-theme 'sanityinc-solarized-light t t)
		 )
  )

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config (progn (load-theme 'sanityinc-tomorrow-day t t)
		 (load-theme 'sanityinc-tomorrow-night t t)
		 (load-theme 'sanityinc-tomorrow-blue t t)
		 (load-theme 'sanityinc-tomorrow-bright t t)
		 (load-theme 'sanityinc-tomorrow-eighties t t)))

(use-package monokai-theme
  :ensure t
  )

(use-package spacemacs-common
  :ensure spacemacs-theme
  :config
  )

(use-package spaceline
  :ensure t
  :init
  (setq powerline-default-separator 'arrow-fade)
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

(use-package cycle-themes
  :ensure t
  ;;  I can't unbind C-c C-t from cycle-themes
  ;;  no matter how hard I try.  ) :
  ;;  This is also annoying for ein/jupyter.  Uses c-t for toggling cells.
  ;;  You should fork it and install following  https://github.com/raxod502/straight.el#integration-with-use-package
  ;; your issue is https://github.com/toroidal-code/cycle-themes.el/issues/3
  :bind (("C-c C-y" . cycle-themes))
  :init (setq cycle-themes-theme-list
	      '(sanityinc-solarized-light
		sanityinc-solarized-dark
		monokai
                ;; zenburn
		))
  :config (progn
	    (cycle-themes-mode)
	    (setq cycle-themes-mode-map
		  (let ((map (make-sparse-keymap)))
		    (define-key map (kbd "C-c C-y") 'cycle-themes)
		    map))
	    )
  )


;; Things I looked at and turned off
;;   *  smart-parens
;;   *  a ton of orgmode things (e.g. special unicode bullets)


(use-package rainbow-delimiters
  ;; I can't see what's going on in lisp code.
  ;; Maybe this will help.
  :ensure t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
    )
  )


(use-package multiple-cursors
  :ensure t
  :config ()
  )

;; I don't really know what this is,
;; but I think forge needs it.
(use-package transient
  :ensure t
  )

(defvar ghub-use-workaround-for-emacs-bug)
(setq ghub-use-workaround-for-emacs-bug nil)

(use-package ghub
  :ensure t
  )

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config (progn
	    ;; The following two are critical for making magit 2.1.0 work more
	    ;; like 1.x for me: WITHOUT these, creating a new topic branch
	    ;; defaults its remote to master -- and pushing the topic branch
	    ;; pushes to master (?!?). WITH these, you're prompted on the
	    ;; first push, and need to supply origin/<topic-branch-name>, but
	    ;; thereafter it's set and all is well.
	    (setq magit-branch-arguments '()) ;do NOT want --track
	    (setq magit-push-arguments '("--set-upstream")))) ;aka -u


;; https://emacsair.me/2018/12/19/forge-0.1/
(use-package forge
  :ensure t
  :after magit
  )

;; ido is 'interactively do' things
;; it powers smex but also lets you find files
;; anywhere.  same with buffers
;;  I removed	ido-everywhere t
;;  b/c it breaks too many things.
(use-package ido
  :ensure t
  :config
  (setq ido-enable-flex-matching t
	ido-create-new-buffer 'always
	ido-everywhere nil
	ido-use-filename-at-point nil
	;; This one prevents in when finding a file
	;; https://stackoverflow.com/a/18089076
	ido-auto-merge-work-directories-length -1)
  (ido-mode +1))


;; smex helps you quickly look up commands
;; C-s and C-r cycle commands, Enter executes selected command.
(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind ("M-x" . smex))


					; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the `comment-dwim' command.
If no region is selected and current line is not blank and we are
  not at the end of the line, then comment current line.
  Replaces default behaviour of `comment-dwim', when it inserts
  comment at the end of the line.
  ARG is passed to `comment-normalize-vars'"
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key "\M-;" 'comment-dwim-line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Full width comment box                                                 ;;
;; from http://irreal.org/blog/?p=374                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bjm-comment-box (b e)
  "Draw a comment box.
Pulled from http://www.star.bris.ac.uk/bjm/emacs-tips.html.
B is beginning of the box.
E is the end."

  (interactive "r")

  (let ((e (copy-marker e t)))
    (goto-char b)
    (end-of-line)
    (insert-char ?  (- fill-column (current-column)))
    (comment-box b e 1)
    (goto-char e)
    (set-marker e nil))
)
(global-set-key (kbd "C-c b b") 'bjm-comment-box)


;; Window / Buffer navigation stuff

;; Windowing
;;  (for multiple monitors)
(global-set-key (kbd "C-x o") 'next-multiframe-window)

;; http://stackoverflow.com/a/17984479
(defun prev-window ()
  (interactive)
  (other-window -1))

;;(define-key global-map (kbd "C-x p") 'prev-window)
(define-key global-map (kbd "C-x p") 'previous-multiframe-window)

(defun rotate-windows ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
		    #'split-window-vertically
		  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
	(other-window 1)
	(switch-to-buffer (other-buffer))))))

;; From http://emacswiki.org/emacs/TransposeWindows
;; They also include this:
;; (define-key ctl-x-4-map (kbd "t") 'transpose-windows)
(defun transpose-windows (arg)
  "Transpose the buffers shown in two windows.
Don't know what ARG does."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
	    (next-win (window-buffer (funcall selector))))
	(set-window-buffer (selected-window) next-win)
	(set-window-buffer (funcall selector) this-win)
	(select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

;; Zooms in on one buffer.  Let's you zoom back out.
;; Think of it as like narrowing but for buffers
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Narrowing.html
;; code from https://gist.github.com/mads-hartmann/3402786#gistcomment-693878
(defun toggle-maximize-buffer () "Maximize buffer."
       (interactive)
       (if (= 1 (length (window-list)))
	   (jump-to-register '_)
	 (progn
	   (window-configuration-to-register '_)
	   (delete-other-windows))))
;;  Good tips on keybinding conventions
;; https://emacs.stackexchange.com/questions/42164/convention-about-using-c-x-or-c-c-as-prefix-keys
(global-set-key (kbd "C-c z") 'toggle-maximize-buffer)



;;  Autocomplete
;;  The major autocompletion tools are company mode and
;;  auto-complete.  company-mode is more actively maintained
;;  and has a better API for plugins.  You will need
;;  to install plugins for different languages just like
;;  you install different backends for syntax checking with
;;  flycheck.
;;
;;  It looks like 'helm' is also a big auto complete thing
;;
;;  Python has two major auto complete backends that work
;;  with company: jedi and rope.  rope is more about
;;  refactoring.
;;  You need to install jedi and etc with pip.
;;
;;  I disable company-mode in eshell and org-mode,
;;  otherwise it messes with the asteriskses.
(use-package company
  :ensure t
  :diminish company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-global-modes '(not eshell-mode shell-mode org-mode))
  ;;(progn
  ;;  jedi breaks w/ pyenv.
  ;;(use-package company-jedi
  ;;   :ensure t)
  ;;)
  ;; :bind
  ;; ("<tab>" . company-complete)
  ;; ("<tab>" . company-complete-common)
  ;;:config
  ;; (defun my/python-mode-hook ()
  ;;  (add-to-list 'company-backends 'company-jedi))
					;(add-hook 'python-mode-hook 'my/python-mode-hook))
  )


;;  Syntax Checking
;;    *  Flycheck is syntax checking specifically for emacs.
;;    *  flymake is the built-in version of syntax checking.
;;    *  flycheck does not check syntax itself, but calls
;;       external programs you need to install.
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  ;; Use C-c ! v to check out flycheck settings
  ;; Use C-c ! n to check the next error!
  )
;; Python has several syntax checkers,
;;   *  pylint and flake8 are the main ones, and mypy does static type checking.
;;   *  You can make a config file for a repo with pylint --generate-rcfile > .pylintrc
;;   *  For flake8, put things in setup.cfg with a [flake8] at the top of the file.
;;      You will also need to add a .dir_locals.el containing
;;      ((python-mode . ((flycheck-flake8rc . "setup.cfg"))))
;;   * My main issue right now is I don't get type checks / they're often incorrect.  I think mypy ought to solve this, but I'm holding off on that until I fix the whole python situation using pyls.

(use-package blacken
  :ensure t
  :config
  ;;(add-hook 'python-mode-hook 'blacken-mode)
  )


(use-package py-yapf
  :ensure t
  )


;;  flycheck uses https://github.com/jimhester/lintr for R


;; (setq pythons-list
;;      '("python2" "python3"))
;;  TODO: change this so it takes a venv as an arg.
(defun py3 ()
  "Tell flycheck to use python3."
  (interactive)
  ;;(setq flycheck-python-pycompile-executable "python3")
  (setq flycheck-python-pylint-executable "/Users/patrickfoley/venvs/396/bin/pylint")
  (setq flycheck-python-flake8-executable "/Users/patrickfoley/venvs/396/bin/flake83")
  )

(defun py2 ()
  "Tell flycheck to use python2."
  (interactive)
  (setq flycheck-python-pycompile-executable "python2")
  (setq flycheck-python-pylint-executable "pylint2")
  (setq flycheck-python-flake8-executable "flake82")
  )

;; Scala
(use-package scala-mode
  :ensure t
  :interpreter
  ("scala" . scala-mode))

;; Golang
(use-package go-mode
  :ensure t
  :init
  (progn
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook 'gofmt-before-save)
    (bind-key [remap find-tag] #'godef-jump))
  :config
  (add-hook 'go-mode-hook 'electric-pair-mode))


;; Copy-Paste
;; This let's me copy and paste w/ OSX
(setq select-enable-clipboard t)

(defun copy-to-clipboard ()
  "Copy like OSX."
  (interactive)
  (if (display-graphic-p)
      (progn
	(message "Yanked region to x-clipboard!")
	(call-interactively 'clipboard-kill-ring-save)
	)
    (if (region-active-p)
	(progn
	  (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
	  (message "Yanked region to clipboard!")
	  (deactivate-mark))
      (message "No region active; can't yank to clipboard!")))
  )

(defun paste-from-clipboard ()
  "Paste like  OSX."
  (interactive)
  (if (display-graphic-p)
      (progn
	(clipboard-yank)
	(message "graphics active")
	)
    (insert (shell-command-to-string "xsel -o -b"))
    )
  )


;;  SQL stuff This is mostly copy pasted from
;;  https://truongtx.me/2014/08/23/setup-emacs-as-an-sql-database-client
;;  https://www.emacswiki.org/emacs?action=browse;oldid=Gmail%2c_Gnus_and_GPG_on_a_Mac;id=Gmail%2c_Gnus_and_GPG#toc11

;; This contains some sql db locations and passwords
;; It is not on github.
(load-file "~/.emacs.d/secrets.el")

(require 'sql)
(use-package sql
  :ensure t
  :init (progn
	  (add-hook 'sql-interactive-mode-hook 'orgtbl-mode)
	  ;; This is great!  You can sort sql results
	  ;; after they show up if you forgot to in the query!
	  ;; C-c ^ will sort!
	  )
  :hook
  (sql-mode . enable-sql-upcase)
  (sql-interactive-mode . enable-sql-upcase)

  :config
  (define-abbrev-table 'sql-mode-abbrev-table
    (mapcar #'(lambda (v) (list v (upcase v) nil 1))
	    '("absolute" "action" "add" "after" "all" "allocate" "alter" "and" "any" "are" "array" "as" "asc" "asensitive" "assertion" "asymmetric" "at" "atomic" "authorization" "avg" "before" "begin" "between" "bigint" "binary" "bit" "bitlength" "blob" "boolean" "both" "breadth" "by" "call" "called" "cascade" "cascaded" "case" "cast" "catalog" "char" "char_length" "character" "character_length" "check" "clob" "close" "coalesce" "collate" "collation" "column" "commit" "condition" "connect" "connection" "constraint" "constraints" "constructor" "contains" "continue" "convert" "corresponding" "count" "create" "cross" "cube" "current" "current_date" "current_default_transform_group" "current_path" "current_role" "current_time" "current_timestamp" "current_transform_group_for_type" "current_user" "cursor" "cycle" "data" "date" "day" "deallocate" "dec" "decimal" "declare" "default" "deferrable" "deferred" "delete" "depth" "deref" "desc" "describe" "descriptor" "deterministic" "diagnostics" "disconnect" "distinct" "do" "domain" "double" "drop" "dynamic" "each" "element" "else" "elseif" "end" "equals" "escape" "except" "exception" "exec" "execute" "exists" "exit" "external" "extract" "false" "fetch" "filter" "first" "float" "for" "foreign" "found" "free" "from" "full" "function" "general" "get" "global" "go" "goto" "grant" "group" "grouping" "handler" "having" "hold" "hour" "identity" "if" "immediate" "in" "indicator" "initially" "inner" "inout" "input" "insensitive" "insert" "int" "integer" "intersect" "interval" "into" "is" "isolation" "iterate" "join" "key" "language" "large" "last" "lateral" "leading" "leave" "left" "level" "like" "limit" "local" "localtime" "localtimestamp" "locator" "loop" "lower" "map" "match" "map" "member" "merge" "method" "min" "minute" "modifies" "module" "month" "multiset" "names" "national" "natural" "nchar" "nclob" "new" "next" "no" "none" "not" "null" "nullif" "numeric" "object" "octet_length" "of" "old" "on" "only" "open" "option" "or" "order" "ordinality" "out" "outer" "output" "over" "overlaps" "pad" "parameter" "partial" "partition" "path" "position" "precision" "prepare" "preserve" "primary" "prior" "privileges" "procedure" "public" "range" "read" "reads" "real" "recursive" "ref" "references" "referencing" "relative" "release" "repeat" "resignal" "restrict" "result" "return" "returns" "revoke" "right" "role" "rollback" "rollup" "routine" "row" "rows" "savepoint" "schema" "scope" "scroll" "search" "second" "section" "select" "sensitive" "session" "session_user" "set" "sets" "signal" "similar" "size" "smallint" "some" "space" "specific" "specifictype" "sql" "sqlcode" "sqlerror" "sqlexception" "sqlstate" "sqlwarning" "start" "state" "static" "submultiset" "substring" "sum" "symmetric" "system" "system_user" "table" "tablesample" "temporary" "then" "time" "timestamp" "timezone_hour" "timezone_minute" "to" "trailing" "transaction" "translate" "translation" "treat" "trigger" "trim" "true" "under" "undo" "union" "unique" "unknown" "unnest" "until" "update" "upper" "usage" "user" "using" "value" "values" "varchar" "varying" "view" "when" "whenever" "where" "while" "window" "with" "within" "without" "work" "write" "year" "zone" "greatest" "least")))

  (defun enable-sql-upcase ()
    (abbrev-mode 1)
    ;; Make underscore a word character so that abbrev stops expanding
    ;; send_count to send_COUNT
    (modify-syntax-entry ?_ "w" sql-mode-syntax-table))

  )

(add-hook 'sql-interactive-mode-hook
	  (lambda ()
	    (toggle-truncate-lines t)))

(defvar sql-postgres-program)
(setq sql-postgres-program "/usr/local/bin/psql")
(defvar sql-send-terminator)
(setq sql-send-terminator t)

;;  I add stuff to this in secrets.el
(defvar sql-connection-alist)
(setq sql-connection-alist '() )



;; Get this from https://github.com/stitchfix/booga/blob/master/gsn/bin/sane-presto
(defvar sql-presto-program)
(defvar sql-presto-login-params)
(setq sql-presto-program "sane-presto"
      sql-presto-login-params '((user :default "patrick")
				(database :default "")))

(defun sql-presto ()
  "Connect to presto."
  (interactive)
  (let ((sql-product 'presto))
    (sql-connect 'presto)
    ))

;; To connect to a local db sqlite db:
;; M-x sql-sqlite
;; <name_of_db.db>

(defun sql-comint-presto (product options x)
  "Interactive connection to presto.
PRODUCT is maybe presto, maybe psql.  OPTIONS I don't use.
We don't know what X is."
  (let ((sql-login-delay 0.9))
    (message "%S" product)
    (message "%S" options)
    (message "%S" x)
    (sql-comint product options)))

(defun set-sql-buffer ()
  "Point to *SQL*."
  (interactive)
  (setq sql-buffer "*SQL*"))

(global-set-key (kbd "C-c q") 'set-sql-bufer)



;; Fonts
;; I want orgmode and markdowns to use variable width fonts.
;; Use variable width font faces in current buffer
;;  This line just declares a variable that apparently
;;  is defined in some other package.
;;  See https://emacs.stackexchange.com/questions/21245/dealing-with-warning-assignment-to-free-variable-when-certain-libraries-can-b for why it' necessary.
(defvar buffer-face-mode-face)
(defun variable-font-buffer ()
  "Set font to a variable width (proportional) fonts in current buffer.  Taken from https://emacs.stackexchange.com/a/3044."
  (interactive)
  (setq buffer-face-mode-face '(
				:family "Times New Roman"
				:height 200
				))
  (buffer-face-mode)
  )


(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("readme\\.md\\'" . gfm-mode)
	 ("\\.md\\'"       . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (progn
	  (setq markdown-command "multimarkdown")
	  ;; I don't think the variable-font-buffer part works.
	  (add-hook 'markdown-mode-hook 'variable-font-buffer)
	  (add-hook 'gfm-mode-hook 'variable-font-buffer)
	  )
  )


(use-package vmd-mode
  ;;  You also need to install vmd
  ;;  You set up nodejs 14.17.5 with asdf
  :ensure t
  :init (progn
	  (add-hook 'markdown-mode-hook 'vmd-mode)
	  )
  )

;;  Org Mode stuff org-mode org .org orgmode 
;;  Don't use use-package, it's already in
;;  vanilla emacs.
(require 'ox-md nil t)

;;  cycle-themes took C-c C-t...
;;  I need to change cycle-themes.
(eval-after-load 'org-mode
  '(define-key org-mode-map (kbd "C-c C-t") 'org-todo)
  )
(global-set-key (kbd "C-c C-t") 'org-todo)

(setq org-hide-emphasis-markers t)

(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

 (let* ((variable-tuple
          (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
                ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                ((x-list-fonts "Verdana")         '(:font "Verdana"))
                ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    ;; (custom-theme-set-faces
    ;;  'user
    ;;  `(org-level-8 ((t (,@headline ,@variable-tuple))))
    ;;  `(org-level-7 ((t (,@headline ,@variable-tuple))))
    ;;  `(org-level-6 ((t (,@headline ,@variable-tuple))))
    ;;  `(org-level-5 ((t (,@headline ,@variable-tuple))))
    ;;  `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
    ;;  `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
    ;;  `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
    ;;  `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
    ;;  `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))


;; R Rlang R ESS
(use-package ess
  :ensure t
  :mode (
	 ("\\.r\\'" . r-mode)
	 ("\\.R\\'" . r-mode)
	 )
  :init (require 'ess-r-mode)
  )

(use-package polymode
  :ensure t
  :commands (poly-markdown+r-mode)
  :mode (("\\.rmd\\'" . poly-markdown+r-mode)
	 ("\\.Rmd\\'" . poly-markdown+r-mode))
  )

(use-package poly-markdown
  :ensure t
  :mode (
	 ("\\.md\\'" . poly-markdown-mode)
	 )
  )

(use-package poly-R
  :ensure t
  )


;; Python python pyls
;;  I'd like to remove this all and use pyls
(defvar python-shell-interpreter)
(defvar python-shell-interpreter-args)
;; From https://github.com/jorgenschaefer/elpy/issues/1106
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))
(setq python-shell-interpreter-args "--simple-prompt -i")

;; ;; You had an issue where emacs would hang when you opened a string.
;; ;; https://github.com/jorgenschaefer/elpy/issues/1381 suggests
;; ;; adding this:
;; (setq elpy-eldoc-show-current-function nil)

;;  EIN - Emacs IPython Notebook
;;  Do not use the old repo maintained by tkf,
;;  check out the new one at https://github.com/millejoh/emacs-ipython-notebook
;;  For some reason, ein:jupyter-server-start does not work
;;  It all works fine if I open the notebook server in an eshell.
;; See https://github.com/millejoh/emacs-ipython-notebook/issues/176#issuecomment-299512815
;; for issues with being unable to log in.
;;  Ein Notes:
;;    run ein:notebooklist-login, use the password
;;    
(use-package ein
  :ensure t
  :commands (ein:notebooklist-open)
  :config
  (defvar ein:jupyter-default-server-command)
  (defvar ein:jupyter-server-args)
  (setq ein:jupyter-default-server-command "/usr/local/bin/jupyter"
	ein:jupyter-server-args (list "--no-browser")))


;;  Does this have to come after rmode?
(use-package restclient
  :ensure t
  :mode (("\\.restclient\\'" . restclient-mode)))


;;  Kind of annoying there is a flymake yaml
;;  but no flycheck-yaml
(use-package yaml-mode
  :ensure t
  :mode "\\.yaml\\'"
  :mode "\\.portal\\'"
  :mode "\\.portal_monitoring\\'")


(use-package projectile
  ;; NOTE - you use this mostly for C-c p s g and C-c p r
  ;; but using M-s . is also really nice!!
  ;; https://stackoverflow.com/a/1775184
  ;;
  ;; http://batsov.com/projectile/
  ;; projectile highly recommends the fix-ido package.
  ;; Maybe I should use it.
  ;; Useful Commands:
  ;;    C-c p s g  Run grep on the files in the project.
  ;;    C-c p b  Display a list of all project buffers currently open (for current project).
  ;;    C-c p p  Display a list of known projects you can switch to.
  ;;    C-c p r  Runs interactive query-replace on all files in the projects.
  ;;    C-c p s s  Runs ag on the project. Requires the presence of ag.el.
  ;;    (This is recommended instead of projectile isearch)
  ;;    C-c p C-h (shows all projectile bindings)
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config (progn
	    (setq projectile-enable-caching t)
	    (setq projectile-switch-project-action 'projectile-dired)))




;;  dired-mode stuff
;;  sets default to be human readable sizes
;; http://pragmaticemacs.com/emacs/dired-human-readable-sizes-and-sort-by-size/
(setq dired-listing-switches "-alh")

;;  let's me sort by size
(defun xah-dired-sort ()
  "Sort dired dir listing in different ways.
Prompt for a choice.
URL `http://ergoemacs.org/emacs/dired_sort.html'
Version 2015-07-30
  It would be nice if dired mode showed column names at the top and let you hit them to sort.  Or if it was an orgmode table to begin with."
  (interactive)
  (let ($sort-by $arg)
    (setq $sort-by (ido-completing-read "Sort by:" '( "date" "size" "name" "dir")))
    (cond
     ((equal $sort-by "name") (setq $arg "-alh"))
     ((equal $sort-by "date") (setq $arg "-alt"))
     ((equal $sort-by "size") (setq $arg "-alhS"))
     (t(error "Logic error 09535")))
    (dired-sort-other $arg)))

(eval-after-load "dired"
  '(progn (define-key dired-mode-map (kbd "s") 'xah-dired-sort)))

(load-theme 'sanityinc-solarized-light t)

(message "Setting custom faces for solarized light")
(custom-theme-set-faces
 'sanityinc-solarized-light
 `(git-commit-summary ((t (:foreground ,"black"))))
 `(ein:cell-input-area ((t (:foreground ,"black"))))
 `(ein:cell-input-area ((t (:background ,"fdf6e3")))))

(message "Setting custom faces for solarized dark")
(custom-theme-set-faces
 'sanityinc-solarized-dark
 `(git-commit-summary ((t (:foreground ,"black")))))

(defun beautify-json ()
  "Format region as json."
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
	(e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
			     "python -m json.tool" (current-buffer) t)))

;; Want to be able to quickly look at json/events from presto
;; pulled from https://stackoverflow.com/questions/435847/emacs-mode-to-edit-json
(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode))
  :config (setq-default js-indent-level 4))

(global-set-key (kbd "C-c C-f") 'beautify-json)


;; https://www.emacswiki.org/emacs/IncrementNumber
 (defun increment-number-at-point ()
      (interactive)
      (skip-chars-backward "0-9")
      (or (looking-at "[0-9]+")
          (error "No number at point"))
      (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(provide 'init)
;;; init.el ends here
