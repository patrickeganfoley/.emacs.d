;;; init.el --- P Foley emacs configs
;;; Commentary:

;; * (interactive) means you can call a function with M-x
;;  <function-name>



;;; Code:
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(electric-indent-mode -1)

(set-frame-font "Menlo 16")


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



(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))




(use-package osx-browse
  :ensure t)


(use-package browse-at-remote
  :ensure t
  :bind (("C-c g g" . 'browse-at-remote))
  :config (progn
	    (setq browse-url-browser-function 'osx-browse-url-safari))
)


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
(fset 'quotify
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote (" \"\342\346 \"" 0 "%d")) arg)))
(global-set-key (kbd "M-'") 'quotify)
(global-set-key (kbd "M-\"") 'quotify)
;; Neat!  this worked!!

(fset 'spacify
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("  " 0 "%d")) arg)))
(global-set-key (kbd "M-<SPC>") 'spacify)

;;  But really I should search for end of token (omitting \_),
;;  not for spaces.


;; TODO - get the shackles package,
;; I think it forces buffers to open in
;; the window you open them


;;  Appearance
(defun transparency (value)
  "Set the transparency of the frame window.
VALUE from 0 = transparent, 100 = opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))


(use-package color-theme-sanityinc-solarized
  :ensure t
  :config (progn (load-theme 'sanityinc-solarized-dark t t)
                 (load-theme 'sanityinc-solarized-light t t)))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config (progn (load-theme 'sanityinc-tomorrow-day t t)
                 (load-theme 'sanityinc-tomorrow-night t t)
		 (load-theme 'sanityinc-tomorrow-blue t t)
		 (load-theme 'sanityinc-tomorrow-bright t t)
		 (load-theme 'sanityinc-tomorrow-eighties t t)))

(use-package danneskjold-theme
  :ensure t
  :config (progn (load-theme 'danneskjold t t)))

(use-package darcula-theme
  :ensure t
  :config (progn (load-theme 'darcula t t)))

(use-package clues-theme
  :ensure t
  :config (progn (load-theme 'clues t t)))


(use-package cycle-themes
  :ensure t
  ;;  I can't unbind C-c C-t from cycle-themes
  ;;  no matter how hard I try.  ) :
  :bind (("C-c C-y" . cycle-themes-next))
  :init (setq cycle-themes-theme-list
	      '(sanityinc-solarized-light
		sanityinc-solarized-dark
		sanityinc-tomorrow-day
		sanityinc-tomorrow-night
		sanityinc-tomorrow-eighties
		sanityinc-tomorrow-bright
		sanityinc-tomorrow-blue
		clues
		darcula
		danneskjold
		monokai))
  :config (progn
	    (cycle-themes-mode)
	    (setq cycle-themes-mode-map
      (let ((map (make-sparse-keymap)))
	(define-key map (kbd "C-c C-y") 'cycle-themes-next)
	map))
	    )
)


;; Things I looked at and turned off
;;   *  smart-parens


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


;; ido is 'interactively do' things
;; it powers smex but also lets you find files
;; anywhere.  same with buffers
;;  I removed 	ido-everywhere t
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
  (set-marker e nil)))

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
;; pep8, pyflakes (flake8 includes both), pylint and pychecker
;; I don't know which I am using.  I have pep8 and pyflakes,
;; so I think I'm using flake8.


;;  I also installed sqlint with
;;  gem install sqlint 
;;  https://github.com/purcell/sqlint
;;  But I'll bet it defaults to postgres and I'd like
;;  to find a hivesql checker 
;;  other than https://sql.treasuredata.com
;;  This is another option for psql https://github.com/markdrago/pgsanity
;;  The only things I could find for hive are 
;;  https://github.com/mayanhui/Hive-SQL-Syntax-Checker and an 
;;  inactive fork that are 5 and 3 years old.

;;  Auto-pep8
;;  yapf is yet another python formatter, maintained
;;  by google.  
;;  To edit defaults, go to ~/.config/yapf/style
(use-package py-yapf
  :ensure t)


;; flycheck uses https://github.com/jimhester/lintr

;;  Syntax checking for python3

;; (setq pythons-list
;;      '("python2" "python3"))
;;  You don't need this anymore!  Pyenv takes care of it for you!
(defun py3 ()
  "Tell flycheck to use python3."
  (interactive)
  (setq flycheck-python-pycompile-executable "python3")
  (setq flycheck-python-pylint-executable "pylint3")
  (setq flycheck-python-flake8-executable "flake83")
)

(defun py2 ()
  "Tell flycheck to use python2."
  (interactive)
  (setq flycheck-python-pycompile-executable "python2")
  (setq flycheck-python-pylint-executable "pylint2")
  (setq flycheck-python-flake8-executable "flake82")
)



;; Scala stuff (for reading Uhura)
(use-package scala-mode
  :ensure t
  :interpreter
  ("scala" . scala-mode))

;; Golang stuff (might not work.)
;; For flotilla-os and a bunch of other
;; platform stuff
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
;; This let's me copy and paste w/ the system.
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
  "Paste like OSX."
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
;;  You may not need most of this.  You probably don't need psql, you

;; encrypt some passwords?
;; #https://www.emacswiki.org/emacs?action=browse;oldid=Gmail%2c_Gnus_and_GPG_on_a_Mac;id=Gmail%2c_Gnus_and_GPG#toc11
;; This stuff doesn't work.  /shrug.
;;  I couldn't get my gnugpg stuff to work, so instead I'm putting
;;  passwords and things into secrets.el, and not version controlling
;;  that file.

;;  don't need the redshift stuff or anything else other than
;;  `sane-presto` and the presto stuf.
(require 'sql)

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

(setq sql-postgres-program "/usr/local/bin/psql")
(setq sql-send-terminator t)

;;  I add stuff to this in secrets.el
(setq sql-connection-alist '() )

;; Get this from https://github.com/stitchfix/booga/blob/master/gsn/bin/sane-presto
(setq sql-presto-program "sane-presto"
      sql-presto-login-params '((user :default "patrick")
                                (database :default "")))

(defun sql-presto ()
  "Connect to presto."
  (interactive)
  (let ((sql-product 'presto))
    (sql-connect 'presto)))


(defun sql-comint-presto (product options)
  (let ((sql-login-delay 0.9))
    (sql-comint product options)))


(defun sql-redshift ()
  "Connect to reshift.  Don't use this.  Redshift is over."
  (interactive)
  (my-sql-connect 'postgres 'redshift))

(defun my-sql-connect (product connection)
  (setq sql-product product)
  (sql-connect connection))

;; This contains some sql db locations and passwords
;; It is not on github. 
(load-file "~/.emacs.d/secrets.el")

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
	  ;; This does not work.  It is using a weird old definition.
	  ;;  It doesn't use the serif font. 
	  (add-hook 'markdown-mode-hook 'variable-font-buffer)
	  (add-hook 'gfm-mode-hook 'variable-font-buffer)
	  )
)

;;  Org Mode stuff org-mode org .org orgmode
;;  Don't use use-package, it's already in 
;;  vanilla emacs.
(require 'ox-md nil t) 
;;  cycle-themes took C-c C-t...
(eval-after-load 'org-mode 
  '(define-key org-mode-map (kbd "C-c C-t") 'org-todo)
)
;; ... why do I need to do this???
;; It doesn't even work....
(global-set-key (kbd "C-c C-t") 'org-todo)
;;  ...  now it's even more broken.
;;  Somehow now C-c C-y changes theme everywhere BUT orgmode
;;  and orgmode still uses C-c C-t for themes...


(use-package polymode
  :ensure t
  :commands (poly-markdown+r-mode)
  :mode (("\\.rmd\\'" . poly-markdown+r-mode)
	 ("\\.Rmd\\'" . poly-markdown+r-mode))
  :init
  (progn
    (require 'poly-R)
    (require 'poly-markdown)))

;; I don't like how you can't really
;; move the cursor in this.
;;  I also changed it from ansi-term to term.  
;; 
;;  eshell     - really weird input prompt (unicode or something)
;;  ansi-term  - no tab complete
;;  term       - better than the other two, but can't move cursor and can't use M-x anything.  Also true of eshell. 
;; (defun ipython ()
;;   "Open an ipython shell."
;;   (interactive)
;;   (term "/usr/local/bin/ipython" "ipython"))
(defvar python-shell-interpreter)
(defvar python-shell-interpreter-args)
;; From https://github.com/jorgenschaefer/elpy/issues/1106
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))
(setq python-shell-interpreter-args "--simple-prompt -i")
;;  Hooray!  This works!  At least for python2.
;;  But since the error seemed related to ipython5, 
;;  let's switch pyenv and check again.
;;  Works fine!  Just complains about grasp, pip can't find it.


;;  EIN - Emacs IPython Notebook
;;  Do not use the old repo maintained by tkf, 
;;  check out the new one at https://github.com/millejoh/emacs-ipython-notebook
;;  For some reason, ein:jupyter-server-start does not work
;;  It all works fine if I open the notebook server in an eshell.
;; See https://github.com/millejoh/emacs-ipython-notebook/issues/176#issuecomment-299512815
;; for issues with being unable to log in.
;;  This does not work if you
(use-package ein
  :ensure t
  :commands (ein:notebooklist-open)
  :config 
  (defvar ein:jupyter-default-server-command)
  (defvar ein:jupyter-server-args)
  (setq ein:jupyter-default-server-command "/usr/local/bin/jupyter"
        ein:jupyter-server-args (list "--no-browser")
  )
)

;;  R / ESS 
(use-package ess
  :ensure t
  :mode (("\\.r\\'" . r-mode)
	 ("\\.R\\'" . r-mode))
  :init (require 'ess-site)
)


;;  Does this have to come after rmode?
(use-package restclient
  :ensure t
  :mode (("\\.restclient\\'" . restclient-mode))
)



;;  Kind of annoying there is a flymake yaml
;;  but no flycheck-yaml
(use-package yaml-mode
  :ensure t
  :mode "\\.yaml\\'"
  :mode "\\.portal\\'"
  )

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
     (t (error "Logic error 09535" )))
    (dired-sort-other $arg ))
)

(eval-after-load "dired" '(progn
  (define-key dired-mode-map (kbd "s") 'xah-dired-sort) ))


;; Slack
;; (use-package slack
;;   :ensure t
;;   :init)

(provide 'init)
;;; init.el ends here
