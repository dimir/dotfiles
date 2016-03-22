;; Additional stuff
(setq load-path (cons "/home/vl/git/dotfiles/.emacs.d" load-path))

(require 'snippet)
(require 'find-recursive)
(require 'vl-search)

;; don't break a line after the first or before the last word of a sentence
(add-to-list 'fill-nobreak-predicate 'fill-single-word-nobreak-p)

;; RoR
; needed for rails mode
;(setq load-path (cons "~/.emacs.d/emacs-rails" load-path))
;(require 'rails)

;; Do we need the #file.txt# backup files?
;(setq make-backup-files nil)

;; Disable welcome message
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)

;; Compilation window shall scroll down
(setq compilation-scroll-output 1)

(global-set-key (kbd "\e\em") 'user-save-and-make-all)
(global-set-key (kbd "\e\ek") 'kill-compilation)

;; Language
(set-terminal-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;; General compile function call "make all"
(defun user-save-and-make-all ()
  "save and call compile as make all"
  (interactive)
;;  (save-buffer)
  (compile "make all")
  (message "make all executed!"))

;; Highlight by default
(global-font-lock-mode 1)

;; Activating ecb
;(global-set-key (kbd "\e\ee") 'ecb-activate)

;; Custom key bindings
(global-set-key "\M-g" 'goto-line)
(global-set-key [f2] 'query-replace)
(global-set-key [f3] 'query-replace-regexp)
(global-set-key [f5] 'other-window)
(global-set-key [f6] 'enlarge-window)
(global-set-key [f7] 'shrink-window)
(global-set-key [f11] 'calendar)

;; PHP mode
(add-to-list 'auto-mode-alist '("\\.inc\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.module\\'" . php-mode))
(add-hook 'php-mode-hook 'my-php-mode-hook)
(defun my-php-mode-hook ()
  "My PHP mode configuration."
  (setq indent-tabs-mode t
	tab-width 8
	c-basic-offset 8))

;; HTML mode
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . sgml-mode))

;; TXT mode
(add-to-list 'auto-mode-alist '("README\\'" . text-mode))
(add-to-list 'auto-mode-alist '("svn-commit." . text-mode))
(add-to-list 'auto-mode-alist '("mutt-" . text-mode))

;; Turn on auto-fill mode for all text buffers
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'java-mode-hook 'turn-on-auto-fill)

;; C mode
(add-hook 'auto-mode-alist '("\\.c\\." . c-mode))
(defun my-c-mode-common-hook ()
  (setq tab-width 8)
  (setq c-basic-offset tab-width)
  (c-set-offset 'substatement-open 0) ; brackets should be at same indentation level as condition
  (c-set-offset 'statement-cont 16)
  (c-set-offset 'arglist-close 16)
  (c-set-offset 'arglist-intro 16)
  (c-set-offset 'arglist-cont-nonempty 16)
  (c-set-offset 'case-label 8))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Perl mode
(defun my-perl-mode-hook ()
  (setq tab-width 8)
  (setq perl-indent-level 8)
  (setq perl-continued-brace-offset -8)
  (setq perl-tab-always-indent t)
  (setq perl-continued-statement-offset 8)
  (setq perl-indent-continued-arguments 8))
(add-hook 'perl-mode-hook 'my-perl-mode-hook)

;; Shell-script mode
(defun my-sh-mode-hook ()
  (setq sh-basic-offset 8)
  (setq indent-tabs-mode t))
(add-hook 'sh-mode-hook 'my-sh-mode-hook)

;; Shell-script mode
(defun my-js-mode-hook ()
  (setq js-indent-level 8)
  (setq indent-tabs-mode t))
(add-hook 'js-mode-hook 'my-js-mode-hook)

;; Wrap text at 70, java code at 120
(add-hook 'text-mode-hook '(lambda() (set-fill-column 70)))
(add-hook 'java-mode-hook '(lambda() (set-fill-column 120)))

;; If you want auto-fill mode on in all major modes, uncomment this:
(setq-default auto-fill-hook 'do-auto-fill)

;; Turn on spellcheck for text mode
(add-hook 'text-mode-hook 'flyspell-mode)

;; Linux indention
(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (setq tab-width 8)
  (setq c-basic-offset 8)
  (interactive)
  (c-mode)
  (c-set-style "K&R")
  (setq indent-tabs-mode t))

;; goodies

;; -- auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)

;; -- gitsum
(require 'gitsum)

;; -- whitespace errors
(require 'show-wspace)
(show-ws-toggle-show-trailing-whitespace)

;; Ignore these extensions in file expansion
(setq completion-ignored-extensions '(".a" ".so" ".o" ".elc" "~" ".dvi"))

;; Ask if there isn't a final newline...
(setq require-final-newline nil)

;; but be sure not to insert a bunch of 'em at the same time.
(setq next-line-add-newlines nil)

;; No silly menus, toolbars, line numbers or columns.
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))

(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(if (fboundp 'line-number-mode)
    (line-number-mode 1))

(if (fboundp 'column-number-mode)
    (column-number-mode 1))

;; I want unique buffer names with hints as to where the files are.
(require 'uniquify)

(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-after-kill-buffer-p t)

;; Tea time!
(defun tea-time (timeval)
  "Ask how long the tea should draw and start a timer."
  (interactive "sHow long (min:sec)? ")
  (if (not (string-match "\\`\\([0-9]*\\)[.:]\\([0-5]?[0-9]?\\)\\'" timeval))
      (error "Strange time."))

  (let* ((minutes (string-to-int (substring timeval (match-beginning 1)
					        (match-end 1))))
	  (seconds (+ (* minutes 60)
		            (string-to-int (substring timeval (match-beginning 2)
						      (match-end 2))))))
    (tea-timer seconds)))

(defun tea-timer (sec)
  "Ding when tea is ready."
  (interactive)
  (run-at-time sec nil '(lambda ()
                          (ding t) (ding t)
                          (message "Your tea is ready!"))))

;; Disable abbrev save to prevent weird annoying
(setq save-abbrevs nil)
