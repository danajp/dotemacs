;; --- general config ------------------------------------------------
;; no menus, buttons, scrollbars or startup screen
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-screen t)

;; put backups in one place instead of littering the file system with files~
(add-to-list 'backup-directory-alist '("." . "~/.emacs.d/backups"))

(global-font-lock-mode t)
(show-paren-mode t)
(column-number-mode 1)
(global-subword-mode t)

(setq font-lock-maximum-decoration t
      transient-mark-mode t
      next-line-add-newlines nil
      grep-command "grep -rni")

;; tabs are evil
(setq-default indent-tabs-mode nil
              tab-width 4)

;; I want to use these functions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; default to utf-8 everywhere
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; don't make me type "yes" or "no"
(defalias 'yes-or-no-p 'y-or-n-p)

;; browse-url customization
(when (fboundp 'browse-url)
  (setq browse-url-browser-function 'browse-url-default-browser))

;; gui stuff
(add-to-list 'default-frame-alist '(font . "Terminus-11"))
(add-to-list 'default-frame-alist '(mouse-wheel-mode 1))
(add-to-list 'default-frame-alist '(fringe-mode 2))

;; things I want in every file buffer
(defun dana-find-file-hook ()
  "Buffer local settings for buffers that are actually files."
  (setq indicate-empty-lines t
        show-trailing-whitespace t))
(add-hook 'find-file-hooks 'dana-find-file-hook)

;; utility functions
(defun dana-buffer-name-to-kill-ring ()
  "Put the name of the current buffer into the kill ring"
  (interactive)
  (kill-new (buffer-name (current-buffer))))

(defun dana-buffer-file-name-to-kill-ring ()
  "Put the name of the current buffer into the kill ring"
  (interactive)
  (kill-new (buffer-file-name (current-buffer))))

(defun dana-install-packages ()
  "Install packages that I almost always want. This is for
bootstrapping a fresh install of emacs."
  (interactive)
  (package-refresh-contents)
  (mapc '(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
	'(ido-ubiquitous
	  magit
      smartparens
      markdown-mode
      yaml-mode
      web-mode
      zencoding-mode)))

;; elpa setup
(require 'package)
(setq package-user-dir "~/.emacs.d/elpa/")
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))


(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY.

This allows us to define configuration for features that aren't
always installed and only eval that configuration after the feature is loaded.

ELPA packages usually provide an -autoloads feature which we can
use to determine if the package is installed/loaded."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

;; --- configure elpa packages ---------------------------------------
;; ido
(after 'ido-ubiquitous-autoloads
  (setq ido-enable-flex-matching t)
  (ido-mode 1)
  (ido-ubiquitous-mode 1))

(after 'smartparens-autoloads
  (smartparens-global-mode)
  (show-smartparens-global-mode)
  (require 'smartparens-config))

;; org
(after 'org
  (add-hook 'org-mode-hook 'turn-on-font-lock)
  (org-clock-persistence-insinuate)
  (setq org-log-done 'time ;; log the time when finishing a todo task
	;; save clock when emacs closes
	org-clock-persist 'history
	;; if idle for 10 min, prompt to resolve
	org-clock-idle-time nil
	;; always clock into LOGBOOK drawer
	org-clock-into-drawer t
	org-default-notes-file (concat org-directory "/refile.org")
	;; only a single line of context in file links
	org-context-in-file-links 1
	org-clock-out-remove-zero-time-clocks t)
  (setq org-agenda-files '("~/org/refile.org"
                           "~/org/projects.org"
                           "~/org/day-to-day.org"
                           "~/org/consult.org"
                           "~/org/overhead.org"))
  (setq org-refile-targets '((nil :maxlevel . 2)
                             (org-agenda-files :maxlevel . 2)))
  (setq org-refile-use-outline-path 'file
        org-completion-use-ido t
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm)
  ;; capture templates
  (setq org-capture-templates `(("t" "todo" entry (file ,org-default-notes-file) "* TODO %?\n  %U\n  %a\n   %i" :clock-in t :clock-resume t)
                                ("i" "interuption" entry (file ,org-default-notes-file) "* %?\n  %U\n  %a\n   %i" :clock-in t :clock-resume t)
                                ("j" "jira" entry (file ,org-default-notes-file) "%^{JIRA}p* %?\n  %U\n  %a\n   %i" :clock-in t :clock-resume t)
                                ("c" "code review item" entry (file ,org-default-notes-file) "%^{DEVELOPER}p* TODO %?\n  %U\n  %a\n  %i")))
  ;; make sure we save all the time
  (add-hook 'org-clock-in-hook 'org-save-all-org-buffers)
  (add-hook 'org-clock-out-hook 'org-save-all-org-buffers))

;; magit
(after 'magit-autoloads
  (global-set-key (kbd "C-x g s") 'magit-status))

;; yaml
(after 'yaml-mode-autoloads
  (autoload 'yaml-mode "yaml-mode")
  (add-to-list 'auto-mode-alist '("\\.ya?ml" . yaml-mode)))

;; php
(after 'php-mode-autoloads
  (c-add-style "php-pear-k&r-mods" '("k&r"
                                     (c-basic-offset . 4)
                                     (c-offsets-alist . ((case-label . +)
                                                         (arglist-close . 0)
                                                         (inline-open . 0)))
                                     (c-hanging-braces-alist . ((defun-open after)
                                                                (inline-open after)
                                                                (substatement-open after)))))
  (add-hook 'php-mode-hook '(lambda ()
                              (message "setting style to php-pear")
                              (c-set-style "php-pear-k&r-mods"))))

;; markdown
(after 'markdown-mode-autoloads
  (add-to-list 'auto-mode-alist '("\\.md" . markdown-mode)))

;; web-mode
(after 'web-mode-autoloads
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

;; zencoding-mode
(after 'zencoding-mode-autoloads
  (add-hook 'sgml-mode-hook 'zencoding-mode))

;; org-present
(after 'org-present-autoloads
  (add-hook 'org-present-mode-hook (lambda ()
                                     (org-present-big)
                                     (org-display-inline-images)))
  (add-hook 'org-present-mode-quit-hook (lambda ()
                                          (org-present-small)
                                          (org-remove-inline-images))))

;; --- configure non-elpa packages -----------------------------------
(add-to-list 'load-path "~/.emacs.d/lib")

;; pl/sql
(after 'plsql
  (setq plsql-indent 4)
  (add-to-list 'auto-mode-alist '("\\.pk[bs]" . plsql-mode)))

(autoload 'plsql "plsql")
(require 'plsql)

;; -- configure builtin packages -------------------------------------

;; uniquify
(after 'uniquify
  (setq uniquify-buffer-name-style 'post-forward
	uniquify-separator ":"
	uniquify-after-kill-buffer-p t
	uniquify-ignore-buffers-re "^\\*"))

(require 'uniquify)

;; ibuffer
(after 'ibuffer
  (setq ibuffer-saved-filter-groups
	'(("default"
	   ("erc" (mode . erc-mode))
	   ("org" (mode . org-mode))
	   ("emacs" (or (name . "^\\*scratch\\*$")
			(name . "^\\*Messages\\*$")
			(name . "^\\*Apropos\\*$")
			(name . "^\\*Help\\*$")
			(name . "^\\*Completions\\*")))
	   ("temp" (name . "^\\*.+\\*$")))))
  (add-hook 'ibuffer-mode-hook (lambda ()
				 (ibuffer-switch-to-saved-filter-groups "default"))))

(require 'ibuffer)

;; erc
(after 'erc
  (setq erc-log-insert-log-on-open t
        erc-log-channels-directory "~/.erc/logs/"
        erc-fill-prefix "      "
        erc-timestamp-only-if-changed-flag nil
        erc-timestamp-format "%H:%M "
        erc-datestamp-format " === %Y-%m-%d %a ===\n")

  (make-variable-buffer-local
   (defvar erc-last-datestamp nil))
  (defun dana-erc-insert-timestamp (string)
    "I don't remember what this does"
    (erc-insert-timestamp-left string)
    (let ((datestamp (erc-format-timestamp (current-time) erc-datestamp-format)))
      (unless (string= datestamp erc-last-datestamp)
        (erc-insert-timestamp-left datestamp)
        (setq erc-last-datestamp datestamp))))
  (setq erc-insert-timestamp-function 'dana-erc-insert-timestamp)

  (defun dana-erc-generate-log-file-name (buffer target nick server port)
    "generate an erc log filename"
    (format "%s.txt" (downcase target)))
  (setq erc-generate-log-file-name-function 'dana-erc-generate-log-file-name)

  (add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
  (add-hook 'erc-send-post-hook 'erc-save-buffer-in-logs)

  ;; modeline stuf
  (setq erc-track-exclude '("&bitlbee")
        erc-track-exclude-server-buffer t
        erc-track-exclude-types '("JOIN" "PART" "QUIT")
        erc-track-shorten-start 5
        erc-track-shorten-cutoff 10)
  (erc-spelling-mode 1))

(require 'erc)

;; tramp
(after 'tramp
  (setq tramp-default-method "ssh"))

;; command == meta on mac
(setq mac-command-modifier 'meta)

;; global key bindings
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c d g") 'goto-line)
(global-set-key (kbd "C-c d r") 'replace-regexp)
(global-set-key (kbd "C-c d a") 'align-regexp)
(global-set-key (kbd "C-c d t") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c d w") 'backward-kill-word)
(global-set-key (kbd "C-c d e") 'rgrep)
(global-set-key (kbd "C-c d s") 'toggle-window-split)
(global-set-key (kbd "C-c d R") 'revert-buffer)

;; org-mode
(global-set-key (kbd "C-c d o i") 'org-clock-in)
(global-set-key (kbd "C-c d o o") 'org-clock-out)
(global-set-key (kbd "C-c d o j") 'org-clock-jump-to-current-clock)
(global-set-key (kbd "C-c d o b") 'org-iswitchb)
(global-set-key (kbd "C-c d o a") 'org-agenda)
(global-set-key (kbd "C-c d o c") 'org-capture)
(global-set-key (kbd "C-c d o l") 'org-store-link)

;; load additional local configuration if it exists
(when (file-exists-p "~/.emacs.d/local.el")
  (load-file "~/.emacs.d/local.el"))
