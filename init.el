(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY.

This allows us to define configuration for features that aren't
always installed and only eval that configuration after the feature is loaded.

ELPA packages usually provide an -autoloads feature which we can
use to determine if the package is installed/loaded."
  (declare (indent defun))
  `(eval-after-load (symbol-name ,mode)
     '(progn ,@body)))

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

(after 'grep
  (add-to-list 'grep-find-ignored-directories "log")
  (add-to-list 'grep-find-ignored-directories "tmp")
  (add-to-list 'grep-find-ignored-directories "vendor")
  (add-to-list 'grep-find-ignored-directories "coverage"))

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
(add-to-list 'default-frame-alist '(font . "Terminus-12"))
(add-to-list 'default-frame-alist '(mouse-wheel-mode . 1))
(add-to-list 'default-frame-alist '(fringe-mode . 2))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              '(lambda (f)
                 (with-selected-frame f
                   (when (window-system f)
                     (load-theme 'solarized-dark))))))

;; things I want in every file buffer
(defun my-find-file-hook ()
  "Buffer local settings for buffers that are actually files."
  (setq indicate-empty-lines t
        show-trailing-whitespace t))
(add-hook 'find-file-hooks 'my-find-file-hook)

;; utility functions
(defun my-buffer-name-to-kill-ring ()
  "Put the name of the current buffer into the kill ring"
  (interactive)
  (kill-new (buffer-name (current-buffer))))

(defun my-buffer-file-name-to-kill-ring ()
  "Put the name of the current buffer into the kill ring"
  (interactive)
  (kill-new (buffer-file-name (current-buffer))))


(defun my-upcase-rectangle-line (startcol endcol)
  (when (= (move-to-column startcol) startcol)
    (upcase-region (point) (progn
                             (move-to-column endcol 'coerce)
                             (point)))))

(defun my-upcase-rectangle (b e)
  "upcase in a rectangle"
  (interactive "r")
  (apply-on-rectangle 'my-upcase-rectangle-line b e))


(defun my-ddg-search (q)
  "Run a search on DuckDuckGo"
  (interactive "ssearch duckduckgo: ")
  (browse-url (concat "https://duckduckgo.com/?q=" (url-hexify-string q))))

;; --- cask setup ----------------------------------------------------
(require 'cask (if (file-exists-p (expand-file-name "~/.cask/cask.el"))
                   (expand-file-name "~/.cask/cask")
                 "/usr/local/share/emacs/site-lisp/cask"))
(cask-initialize)

;; --- configure elpa packages ---------------------------------------
;; ido
(after 'ido-ubiquitous-autoloads
  (setq ido-enable-flex-matching t
        ido-auto-merge-work-directories-length -1)
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
                           "~/org/diary.org"
                           "~/org/xls"))
  (setq org-refile-targets '((nil :maxlevel . 2)
                             (org-agenda-files :maxlevel . 2)))
  (setq org-refile-use-outline-path 'file
        org-completion-use-ido t
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm)
  ;; capture templates
  (setq org-capture-templates `(("t" "todo" entry (file ,org-default-notes-file)
                                 "* TODO %?\n  %U\n  %a\n   %i" :clock-in t :clock-resume t)
                                ("m" "meeting" entry (file ,org-default-notes-file)
                                 "* MEETING %? :MEETING:\n%U" :clock-in t :clock-resume t)
                                ("i" "interuption" entry (file ,org-default-notes-file)
                                 "* INTERUPTION with %? :INTERUPTION:\n%U" :clock-in t :clock-resume t)
                                ("j" "journal" entry (file+datetree "~/org/diary.org")
                                 "* %?\n%U\n" :clock-in t :clock-resume t)))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING")))

  (setq org-todo-keyword-faces
        '(("TODO" . org-todo)
          ("NEXT" . org-scheduled)
          ("DONE" . org-done)
          ("WAITING" . org-warning)
          ("HOLD" . org-done)
          ("CANCELLED" . org-done)
          ("MEETING" . org-done)))
  ;; make sure we save all the time
  (add-hook 'org-clock-in-hook 'org-save-all-org-buffers)
  (add-hook 'org-clock-out-hook 'org-save-all-org-buffers)
  (setq org-agenda-clockreport-parameter-plist
        '(:link t :maxlevel 3 :step day :stepskip0 t :fileskip0 t))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (ditaa-docker . t))))

;; magit
(after 'magit-autoloads
  (setq magit-push-always-verify nil)
  (global-set-key (kbd "C-x g s") 'magit-status))

;; yaml
(after 'yaml-mode-autoloads
  (autoload 'yaml-mode "yaml-mode")
  (add-to-list 'auto-mode-alist '("\\.ya?ml" . yaml-mode)))

;; php
(after 'php-mode-autoloads
  (add-hook 'php-mode-hook '(lambda ()
                              (php-enable-psr2-coding-style)
                              ;; psr2 turns this off, turn it back on
                              (setq show-trailing-whitespace t)))
  (add-to-list 'auto-mode-alist '("\\.inc" . php-mode)))

;; markdown
(after 'markdown-mode-autoloads
  (add-to-list 'auto-mode-alist '("\\.md" . markdown-mode)))

;; web-mode
(after 'web-mode-autoloads
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\.erb\\'" . web-mode))
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2)
  (after 'web-mode-autoloads
    (sp-local-pair 'web-mode "<" ">" :actions nil)))

;; zencoding-mode
(after 'zencoding-mode-autoloads
  (add-hook 'web-mode-hook 'zencoding-mode)
  (setq zencoding-indentation 2))

;; org-present
(after 'org-present-autoloads
  (add-hook 'org-present-mode-hook (lambda ()
                                     (org-present-big)
                                     (org-display-inline-images)))
  (add-hook 'org-present-mode-quit-hook (lambda ()
                                          (org-present-small)
                                          (org-remove-inline-images))))

(after 'auto-complete-autoloads
  (require 'auto-complete-config)
  (ac-config-default))

(after 'projectile-autoloads
  (projectile-global-mode)
  (add-to-list 'projectile-globally-ignored-directories "log")
  (add-to-list 'projectile-globally-ignored-directories "tmp"))

(after 'projectile-rails-autoloads
  (add-hook 'projectile-mode-hook 'projectile-rails-on))

(after 'haskell-mode-autoloads
  (add-hook 'haskell-mode-hook 'haskell-simple-indent-mode))

(after 'js
  (setq js-indent-level 2))

(after 'scss-mode
  (setq css-indent-offset 2))

(after 'ace-window-autoloads
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l))
  (global-set-key (kbd "C-x o") 'ace-window))

(after 'ruby-mode
  (setq ruby-align-to-stmt-keywords t))

(if (file-directory-p "/usr/local/bin/rbenv")
    (setq rbenv-installation-directory "/usr/local/bin/rbenv"))
(after 'rbenv-autoloads
  (require 'rbenv)
  (global-rbenv-mode))

(after 'rspec-mode-autoloads
  (require 'rspec-mode)
  (setq rspec-use-rake-when-possible nil)
  (add-hook 'after-init-hook 'inf-ruby-switch-setup))

(after 'yasnippet-autoloads
  (require 'yasnippet)
  (yas-global-mode 1))

;; --- configure non-elpa packages -----------------------------------
(add-to-list 'load-path "~/.emacs.d/lib")

;; pl/sql
(after 'plsql
  (setq plsql-indent 4)
  (add-to-list 'auto-mode-alist '("\\.pk[bs]" . plsql-mode)))

(autoload 'plsql "plsql")
(require 'plsql)

;; -- configure builtin packages -------------------------------------

;; shell-script-mode
(setq sh-basic-offset 2
      sh-indentation 2)

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
       ("magit status" (mode . magit-status-mode))
       ("magit process" (mode . magit-process-mode))
       ("magit other" (name . "^\\*magit-"))
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
  (defun my-erc-insert-timestamp (string)
    "I don't remember what this does"
    (erc-insert-timestamp-left string)
    (let ((datestamp (erc-format-timestamp (current-time) erc-datestamp-format)))
      (unless (string= datestamp erc-last-datestamp)
        (erc-insert-timestamp-left datestamp)
        (setq erc-last-datestamp datestamp))))
  (setq erc-insert-timestamp-function 'my-erc-insert-timestamp)

  (defun my-erc-generate-log-file-name (buffer target nick server port)
    "generate an erc log filename"
    (format "%s.txt" (downcase target)))
  (setq erc-generate-log-file-name-function 'my-erc-generate-log-file-name)

  (add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
  (add-hook 'erc-send-post-hook 'erc-save-buffer-in-logs)

  ;; modeline stuf
  (setq erc-track-exclude '("&bitlbee")
        erc-track-exclude-server-buffer t
        erc-track-exclude-types '("JOIN" "PART" "QUIT")
        erc-hide-list '("JOIN" "PART" "MODE" "QUIT")
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
(global-set-key (kbd "C-c d s") 'my-ddg-search)
(global-set-key (kbd "C-c d R") 'revert-buffer)
(global-set-key (kbd "C-x r u") 'my-upcase-rectangle)

;; org-mode
(global-set-key (kbd "C-c d o i") 'org-clock-in)
(global-set-key (kbd "C-c d o o") 'org-clock-out)
(global-set-key (kbd "C-c d o j") 'org-clock-jump-to-current-clock)
(global-set-key (kbd "C-c d o b") 'org-iswitchb)
(global-set-key (kbd "C-c d o a") 'org-agenda)
(global-set-key (kbd "C-c d o c") 'org-capture)
(global-set-key (kbd "C-c d o l") 'org-store-link)

;; --- solarized theme -----------------------------------------------
(setq solarized-scale-org-headlines nil
      solarized-use-less-bold t
      solarized-use-variable-pitch nil)

;; load additional local configuration if it exists
(when (file-exists-p "~/.emacs.d/local.el")
  (load-file "~/.emacs.d/local.el"))

;; --- everything below this line was probably added by customize ----

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(safe-local-variable-values (quote ((c-indent-level . 8)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
