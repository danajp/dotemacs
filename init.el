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

(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY.

This allows us to define configuration for features that aren't
always installed and only eval that configuration after the feature is loaded.

ELPA packages usually provide an -autoloads feature which we can
use to determine if the package is installed/loaded."
  (declare (indent defun))
  `(eval-after-load (symbol-name ,mode)
     '(progn ,@body)))

;; --- cask setup ----------------------------------------------------
(require 'cask (expand-file-name "~/.cask/cask"))
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
        '(:link t :maxlevel 3 :step day :stepskip0 t :fileskip0 t)))

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

(after 'auto-complete-autoloads
  (require 'auto-complete-config)
  (ac-config-default))

(after 'projectile-autoloads
  (projectile-global-mode))

(after 'haskell-mode-autoloads
  (add-hook 'haskell-mode-hook 'haskell-simple-indent-mode))

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

(after 'tramp-sh
  ;; fix for http://debbugs.gnu.org/cgi/bugreport.cgi?bug=17238 until it's released in 24.4
  (defun tramp-sh-handle-file-truename (filename &optional counter prev-dirs)
    "Like `file-truename' for Tramp files."
    (with-parsed-tramp-file-name (expand-file-name filename) nil
      (tramp-make-tramp-file-name method user host
        (with-tramp-file-property v localname "file-truename"
  	(let ((result nil))			; result steps in reverse order
  	  (tramp-message v 4 "Finding true name for `%s'" filename)
  	  (cond
  	   ;; Use GNU readlink --canonicalize-missing where available.
  	   ((tramp-get-remote-readlink v)
  	    (setq result
  		  (tramp-send-command-and-read
  		   v
  		   (format "echo \"\\\"`%s --canonicalize-missing %s`\\\"\""
  			   (tramp-get-remote-readlink v)
  			   (tramp-shell-quote-argument (tramp-shell-quote-argument localname))))))

  	   ;; Use Perl implementation.
  	   ((and (tramp-get-remote-perl v)
  		 (tramp-get-connection-property v "perl-file-spec" nil)
  		 (tramp-get-connection-property v "perl-cwd-realpath" nil))
  	    (tramp-maybe-send-script
  	     v tramp-perl-file-truename "tramp_perl_file_truename")
  	    (setq result
  		  (tramp-send-command-and-read
  		   v
  		   (format "tramp_perl_file_truename %s"
  			   (tramp-shell-quote-argument localname)))))

  	   ;; Do it yourself.  We bind `directory-sep-char' here for
  	   ;; XEmacs on Windows, which would otherwise use backslash.
  	   (t (let* ((directory-sep-char ?/)
  		     (steps (tramp-compat-split-string localname "/"))
  		     (localnamedir (tramp-run-real-handler
  				    'file-name-as-directory (list localname)))
  		     (is-dir (string= localname localnamedir))
  		     (thisstep nil)
  		     (numchase 0)
  		     ;; Don't make the following value larger than
  		     ;; necessary.  People expect an error message in
  		     ;; a timely fashion when something is wrong;
  		     ;; otherwise they might think that Emacs is hung.
  		     ;; Of course, correctness has to come first.
  		     (numchase-limit 20)
  		     symlink-target)
  		(while (and steps (< numchase numchase-limit))
  		  (setq thisstep (pop steps))
  		  (tramp-message
  		   v 5 "Check %s"
  		   (mapconcat 'identity
  			      (append '("") (reverse result) (list thisstep))
  			      "/"))
  		  (setq symlink-target
  			(nth 0 (file-attributes
  				(tramp-make-tramp-file-name
  				 method user host
  				 (mapconcat 'identity
  					    (append '("")
  						    (reverse result)
  						    (list thisstep))
  					    "/")))))
  		  (cond ((string= "." thisstep)
  			 (tramp-message v 5 "Ignoring step `.'"))
  			((string= ".." thisstep)
  			 (tramp-message v 5 "Processing step `..'")
  			 (pop result))
  			((stringp symlink-target)
  			 ;; It's a symlink, follow it.
  			 (tramp-message
  			  v 5 "Follow symlink to %s" symlink-target)
  			 (setq numchase (1+ numchase))
  			 (when (file-name-absolute-p symlink-target)
  			   (setq result nil))
  			 ;; If the symlink was absolute, we'll get a
  			 ;; string like "/user@host:/some/target";
  			 ;; extract the "/some/target" part from it.
  			 (when (tramp-tramp-file-p symlink-target)
  			   (unless (tramp-equal-remote filename symlink-target)
  			     (tramp-error
  			      v 'file-error
  			      "Symlink target `%s' on wrong host"
  			      symlink-target))
  			   (setq symlink-target localname))
  			 (setq steps
  			       (append (tramp-compat-split-string
  					symlink-target "/")
  				       steps)))
  			(t
  			 ;; It's a file.
  			 (setq result (cons thisstep result)))))
  		(when (>= numchase numchase-limit)
  		  (tramp-error
  		   v 'file-error
  		   "Maximum number (%d) of symlinks exceeded" numchase-limit))
  		(setq result (reverse result))
  		;; Combine list to form string.
  		(setq result
  		      (if result
  			  (mapconcat 'identity (cons "" result) "/")
  			"/"))
  		(when (and is-dir
  			   (or (string= "" result)
  			       (not (string= (substring result -1) "/"))))
  		  (setq result (concat result "/"))))))

  	  (tramp-message v 4 "True name of `%s' is `%s'" localname result)
  	  result))))))

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

;; --- everything below this line was probably added by customize ----

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
