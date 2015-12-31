(require 'ob)
(require 'org-compat)

(defvar org-babel-default-header-args:ditaa-docker
  '((:results . "file")
    (:exports . "results")))

; (defcustom org-ditaa-docker-image-name "leafac/docker-ditaa"
;   "Ditaa docker image to use"
;   :group 'org-babel
;   :type 'string)

(defvar org-ditaa-docker-image-name "leafac/docker-ditaa"
  "Ditaa docker image to use")

(defun org-babel-execute:ditaa-docker (body params)
  "Execute a block of Ditaa code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((out-file (expand-file-name (or (cdr (assoc :file params))
                                         (error
                                          "ditaa code block requires :file header argument"))))
         (cmdline (cdr (assoc :cmdline params)))
         (java (cdr (assoc :java params)))
         (in-file (org-babel-temp-file "ditaa-"))
         (in-dir (file-name-directory in-file))
         (out-dir (file-name-directory out-file))
         (docker-in-dir "/input")
         (docker-out-dir "/output")
         (docker-in-file (concat docker-in-dir "/" (file-name-nondirectory in-file)))
         (docker-out-file (concat docker-out-dir "/" (file-name-nondirectory out-file)))
         (cmd (concat "docker"
                      " " "run --rm"
                      " " "--volume=" out-dir ":" docker-out-dir
                      " " "--volume=" in-dir ":" docker-in-dir
                      " " "--workdir=/input"
                      " " org-ditaa-docker-image-name
                      " " cmdline
                      " " docker-in-file
                      " " docker-out-file)))
    (with-temp-file in-file (insert body))
    (message cmd)
    (shell-command cmd)
    nil)) ;; signal that output has already been written to file

(defun org-babel-prep-session:ditaa-docker (_session _params)
  "Return an error because ditaa does not support sessions."
  (error "Ditaa docker does not support sessions"))

(provide 'ob-ditaa-docker)
