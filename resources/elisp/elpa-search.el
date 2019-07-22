;; This is a script for searching ELPA databases. It is called with
;; three command-line arguments: dir, action, and arg. dir is the name
;; of a temporary directory which can be used as package-user-dir for
;; package.el (normally this would be ~/.emacs.d/elpa). action is
;; either "search" or "info". arg in the case of "search" is a search
;; query, which is split on whitespace and applied conjunctively to
;; filter the results. arg in the case of "info" is the name of a
;; package for which to retrieve info. The script writes to stdout in
;; JSON format, either as a map or an array of maps (see api.PkgInfo).

(require 'cl-lib)
(require 'json)
(require 'map)
(require 'package)
(require 'subr-x)

;; Give MELPA priority as it has more up-to-date versions.
(setq package-archives '((melpa . "https://melpa.org/packages/")
                         (gnu . "https://elpa.gnu.org/packages/")
                         (org . "https://orgmode.org/elpa/")))

(defun upm-convert-package-desc (desc)
  "Convert package descriptor DESC to alist.
The JSON representation of the alist can be unmarshaled directly
into a PkgInfo struct in Go."
  (let ((extras (package-desc-extras desc)))
    `((name . ,(symbol-name (package-desc-name desc)))
      (description . ,(package-desc-summary desc))
      (version . ,(package-version-join (package-desc-version desc)))
      (homepageURL . ,(alist-get :url extras))
      (author . ,(when-let ((mnt (alist-get :maintainer extras)))
                   (let ((parts nil))
                     (when-let ((email (cdr mnt)))
                       (push (format "<%s>" email) parts))
                     (when-let ((name (car mnt)))
                       (push name parts))
                     (when parts
                       (string-join parts " ")))))
      (dependencies . ,(cl-remove-if
                        (lambda (dep)
                          (string= dep "emacs"))
                        (mapcar
                         (lambda (link)
                           (symbol-name (car link)))
                         (package-desc-reqs desc)))))))

(defun upm-package-info (package)
  "Given PACKAGE string, return alist of metadata for it, or nil."
  (when-let ((descs (alist-get (intern package) package-archive-contents)))
    ;; If the same package is available from multiple repositories,
    ;; prefer the one from the repository which is listed first in
    ;; `package-archives' (which package.el puts at the *end* of the
    ;; `package-desc' list).
    (upm-convert-package-desc
     (car (last descs)))))

(defvar upm-num-archives-fetched 0
  "Number of package.el archives which have been fetched so far.")

(defun upm-download-callback (action arg)
  "Callback for downloading on a package.el archive.
ACTION is either \"search\" or \"info\". ARG for \"search\" is a
search query; ARG for \"info\" is a package name (in either case
ARG is a string). Write JSON to stdout."
  ;; No race condition, Elisp does not have preemptive multithreading.
  (when (>= (cl-incf upm-num-archives-fetched) (length package-archives))
    (package-read-all-archive-contents)
    (pcase action
      ("search"
       (let ((queries (mapcar
                       #'regexp-quote (split-string arg nil 'omit-nulls))))
         (thread-last package-archive-contents
           (map-keys)
           (mapcar #'symbol-name)
           (cl-remove-if-not (lambda (package)
                               (cl-every (lambda (query)
                                           (string-match-p query package))
                                         queries)))
           (funcall (lambda (packages)
                      (cl-sort packages #'< :key #'length)))
           (mapcar #'upm-package-info)
           (json-encode)
           (princ))
         (terpri)))
      ("info"
       (princ
        (json-encode (upm-package-info arg)))
       (terpri))
      (_ (error "No such action: %S" action)))))

(cl-destructuring-bind (dir action arg) command-line-args-left
  (setq command-line-args-left nil)
  (setq package-user-dir dir)
  (let ((archives-dir (expand-file-name "archives" package-user-dir)))
    (dolist (elt package-archives)
      (cl-destructuring-bind (archive . url) elt
        (let* ((url (concat url "archive-contents"))
               (archive-dir
                (expand-file-name (symbol-name archive) archives-dir))
               (archive-file (expand-file-name "archive-contents" archive-dir)))
          (make-directory archive-dir 'parents)
          (make-process
           :name (format "upm-elpa-%S" archive)
           :command `("curl" "-s" "-o" ,archive-file "--" ,url)
           :noquery t
           :sentinel
           (lambda (proc _event)
             (unless (process-live-p proc)
               (unless (zerop (process-exit-status proc))
                 (error "Failed to download %s: exit code %d"
                        url (process-exit-status proc)))
               (with-current-buffer (find-file-noselect archive-file)
                 (upm-download-callback action arg))))))))))

;; Wait until all the code has finished running before exiting.
(while (< upm-num-archives-fetched (length package-archives))
  ;; 50ms is small enough to be imperceptible to the user.
  (accept-process-output nil 0.05))
