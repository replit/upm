;; This is code that Cask can evaluate which prints a list of all the
;; currently installed packages to stdout, in "name=version" format.

(dolist (dir load-path)
  (when (string-match "elpa/\\(.+\\)-\\([^-]+\\)" dir)
    (princ (format "%s=%s\n"
                   (match-string 1 dir)
                   (match-string 2 dir)))))
