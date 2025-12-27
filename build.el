(progn
  (require 'package)
  (add-to-list 'package-archives
	       '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  (package-refresh-contents)
  ;; Install one.el dependencies
  (package-install 'jack)
  (package-install 'htmlize)
  ;; Add fork to load-path and load it
  (add-to-list 'load-path "/usr/src/app/one.el")
  (require 'one)
  (find-file "/usr/src/app/one.org")
  (one-build))
