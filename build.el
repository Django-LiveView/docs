(progn
  (require 'package)
  (add-to-list 'package-archives
	       '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  (package-refresh-contents)
  (package-install 'one)
  (find-file "/usr/src/app/one.org")
  (one-build))
