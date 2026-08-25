(progn
  (require 'package)
  (add-to-list 'package-archives
	       '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  (package-refresh-contents)
  ;; Dependencies: jack (HTML templating) and htmlize (code highlighting)
  (package-install 'jack)
  (package-install 'htmlize)
  (require 'org)
  (require 'ox)
  ;; Load the site generator and build into ./public/
  (setq lv-root "/usr/src/app/")
  (load "/usr/src/app/publish.el")
  (lv-build))
