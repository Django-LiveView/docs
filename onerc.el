(defun one-custom-default (page-tree pages _global)
  "Default render function.

See `one-is-page', `one-render-pages' and `one-default-css'."
  (let* ((title (org-element-property :raw-value page-tree))
         (path (org-element-property :CUSTOM_ID page-tree))
         (content (org-export-data-with-backend
                   (org-element-contents page-tree)
                   'one-ox nil))
         (website-name (one-default-website-name pages))
         (nav (one-default-nav path pages)))
    (jack-html
     "<!DOCTYPE html>"
     `(:html
       (:head
	(:meta (@ :charset "utf-8"))
        (:meta (@ :name "viewport" :content "width=device-width,initial-scale=1"))
	(:link (@ :rel "stylesheet" :type "text/css" :href "https://cdnjs.cloudflare.com/ajax/libs/normalize/8.0.1/normalize.min.css"))
        (:link (@ :rel "stylesheet" :type "text/css" :href "/one.css"))
        (:title ,title))
       (:body
	(:main.main
         ,content))))))
