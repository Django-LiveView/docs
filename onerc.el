(defun make-title (title)
  "If title is empty, return the website name. Otherwise, return the title with the website name."
  (concat (when (not (string-empty-p title)) (concat title " | ")) "Django LiveView"))

(defun render-layout-html (title description tree-content)
  "Render the HTML layout with the given title, description and content."
  (let ((full-title (make-title title)))
    (jack-html
     "<!DOCTYPE html>"
     `(:html (@ :lang "en")
	     (:head
	      ;; Generals
	      (:meta (@ :charset "utf-8"))
	      (:link (@ :rel "icon" :type "image/png" :href "/img/favicon.png"))
	      (:meta (@ :name "viewport" :content "width=device-width,initial-scale=1.0, shrink-to-fit=no"))
	      (:meta (@ :name "author" :content "Andros Fenollosa"))
	      (:meta (@ :name "generator" :content "One.el"))
	      ;; SEO
	      (:title ,full-title)
	      (:meta (@ :name "description" :content ,description))
	      (:meta (@ :name "og:image" :content "https://django-liveview.andros.dev/img/og-image.webp"))
	      ;; Fonts
	      (:link (@ :rel "preconnect" :href "https://fonts.googleapis.com"))
	      (:link (@ :rel "preconnect" :href "https://fonts.gstatic.com" :crossorigin t))
	      (:link (@ :rel "stylesheet" :href "https://fonts.googleapis.com/css2?family=Fira+Code&family=Open+Sans:wght@400;700&display=swap"))
	      ;; CSS
	      (:link (@ :rel "stylesheet" :type "text/css" :href "https://cdnjs.cloudflare.com/ajax/libs/normalize/8.0.1/normalize.min.css"))
	      (:link (@ :rel "stylesheet" :type "text/css" :href "/css/main.css")))
	     (:body
	      (:header.header
	       (:div.container
		(:nav.nav-main
		 (:ul.nav__list.nav-main__list
		  (:li.nav-main__item
		   (:a.nav-main__link.nav-main__link--logo (@ :href "/") (:img.nav-main__logo (@ :alt "Django LiveView" :src "/img/logo.webp"))))
		  (:li.nav-main__item
		   (:a.button.nav-main__link (@ :href "/docs/quickstart/") "Docs"))
		  (:li.nav-main__item
		   (:a.button.nav-main__link (@ :href "/tutorials/") "Tutorials"))
		  (:li.nav-main__item
		   (:a.button.nav-main__link (@ :href "/source-code/") "Source code"))
		  (:li.nav-main__item
		   (:a.button.nav-main__link (@ :href "https://django-liveview-demo.andros.dev/" :target "_blank") "Demo"))))))
	      ,tree-content
	      (:footer.footer
	       (:p "Created with ❤️ by " (:a.link (@ :href "https://andros.dev/" :target "_blank") "Andros Fenollosa"))
	       (:p "🐍 " ,(format-time-string "%Y"))))))))

(defun one-custom-default-page (page-tree pages _global)
  "Default render function by home page."
  (let* ((title (org-element-property :raw-value page-tree))
	 (description (org-element-property :DESCRIPTION page-tree))
	 (path (org-element-property :CUSTOM_ID page-tree))
         (content (org-export-data-with-backend
                   (org-element-contents page-tree)
                   'one-ox nil))
         (website-name (one-default-website-name pages))
         (nav (one-default-nav path pages)))
    (render-layout-html
     title
     description
     (jack-html `(:main.main
		  (:section
		   (:div.container ,content)))))))

(defun one-custom-default-home (page-tree pages _global)
  "Default render function by home page."
  (let* ((title (org-element-property :TITLE page-tree))
	 (path (org-element-property :CUSTOM_ID page-tree))
	 (description (org-element-property :DESCRIPTION page-tree))
         (content (org-export-data-with-backend
                   (org-element-contents page-tree)
                   'one-ox nil))
         (website-name (one-default-website-name pages))
         (nav (one-default-nav path pages)))
    (render-layout-html
     title
     description
     (jack-html `(:main.main
		  (:section.hero
		   (:div.container
		    (:hgroup.hero__hgroup
		     (:h1.hero__title "Django LiveView")
		     (:h2.hero__subtitle "Framework for creating Realtime SPAs using HTML over the Wire technology")
		     (:img.image.hero__logo (@ :alt "pet" :src "img/pet.webp")))))
		  (:section
		   (:div.container ,content)))))))

(defun one-custom-default-doc (page-tree pages _global)
  "Default render function by home page."
  (let* ((title (org-element-property :raw-value page-tree))
	 (description (org-element-property :DESCRIPTION page-tree))
	 (path (org-element-property :CUSTOM_ID page-tree))
         (content (org-export-data-with-backend
                   (org-element-contents page-tree)
                   'one-ox nil))
         (website-name (one-default-website-name pages))
         (nav (one-default-nav path pages)))
    (render-layout-html
     title
     description
     (jack-html `(:div.container.docs
		  (:aside.aside-docs
		   (:nav.nav-docs
		    (:ul.nav__list.nav__list--docs.nav-docs__list
		     (:li.nav-docs__item
		      (:a.nav-docs__link (@ :href "/docs/quickstart/") "Quickstart"))
		     (:li.nav-docs__item
		      (:a.nav-docs__link (@ :href "/docs/installation/") "Installation"))
		     (:li.nav-docs__item
		      (:a.nav-docs__link (@ :href "/docs/usage/") "Usage"))
		     (:li.nav-docs__item
		      (:a.nav-docs__link (@ :href "/docs/faq/") "FAQ"))
		     (:li.nav-docs__item
		      (:a.nav-docs__link (@ :href "/docs/api/") "API")))))
		  (:main.main.main--docs
		    ,content))))))
