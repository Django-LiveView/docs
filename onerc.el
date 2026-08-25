;; Variables
(defvar domain "django-liveview.andros.dev")

;; Raw HTML export blocks
;; The `one-ox' backend ships without an `export-block' transcoder, so
;; `#+begin_export html ... #+end_export' is dropped by default. Register one so
;; raw HTML (used to embed Mermaid diagrams) is emitted verbatim.
(defun one-ox-export-block (export-block _contents _info)
  "Transcode an EXPORT-BLOCK from Org to HTML.
Emit the block verbatim when its type is HTML, ignore it otherwise."
  (when (string= (org-element-property :type export-block) "HTML")
    (org-remove-indentation
     (org-element-property :value export-block))))

(let ((backend (org-export-get-backend 'one-ox)))
  (when backend
    (setf (org-export-backend-transcoders backend)
          (cons '(export-block . one-ox-export-block)
                (org-export-backend-transcoders backend)))))

;; Utils
(defun make-title (title)
  "If title is empty, return the website name. Otherwise, return the title with the website name."
  (concat (when (not (string-empty-p title)) (concat title " | ")) "Django LiveView"))

(defun one-ox-link (link desc info)
      "Transcode a LINK object from Org to HTML.
    DESC is the description part of the link, or the empty string.
    INFO is a plist holding contextual information."
      (let* ((type (org-element-property :type link))
             (path (org-element-property :path link))
             (raw-link (org-element-property :raw-link link))
             (custom-type-link
              (let ((export-func (org-link-get-parameter type :export)))
                (and (functionp export-func)
                     (funcall export-func path desc 'one-ox info))))
             (href (cond
                    ((string= type "custom-id") path)
                    ((string= type "fuzzy")
                     (let ((beg (org-element-property :begin link)))
                       (signal 'one-link-broken
                               `(,raw-link
                                 "fuzzy links not supported"
                                 ,(format "goto-char: %s" beg)))))
                    ((string= type "file")
                     (or
                      ;; ./assets/images/image-1.png --> /images/image-1.png
                      ;; ./public/blog/page-1.md     --> /blog/page-1.md
                      (and (string-match "\\`\\./\\(assets\\|public\\)" path)
                           (replace-match "" nil nil path))
                      (let ((beg (org-element-property :begin link)))
                        (signal 'one-link-broken
                                `(,raw-link ,(format "goto-char: %s" beg))))))
                    (t raw-link)))
             (class (if-let ((parent (org-export-get-parent-element link))
                             (class (plist-get (org-export-read-attribute :attr_html parent)
                                               :class)))
                        (concat " class=\"" class "\" ")
                      " ")))
        (or custom-type-link
            (and
             (string-match one-ox-link-image-extensions path)
             (format "<p><img%ssrc=\"%s\" alt=\"%s\" /></p>"
                     class href (or (org-string-nw-p desc) href)) )
            (format "<a%shref=\"%s\">%s</a>"
                    class href (or (org-string-nw-p desc) href)))))

;; Layouts

(defun render-layout-html (title description navigator-active tree-content)
  "Render the HTML layout with the given title, description and content."
  (let ((full-title (make-title title))
	(class-name-navigator-active "nav-main--active"))
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
	      (:link (@ :rel "stylesheet" :href "https://fonts.googleapis.com/css2?family=Fraunces:opsz,wght@9..144,600;9..144,700&family=IBM+Plex+Mono:wght@400;500;600&family=IBM+Plex+Sans:wght@400;500;600;700&display=swap"))
	      ;; CSS
	      (:link (@ :rel "stylesheet" :type "text/css" :href "https://cdnjs.cloudflare.com/ajax/libs/normalize/8.0.1/normalize.min.css"))
	      (:link (@ :rel "stylesheet" :type "text/css" :href ,(concat "/css/main.css?cache=" (format-time-string "%s")))))
	     (:body
	      (:a.skip-link (@ :href "#main-content") "Skip to content")
	      (:header.header
	       (:div.container
		(:nav.nav-main
		 (:ul.nav__list.nav-main__list
		  (:li.nav-main__item
		   (:a.nav-main__link.nav-main__link--logo (@ :href "/") (:img.nav-main__logo (@ :alt "Django LiveView" :src "/img/logo.webp"))))
		  (:li.nav-main__item
		   (:a.nav-main__link (@ :href "/docs/install/" :class ,(when (string= "docs" navigator-active) class-name-navigator-active)) "Docs"))
		  (:li.nav-main__item
		   (:a.nav-main__link (@ :href "/quick-start/" :class ,(when (string= "tutorial" navigator-active) class-name-navigator-active)) "Quick start"))
		  (:li.nav-main__item
		   (:a.nav-main__link (@ :href "/books/" :class ,(when (string= "books" navigator-active) class-name-navigator-active)) "Books"))
		  (:li.nav-main__item
		   (:a.nav-main__link.nav-main__link--source (@ :href "https://github.com/Django-LiveView/liveview" :target "_blank") "Source code"))))))
	      (:div (@ :id "main-content" :tabindex "-1")
		    ,tree-content)
	      (:footer.footer
	       (:div.container
		(:ul.footer_nav
		 (:li (:i (@ :aria-label "bug") "🪲") " Bugs: " (:a.link (@ :href "https://github.com/Django-LiveView/docs/blob/main/one.org" :target "_blank") "Documentation"))
		 (:li (:i (@ :aria-label "chat") "🐘") " Follow me: " (:a.link (@ :href "https://activity.andros.dev/@andros" :target "_blank") "ActivityPub/Fediverse "))
		 (:li (:span (@ :aria-hidden "true") "💰 ") " Support the project: " (:a.link (@ :href "https://liberapay.com/androsfenollosa/" :target "_blank") "Liberapay")))
		(:p "Created with " (:i (@ :aria-label "love") "❤️") " by " (:a.link (@ :href "https://andros.dev/" :target "_blank") "Andros Fenollosa"))
		(:p "🐍 " ,(format-time-string "%Y")))))
	      (:script (@ :type "text/javascript") "(function() {var headingMap = {'Basic': 'basic', 'Intermediate': 'intermediate', 'Advanced': 'advanced', 'UI Features': 'ui-features', 'System Features': 'system-features', 'Data Handling': 'data-handling'}; document.querySelectorAll('h3').forEach(function(h3) {var text = h3.textContent.trim(); if (headingMap[text]) {h3.id = headingMap[text];}});})();")
	      ;; Mermaid diagrams, themed to match the site palette
	      (:script (@ :type "module") "import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@11/dist/mermaid.esm.min.mjs';
mermaid.initialize({
  startOnLoad: true,
  theme: 'base',
  fontFamily: '\"IBM Plex Sans\", sans-serif',
  sequence: { useMaxWidth: false },
  flowchart: { useMaxWidth: false },
  themeVariables: {
    background: '#eef1ec',
    primaryColor: '#ffffff',
    primaryBorderColor: '#17211c',
    primaryTextColor: '#17211c',
    lineColor: '#17211c',
    secondaryColor: '#b7edd0',
    tertiaryColor: '#ffe4d0',
    actorBkg: '#b7edd0',
    actorBorder: '#17211c',
    actorTextColor: '#17211c',
    actorLineColor: '#17211c',
    signalColor: '#17211c',
    signalTextColor: '#17211c',
    labelBoxBkgColor: '#eef1ec',
    labelBoxBorderColor: '#17211c',
    labelTextColor: '#17211c',
    noteBkgColor: '#ffe4d0',
    noteBorderColor: '#17211c',
    noteTextColor: '#17211c'
  }
});")))))

(defun one-custom-default-page (page-tree pages _global)
  "Default render function by home page."
  (let* ((title (org-element-property :raw-value page-tree))
	 (description (org-element-property :DESCRIPTION page-tree))
	 (navigator-active (org-element-property :NAVIGATOR-ACTIVE page-tree))
	 (path (org-element-property :CUSTOM_ID page-tree))
         (content (org-export-data-with-backend
                   (org-element-contents page-tree)
                   'one-ox nil))
         (website-name (one-default-website-name pages))
         (nav (one-default-nav path pages)))
    (render-layout-html
     title
     description
     navigator-active
     (jack-html `(:main.main
		  (:section
		   (:div.container ,content)))))))

(defun one-custom-default-home (page-tree pages _global)
  "Default render function by home page."
  (let* ((title (org-element-property :TITLE page-tree))
	 (path (org-element-property :CUSTOM_ID page-tree))
	 (description (org-element-property :DESCRIPTION page-tree))
	 (navigator-active (org-element-property :NAVIGATOR-ACTIVE page-tree))
         (content (org-export-data-with-backend
                   (org-element-contents page-tree)
                   'one-ox nil))
         (website-name (one-default-website-name pages))
         (nav (one-default-nav path pages)))
    (render-layout-html
     title
     description
     navigator-active
     (jack-html `(:main.main
		  (:section.hero
		   (:div.container
		    (:hgroup.hero__hgroup
		     (:h1.hero__title "Django LiveView")
		     (:h2.hero__subtitle "Build real-time, reactive interfaces with Django using WebSockets: " (:strong "write Python, not JavaScript"))
		     (:img.image.hero__logo (@ :alt "pet" :src "img/pet.webp")))))
		  (:section.home
		   (:div.container ,content)))))))

(defun one-custom-default-doc (page-tree pages _global)
  "Default render function by home page."
  (let* ((title (org-element-property :raw-value page-tree))
	 (description (org-element-property :DESCRIPTION page-tree))
	 (navigator-active (org-element-property :NAVIGATOR-ACTIVE page-tree))
	 (path (org-element-property :CUSTOM_ID page-tree))
         (content (org-export-data-with-backend
                   (org-element-contents page-tree)
                   'one-ox nil))
         (website-name (one-default-website-name pages))
         (nav (one-default-nav path pages)))
    (render-layout-html
     title
     description
     navigator-active
     (jack-html `(:div.container.docs
		  (:aside.aside-docs
		   (:nav.nav-docs
		    (:ul.nav__list.nav__list--docs.nav-docs__list
		     (:li.nav-docs__item
		      (:a.nav-docs__link (@ :href "/docs/install/") "Install"))
		     (:li.nav-docs__item
		      (:a.nav-docs__link (@ :href "/docs/handlers/") "Handlers")
		      (:ul.nav-docs__sublist
		       (:li.nav-docs__subitem
			(:a.nav-docs__sublink (@ :href "/docs/handlers/#basic") "Basic"))
		       (:li.nav-docs__subitem
			(:a.nav-docs__sublink (@ :href "/docs/handlers/#intermediate") "Intermediate"))
		       (:li.nav-docs__subitem
			(:a.nav-docs__sublink (@ :href "/docs/handlers/#advanced") "Advanced"))))
		     (:li.nav-docs__item
		      (:a.nav-docs__link (@ :href "/docs/frontend/") "Frontend Integration"))
		     (:li.nav-docs__item
		      (:a.nav-docs__link (@ :href "/docs/forms/") "Forms"))
		     (:li.nav-docs__item
		      (:a.nav-docs__link (@ :href "/docs/state-management/") "State Management"))
		     (:li.nav-docs__item
		      (:a.nav-docs__link (@ :href "/docs/broadcasting/") "Broadcasting"))
		     (:li.nav-docs__item
		      (:a.nav-docs__link (@ :href "/docs/advanced/") "Advanced Features")
		      (:ul.nav-docs__sublist
		       (:li.nav-docs__subitem
			(:a.nav-docs__sublink (@ :href "/docs/advanced/#ui-features") "UI Features"))
		       (:li.nav-docs__subitem
			(:a.nav-docs__sublink (@ :href "/docs/advanced/#system-features") "System Features"))
		       (:li.nav-docs__subitem
			(:a.nav-docs__sublink (@ :href "/docs/advanced/#data-handling") "Data Handling"))))
		     (:li.nav-docs__item
		      (:a.nav-docs__link (@ :href "/docs/error-handling/") "Error Handling"))
		     (:li.nav-docs__item
		      (:a.nav-docs__link (@ :href "/docs/testing/") "Testing"))
		     (:li.nav-docs__item
		      (:a.nav-docs__link (@ :href "/docs/deployment/") "Deployment"))
		     (:li.nav-docs__item
		      (:a.nav-docs__link (@ :href "/docs/api-reference/") "API Reference"))
		     (:li.nav-docs__item
		      (:a.nav-docs__link (@ :href "/docs/internationalization/") "Internationalization"))
		     (:li.nav-docs__item
		      (:a.nav-docs__link (@ :href "/docs/loading-indicator/") "Loading Indicator"))
		     (:li.nav-docs__item
		      (:a.nav-docs__link (@ :href "/docs/browser-history/") "Browser History"))
		     (:li.nav-docs__item
		      (:a.nav-docs__link (@ :href "/docs/faq/") "FAQ"))
		     )))
		  (:main.main.main--docs
		   ,content))))))

(defun one-custom-default-tutorials (page-tree pages _global)
  "Default render function by tutorials page."
  (let* ((title (org-element-property :raw-value page-tree))
	 (description (org-element-property :DESCRIPTION page-tree))
	 (navigator-active (org-element-property :NAVIGATOR-ACTIVE page-tree))
	 (path (org-element-property :CUSTOM_ID page-tree))
         (content (org-export-data-with-backend
                   (org-element-contents page-tree)
                   'one-ox nil))
         (website-name (one-default-website-name pages))
         (nav (one-default-nav path pages)))
    (render-layout-html
     title
     description
     navigator-active
     (jack-html `(:main.main
		  (:section.tutorials
		   (:div.container ,content)))))))
;; Sitemap

(defun make-sitemap (pages tree global)
  "Produce file ./public/sitemap.txt"
  (with-temp-file "./public/sitemap.txt"
    (insert
     (mapconcat 'identity (mapcar
			   (lambda (page)
			     (let* ((path (plist-get page :one-path))
				    (link (concat "https://" domain path)))
			       link
			       ))
			   pages) "\n"))))

(add-hook 'one-hook 'make-sitemap)
