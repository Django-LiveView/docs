;; Variables
(defvar domain "django-liveview.andros.dev")

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

(defun render-layout-html (title description tree-content)
  "Render the HTML layout with the given title, description and content."
  (let ((full-title (make-title title)))
    (jack-html
     "<!DOCTYPE html>"
     `(:html (@ :lang "en")
	     (:head
	      ;; Yandex.Metrika counter
	      (:script (@ :type "text/javascript") "(function(m,e,t,r,i,k,a){m[i]=m[i]||function(){(m[i].a=m[i].a||[]).push(arguments)};
   m[i].l=1*new Date();
   for (var j = 0; j < document.scripts.length; j++) {if (document.scripts[j].src === r) { return; }}
   k=e.createElement(t),a=e.getElementsByTagName(t)[0],k.async=1,k.src=r,a.parentNode.insertBefore(k,a)})
   (window, document, 'script', 'https://mc.yandex.ru/metrika/tag.js', 'ym');

   ym(95556716, 'init', {
        clickmap:true,
        trackLinks:true,
        accurateTrackBounce:true,
        webvisor:true
   });")
	      (:noscript
	       (:div (:img (@ :src "https://mc.yandex.ru/watch/95556716" :style "position:absolute; left:-9999px;" :alt ""))))
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
	      (:link (@ :rel "stylesheet" :type "text/css" :href ,(concat "/css/main.css?cache=" (format-time-string "%s")))))
	     (:body
	      (:header.header
	       (:div.container
		(:nav.nav-main
		 (:ul.nav__list.nav-main__list
		  (:li.nav-main__item
		   (:a.nav-main__link.nav-main__link--logo (@ :href "/") (:img.nav-main__logo (@ :alt "Django LiveView" :src "/img/logo.webp"))))
		  (:li.nav-main__item
		   (:a.button.nav-main__link (@ :href "/docs/install/") "Docs"))
		  (:li.nav-main__item
		   (:a.button.nav-main__link (@ :href "/tutorials/") "Tutorials"))
		  (:li.nav-main__item
		   (:a.button.nav-main__link (@ :href "https://django-liveview-demo.andros.dev/" :target "_blank") "Demo"))
		  (:li.nav-main__item
		   (:a.button.nav-main__link (@ :href "/books/") "Books"))
		  (:li.nav-main__item
		   (:a.button.nav-main__link (@ :href "/source-code/") "Source code"))))))
	      ,tree-content
	      (:footer.footer
	       (:div.container
		(:ul.footer_nav
		 (:li (:i (@ :aria-label "bug") "🪲") " Bugs: " (:a.link (@ :href "https://github.com/Django-LiveView/docs/blob/main/one.org" :target "_blank") "Documentation"))
		 (:li (:i (@ :aria-label "chat") "💬") " Get help: "(:a.link (@ :href "xmpp://django-liveview@conference.im.andros.dev?join" :target "_blank") "Jabber/XMPP group "))
		 (:li (:i (@ :aria-label "chat") "🐘") " Follow me: " (:a.link (@ :href "https://hostux.social/@andros" :target "_blank") "ActivityPub/Fediverse "))
		 (:li (:span (@ :aria-hidden "true") "💰 ") " Support the project: " (:a.link (@ :href "https://liberapay.com/androsfenollosa/" :target "_blank") "Liberapay")))
		(:p "Created with " (:i (@ :aria-label "love") "❤️") " by " (:a.link (@ :href "https://andros.dev/" :target "_blank") "Andros Fenollosa") " with " (:a.link (@ :href "https://one.tonyaldon.com/" :target "_blank") "one.el"))
		(:p "🐍 " ,(format-time-string "%Y")))))))))

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
		  (:section.home
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
		      (:a.nav-docs__link (@ :href "/docs/install/") "Install"))
		     (:li.nav-docs__item
		      (:a.nav-docs__link (@ :href "/docs/actions/") "Actions"))
		     (:li.nav-docs__item
		      (:a.nav-docs__link (@ :href "/docs/views/") "Views"))
		     (:li.nav-docs__item
		      (:a.nav-docs__link (@ :href "/docs/routing/") "Routing"))
		     (:li.nav-docs__item
		      (:a.nav-docs__link (@ :href "/docs/forms/") "Forms"))
		     (:li.nav-docs__item
		      (:a.nav-docs__link (@ :href "/docs/history/") "History"))
		     (:li.nav-docs__item
		      (:a.nav-docs__link (@ :href "/docs/internationalization/") "Internationalization"))
		     (:li.nav-docs__item
		      (:a.nav-docs__link (@ :href "/docs/loading/") "Loading"))
		     (:li.nav-docs__item
		      (:a.nav-docs__link (@ :href "/docs/lost-connection/") "Lost connection"))
		     (:li.nav-docs__item
		      (:a.nav-docs__link (@ :href "/docs/deploy/") "Deploy"))
		     (:li.nav-docs__item
		      (:a.nav-docs__link (@ :href "/docs/faq/") "FAQ"))
		     )))
		  (:main.main.main--docs
		    ,content))))))

(defun one-custom-default-tutorials (page-tree pages _global)
  "Default render function by tutorials page."
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
