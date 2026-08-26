;;; publish.el --- Build the Django LiveView site with org-publish -*- lexical-binding: t; -*-

;; Static site generator for the Django LiveView documentation.
;;
;; It drives the standard `org-publish' infrastructure with a small, custom
;; export backend (`lv') that emits lean, class-controlled HTML, plus a set of
;; layout functions written with `jack'.  Each page is a single Org file under
;; `content/' whose first level-1 headline carries the ONE/CUSTOM_ID/TITLE/
;; DESCRIPTION/NAVIGATOR-ACTIVE properties.
;;
;; The `lv' export backend (transcoders and htmlize helpers) is adapted from
;; one.el by Tony Aldon (GPLv3), which previously powered this site.

;;; Code:

(require 'jack)
(require 'ox)
(require 'htmlize)
(require 'ox-publish)

(defvar htmlize-buffer-places)

(defvar lv-root default-directory
  "Absolute path of the project root (must end with a slash).")

(defvar lv-domain "django-liveview.andros.dev"
  "Domain used to build the sitemap.")

(defvar lv--sitemap-paths nil
  "Accumulated page paths, filled while publishing, used for the sitemap.")

;;; Utils

(defun lv-escape (s)
  "Return string S with `<', `>', `&', `\"' and `'' escaped."
  (replace-regexp-in-string
   "\\(<\\)\\|\\(>\\)\\|\\(&\\)\\|\\(\"\\)\\|\\('\\)"
   (lambda (m) (pcase m
                 ("<"  "&lt;")
                 (">"  "&gt;")
                 ("&"  "&amp;")
                 ("\"" "&quot;")
                 ("'"  "&apos;")))
   s))

;;; Export backend `lv'

(defun lv-ox-headline (headline contents _info)
  "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline."
  (let* ((level (org-element-property :level headline))
         (title (org-element-property :raw-value headline))
         (id (org-element-property :one-internal-id headline))
         (ct (if (null contents) "" contents)))
    (format "<div><h%s id=\"%s\">%s</h%s>%s</div>" level id title level ct)))

(defun lv-ox-section (_section contents _info)
  "Transcode a SECTION element from Org to HTML.
CONTENTS holds the contents of the section."
  (if (null contents) "" (format "<div>%s</div>" contents)))

(defun lv-ox-paragraph (_paragraph contents _info)
  "Transcode a PARAGRAPH element from Org to HTML.
CONTENTS is the contents of the paragraph, as a string."
  (format "<p>%s</p>" contents))

(defun lv-ox-plain-text (text _info)
  "Transcode a TEXT string from Org to HTML."
  (lv-escape text))

(defun lv-ox-bold (_bold contents _info)
  "Transcode BOLD from Org to HTML with CONTENTS."
  (format "<b>%s</b>" contents))

(defun lv-ox-italic (_italic contents _info)
  "Transcode ITALIC from Org to HTML with CONTENTS."
  (format "<i>%s</i>" contents))

(defun lv-ox-strike-through (_strike-through contents _info)
  "Transcode STRIKE-THROUGH from Org to HTML with CONTENTS."
  (format "<del>%s</del>" contents))

(defun lv-ox-underline (_underline contents _info)
  "Transcode UNDERLINE from Org to HTML with CONTENTS."
  (format "<u>%s</u>" contents))

(defun lv-ox-code (code _contents _info)
  "Transcode inline CODE from Org to HTML."
  (format "<code class=\"one-hl one-hl-inline\">%s</code>"
          (lv-escape (org-element-property :value code))))

(defun lv-ox-verbatim (verbatim _contents _info)
  "Transcode VERBATIM from Org to HTML."
  (format "<code class=\"one-hl one-hl-inline\">%s</code>"
          (lv-escape (org-element-property :value verbatim))))

(defun lv-ox-plain-list (plain-list contents _info)
  "Transcode a PLAIN-LIST element from Org to HTML with CONTENTS."
  (let ((type (pcase (org-element-property :type plain-list)
                (`ordered "ol")
                (`unordered "ul")
                (other (error "`lv' doesn't support list type: %s" other)))))
    (format "<%s>%s</%s>" type contents type)))

(defun lv-ox-item (_item contents _info)
  "Transcode an ITEM element from Org to HTML with CONTENTS."
  (format "<li>%s</li>" contents))

(defun lv-ox-table (_table contents _info)
  "Transcode a TABLE element from Org to HTML with CONTENTS."
  (format "<table>%s</table>" contents))

(defun lv-ox-table-row (table-row contents _info)
  "Transcode a TABLE-ROW element from Org to HTML with CONTENTS."
  (if (eq 'rule (org-element-property :type table-row))
      ""
    (format "<tr>%s</tr>" contents)))

(defun lv-ox-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to HTML with CONTENTS."
  (let* ((row (org-export-get-parent table-cell))
         (table (org-export-get-parent-table table-cell))
         (has-header (org-export-table-has-header-p table info))
         (row-number (org-export-table-row-number row info))
         (tag (if (and has-header (= row-number 0)) "th" "td")))
    (format "<%s>%s</%s>" tag (or contents "") tag)))

(defun lv-ox-no-subscript (_subscript contents _info)
  "Transcode a SUBSCRIPT object from Org to HTML with CONTENTS."
  (concat "_" contents))

(defun lv-ox-no-superscript (_superscript contents _info)
  "Transcode a SUPERSCRIPT object from Org to HTML with CONTENTS."
  (concat "^" contents))

(defun lv-ox-fontify-code (code lang)
  "Colour CODE with htmlize for language LANG (a string, or nil)."
  (when code
    (let* ((lang (or (assoc-default lang org-src-lang-modes) lang))
           (lang-mode (and lang (intern (format "%s-mode" lang)))))
      (if (functionp lang-mode)
          (let ((code
                 (let ((inhibit-read-only t))
                   (with-temp-buffer
                     (funcall lang-mode)
                     (insert code)
                     (font-lock-ensure)
                     (org-src-mode)
                     (set-buffer-modified-p nil)
                     (let* ((htmlize-output-type 'css)
                            (htmlize-css-name-prefix "one-hl-")
                            (htmlbuf (htmlize-buffer)))
                       (unwind-protect
                           (with-current-buffer htmlbuf
                             (buffer-substring
                              (plist-get htmlize-buffer-places 'content-start)
                              (plist-get htmlize-buffer-places 'content-end)))
                         (kill-buffer htmlbuf)))))))
            (if-let ((beg (and (string-match "\\`<pre[^>]*>\n?" code) (match-end 0)))
                     (end (string-match "</pre>\\'" code)))
                (substring code beg end)
              code))
        (lv-escape code)))))

(defun lv-ox-htmlize (code lang &optional is-results-p)
  "Return CODE htmlized in language LANG as an HTML block.
If IS-RESULTS-P is non-nil the code is a babel results block."
  (let ((class (if is-results-p "one-hl one-hl-results" "one-hl one-hl-block")))
    (format "<pre><code class=\"%s\">%s</code></pre>"
            class
            (replace-regexp-in-string
             "<span class=\"one-hl-default\">\\([^<]*\\)</span>"
             "\\1"
             (if (null lang)
                 (lv-escape (or code ""))
               (lv-ox-fontify-code code lang))))))

(defun lv-ox-src-block (src-block _contents _info)
  "Return SRC-BLOCK element htmlized."
  (let ((code (car (org-export-unravel-code src-block)))
        (lang (org-element-property :language src-block))
        (is-results-p (org-element-property :results src-block)))
    (lv-ox-htmlize code lang is-results-p)))

(defun lv-ox-example-block (example-block _contents _info)
  "Return EXAMPLE-BLOCK element htmlized."
  (let ((code (car (org-export-unravel-code example-block)))
        (is-results-p (org-element-property :results example-block)))
    (lv-ox-htmlize code "text" is-results-p)))

(defun lv-ox-fixed-width (fixed-width _contents _info)
  "Return FIXED-WIDTH element htmlized."
  (let ((code (car (org-export-unravel-code fixed-width)))
        (is-results-p (org-element-property :results fixed-width)))
    (lv-ox-htmlize code "text" is-results-p)))

(defun lv-ox-quote-block (_quote-block contents _info)
  "Transcode a QUOTE-BLOCK element from Org to HTML with CONTENTS."
  (format "<blockquote class=\"one-blockquote\">%s</blockquote>" contents))

(defun lv-ox-export-block (export-block _contents _info)
  "Transcode an EXPORT-BLOCK from Org to HTML.
Emit the block verbatim when its type is HTML (used for Mermaid diagrams)."
  (when (string= (org-element-property :type export-block) "HTML")
    (org-remove-indentation
     (org-element-property :value export-block))))

;;;; links

(define-error 'lv-link-broken "Unable to resolve link")

(defvar lv-ox-link-image-extensions
  (format "\\.%s\\'"
          (regexp-opt
           '("webp" "avif" "png" "jpeg" "jpg" "gif"
             "tiff" "tif" "xbm" "xpm" "pbm" "pgm" "ppm")
           t))
  "Regexp matching image extensions, see `lv-ox-link'.")

(defun lv-ox-link (link desc info)
  "Transcode a LINK object from Org to HTML.
DESC is the description part of the link, INFO the export plist.
Images and links honour the `:class' attribute set via `#+ATTR_HTML'."
  (let* ((type (org-element-property :type link))
         (path (org-element-property :path link))
         (raw-link (org-element-property :raw-link link))
         (custom-type-link
          (let ((export-func (org-link-get-parameter type :export)))
            (and (functionp export-func)
                 (funcall export-func path desc 'lv info))))
         (href (cond
                ((string= type "custom-id") path)
                ((string= type "fuzzy")
                 (let ((beg (org-element-property :begin link)))
                   (signal 'lv-link-broken
                           `(,raw-link
                             "fuzzy links not supported"
                             ,(format "goto-char: %s" beg)))))
                ((string= type "file")
                 (or
                  (and (string-match "\\`\\./\\(assets\\|public\\)" path)
                       (replace-match "" nil nil path))
                  (let ((beg (org-element-property :begin link)))
                    (signal 'lv-link-broken
                            `(,raw-link ,(format "goto-char: %s" beg))))))
                (t raw-link)))
         (class (if-let ((parent (org-export-get-parent-element link))
                         (class (plist-get (org-export-read-attribute :attr_html parent)
                                           :class)))
                    (concat " class=\"" class "\" ")
                  " ")))
    (or custom-type-link
        (and
         (string-match lv-ox-link-image-extensions path)
         (format "<p><img%ssrc=\"%s\" alt=\"%s\" /></p>"
                 class href (or (org-string-nw-p desc) href)))
        (format "<a%shref=\"%s\">%s</a>"
                class href (or (org-string-nw-p desc) href)))))

(org-export-define-backend 'lv
  '((headline . lv-ox-headline)
    (section . lv-ox-section)
    (paragraph . lv-ox-paragraph)
    (plain-text . lv-ox-plain-text)
    (bold . lv-ox-bold)
    (italic . lv-ox-italic)
    (strike-through . lv-ox-strike-through)
    (underline . lv-ox-underline)
    (code . lv-ox-code)
    (verbatim . lv-ox-verbatim)
    (subscript . lv-ox-no-subscript)
    (superscript . lv-ox-no-superscript)
    (plain-list . lv-ox-plain-list)
    (item . lv-ox-item)
    (src-block . lv-ox-src-block)
    (example-block . lv-ox-example-block)
    (fixed-width . lv-ox-fixed-width)
    (quote-block . lv-ox-quote-block)
    (export-block . lv-ox-export-block)
    (table . lv-ox-table)
    (table-row . lv-ox-table-row)
    (table-cell . lv-ox-table-cell)
    (link . lv-ox-link)))

;;; Internal ids (for stable per-page heading ids)

(defun lv-internal-id (headline)
  "Return an id string for HEADLINE.
Built from the CUSTOM_ID fragment when present, otherwise random."
  (let ((custom-id (org-element-property :CUSTOM_ID headline)))
    (or (and custom-id
             (string-match "\\`\\(?:[^#]+\\S-*\\)#\\(.+\\)" custom-id)
             (match-string-no-properties 1 custom-id))
        (format "one-%x" (random #x10000000000)))))

(defun lv-parse-buffer ()
  "Parse the current Org buffer, adding `:one-internal-id' to each headline."
  (let ((tree (org-element-parse-buffer)))
    (org-element-map tree 'headline
      (lambda (elt)
        (org-element-put-property elt :one-internal-id (lv-internal-id elt))))
    tree))

;;; Layout

(defun lv-make-title (title)
  "Return the document <title> for page TITLE."
  (concat (when (and title (not (string-empty-p title))) (concat title " | "))
          "Django LiveView"))

(defun lv-render-layout (title description navigator-active tree-content)
  "Render the full HTML page shell around TREE-CONTENT.
TITLE, DESCRIPTION and NAVIGATOR-ACTIVE come from the page properties."
  (let ((full-title (lv-make-title title))
        (active "nav-main--active"))
    (jack-html
     "<!DOCTYPE html>"
     `(:html (@ :lang "en")
             (:head
              (:meta (@ :charset "utf-8"))
              (:link (@ :rel "icon" :type "image/png" :href "/img/favicon.png"))
              (:meta (@ :name "viewport" :content "width=device-width,initial-scale=1.0, shrink-to-fit=no"))
              (:meta (@ :name "author" :content "Andros Fenollosa"))
              (:meta (@ :name "generator" :content "Org Publish"))
              (:title ,full-title)
              (:meta (@ :name "description" :content ,description))
              (:meta (@ :name "og:image" :content "https://django-liveview.andros.dev/img/og-image.webp"))
              (:link (@ :rel "preconnect" :href "https://fonts.googleapis.com"))
              (:link (@ :rel "preconnect" :href "https://fonts.gstatic.com" :crossorigin t))
              (:link (@ :rel "stylesheet" :href "https://fonts.googleapis.com/css2?family=Fraunces:opsz,wght@9..144,600;9..144,700&family=IBM+Plex+Mono:wght@400;500;600&family=IBM+Plex+Sans:wght@400;500;600;700&display=swap"))
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
                   (:a.nav-main__link (@ :href "/docs/install/" :class ,(when (string= "docs" navigator-active) active)) "Docs"))
                  (:li.nav-main__item
                   (:a.nav-main__link (@ :href "/quick-start/" :class ,(when (string= "tutorial" navigator-active) active)) "Quick start"))
                  (:li.nav-main__item
                   (:a.nav-main__link (@ :href "/books/" :class ,(when (string= "books" navigator-active) active)) "Books"))
                  (:li.nav-main__item
                   (:a.nav-main__link.nav-main__link--source (@ :href "https://github.com/Django-LiveView/liveview" :target "_blank") "Source code"))))))
              (:div (@ :id "main-content" :tabindex "-1")
                    ,tree-content)
              (:footer.footer
               (:div.container
                (:ul.footer_nav
                 (:li (:i (@ :aria-label "bug") "🪲") " Bugs: " (:a.link (@ :href "https://github.com/Django-LiveView/docs/tree/main/content/docs" :target "_blank") "Documentation"))
                 (:li (:i (@ :aria-label "chat") "🐘") " Follow me: " (:a.link (@ :href "https://activity.andros.dev/@andros" :target "_blank") "ActivityPub/Fediverse "))
                 (:li (:span (@ :aria-hidden "true") "💰 ") " Support the project: " (:a.link (@ :href "https://liberapay.com/androsfenollosa/" :target "_blank") "Liberapay")))
                (:p "Created with " (:i (@ :aria-label "love") "❤️") " by " (:a.link (@ :href "https://andros.dev/" :target "_blank") "Andros Fenollosa") " · 🐍 " ,(format-time-string "%Y"))))
              (:script (@ :type "text/javascript") "(function() {var headingMap = {'Basic': 'basic', 'Intermediate': 'intermediate', 'Advanced': 'advanced', 'UI Features': 'ui-features', 'System Features': 'system-features', 'Data Handling': 'data-handling'}; document.querySelectorAll('h3').forEach(function(h3) {var text = h3.textContent.trim(); if (headingMap[text]) {h3.id = headingMap[text];}});})();")
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
});"))))))

(defun lv-export-body (page-tree)
  "Export the contents of PAGE-TREE with the `lv' backend."
  (org-export-data-with-backend (org-element-contents page-tree) 'lv nil))

(defun one-custom-default-page (page-tree)
  "Render a plain content page (Books, Source code)."
  (lv-render-layout
   (org-element-property :raw-value page-tree)
   (org-element-property :DESCRIPTION page-tree)
   (org-element-property :NAVIGATOR-ACTIVE page-tree)
   (jack-html `(:main.main
                (:section
                 (:div.container ,(lv-export-body page-tree)))))))

(defun one-custom-default-home (page-tree)
  "Render the home page with its hero."
  (lv-render-layout
   (org-element-property :TITLE page-tree)
   (org-element-property :DESCRIPTION page-tree)
   (org-element-property :NAVIGATOR-ACTIVE page-tree)
   (jack-html `(:main.main
                (:section.hero
                 (:div.container
                  (:hgroup.hero__hgroup
                   (:h1.hero__title "Django LiveView")
                   (:h2.hero__subtitle "Build real-time apps: " (:strong "write Python, not " (:s.hero__strike "JavaScript")))
                   (:img.image.hero__logo (@ :alt "pet" :src "img/pet.webp")))))
                (:section.home
                 (:div.container ,(lv-export-body page-tree)))))))

(defun one-custom-default-doc (page-tree)
  "Render a documentation page with the docs sidebar."
  (lv-render-layout
   (org-element-property :raw-value page-tree)
   (org-element-property :DESCRIPTION page-tree)
   (org-element-property :NAVIGATOR-ACTIVE page-tree)
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
                    (:a.nav-docs__link (@ :href "/docs/faq/") "FAQ")))))
                (:main.main.main--docs
                 ,(lv-export-body page-tree))))))

(defun one-custom-default-tutorials (page-tree)
  "Render the tutorial page."
  (lv-render-layout
   (org-element-property :raw-value page-tree)
   (org-element-property :DESCRIPTION page-tree)
   (org-element-property :NAVIGATOR-ACTIVE page-tree)
   (jack-html `(:main.main
                (:section.tutorials
                 (:div.container ,(lv-export-body page-tree)))))))

;;; Publishing

(defun lv-publish-to-html (_plist filename pub-dir)
  "Publish the Org FILENAME to PUB-DIR/index.html using its page layout."
  (let* ((visiting (find-buffer-visiting filename))
         (work (or visiting (find-file-noselect filename))))
    (unwind-protect
        (with-current-buffer work
          (org-with-wide-buffer
           (let* ((tree (lv-parse-buffer))
                  (page (org-element-map tree 'headline
                          (lambda (h)
                            (and (= 1 (org-element-property :level h))
                                 (org-element-property :ONE h)
                                 (org-element-property :CUSTOM_ID h)
                                 h))
                          nil t)))
             (unless page (error "No page headline in %s" filename))
             (let* ((render (intern (org-element-property :ONE page)))
                    (path (org-element-property :CUSTOM_ID page))
                    (html (funcall render page))
                    (out (expand-file-name "index.html" pub-dir)))
               (push path lv--sitemap-paths)
               (make-directory pub-dir t)
               (with-temp-file out (insert html))
               (message "Built page `%s'" path)))))
      (unless visiting (kill-buffer work)))))

(defun lv-write-sitemap ()
  "Write public/sitemap.txt from the published page paths."
  (let ((file (expand-file-name "public/sitemap.txt" lv-root)))
    (with-temp-file file
      (insert (mapconcat (lambda (p) (concat "https://" lv-domain p))
                         (sort (copy-sequence lv--sitemap-paths) #'string<)
                         "\n")
              "\n"))))

(defun lv-project-alist (root)
  "Return the `org-publish-project-alist' for the project at ROOT."
  (let ((content (expand-file-name "content" root))
        (public (expand-file-name "public" root))
        (assets (expand-file-name "assets" root)))
    `(("lv-pages"
       :base-directory ,content
       :base-extension "org"
       :recursive t
       :publishing-directory ,public
       :publishing-function lv-publish-to-html)
      ("lv-assets"
       :base-directory ,assets
       :base-extension "css\\|js\\|png\\|jpg\\|jpeg\\|gif\\|webp\\|avif\\|svg\\|ico\\|txt\\|xml\\|json\\|woff\\|woff2\\|ttf\\|eot"
       :recursive t
       :publishing-directory ,public
       :publishing-function org-publish-attachment)
      ("lv" :components ("lv-assets" "lv-pages")))))

(defun lv-build ()
  "Clean and rebuild the whole site under public/."
  (setq lv-root (file-name-as-directory (expand-file-name lv-root)))
  (let ((public (expand-file-name "public" lv-root)))
    (setq lv--sitemap-paths nil
          org-publish-use-timestamps-flag nil
          org-export-with-section-numbers nil
          org-export-with-toc nil
          org-export-with-smart-quotes nil
          org-confirm-babel-evaluate nil
          make-backup-files nil)
    (when (file-exists-p public) (delete-directory public t))
    (setq org-publish-project-alist (lv-project-alist lv-root))
    (org-publish "lv" t)
    (lv-write-sitemap)
    (message "Build complete: %d pages." (length lv--sitemap-paths))))

(provide 'publish)
;;; publish.el ends here
