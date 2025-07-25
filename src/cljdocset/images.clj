(ns cljdocset.images
  (:require
   [babashka.fs :as fs]
   [babashka.http-client :as http]
   [cljdocset.util :as util]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [hickory.core :as hickory]
   [hickory.select :as s])
  (:import
   [java.security MessageDigest]))

(defn find-image-references
  "Extracts all img src URLs from a parsed HTML document.
  Takes a Hickory DOM structure and returns a sequence of URL strings."
  [hickory-dom]
  (let [img-elements (s/select (s/tag :img) hickory-dom)]
    (keep #(get-in % [:attrs :src]) img-elements)))

(defn classify-image-url
  "Categorizes a URL as :remote (http/https), :local (relative path), or :data-uri.
  Returns a map with :type and :url keys."
  [url]
  (cond
    (re-matches #"https?://.*" url)
    {:type :remote :url url}

    (str/starts-with? url "data:")
    {:type :data-uri :url url}

    :else
    {:type :local :url url}))

(defn content-hash
  "Generates a SHA-256 hash of the given byte array.
  Returns a hex string representation of the hash."
  [^bytes content]
  (let [digest (MessageDigest/getInstance "SHA-256")
        hash-bytes (.digest digest content)]
    (str/join (map #(format "%02x" %) hash-bytes))))

(def url-extension-map
  "Map of URL extension sets to normalized extensions"
  {#{".png"} ".png"
   #{".jpg" ".jpeg"} ".jpg"
   #{".gif"} ".gif"
   #{".svg"} ".svg"
   #{".webp"} ".webp"
   #{".ico"} ".ico"})

(def mime-to-extension-map
  "Map of MIME type sets to file extensions"
  {#{"image/png"} ".png"
   #{"image/jpeg" "image/jpg"} ".jpg"
   #{"image/gif"} ".gif"
   #{"image/svg+xml"} ".svg"
   #{"image/webp"} ".webp"
   #{"image/x-icon" "image/vnd.microsoft.icon"} ".ico"})

(defn normalize-content-type
  "Strips parameters from content-type to get the base MIME type.
  Example: 'image/svg+xml;charset=utf-8' -> 'image/svg+xml'"
  [content-type]
  (when content-type
    (-> content-type
        (str/split #";")
        first
        str/trim)))

(defn calculate-relative-images-path
  "Calculates the relative path from an HTML file to the images directory.
  Both paths should be relative to the documents directory."
  [html-file-path documents-dir]
  (let [html-path (fs/path html-file-path)
        html-relative (fs/relativize documents-dir html-path)
        html-parent (fs/parent html-relative)
        depth (if html-parent
                (count (fs/components html-parent))
                0)
        up-dirs (str/join "/" (repeat depth ".."))]
    (if (zero? depth)
      "images/"
      (str up-dirs "/images/"))))

(defn determine-image-extension
  "Determines the file extension for an image based on URL first, then content-type.
  Returns the extension with leading dot (e.g., '.png') or nil if cannot determine."
  [content-type url]
  (let [url-lower (str/lower-case url)
        normalized-content-type (normalize-content-type content-type)
        ext-from-mime (some (fn [[mime-set ext]]
                              (when (contains? mime-set normalized-content-type) ext))
                            mime-to-extension-map)
        ext-from-url (some (fn [[suffix-set ext]]
                             (when (some #(str/ends-with? url-lower %) suffix-set) ext))
                           url-extension-map)
        result (or ext-from-mime ext-from-url)]
    (when (nil? result)
      (util/error "Cannot determine image extension" {:url url :content-type content-type}))
    result))

(defn extract-filename-from-url
  "Extracts the filename portion from a URL, without extension.
  Returns nil if no valid filename found."
  [url]
  (when-let [path (try
                    (-> url
                        (java.net.URL.)
                        (.getPath))
                    (catch Exception _ nil))]
    (when (and path (not (str/blank? path)))
      (let [filename (-> path
                         (str/split #"/")
                         last)]
        ;; Check if filename is not nil (happens when path is just "/")
        (when filename
          ;; Remove extension and any query params
          (let [base-name (-> filename
                              (str/split #"\?")
                              first
                              (str/split #"\.")
                              butlast
                              (#(str/join "." %)))]
            (when (and base-name (not (str/blank? base-name)))
              base-name)))))))

(defn download-image
  "Downloads an image from a URL and returns a map with :content (byte array),
  :content-type, and :success? keys. Implements retry logic for transient failures."
  [url & {:keys [max-retries] :or {max-retries 3}}]
  (letfn [(attempt-download [attempt-num]
            (try
              (let [response (http/get url {:as :bytes :throw false})
                    status (:status response)
                    content-type (get-in response [:headers "content-type"])]
                (cond
                  ;; Success
                  (<= 200 status 299)
                  {:success? true
                   :content (:body response)
                   :content-type content-type}

                  ;; Client error - don't retry
                  (<= 400 status 499)
                  {:success? false
                   :error (str "Client error " status " for URL: " url)}

                  ;; Server error or other - maybe retry
                  :else
                  (if (< attempt-num max-retries)
                    (do
                      (Thread/sleep (* attempt-num 500)) ; Exponential backoff
                      (attempt-download (inc attempt-num)))
                    {:success? false
                     :error (str "Failed after " max-retries " attempts. Status " status " for URL: " url)})))
              (catch Exception e
                (if (< attempt-num max-retries)
                  (do
                    (Thread/sleep (* attempt-num 500))
                    (attempt-download (inc attempt-num)))
                  {:success? false
                   :error (str "Exception downloading " url ": " (.getMessage e))}))))]
    (attempt-download 1)))

(defn save-image-to-docset
  "Saves image content to the docset's images directory with a hash-based filename
  that includes the original filename for debugging.
  Returns just the filename, or nil if extension cannot be determined."
  [{:keys [content url content-type images-dir]}]
  (let [extension (determine-image-extension content-type url)]
    (when extension
      (let [filename (str (content-hash content) extension)
            target-path (fs/path images-dir filename)]
        (fs/create-dirs images-dir)
        (io/copy content (fs/file target-path))
        filename))))

(defn build-image-mapping
  "Builds a mapping from original URLs to local paths for all images in a sequence.
  Takes a sequence of {:url :local-path} maps and returns a map of url->local-path."
  [image-entries]
  (into {} (map (juxt :url :local-path) image-entries)))

(defn rewrite-image-urls
  "Rewrites image URLs in HTML content using the provided mapping.
  Handles various quote styles and preserves the original HTML structure."
  [html-content url-mapping]
  (reduce (fn [html [original-url local-path]]
            ;; Handle three quote patterns: src="url", src='url', and src=url
            (-> html
                (str/replace (str "src=\"" original-url "\"")
                             (str "src=\"" local-path "\""))
                (str/replace (str "src='" original-url "'")
                             (str "src='" local-path "'"))
                ;; Handle unquoted URLs (less common but possible)
                (str/replace (re-pattern (str "src=" (java.util.regex.Pattern/quote original-url) "(?=\\s|>)"))
                             (str "src=\"" local-path "\""))))
          html-content
          url-mapping))

(defn rewrite-navigation-links
  "Rewrites navigation links that have href=\"#\" to point to index.html instead.
  Uses simple string replacement to avoid complex DOM manipulation."
  [html-content]
  (try
    ;; Simple regex replacement for href="#" to href="index.html"
    ;; This handles various quote styles
    (-> html-content
        (str/replace #"href\s*=\s*[\"']#[\"']" "href=\"index.html\"")
        (str/replace #"href\s*=\s*#(?=\s|>)" "href=\"index.html\""))

    (catch Exception e
      (util/error "Error rewriting navigation links:" (.getMessage e))
      ;; Return original content on error
      html-content)))

(defn process-html-file
  "Processes a single HTML file: finds images, downloads remotes, rewrites URLs, and fixes navigation links.
  Returns a map with :content (updated HTML), :images (list of processed images)."
  [{:keys [file-path images-dir documents-dir]}]
  (let [html-content (slurp file-path)
        hickory-dom (-> html-content hickory/parse hickory/as-hickory)
        image-urls (find-image-references hickory-dom)
        classified-urls (map classify-image-url image-urls)

        relative-images-path (calculate-relative-images-path file-path documents-dir)

        remote-images (filter #(= :remote (:type %)) classified-urls)

        processed-images
        (doall
         (for [{:keys [url]} remote-images]
           (let [download-result (download-image url)]
             (if (:success? download-result)
               (let [filename (save-image-to-docset
                               {:content (:content download-result)
                                :url url
                                :content-type (:content-type download-result)
                                :images-dir images-dir})]
                 (if filename
                   {:url url
                    :local-path (str relative-images-path filename)
                    :success? true}
                   {:url url
                    :success? false
                    :error "Cannot determine image extension"}))
               {:url url
                :success? false
                :error (:error download-result)}))))

        successful-images (filter :success? processed-images)
        url-mapping (build-image-mapping successful-images)

        ;; Apply image URL rewriting first, then navigation link rewriting
        updated-html (-> html-content
                         (cond-> (seq url-mapping) (rewrite-image-urls url-mapping))
                         rewrite-navigation-links)]

    {:content updated-html
     :images processed-images}))

(defn download-all-images
  "Main pipeline function that processes all HTML files in the docset to download
  and inline remote images. Updates the context with image processing results."
  [{:keys [paths] :as ctx}]
  (util/info "Processing remote images...")
  (let [documents-dir (:documents-dir paths)
        images-dir (fs/path documents-dir "images")

        ;; Find all HTML files
        html-files (concat
                    (fs/glob documents-dir "*.html")
                    (fs/glob documents-dir "**/*.html"))

        ;; Process each file
        all-results
        (doall
         (for [html-file html-files]
           (let [relative-path (str (fs/relativize documents-dir html-file))
                 _ (util/info (str "Processing images in: " relative-path))
                 result (process-html-file
                         {:file-path (str html-file)
                          :images-dir (str images-dir)
                          :documents-dir documents-dir})]
             ;; Write updated HTML back
             (spit (str html-file) (:content result))

             ;; Return processing summary
             {:file relative-path
              :images (:images result)})))

        ;; Collect statistics
        total-images (reduce + (map #(count (:images %)) all-results))
        successful-images (reduce + (map #(count (filter :success? (:images %))) all-results))
        failed-images (- total-images successful-images)]

    (util/info (format "Processed %d images: %d successful, %d failed"
                       total-images successful-images failed-images))

    ;; Add results to context
    (assoc ctx :images {:total total-images
                        :successful successful-images
                        :failed failed-images
                        :details all-results})))
