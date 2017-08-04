(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun get-string-from-buffer (buffer)
  "Return filePath's file content."
  (with-current-buffer buffer
    (buffer-string)))

(require 'url)
(defun url-get-tree (url)
  "HTTP GET to URL RETURNS TREE."
  (with-current-buffer
      (url-retrieve-synchronously url)
    (libxml-parse-html-region (point-min) (point-max) nil t)))

(defun url-get-readable-tree (url)
  (let (
        (url-tree (url-get-tree url))
        )
    url-tree
    ))

(require 'dom)
(defun clean-html (s)
  (let* (
         (s-no-newline (replace-regexp-in-string "\n+" " " s))
         (s-no-bar (replace-regexp-in-string "|+" "" s-no-newline))
         (s-no-omnispace (replace-regexp-in-string "\s+" " " s-no-bar))
         )
    s-no-omnispace
    ))

(defun list-freq (sequence)
  (let ((frequencies (make-hash-table :test 'equal)))
    (dolist (e sequence) (incf (gethash e frequencies 0)))
    (loop for key being the hash-keys of frequencies
          collecting (cons key (gethash key frequencies)) into freqs
          finally return (append freqs (list (cons "<unk>" (length freqs))))
          )))

(defun assoc-cdr (assoc-list) (mapcar (lambda (el) (cdr el)) assoc-list))

(defun assoc-car (assoc-list) (mapcar (lambda (el) (car el)) assoc-list))

(defun cons-cdr-sort (sequence)
  (-sort (lambda (rel lel) (< (cdr lel) (cdr rel))) sequence))

(defun unigram-to-n-gram (unigram n)
  (let* ((size (- (length unigram) (1- n))))
    (loop for i from 0 to size
          collect (mapconcat (lambda (el) el) (loop for j from 0 to (1- n) collect (nth (+ i j) unigram)) " "))))

(defun n-gram (corpus n)
  (let* ((unigram (split-string (downcase corpus) " "))
         (corpus-gram (if (= n 1) unigram (unigram-to-n-gram unigram n)))
         (corpus-freq (list-freq corpus-gram))
         (corpus-freq-sort (cons-cdr-sort corpus-freq)))
    corpus-freq-sort))

(defun perplexity (language-model corpus)
  (let* (
         (lm-size (float (apply '+ (assoc-cdr language-model))))
         (probabilities (mapcar (lambda (corpus-unit) (/ (alist-get corpus-unit language-model) lm-size)) corpus))
         (total-probabilities (apply '+ probabilities))
         )
    (expt total-probabilities (/ (float -1) (length corpus)))
    ))


(defun generate-text (unigram bigram trigram)
  (let* ((assoc-cdr (lambda (assoc-list) (mapcar (lambda (el) (cdr el)) assoc-list)))
         (assoc-car (lambda (assoc-list) (mapcar (lambda (el) (car el)) assoc-list)))
         (word-at-percent (lambda (assoc-list percent)
                            (loop for el in assoc-list
                                  until (< percent assoc-sum)
                                  summing (cdr el) into assoc-sum
                                  finally return (car el))))
         (uni-size (apply '+ (funcall assoc-cdr unigram)))
         (bi-size (apply '+ (funcall assoc-cdr bigram)))
         (tri-size (apply '+ (funcall assoc-cdr trigram)))
         (uni-percent (mapcar (lambda (freq) (/ (float freq) uni-size)) (funcall assoc-cdr unigram)))
         (bi-percent (mapcar (lambda (freq) (/ (float freq) bi-size)) (funcall assoc-cdr bigram)))
         (tri-percent (mapcar (lambda (freq) (/ (float freq) tri-size)) (funcall assoc-cdr trigram)))
         (uni-pair (pairlis (funcall assoc-car unigram) uni-percent))
         (bi-pair (pairlis (funcall assoc-car bigram) bi-percent))
         (tri-pair (pairlis (funcall assoc-car trigram) tri-percent))
         (uni-words (loop for i from 0 to 33 collect (funcall word-at-percent uni-pair (cl-random 1.0))))
         (bi-words (loop for i from 0 to 33 collect (funcall word-at-percent bi-pair (cl-random 1.0))))
         (tri-words (loop for i from 0 to 33 collect (funcall word-at-percent tri-pair (cl-random 1.0))))
         )
    ;;(mapconcat (lambda (el) el) (mapcar* (lambda (uni-word bi-word tri-word) (concat uni-word " " bi-word " " tri-word)) uni-words bi-words tri-words) " ")
    ;;(mapcar (lambda (el) el) (mapcar* (lambda (uni-word bi-word tri-word) (concat uni-word " " bi-word " " tri-word)) uni-words bi-words tri-words))
    uni-words
    ))

;; (setq test-unigram (n-gram (get-string-from-buffer "*scratch*") 1))
;; (setq test-bigram  (n-gram (get-string-from-buffer "*scratch*") 2))
;; (setq test-trigram (n-gram (get-string-from-buffer "*scratch*") 3))
;; (setq test-quadgram (n-gram (get-string-from-buffer "*scratch*") 4))
;; (perplexity (generate-text test-unigram test-bigram test-trigram) test-unigram)

(setq test-url
      "http://www.vancouversun.com/news/Vancouver+councillor+bring+shark+fins+city+hall/7259812/story.html"
)


(defun clean-html (s)
  (let* (
         (s-no-newline (replace-regexp-in-string "\n+" " " s))
         (s-no-bar (replace-regexp-in-string "[ |]+" "" s-no-newline))
         (s-no-inline (replace-regexp-in-string "{.*}" "" s-no-bar))
         (s-no-symbol (replace-regexp-in-string "" "" s-no-inline))
      ;;   (s-no-non-alphanum (replace-regexp-in-string "[^a-zA-Z0-9 -]" "" s-no-newline))
         (s-no-tab (replace-regexp-in-string "\t+" " " s-no-symbol))
         (s-no-omnispace (replace-regexp-in-string "[ ]+" " " s-no-tab))
         )
    s-no-omnispace
    ))

(defun extract-content-tag (tag dom-tree)
  (let* (
         (tag-content (mapcar (lambda (tag) (dom-text tag)) (dom-by-tag dom-tree tag)))
         (clean-content (cl-remove-if
                         (lambda (child) (or (string-match "\\`[\n\r\t |  ]*\\'" child)
                                             (equal child "")
                                             (string-match "HTTP\/\d\.\d \d{3}.*" child)
                                             ))
                         tag-content))
         )
    tag-content
    ))

(defun extract-content-html (url)
  (let* (
         (dom-tree (url-get-readable-tree url))
         (tags (list 'title 'span 'p))
         (content (apply 'append (mapcar (lambda (tag) (extract-content-tag tag dom-tree)) tags)))
         (cleaned-content (mapconcat (lambda (s) (clean-html s)) content " "))
         )
    cleaned-content
    ))

(setq test-content (extract-content-html test-url))

(setq test-tree (url-get-readable-tree test-url))

;;(setq test-trees (mapconcat (lambda (url) (extract-content-html url)) urls " "))

(extract-content-tag 'p testee)

(setq test-content (extract-content-html test-url))


(setq test-unigram (n-gram test-trees 1))
(setq test-bigram  (n-gram test-trees 2))
(setq test-trigram (n-gram test-trees 3))
(setq test-quadgram (n-gram test-trees 4))
(perplexity test-unigram (generate-text test-unigram test-bigram test-trigram))

(setq testee (dom-remove-by-tag test-tree 'script))
(equal 'html (dom-tag testee))
(defun dom-remove-by-tag (dom tag)
  (if (stringp dom) dom
  (let* ((d-tag (dom-tag dom))
         (attr (dom-attributes dom))
         (children (loop for child in (dom-children dom)
                         if (or (stringp child) (not (equal (dom-tag child) tag)))
                         collect (dom-remove-by-tag child tag)
                         ))
         )
    (cons d-tag (cons attr children)))))
(setq dom-adf (dom-test test-tree 'script))

(defun dom-exclude-tag (dom tag)
  "Return elements in DOM that is of type TAG.
A name is a symbol like `td'."
  (let ((matches (cl-loop for child in (dom-children dom)
                          for matches = (and (not (stringp child))
                                             (dom-exclude-tag child tag))
                          when matches
                          append matches)))
    (if (not (equal (dom-tag dom) tag))
        (cons dom matches)
      matches)))

