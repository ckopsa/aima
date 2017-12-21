(require 'dom)

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
  (let* ((lm-size (float (apply '+ (assoc-cdr language-model))))
         (probabilities (map 'vector (lambda (corpus-unit) (/ (alist-get corpus-unit language-model) lm-size)) corpus))
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
    uni-words
    ))

(defun eww-score-readability (node)
  (let ((score -1))
    (cond
     ((memq (dom-tag node) '(script head comment))
      (setq score -2))
     ((eq (dom-tag node) 'meta)
      (setq score -1))
     ((eq (dom-tag node) 'img)
      (setq score 2))
     ((eq (dom-tag node) 'a)
      (setq score (- (length (split-string (dom-text node))))))
     (t
      (dolist (elem (dom-children node))
        (if (stringp elem)
            (setq score (+ score (length (split-string elem))))
          (setq score (+ score
                         (or (cdr (assoc :eww-readability-score (cdr elem)))
                             (eww-score-readability elem))))))))
    ;; Cache the score of the node to avoid recomputing all the time.
    (dom-set-attribute node :eww-readability-score score)
    score))

(defun eww-highest-readability (node)
  (let ((result node)
        highest)
    (dolist (elem (dom-non-text-children node))
      (when (> (or (dom-attr
                    (setq highest (eww-highest-readability elem))
                    :eww-readability-score)
                   most-negative-fixnum)
               (or (dom-attr result :eww-readability-score)
                   most-negative-fixnum))
        (setq result highest)))
    result))

(defun get-readable-text (url)
  (let* ((dom-root-node (url-get-tree url))
         (read-score (eww-score-readability dom-root-node))
         (base url))
    (list read-score base)
    (with-temp-buffer
      (shr-insert-document dom-root-node)
      (replace-regexp-in-string "\s\s+" "" (replace-regexp-in-string "[^A-Za-z0-9\']+" " " (buffer-string)))
      )))

(setq test-unigram (n-gram read-test 1))
(setq test-bigram  (n-gram read-test 2))
(setq test-trigram (n-gram read-test 3))
(setq test-quadgram (n-gram read-test 4))
(generate-text test-unigram test-bigram test-trigram)
(perplexity test-unigram read-test)

(setq test-url "http://www.vancouversun.com/news/Vancouver+councillor+bring+shark+fins+city+hall/7259812/story.html")
(setq read-test (substring-no-properties (get-readable-text test-url)))

(setq test (do-stuff test-url))

(float (apply '+ (assoc-cdr test-unigram)))
(mapcar (lambda (corpus-unit) (/ (alist-get (downcase corpus-unit) test-unigram) 1847.0)) (split-string read-test))
(alist-get "the" test-unigram)
(mapcar (lambda (corpus-unit) corpus-unit) (split-string read-test))
(defun perplexity (language-model corpus)
  (let* ((lm-size (float (apply '+ (assoc-cdr language-model))))
         (probabilities (mapcar (lambda (corpus-unit) (/ (alist-get corpus-unit language-model) lm-size)) corpus))
         (total-probabilities (apply '+ probabilities))
         )
    (expt total-probabilities (/ (float -1) (length corpus)))
    ))
(mapcar (lambda (corpus-unit) (/ (alist-get corpus-unit language-model) lm-size)) read-test)
