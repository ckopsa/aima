(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun list-freq (sequence)
  (let ((frequencies (make-hash-table :test 'equal)))
    (dolist (e sequence) (incf (gethash e frequencies 0)))
    (loop for key being the hash-keys of frequencies
          collect (cons key (gethash key frequencies)))))

(defun cons-cdr-sort (sequence)
  (-sort (lambda (rel lel) (< (cdr lel) (cdr rel))) sequence))

(defun unigram (corpus)
  (let* ((corpus-gram (split-string (downcase corpus) " "))
         (corpus-freq (list-freq corpus-gram))
         (corpus-freq-sort (cons-cdr-sort corpus-freq)))
    corpus-freq-sort))

(defun unigram-to-bigram (unigram)
  (let* ((size (1- (length unigram))))
    (loop for i from 0 to size
          collect (concat (nth i unigram) " " (nth (1+ i) unigram)))))

(defun unigram-to-trigram (unigram)
  (let* ((size (- (length unigram) 2)))
    (loop for i from 0 to size
          collect (concat (nth i unigram) " " (nth (1+ i) unigram) " " (nth (1+ (1+ i)) unigram)))))

(defun bigram (corpus)
  (let* ((corpus-gram (unigram-to-bigram (split-string (downcase corpus) " ")))
         (corpus-freq (list-freq corpus-gram))
         (corpus-freq-sort (cons-cdr-sort corpus-freq)))
    corpus-freq-sort))

(defun trigram (corpus)
  (let* ((corpus-gram (unigram-to-trigram (split-string (downcase corpus) " ")))
         (corpus-freq (list-freq corpus-gram))
         (corpus-freq-sort (cons-cdr-sort corpus-freq)))
    corpus-freq-sort))

(setq hf-unigram (unigram (get-string-from-file "~/huckfinn.txt")))

(setq hf-bigram (bigram (get-string-from-file "~/huckfinn.txt")))

(setq hf-trigram (trigram (get-string-from-file "~/huckfinn.txt")))

(setq bm-unigram (unigram (get-string-from-file "~/bom.txt")))
(setq bm-bigram (bigram (get-string-from-file "~/bom.txt")))
(setq bm-trigram (trigram (get-string-from-file "~/bom.txt")))

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
    (mapconcat (lambda (el) el) (mapcar* (lambda (uni-word bi-word tri-word) (concat uni-word " " bi-word " " tri-word)) uni-words bi-words tri-words) " ")
    ))

(generate-text hf-unigram hf-bigram hf-trigram)

(length bm-unigram)
