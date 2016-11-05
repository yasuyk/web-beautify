(add-to-list 'load-path default-directory)
(defvar web-beautify-el "web-beautify.el")
;; test byte-comple
(mapc #'byte-compile-file `(,web-beautify-el))

;; test checkdoc
(with-current-buffer (find-file-noselect web-beautify-el)
    (let ((checkdoc-diagnostic-buffer "*warn*"))
      (checkdoc-current-buffer t)))
