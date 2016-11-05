(require 'cask)
(cask-initialize ".")
(setq byte-compile-error-on-warn t)
(require 'package)
(package-install-file "web-beautify.el")


(defun should-autoloadp (c)
  (let ((s (symbol-function c)))
    (unless (autoloadp s)
      (message "%s is not autoload" c)
      (kill-emacs 1))))

(should-autoloadp 'web-beautify-html)
(should-autoloadp 'web-beautify-css)
(should-autoloadp 'web-beautify-js)
(should-autoloadp 'web-beautify-html-buffer)
(should-autoloadp 'web-beautify-css-buffer)
(should-autoloadp 'web-beautify-js-buffer)
