(require 'ert)
(when (require 'undercover nil t)
  (undercover "web-beautify.el"))

(require 'web-beautify)
