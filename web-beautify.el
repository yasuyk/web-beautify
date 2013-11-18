;;; web-beautify.el --- Format HTML, CSS and JavaScript/JSON by js-beautify

;; Copyright (C) 2013 Yasuyuki Oka <yasuyk@gmail.com>

;; Author: Yasuyuki Oka <yasuyk@gmail.com>
;; Version: 0.1
;; URL: https://github.com/yasuyk/web-beautify

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Add the following to your Emacs init file.

;;     (require 'web-beautify) ;; Not necessary if using ELPA package
;;     (eval-after-load 'js2-mode
;;       '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
;;     (eval-after-load 'json-mode
;;       '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
;;     (eval-after-load 'sgml-mode
;;       '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))
;;     (eval-after-load 'css-mode
;;       '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))

;; If you want to automatically format before saving a file,
;; add the following hook to your Emacs configuration:

;;     (eval-after-load 'js2-mode
;;       '(add-hook 'js2-mode-hook
;;                  (lambda ()
;;                    (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

;;     (eval-after-load 'json-mode
;;       '(add-hook 'json-mode-hook
;;                  (lambda ()
;;                    (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

;;     (eval-after-load 'sgml-mode
;;       '(add-hook 'html-mode-hook
;;                  (lambda ()
;;                    (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))

;;     (eval-after-load 'css-mode
;;       '(add-hook 'css-mode-hook
;;                  (lambda ()
;;                    (add-hook 'before-save-hook 'web-beautify-css-buffer t t))))

;; For more information, See URL https://github.com/yasuyk/web-beautify.


;;; Code:

(defvar web-beautify-html-program "html-beautify"
  "The executable to use for formatting HTML.")

(defvar web-beautify-css-program "css-beautify"
  "The executable to use for formatting CSS.")

(defvar web-beautify-js-program "js-beautify"
  "The executable to use for formatting JavaScript and JSON.")

(defconst web-beautify-args '("-f" "-"))

(defun web-beautify-command-not-found-message (program)
  "Construct a message about PROGRAM not found."
  (format
   "%s not found. Install it by typing: \"npm -g install js-beautify\" "
   program))

(defun web-beautify-format-error-message (bufname)
  "Construct a format error message with BUFNAME."
  (format
   "Could not apply web-beautify. See %s to check errors for details"
   bufname))

(defun web-beautify-format-region (program beg end)
  "By PROGRAM, format each line in the BEG .. END region."
  (if (executable-find program)
      (save-excursion
        (apply 'call-process-region beg end program t
               (list t nil) t web-beautify-args))
    (message (web-beautify-command-not-found-message program))))

(defun web-beautify-format-buffer (program extenstion)
  "By PROGRAM, format current buffer with EXTENSTION."
    (if (executable-find program)
        (web-beautify-format-buffer-1 program extenstion)
      (message (web-beautify-command-not-found-message program))))

(defun web-beautify-format-buffer-1 (program extenstion)
  "Internal function of `web-beautify-format-buffer'.

By PROGRAM, format current buffer with EXTENSTION."
  (let* ((tmpfile (make-temp-file "web-beautify" nil
                                  (format ".%s" extenstion)))
         (outputbufname (format "*web-beautify-%s*" extenstion))
         (outputbuf (get-buffer-create outputbufname))
         (args (append web-beautify-args (list tmpfile))))
    (unwind-protect
        (progn
          (with-current-buffer outputbuf (erase-buffer))
          (write-region nil nil tmpfile)

          (if (zerop (apply 'call-process program nil outputbuf nil args))
              (let ((p (point)))
                (save-excursion
                  (with-current-buffer (current-buffer)
                    (erase-buffer)
                    (insert-buffer-substring outputbuf)))
                (goto-char p)
                (message "Applied web-beautify")
                (kill-buffer outputbuf))
            (message (web-beautify-format-error-message outputbufname))
            (display-buffer outputbuf)))
      (progn
        (delete-file tmpfile)))))

;;;###autoload
(defun web-beautify-html ()
  "Format region if active, otherwise the current buffer.

Formatting is done according to the html-beautify command."
  (interactive)
  (if (use-region-p)
      (web-beautify-format-region
       web-beautify-html-program
       (region-beginning) (region-end))
    (web-beautify-html-buffer)))

;;;###autoload
(defun web-beautify-html-buffer ()
  "Format the current buffer according to the html-beautify command."
  (web-beautify-format-buffer web-beautify-html-program "html"))

;;;###autoload
(defun web-beautify-css ()
  "Format region if active, otherwise the current buffer.

Formatting is done according to the css-beautify command."
  (interactive)
  (if (use-region-p)
      (web-beautify-format-region
       web-beautify-css-program
    (region-beginning) (region-end))
    (web-beautify-css-buffer)))

;;;###autoload
(defun web-beautify-css-buffer ()
  "Format the current buffer according to the css-beautify command."
  (web-beautify-format-buffer web-beautify-css-program "css"))

;;;###autoload
(defun web-beautify-js ()
  "Format region if active, otherwise the current buffer.

Formatting is done according to the js-beautify command."
  (interactive)
  (if (use-region-p)
      (web-beautify-format-region
       web-beautify-js-program
       (region-beginning) (region-end))
    (web-beautify-js-buffer)))

;;;###autoload
(defun web-beautify-js-buffer ()
  "Format the current buffer according to the js-beautify command."
  (web-beautify-format-buffer web-beautify-js-program "js"))


(provide 'web-beautify)

;; Local Variables:
;; coding: utf-8
;; eval: (checkdoc-minor-mode 1)
;; End:

;;; web-beautify.el ends here
