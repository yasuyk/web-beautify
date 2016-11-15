;;; web-beautify.el --- Format HTML, CSS and JavaScript/JSON

;; Copyright (C) 2013-2016 Yasuyuki Oka and web-beautify contributors

;; Author: Yasuyuki Oka <yasuyk@gmail.com>
;; Version: 0.3.2
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
   "%s not found. Install it with `npm -g install js-beautify`."
   program))

(defun web-beautify-format-error-message (buffer-name)
  "Construct a format error message with BUFFER-NAME."
  (format
   "Could not apply web-beautify. See %s to for details."
   buffer-name))

(defun web-beautify-get-shell-command (program)
  "Join PROGRAM with the constant js-beautify args."
  (mapconcat 'identity (append (list program) web-beautify-args) " "))

(declare-function web-mode-reload "ext:web-mode")
(declare-function js2-mode "ext:js2-mode")

(defun web-beautify-reload ()
  "Reload mode to activate faces."
  (deactivate-mark)
  (cond ((eq major-mode 'web-mode)
         (web-mode-reload))
        ((eq major-mode 'js2-mode)
         (js2-mode))))

(defun web-beautify-format-region (program beginning end)
  "By PROGRAM, format each line in the BEGINNING .. END region."
  ;; Check that js-beautify is installed.
  (if (executable-find program)
      (let* ((output-buffer-name "*Web Beautify Errors*")
             (output-buffer (get-buffer-create output-buffer-name))
             ;; Stash the previous point/window positions so they can be
             ;; reclaimed after the buffer is replaced. Otherwise there is a
             ;; disturbing "jump" to vertically-center point after being
             ;; momentarily bounced to the top of the file.
             (previous-point (point))
             (previous-window-start (window-start))
             (shell-command (web-beautify-get-shell-command program)))
        ;; Run the command.
        (if (zerop (shell-command-on-region beginning end shell-command (current-buffer) t output-buffer t))
            (progn
              ;; Reclaim position for a smooth transition.
              (goto-char previous-point)
              (set-window-start nil previous-window-start)
              (message "Applied web-beautify.")
              (web-beautify-reload)
              (kill-buffer output-buffer))
          ;; Unfortunately an error causes the buffer to be replaced with
          ;; emptiness... so undo that. Kind of an ugly hack. But a
          ;; properly-configured web-beautify shouldn't encounter this much, if
          ;; ever.
          (undo)
          (message (web-beautify-format-error-message output-buffer-name))))
    (message (web-beautify-command-not-found-message program))))

(defun web-beautify-format-buffer (program)
  "By PROGRAM, format current buffer with EXTENSTION."
  (web-beautify-format-region program (point-min) (point-max)))

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
  (web-beautify-format-buffer web-beautify-html-program))

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
  (web-beautify-format-buffer web-beautify-css-program))

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
  (web-beautify-format-buffer web-beautify-js-program))


(provide 'web-beautify)

;; Local Variables:
;; coding: utf-8
;; eval: (checkdoc-minor-mode 1)
;; End:

;;; web-beautify.el ends here
