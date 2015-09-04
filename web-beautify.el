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

;; Copy of go--goto-line from https://github.com/dominikh/go-mode.el
(defun web-beautify--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

;; Copy of go--delete-whole-line from https://github.com/dominikh/go-mode.el
(defun web-beautify--delete-whole-line (&optional arg)
  "Delete the current line without putting it in the kill-ring."
  ;; Derived from `kill-whole-line'.
  ;; ARG is defined as for that function.
  (setq arg (or arg 1))
  (if (and (> arg 0)
           (eobp)
           (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0)
           (bobp)
           (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (cond ((zerop arg)
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (end-of-visible-line) (point))))
        ((< arg 0)
         (delete-region (progn (end-of-visible-line) (point))
                        (progn (forward-visible-line (1+ arg))
                               (unless (bobp)
                                 (backward-char))
                               (point))))
        (t
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (forward-visible-line arg) (point))))))

;; Copy of go--apply-rcs-patch from https://github.com/dominikh/go-mode.el
(defun web-beautify--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current
buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "invalid rcs patch or internal error in `web-beautify-fixer--apply-rcs-patch`"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (web-beautify--goto-line (- from line-offset))
                (incf line-offset len)
                (web-beautify--delete-whole-line len)))
             (t
              (error "invalid rcs patch or internal error in `web-beautify--apply-rcs-patch`")))))))))

;; Copy of go--kill-error-buffer from https://github.com/dominikh/go-mode.el
(defun web-beautify--kill-error-buffer (errbuf)
  (let ((win (get-buffer-window errbuf)))
    (if win
        (quit-window t win)
      (kill-buffer errbuf))))

(defun web-beautify-format-buffer-1 (program extenstion)
  "Internal function of `web-beautify-format-buffer'.

By PROGRAM, format current buffer with EXTENSTION."
  (let ((tmpfile (make-temp-file "web-beautify" nil
                                 (format ".%s" extenstion)))
        (patchbuf (get-buffer-create (format "*web-beautify-%s*" extenstion)))
        (errbuf (get-buffer-create (format "*web-beautify-error-%s*" extenstion))))

    (save-restriction
      (widen)
      (if errbuf
          (with-current-buffer errbuf
            (setq buffer-read-only nil)
            (erase-buffer)))
      (with-current-buffer patchbuf
        (erase-buffer))

      (write-region nil nil tmpfile)

      ;; We're using errbuf for the mixed stdout and stderr output. This
      ;; is not an issue because -q does not produce any stdout
      ;; output in case of success.
      (when (zerop (apply 'call-process program
                          nil errbuf nil
                          (append web-beautify-args (list "-q" "-r" tmpfile))))
            (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
                (message "Buffer is already beautified")
              (web-beautify--apply-rcs-patch patchbuf)
              (message "Applied web-beautify"))
            (web-beautify--kill-error-buffer errbuf))
      (if errbuf (with-current-buffer errbuf
                   (progn
                     (error "%s" (buffer-string))
                     (web-beautify--kill-error-buffer errbuf))))

      (kill-buffer patchbuf)
      (delete-file tmpfile))))

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
