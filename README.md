# Synopsis

**Web-beautify** is a formatting package of HTML, CSS and JavaScript/JSON for Emacs.
It uses the command-line/node.js javascript formatter from http://jsbeautifier.org/ to format whole html, css, js or json files, or region.

![](https://github.com/yasuyk/web-beautify/raw/master/image/web-beautify.gif)

# Features
* HTML, CSS, and JavaScript/JSON formatting
* all settings are customizable (whitespace, formatting style, etc..) by `.jsbeautifyrc` configuration file.

# Requirements

* `js-beautify` installed by typing: `npm -g install js-beautify`

# Installation

## Manual

Just drop `web-beautify.el`. somewhere in your `load-path`.

```lisp
(add-to-list 'load-path "~/somewhere")
```

## MELPA

If you're an Emacs 24 user or you have a recent version of package.el
you can install `web-beautify.el` from the [MELPA](http://melpa.milkbox.net/) repository.

# Usage

## Basic setup

Add the following to your emacs init file.

    (require 'web-beautify) ;; Not necessary if using ELPA package
    (eval-after-load 'js2-mode
      '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
    ;; Or if you're using 'js-mode' (a.k.a 'javascript-mode')
    (eval-after-load 'js
      '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js))

    (eval-after-load 'json-mode
      '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))

    (eval-after-load 'sgml-mode
      '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))

    (eval-after-load 'css-mode
      '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))

If you want to automatically format before saving a file,
add the following hook to your emacs configuration:

    (eval-after-load 'js2-mode
      '(add-hook 'js2-mode-hook
                 (lambda ()
                   (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

    ;; Or if you're using 'js-mode' (a.k.a 'javascript-mode')
    (eval-after-load 'js
      '(add-hook 'js-mode-hook
                 (lambda ()
                   (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

    (eval-after-load 'json-mode
      '(add-hook 'json-mode-hook
                 (lambda ()
                   (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

    (eval-after-load 'sgml-mode
      '(add-hook 'html-mode-hook
                 (lambda ()
                   (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))

    (eval-after-load 'css-mode
      '(add-hook 'css-mode-hook
                 (lambda ()
                   (add-hook 'before-save-hook 'web-beautify-css-buffer t t))))


## `js-beautify` settings

The following `js-beautify` settings are available with `.jsbeautifyrc`. Check out the official [jsbeautifier documentation](https://github.com/einars/js-beautify#options) for more details on the options:

### JavaScript

Thease are JavaScript Beautifier Options:

* `indent_size`: Indentation size [4]
* `indent_char`: Indentation character [" "]
* `indent_level`: Initial indentation level [0]
* `indent_with_tabs`: Indent with tabs [false]
* `preserve_newlines`: Preserve line-breaks [true]
* `max_preserve_newlines`: Number of line-breaks to be preserved in one chunk [10]
* `space_in_paren`: Add padding spaces within paren, ie. f( a, b ) [false]
* `jslint_happy`: Enable jslint-stricter mode [false]
* `brace_style`: \[collapse|expand|end-expand] ["collapse"]
* `break_chained_methods`: Break chained method calls across subsequent lines [false]
* `keep_array_indentation`: Preserve array indentation [false]
* `unescape_strings`: Decode printable characters encoded in xNN notation [false]
* `wrap_line_length`: Wrap lines at next opportunity after N characters [0]
* `e4x`: Pass E4X xml literals through untouched [false]
* `space_before_conditional`: should the space before conditional statement be added [true]
* `keep_function_indentation`: false
* `eval_code`: false

### CSS

Thease are CSS Beautifier Options:

* `indent_size`: Indentation size [4]
* `indent_char`: Indentation character [" "]

### HTML

Thease are HTML Beautifier Options:

* `indent_inner_html`:  Indent <head> and <body> sections. [false]
* `indent_size`: Indentation size [4]
* `indent_char`: Indentation character [" "]
* `brace_style`: \[collapse|expand|end-expand] ["collapse"]
* `indent_scripts`: \[keep|separate|normal] ["normal"]
* `wrap_line_length`: Maximum characters per line (0 disables) [250]
* `preserve_newlines`: Preserve line-breaks [true]
* `max_preserve_newlines`: Number of line-breaks to be preserved in one chunk [10]
* `unformatted`:  List of tags (defaults to inline) that should not be reformatted

### .jsbeautifyrc files
**Web-beautify** supports `.jsbeautifyrc` JSON files.

A hierarchy of `.jsbeautifyrc` files is supported, where rc files at the deeper levels override the settings from rc files at higher levels. For example, given the file structure listed below, formatting `/home/you/myProject/app.js` would inherit settings from: default -> `/home/you/myProject/.jsbeautifyrc`, while formatting `/home/you/myProject/tests/test.js` would inherit settings from: default -> `/home/you/myProject/.jsbeautifyrc` -> `/home/you/myProject/tests/.jsbeautifyrc`

- /home/you/myProject/.jsbeautifyrc
- /home/you/myProject/app.js
- /home/you/myProject/tests/.jsbeautifyrc
- /home/you/myProject/tests/test.js

## Commands

#### <kbd>M-x</kbd> `web-beautify-js`

Format region if active, otherwise the current buffer. Formatting is done according to the `js-beautify` command.

#### <kbd>M-x</kbd> `web-beautify-css`

Format region if active, otherwise the current buffer. Formatting is done according to the `css-beautify` command.

#### <kbd>M-x</kbd> `web-beautify-html`

Format region if active, otherwise the current buffer. Formatting is done according to the `html-beautify` command.

# License

See [LICENSE](https://github.com/yasuyk/web-beautify/blob/master/LICENSE) for details.
