# Installation
## To install from command line
cd  YOUR-INSTALLATION-DIRECTORY
git clone https://github.com/yuutayamada/ger

## Paste your .emacs .etc..

    ;; lisp code Example
    (let* ((ger-directory "~/.emacs.d/lisp/ger/")) <- set your ger directory
       (add-to-list 'load-path (concat ger-directory "lisp/"))
       (setq ger-ruby-exe-path (concat ger-directory "bin/ger")
             ger-registering-dir "~/") ;; <- prefer directory
       (autoload 'ger "ger")
       (global-set-key (kbd "C-S-i") 'ger))

## To start using
Note that Require downloading to google-reader-subscriptions.xml from 'http://www.google.com/reader/subscriptions/export?hl=ja'.

Execute command from Emacs

    M-x ger-reload (extract feed)
    M-x ger        (display result)
