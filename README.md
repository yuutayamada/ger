# Installation
## Requirements
This program require Ruby 1.9, Thor and GoogleReaderApi of Ruby Gem.

## To install from command line

    cd  YOUR-INSTALLATION-DIRECTORY
    git clone https://github.com/yuutayamada/ger
    cd ./ger
    bundle install

## Paste your .emacs .etc..

    ;; lisp code Example
    (let* ((ger-directory "~/.emacs.d/lisp/ger/"))
       (add-to-list 'load-path (concat ger-directory "lisp/"))
       (setq ger-ruby-exe-path (concat ger-directory "bin/ger")
             ger-registering-dir "~/"
             ger-gmail-account "your-gmail-address")
       (autoload 'ger "ger")
       (autoload 'ger-fetch "ger")
       (global-set-key (kbd "C-S-i") 'ger))

## To start using

Execute command from Emacs

    M-x ger-fetch  (fetch feeds)
    M-x ger        (display result)
