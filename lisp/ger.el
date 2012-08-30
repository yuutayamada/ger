;;; -*- coding: utf-8 -*-
;;; ger.el --- Emacs front-end for Google Reader

;; Copyright (C) 2012 by Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; URL: https://github.com/yuutayamada/ger
;; Version: 0.0.1
;; Keywords: toy

;;; License:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Usage
;; example
;; (let* ((ger-directory "~/.emacs.d/lisp/ger/")) <- set your ger directory
;;   (add-to-list 'load-path (concat ger-directory "lisp/"))
;;   (setq ger-ruby-exe-path (concat ger-directory "bin/ger")
;;         ger-registering-dir "~/")
;;   (autoload 'ger "ger")
;;   (global-set-key (kbd "C-S-i") 'ger))

(eval-when-compile
  (require 'cl))

(require 'json)
(require 'thingatpt)
(require 'org)

(require 'windows  nil t)
(require 'w3m      nil t)

(defvar ger-buffer-name "*ger*")
(defvar ger-base-buffer "")
(defvar ger-browse-fuction :w3m)
(defvar ger-ruby-exe-path "")
(defvar ger-registering-dir nil)
(defvar ger-format-style :org)

(defvar ger-mode-map
  (lexical-let* ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'ger-quit)
    (define-key map (kbd "j") 'ger-next-subject)
    (define-key map (kbd "k") 'ger-previous-subject)
    (define-key map (kbd "t") 'org-shifttab)
    (define-key map (kbd "h") 'ger-hide-org-buffer)
    (define-key map (kbd "o") 'ger-open-org-buffer)
    (define-key map (kbd "n") 'scroll-up-command)
    (define-key map (kbd "p") 'scroll-down-command)
    (define-key map (kbd "v") 'scroll-up-command)
    (define-key map (kbd "c") 'scroll-down-command)
    (define-key map (kbd "i") 'org-cycle)
    (define-key map (kbd "l") 'ger-refer-to-html)
    (define-key map (kbd "R") 'ger-reload)
    map))

(defun ger-hide-org-buffer ()
  (interactive)
  (org-overview))

(defun ger-open-org-buffer ()
  (interactive)
  (show-all))

(defun ger-next-subject ()
  (interactive)
  (if (not (eq ger-format-style :org))
      (re-search-forward "^----" nil t)
    (re-search-forward "^*[^*]" nil t)
    (re-search-forward "^*[^*]" nil t)
    (beginning-of-line)))

(defun ger-previous-subject ()
  (interactive)
  (if (eq ger-format-style :org)
      (re-search-backward "^*[^*]" nil t)
    (re-search-backward "^\\\\*" nil t)
    (re-search-backward "^----" nil t)
    (re-search-backward "^----" nil t)
    (end-of-line)))

(defvar ger/windows-switch-window-number 2)

(defun ger-quit ()
  (interactive)
  (kill-buffer ger-buffer-name)
  (switch-to-buffer ger-base-buffer))

;;;###autoload
(defun ger ()
  (interactive)
  (when (require 'windows nil t)
    (win-switch-to-window 1 ger/windows-switch-window-number))
  (ger-ather-window-or-split)
  (setq ger-base-buffer (buffer-name))
  (ger-make-buffer (ger-fetch-feeds)))

(defun ger-mode-on ()
  (if (not (eq ger-format-style :org))
      (ger-mode)
    (org-mode)
    (ger-minor-mode t)))

(defun ger-make-buffer (content)
  (save-current-buffer
    (with-temp-buffer
      (switch-to-buffer (get-buffer-create ger-buffer-name))
      (setq buffer-read-only nil)
      (ger-mode-on)
      (erase-buffer)
      (insert content)
      (setq buffer-read-only t)
      (goto-char (point-min)))))

(defun ger-ather-window-or-split ()
  (when (one-window-p)
    (if (< 90 (window-width))
        (split-window-horizontally)
      (split-window-vertically)))
  (other-window 1))

(defun ger-get-buffer-string ()
  (let* ((rss-json (concat ger-registering-dir ".ger.json"))
         (buffer-name (get-file-buffer rss-json)))
    (with-temp-buffer
      (find-file rss-json)
      (buffer-string))))

(defun ger/org-browse-link ()
  (when (eq major-mode 'org-mode)
    (org-show-subtree)
    (org-next-link)
    (save-current-buffer
      (beginning-of-line)
      (when (looking-at "** link.+")
        (org-next-link)
        (org-return)))))

(defun ger-refer-to-html ()
  (interactive)
  (if (eq major-mode 'org-mode)
      (ger/org-browse-link)
    (lexical-let* ((line (thing-at-point 'line))
                   (url  (replace-regexp-in-string " " "" line)))
      (save-current-buffer
        (beginning-of-line)
        (if (not (looking-at "http"))
            (re-search-forward "^http://" nil nil)
          (if (and (eq ger-browse-fuction :w3m)
                   (require 'w3m nil t))
              (progn
                (ger-ather-window-or-split)
                (w3m-goto-url url))
            (browse-url url)))))))

(defun ger-fetch-feeds ()
  (let* ((json (ger-get-buffer-string)))
    (loop with result = '()
          for factors across (json-read-from-string json)
          collect (ger-extract-subject factors) into result
          finally return (mapconcat 'identity result "\n----\n"))))

(defun ger-format (feed)
  (loop with format
        with ti and de and li and dt
        for (title description link date) in (list feed) do
        (setq ti (concat "* " title
                         (unless (string-match "\\w" description)
                           "<E>"))
              de (concat "** " description)
              li (concat "** link [[" link "][...]]")
              dt (concat "** " date)
              format (list ti de li dt))
        finally return (mapconcat 'identity format "\n")))

(defun ger-extract-subject (factors)
  (loop with content = '()
        with title and description and link and date
        for (subject . statement) in factors do
        (case subject
          ('title       (ger-fontify      statement 'Info-title-3-face)
                        (setq title       statement))
          ('description (setq description statement))
          ('link        (ger-fontify      statement 'link)
                        (setq link        statement))
          ('date        (setq date        statement)))
        finally return (ger-format `(,title ,description ,link ,date))))

(defun ger-fontify (word &optional face)
  (let* ((length (length word)))
    (put-text-property 0 length 'face (if face face 'warning) word)))

(easy-mmode-define-minor-mode
 ger-minor-mode
 " Ger " nil " Ger"
 (copy-keymap ger-mode-map))

(defun ger-mode ()
  (interactive)
  (when (equal (buffer-name) ger-buffer-name)
    (setq major-mode 'ger-mode
          mode-name "GR")
    (use-local-map ger-mode-map)))

(defun ger-reload ()
  (interactive)
  (let* ((command (concat ger-ruby-exe-path " reload"))
         dir)
    (when ger-registering-dir
      (setq dir (concat " --directory=" (cd-absolute ger-registering-dir))
            command (concat command dir)))
    (async-shell-command command)))

(provide 'ger)
