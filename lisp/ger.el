;;; -*- coding: utf-8 -*-
;;; ger.el --- Google Reader front-end for Emacs

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

(eval-when-compile
  (require 'cl))

(require 'json)
(require 'w3m)
(require 'thingatpt)
(require 'windows)

(defvar ger-buffer-name "*ger*")
(defvar ger-base-buffer "")
(defvar ger-browse-fuction :w3m)
(defvar ger-ruby-exe-path "~/bb/work/ger/bin/ger")

(defvar ger-map
  (lexical-let* ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'ger-quit)
    (define-key map (kbd "j") 'ger-next-subject)
    (define-key map (kbd "k") 'ger-previous-subject)
    (define-key map (kbd "n") 'scroll-up-command)
    (define-key map (kbd "p") 'scroll-down-command)
    (define-key map (kbd "l") 'ger-refer-to-html)
    map))

(defun ger-next-subject ()
  (interactive)
  (re-search-forward "^----" nil t))

(defun ger-previous-subject ()
  (interactive)
  (re-search-backward "^----" nil t)
  (re-search-backward "^----" nil t)
  (end-of-line))

(defvar ger/windows-switch-window-number 2)

(defun ger-quit ()
  (interactive)
  (kill-buffer ger-buffer-name)
  (switch-to-buffer ger-base-buffer))

;;;###autoload
(defun ger ()
  (interactive)
  (win-switch-to-window 1 ger/windows-switch-window-number)
  (ger-ather-window-or-split)
  (setq ger-base-buffer (buffer-name))
  (ger-make-buffer (ger-fetch-feeds)))

(defun ger-make-buffer (content)
  (save-current-buffer
    (with-temp-buffer
      (switch-to-buffer (get-buffer-create ger-buffer-name))
      (setq buffer-read-only nil)
      (ger-mode)
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
  (let* ((rss-json "~/myrss.json")
         (buffer-name (get-file-buffer rss-json)))
    (with-temp-buffer
      (find-file rss-json)
      (buffer-string))))

(defun ger-refer-to-html ()
  (interactive)
  (lexical-let* ((line (thing-at-point 'line))
                 (url  (replace-regexp-in-string " " "" line)))
    (save-current-buffer
      (beginning-of-line)
      (if (not (looking-at "http"))
          (re-search-forward "^http://" nil nil)
        (if (eq ger-browse-fuction :w3m)
            (progn
              (ger-ather-window-or-split)
              (w3m-browse-url url))
          (browse-url url))))))

(defun ger-fetch-feeds ()
  (let* ((json (ger-get-buffer-string)))
    (loop with result = '()
          for factors across (json-read-from-string json)
          collect (ger-extract-subject factors) into result
          finally return (mapconcat 'identity result "\n----\n"))))

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
        finally return (mapconcat
                        'identity `(,title ,description ,link ,date) "\n")))

(defun ger-fontify (word &optional face)
  (let* ((length (length word)))
    (put-text-property 0 length 'face (if face face 'warning) word)))

(defun ger-mode ()
  (when (equal (buffer-name) ger-buffer-name)
    (setq major-mode 'ger-mode
          mode-name "GR")
    (use-local-map ger-map)))

(defun ger-reload ()
  (interactive)
  (async-shell-command
   (concat
    ger-ruby-exe-path " reload")))

(provide 'ger)
