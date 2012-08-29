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

(defvar ger-buffer-name "*ger*")
(defvar ger-base-buffer "")

;;;###autoload
(defun ger ()
  (interactive)
  (ger-ather-window-or-split)
  (setq ger-base-buffer (buffer-name))
  (ger-make-buffer (ger-fetch-feeds)))

(defun ger-make-buffer (content)
  (save-current-buffer
    (with-temp-buffer
      (switch-to-buffer (get-buffer-create ger-buffer-name))
      (erase-buffer)
      (insert content)
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

(defun ger-fetch-feeds ()
  (let* ((json (ger-get-buffer-string)))
    (loop with result = '()
          for factors across (json-read-from-string json)
          collect (ger-extract-subject factors) into result
          finally return (mapconcat 'identity result "\n----\n"))))

(defun ger-extract-subject (factors)
  (loop with content = '()
        with title and description and link
        for (subject . statement) in factors do
        (case subject
          ('title       (ger-fontify      statement)
                        (setq title       statement))
          ('description (setq description statement))
          ('link        (setq link        statement)))
        finally return (mapconcat 'identity `(,title ,description ,link) "\n")))

(defun ger-fontify (word)
  (let* ((length (length word)))
    (put-text-property 0 length 'face 'warning word)))

(provide 'ger)
