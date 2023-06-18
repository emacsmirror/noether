;;; noether.el --- A modeline which plays hide and seek  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2023 Sameer Rahmani <lxsameer@gnu.org>
;;
;; Author: Sameer Rahmani <lxsameer@gnu.org>
;; URL: https://devheroes.codes/lxsameer/noether
;; Version: 0.1.0
;; Keywords: frames, modeline
;; Package-Requires: (posframe (emacs "26.1"))
;;
;; This program is free software; you can redistribute it and/or modify
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
;;
;;; Commentary:
;;; Change Log:
;;; Code:
(setq debug-on-error t)
(require 'noether)

(defvar noether--line 1)

(defun noether--update-line ()
  "Update the `noether--line' variable after each command."
  ;; TODO: calling `line-number-at-pos' is not performant
  ;; replace this with a better alt
  (setq noether--line (line-number-at-pos)))

(defun noether--line-format (_ v _ _)
  (format "%04d" v))

(defvar noether--time "")
(defvar noether--timer nil)
(defun noether--set-time ()
  (setq noether--time (format-time-string "%H:%M:%S")))

(defun noether--time-format (_ v _ _)
  "Just return the current time."
  v)

(defunit line-unit
  "Show the line number."
  :label "L:"
  :len 4
  :init  (lambda ()
              (add-hook 'post-command-hook #'noether--update-line))
  :deinit (lambda ()
               (remove-hook 'post-command-hook #'noether--update-line))
  :var 'noether--line
  :fn #'noether--line-format)

(defunit time-unit
  "just the time for your bar."
  :label "T:"
  :len 8
  :init  (lambda ()
           (setq noether--timer
                 (run-with-timer 1 1 #'noether--set-time)))
  :deinit (lambda ()
            (when noether--timer
              (cancel-timer noether--timer)))

  :var 'noether--time
  :fn #'noether--time-format)

(defview example-bar
  "Just a test view"
  :managed? t
  :buffer "*mainview*"
  :binding (kbd "C-c 1")
  :separator "|"
  :units
  (list
   (line-unit)
   (time-unit :label "B:")))


(setq noether-views (list example-bar))

(noethor-global-mode t)

(provide 'noether.example)
;;; noether.example.el ends here
