;;; noether.el --- A modeline which plays hide and seek  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2023 Sameer Rahmani <lxsameer@gnu.org>
;;
;; Author: Sameer Rahmani <lxsameer@gnu.org>
;; URL: https://devheroes.codes/lxsameer/noether
;; Version: 0.1.0
;; Keywords: frames, modeline
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
(require 'noether)

;; line number indicator ======================================================
(defvar noether--line 1)

(defun noether--update-line ()
  "Update the `noether--line' variable after each command."
  ;; TODO: calling `line-number-at-pos' is not performant
  ;; replace this with a better alt
  (setq noether--line (list (line-number-at-pos) (current-column))))

(defun noether--line-format (_ v _ _)
  "Format the current line number and column given as pair V."
  (format "%4d:%4d" (car v) (cadr v)))


(defunit line-unit
  "Show the line number."
  :label (propertize "L" 'face 'highlight)
  :len 9
  :init  (lambda ()
           (add-hook 'post-command-hook #'noether--update-line))
  :deinit (lambda ()
            (remove-hook 'post-command-hook #'noether--update-line))
  :var 'noether--line
  :fn #'noether--line-format)

;; Time indicator =============================================================
(defvar noether--time "")
(defvar noether--timer nil)
(defun noether--set-time ()
  "Set the current time to the internal var which is being watched."
  (setq noether--time (format-time-string "%H:%M:%S")))

(defun noether--time-format (_ v _ _)
  "Just return the current time V."
  v)


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



(provide 'noether-units)
;;; noether-units.el ends here
