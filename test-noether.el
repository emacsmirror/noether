;;; noether.el --- A modeline which plays hide and seek  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2023-2024 Sameer Rahmani <lxsameer@gnu.org>
;;
;; Author: Sameer Rahmani <lxsameer@gnu.org>
;; URL: https://devheroes.codes/lxsameer/noether
;; Version: 0.1.0
;; Keywords: frames, modeline
;; Package-Requires: ((posframe "1.4.2") (emacs "27.1"))
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
;; A simple frame manager framework that can be utilized to create a mode line
;; alternative.
;;; Change Log:
;;; Code:


(setq debug-on-error t)

(require 'noether)
(require 'noether-units)
(require 'noether-views)
(load-theme 'tsdh-dark)

(defmacro comment (&rest _) nil)

(defun noether--bottom-right (info)
  (cons -1 -1))

(defun noether-autohide-on-echo (_)
  (minibuffer-prompt))

(comment
  (with-current-buffer (get-buffer "*modeline*")
  (set-frame-parameter posframe--frame 'sticky t)
  (pp (frame-parameters posframe--frame))))
;; (setq f1 (make-frame '((name . "f1") )))
;; (setq f2 (make-frame '((name . "f2") )))
;; (setq f3 (make-frame '((name . "f3") )))

;; (nth 3 (frame-list))
;; (visible-frame-list)
;; (select-frame (nth 2 (frame-list)))

(noether-defview mode-line
  "A simple and minimalist mode-line like status bar"
  :managed? t
  :buffer "*modeline*"
  :binding (kbd "C-c 1")
  :separator "|"
  :visible? t
  :frame
  (list
   :name "modeline"
   ;; :position (cons (- (frame-outer-width) 10)
   ;;                 (- (frame-outer-height) 40))
   :poshandler #'noether--bottom-right
   :border-width 0

   ;;   :parent-frame f1
   ;;:timeout 5
   :hidehandler #'noether-autohide-on-echo
   :border-color "#bd93f9")

  :units
  (list
   (line-unit :label "")
   (buffer-name-unit :label "B: ")
   ))


(noether-defview minibuffer-ex
  "A simple and minimalist mode-line like status bar"
  :managed? t
  :buffer "*modeline*"
  :binding (kbd "C-c 2")
  :visible? t
  :sticky? t
  :hide-when-minibuffer? t
  :frame
  (list
   :name "ex"
   :explicit-name "ex"
   :poshandler #'posframe-poshandler-frame-center
   :border-width 1
   ;;   :parent-frame f2
   ;; :hidehandler #'noether-autohide-on-echo
   :border-color "#bd93f9")

  :units
  (list
   (mode-name-unit :label "M: ")

   (project-unit)))

(setq noether-views (list minibuffer-ex ))

(noether-global-mode t)


(provide 'test-noether)
;;; test-noether.el ends here
