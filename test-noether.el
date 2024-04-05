;;; noether.el --- A modeline which plays hide and seek  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2023 Sameer Rahmani <lxsameer@gnu.org>
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

(noether-defview mode-line
  "A simple and minimalist mode-line like status bar"
  :managed? t
  :buffer "*modeline*"
  ;;:binding (kbd "C-c 1")
  :timeout 0
  :separator "|"
  :visible? t
  :frame
  (list
   :position (cons (- (frame-outer-width) 10)
                   (- (frame-outer-height) 40))
   ;;:poshandler #'posframe-poshandler-frame-top-center
   :border-width 0
   :border-color "#bd93f9")

  :units
  (list
   (line-unit :label "")
   (buffer-name-unit :label "B: ")
   (mode-name-unit :label "M: ")))


(setq noether-views (list mode-line))

(noether-global-mode t)


(provide 'test-noether)
;;; test-noether.el ends here
