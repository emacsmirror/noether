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
(setq debug-on-error t)

(require 'noether-units)
;;(debug-on-entry 'noether--update-buffer-name)

(defview example-bar
  "Just a test view"
  :managed? t
  :buffer "*mainview*"
  :binding (kbd "C-c 1")
  :separator "|"
  :frame
  (list
   :position '(110 . 120)
   :border-width 1
   ;; :left-fringe 5
   ;; :right-fringe 5
   :border-color "#bd93f9")
  :units
  (list
   (line-unit)
   (time-unit :label "Time:")
   (buffer-name-unit)
   (mode-name-unit)))


(setq noether-views (list example-bar))

(noether-global-mode t)

(provide 'noether.example)
;;; noether.example.el ends here
