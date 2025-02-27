;;; noether.el --- A modeline which plays hide and seek  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2023-2025 Sameer Rahmani <lxsameer@gnu.org>
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
(require 'posframe)

(require 'noether)
(require 'noether-units)

(noether-defview noether-minimal-mode-line
  "A simple and minimalist mode-line like status bar"
  :managed? t
  :buffer "*mainview*"
  :binding (kbd "C-c 1")
  :separator "|"
  :frame
  (list
   :position nil
   :poshandler #'posframe-poshandler-frame-top-center
   :border-width 1
   :border-color "#bd93f9")

  :units
  (list
   ;; (line-unit :label "")
   (buffer-name-unit :label "B: ")
   (mode-name-unit :label "M: ")
   (project-unit :label "P: ")
   (git-branch-unit :label "Br: ")))


(noether-defview noether-minimal-location
  "A super simple bar containing the line number and column number that
Appears on the center of the current window."
  :managed? t
  :buffer "*location*"
  :binding (kbd "C-c 2")
  :separator "|"
  :frame
  (list
   :position nil
   :poshandler #'posframe-poshandler-window-center
   :border-width 1
   :timeout 5
   :border-color "#bd93f9")

  :units
  (list
   (line-unit :label "")))


(provide 'noether-views)
;;; noether-views.el ends here
