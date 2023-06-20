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
   :position (cons (- (frame-outer-width) 10)
                   (- (frame-outer-height) 40))
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

(defface noether-active-modeline
  '((((background light))
     :background "#55ced1" :height 0.14 :box nil)
    (t
     :background "#008b8b" :height 0.14 :box nil))
  "A new face for modeline in active state."
  :group 'noether)

(defface noether-inactive-modeline
  '((((background light))
     :background "#55ced1" :height 0.14 :box nil)
    (t
     :background "#339933" :height 0.14 :box nil))
  "A new face for modeline in inactive state."
  :group 'noether)

(setq noether-views (list example-bar))

(add-hook
 'noether-global-mode-hook
 (lambda ()
   (setq-default mode-line-format "")
   (let ((face-remaps (default-value 'face-remapping-alist)))
     (setf (alist-get 'mode-line face-remaps)
           'noether-active-modeline
           (alist-get 'mode-line-inactive face-remaps)
           'noether-inactive-modeline
           (default-value 'face-remapping-alist) face-remaps))))

(noether-global-mode t)


(provide 'noether.example)
;;; noether.example.el ends here
