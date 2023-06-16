;;; noether.el --- A modeline which plays hide and seek  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2023 Sameer Rahmani <lxsameer@gnu.org>
;;
;; Author: Sameer Rahmani <lxsameer@gnu.org>
;; URL: https://devheroes.codes/lxsameer/noether
;; Version: 0.1.0
;; Keywords: frames, modeline
;; Package-Requires: (posframe seq)
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
(require 'seq)
(require 'posframe)

(defmacro comment (&rest _) nil)

;;(defgroup noether)

(defvar noether/views ())
(defvar noether/-internal-state)

(defvar noether/-line ())

(defun noether/-update-line ()
  "Update the `noether/-line' variable after each command."
  ;; TODO: calling `line-number-at-pos' is not performant
  ;; replace this with a better alt
  (setq noether/-line (line-number-at-pos)))

(defun noether/-line-format ()
  (format "%04d" noether/-line))

(defvar testt
  (list
   :managed? t
   :buffer "*mainview*"
   :binding (kbd "C-c 1")
   :units
   (list
    (list
     :label "L:"
     :name :line
     :len 4
     :init (lambda ()
             (add-hook 'post-command-hook #'noether/-update-line))
     :var 'noether/-line
     :fn #'noether/-line-format))))

(comment
  (remove-hook 'post-command-hook #'noether/-update-line)
  (line-number-mode)
  (line-number-at-pos)
  (add-variable-watcher))

(defmacro noether/-unit-get (unit key &optional default)
  ""
  `(or (plist-get ,unit ,key) ,default))


(defmacro noether/-view-get (view key &optional default)
  ""
  `(or (plist-get ,view ,key) ,default))


(defun noether/show (view)
  (interactive)
  (let ((buf (get-buffer-create (noether/-view-get view :buffer "*noether*"))))
    (message "> %s" (noether/-view-get view :buffer "*noether*"))
    (when (noether/-view-get view :managed?)
      (with-current-buffer buf
        (erase-buffer)
        (insert "A | B | C")))

    (posframe-show
     buf
     :min-height (noether/-view-get view :height 1)
     :min-width (noether/-view-get view :width 10)

     :position (cons (- (frame-outer-width) 10) (- (frame-outer-height) 10))

     ;;:poshandler #'posframe-poshandler-frame-bottom-right-corner
     :border-width (noether/-view-get view :border 0)
     :border-color (noether/-view-get view :border-color "#eeeefe")
     :accept-focus (noether/-view-get view :accept-focus)
     :timeout (noether/-view-get view :timeout 5)
     :refresh (noether/-view-get view :refresh 1))))

(comment
  (noether/show testt)
  (setq noether/views (cons testt noether/views))
  (setq noether/views nil)
  noether/views
  (mapc #'noether/-setup-views noether/views)
  (posframe-delete-all))


(defun noether/update (view unit)
  "Update the the given UNIT name in the given VIEW."
  (let ((f (noether/-unit-get unit :f (lambda (_ _) (error (format "No `fn' in %s" unit)))))
        (state (plist-get view unit)))
    (when (null state)
      (error (format "Can't find unit ")))
    (funcall f state)))


(defun noether/-setup-unit (state view unit)
  (let* ((init-fn (noether/-unit-get unit :init))
         (var (noether/-unit-get unit :var))
         (name (noether/-unit-get unit :name))
         (start-point (+ (or (plist-get view :point) 0) (length (noether/-unit-get unit :label ""))))
         ;; TODO: We need to raise an error earlier if size is not provided
         (end-point (+ start-point (noether/-unit-get unit :size 0))))

    (when (null name)
      (error (format "No :name for unit %s" unit)))

    ;; (setq state
    ;;       (plist-put
    ;;        (plist-put state :point end-point)
    ;;        name
    ;;        `(:unit unit :cell (,start-point ,end-point))))
    (setq view
          (plist-put
           (plist-put view :point end-point)
           name
           `(:unit unit :cell (,start-point ,end-point))))

    (when init-fn
      (funcall init-fn))

    (when var
      (add-variable-watcher var (lambda ()
                                  (noether/update view :name))))))


(defun noether/-setup-views (view)
  "Setup the given VIEW by setting up its units."
  (setq noether/-internal-state
        (seq-reduce
         (lambda (state u) (noether/-setup-unit state view u))
         (noether/-view-get view :units)
         ())))


(define-minor-mode noether/global-statue-mode
  "A minor mode that keep tracks of different status blocks.
It reports them back in a status bar like frame."
  :global t
  :lighter " ST42"
  :keymap (let ((map (make-sparse-keymap)))
            (progn
              (mapc
               (lambda (view)
                 (let ((binding (noether/-view-get view :binding)))
                   (when binding
                     (define-key map binding (lambda ()
                                               (noether/show view))))))
               noether/views)
              map))
  (mapc #'noether/-setup-views noether/views))

(comment
  (noether/global-statue-mode))

(provide 'noether)
;;; noether.el ends here
