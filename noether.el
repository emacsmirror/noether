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

(defun noether/-extract-props (body-list &optional acc)
  "Extract the props pairs from BODY-LIST with an optional accumulator ACC.

It will returen a pair in form of (body . props)."
  (let ((k (car body-list))
        (rest (cdr body-list)))

    (if (and k (keywordp k))
        (fg42/extract-props
         (cdr rest)
         (cons (cdr rest) (plist-put (cdr acc) k (car rest))))
      (cons body-list (cdr acc)))))

(defun noether/-create-placeholder (unit)
  "Create a placeholder for UNIT based on its :label and :len."
  (concat
   (noether/-unit-get unit :label "")
   (make-string (noether/-unit-get unit :len 0) ? )))


(defmacro defview (name docs &rest body)
  "Create a new view with the given NAME with the given DOCS and BODY.
BODY will be parsed in a way that any starting pair of keyword and value
will be used as the view properties and the rest will be the body of
the show function."
  (declare (doc-string 2))
  (let* ((parsed-body (fg42/extract-props body))
         (show-body (car parsed-body))
         (props (cdr parsed-body))
         (initial-content
          (mapconcat #'noether/-create-placeholder (eval (plist-get props :units))
                     (or (plist-get props :separator) ""))))

    `(progn
       (defvar ,name
         (list
          :name ',name
          ,@props
          :show (lambda () ,@show-body))
         ,docs)
       ;; It's not necessary but well doesn't hurt either
       ;; for future reference
       (put ',name :updaters ())
       (put ',name :initial-content ,initial-content)
       t)))


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

(defview testt
  "Just a test view"
  :managed? t
  :buffer "*mainview*"
  :binding (kbd "C-c 1")
  :units
  (list
   (list
    :label "L: "
    :name :line
    :len 4
    :init (lambda ()
            (add-hook 'post-command-hook #'noether/-update-line))
    :var 'noether/-line
    :fn #'noether/-line-format)))

(comment
  (remove-hook 'post-command-hook #'noether/-update-line)
  (line-number-mode)
  (line-number-at-pos)
  (add-variable-watcher)
  (noether/show testt)
  (setq noether/views (cons testt noether/views))
  (setq noether/views nil)
  (noether/-unit-get (car (noether/-view-get testt :units)) :len)
  noether/views
  (pp (car (get 'testt :updaters)))
  (pp  (get 'testt :initial-content))
  (pp (mapc #'noether/-setup-views noether/views))
  post-command-hook
  (posframe-delete-all))

(defmacro noether/-unit-get (unit key &optional default)
  ""
  `(or (plist-get ,unit ,key) ,default))


(defmacro noether/-view-get (view key &optional default)
  ""
  `(or (plist-get ,view ,key) ,default))


(defun noether/show (view)
  ""
  ;; View has to be processed
  (interactive)
  (let* ((show-fn (noether/-view-get view :show (lambda ())))
         (name (noether/-view-get view :name))
         ;; What if the user killed the buffer before?
         (buf (get-buffer-create (noether/-view-get view :buffer (format "*%s*" name)))))
    ;; TODO: Check to see whether the buffer is populated. If not, it means
    ;;       that user killed the buffer manually. We need to repopulate it
    ;;       again
    (when (noether/-view-get view :managed?)
      (with-current-buffer buf
        (funcall show-fn)
        (mapc
         (lambda (updater) (funcall updater))
         (get name :updaters))))

    (posframe-show
     buf
     :min-height (noether/-view-get view :height 1)
     :min-width (noether/-view-get view :width 10)

     :position (cons (- (frame-outer-width) 10) (- (frame-outer-height) 10))

     ;;:poshandler #'posframe-poshandler-frame-bottom-right-corner
     :border-width (noether/-view-get view :border 0)
     :border-color (noether/-view-get view :border-color "#eeeefe")
     :accewpt-focus (noether/-view-get view :accept-focus)
     :timeout (noether/-view-get view :timeout 5)
     :refresh (noether/-view-get view :refresh 1))))


;; We need to keep this function as simple as possible
;; and avoid any performance pitfalls
(defun noether/update-unit (buf f start-point len)
  "Update the buffer BUF at START-POINT with length LEN by calling F."
  ;; call f get the return value and put it in the dedicated cell
  (let ((res (apply f)))
    (with-current-buffer buf
      (save-excursion
        (goto-char start-point)
        (insert (truncate-string-to-width res len))))))

(defun noether/-make-updater (buf f start-point len)
  "Create an updater for the given buffer BUF using the function F.
It will call `noether/update-unit' and path START-POINT and LEN along
side BUF and F to it.  It's simple trick to make small a closure."
  (lambda () (noether/update-unit buf f start-point len)))


(defun noether/-setup-unit (point-state view unit)
  "Setup the given UNIT in respect of VIEW using the POINT-STATE as the boundary."
  (let* ((init-fn (noether/-unit-get unit :init))
         (f (noether/-unit-get unit :fn))
         (len (noether/-unit-get unit :len))
         (label (noether/-unit-get unit :label ""))
         (buf (noether/-view-get view :buffer))
         (sep (noether/-view-get view :separator))
         (var (noether/-unit-get unit :var))
         (name (noether/-unit-get unit :name))
         (start-point (+ point-state (length label)))
         (end-point (+ start-point (noether/-unit-get unit :len 0)))
         ;; Just a small trick to make the resulting closure smaller
         (updater (noether/-make-updater buf f start-point len)))

    (when (null name)
      (error (format "No :name for unit %s" unit)))

    (when (null f)
      (error (format "No `fn' in %s" unit)))

    (when init-fn
      (funcall init-fn))

    (when var
      (add-variable-watcher var updater))

    (let ((name (noether/-view-get view :name)))
      (put name :updaters
           (cons updater (get name :updaters))))

    ;; Move the point to the location of the next unit
    (+ end-point (length (or sep "")))))


(defun noether/-reset-view-state (view)
  "Reset the state stored in VIEW.
E.g. the updaters list."
  (put (noether/-view-get view :name) :updaters nil))


(defun noether/-setup-views (view)
  "Setup the given VIEW by setting up its units."
  (noether/-reset-view-state view)

  (let ((name (noether/-view-get view :name)))
    (with-current-buffer (get-buffer-create (noether/-view-get view :buffer (format "*%s*" name)))
      (erase-buffer)
      (goto-char 0)
      (insert (get name :initial-content))))

  (seq-reduce
   (lambda (state u)
     (noether/-setup-unit state view u))
   (noether/-view-get view :units)
   0))


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
