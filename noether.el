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
(require 'seq)
(require 'posframe)


(defvar noether-views ()
  "A list of views that noether should manage.

You should adding your views to this var, so noether can activate them
on demand.")


(defmacro noether--unit-get (unit key &optional default)
  "Return the value of the KEY in UNIT or the DEFAULT value if it doesn't exist."
  `(or (plist-get ,unit ,key) ,default))


(defmacro noether--view-get (view key &optional default)
  "Return the value of the KEY in VIEW or the DEFAULT value if it doesn't exist."
  `(or (plist-get ,view ,key) ,default))


(defun noether--extract-props (body-list &optional acc)
  "Extract the props pairs from BODY-LIST with an optional accumulator ACC.

It will returen a pair in form of (body . props)."
  (let ((k (car body-list))
        (rest (cdr body-list)))

    (if (and k (keywordp k))
        (noether--extract-props
         (cdr rest)
         (cons (cdr rest) (plist-put (cdr acc) k (car rest))))
      (cons body-list (cdr acc)))))


(defun noether--create-placeholder (unit)
  "Create a placeholder for UNIT based on its :label and :len."
  (concat
   (noether--unit-get unit :label "")
   (make-string (noether--unit-get unit :len 0) ? )))


(defmacro noether-from-modeline (name docs label format-str len)
  "Define a new unit with the given NAME and doc-string DOCS from the modeline.
It will use the given LABEL and LEN to pass the to the `defuit' macro.

The most important part of this macro is the FORMAT-STR parameter.  It
should be a format string that is understandable by `format-modeline'
function."
  (declare (doc-string 2) (indent defun))
  (let ((new-var-sym (gensym)))
    `(progn
       (defvar ,(intern (format "%s--internal-state-var" (symbol-name name))))

       (defun ,(intern (format "%s--update-internal-state-var" (symbol-name name))) ()
         (setq ,(intern (format "%s--internal-state-var" (symbol-name name)))
               (format-mode-line ,format-str)))


       (defun ,(intern (format "%s--format-final-result" (symbol-name name))) (_ ,new-var-sym _ _)
         "Format the buffer name V."
         (string-trim ,new-var-sym))


       (defunit ,name
         ,docs
         :label ,label
         :len ,len
         :init (lambda ()
                 (add-hook 'post-command-hook
                           #',(intern (format "%s--update-internal-state-var" (symbol-name name)))))

         :deinit (lambda ()
                   (remove-hook 'post-command-hook
                                #',(intern (format "%s--update-internal-state-var" (symbol-name name)))))

         :var ',(intern (format "%s--internal-state-var" (symbol-name name)))
         :fn #',(intern (format "%s--format-final-result" (symbol-name name)))))))


(defmacro defview (name docs &rest body)
  "Create a new view with the given NAME with the given DOCS and BODY.
BODY will be parsed in a way that any starting pair of keyword and value
will be used as the view properties and the rest will be the body of
the show function."
  (declare (doc-string 2))
  (let* ((parsed-body (noether--extract-props body))
         (show-body (car parsed-body))
         (props (cdr parsed-body))
         (initial-content
          (mapconcat #'noether--create-placeholder (eval (plist-get props :units))
                     (or (plist-get props :separator) ""))))

    `(progn
       (defvar ,name
         (list
          ,@props
          :name ',name
          :show (lambda () ,@show-body))
         ,docs)
       ;; It's not necessary but well doesn't hurt either
       ;; for future reference
       (put ',name :updaters ())
       (put ',name :initial-content ,initial-content)
       t)))



(defmacro defunit (name docs &rest props)
  "Define a unit with the given NAME, DOCS and a set of PROPS.
It will define a function with the given NAME that accepts any
parameter in form of key/values that will override any original
key/value from the original definition."

  (declare (doc-string 2))
  (let* ((parsed-body (noether--extract-props props))
         ;; For now we don't have any use for the body
         (_ (car parsed-body))
         (orig-props (cdr parsed-body)))

    `(defun ,name (&rest f-props)
       ,docs
       (append '(:name ,(intern (format ":%s" name)))
               f-props (list ,@orig-props)))))


(defvar noether--frame-defaults
  (list
   :min-height 1  ;; (noether--view-get view :height 1)
   :min-width 10  ;; (noether--view-get view :width 10)
   :position '(0 . 0) ;;(cons (- (frame-outer-width) 10) (- (frame-outer-height) 10))
   ;;:position (noether--view-get view :position '(0 . 0))
   ;;:poshandler (noether--view-get view :poshandler)
   :border-width 0 ;; (noether--view-get view :border 0)
   ;;:border-color (noether--view-get view :border-color "#eeeefe")
   :accewpt-focus nil ;;(noether--view-get view :accept-focus)
   :timeout 10        ;;(noether--view-get view :timeout 5)
   :refresh 1         ;;(noether--view-get view :refresh 1)))
   )
  )

(defun noether-show (view)
  "Draw the given VIEW on the screen."
  ;; View has to be processed at this stage
  (interactive)
  (let* ((show-fn (noether--view-get view :show (lambda ())))
         (name (noether--view-get view :name))
         ;; What if the user killed the buffer before?
         (buf (get-buffer-create (noether--view-get view :buffer (format "*%s*" name))))
         (props (noether--view-get view :frame)))
    ;; TODO: Check to see whether the buffer is populated. If not, it means
    ;;       that user killed the buffer manually. We need to repopulate it
    ;;       again
    (when (noether--view-get view :managed?)
      (with-current-buffer buf
        (funcall show-fn)))

    (let ((params (append (list buf) props noether--frame-defaults)))
      (apply #'posframe-show params))))


;; We need to keep this function as simple as possible
;; and avoid any performance pitfalls
(defun noether-update-unit (buf f start-point len watch-params)
  "Update the buffer BUF at START-POINT with length LEN by calling F.
It will pass WATCH-PARAMS to the unit's `:fn'"
  ;; call f get the return value and put it in the dedicated cell
  (let ((res (apply f watch-params)))
    (with-current-buffer buf
      (save-excursion
        (let ((txt (truncate-string-to-width res len)))
          (replace-region-contents
           (+ 1 start-point)
           (+ 1 start-point len)
           (lambda () (string-pad (truncate-string-to-width res len) len))))))))


(defun noether--make-updater (buf f start-point len)
  "Create an updater for the given buffer BUF using the function F.
It will call `noether-update-unit' and path START-POINT and LEN along
side BUF and F to it.  It's simple trick to make small a closure."

  ;; `add-watch-params' is a list of 4 elements that `add-variable-watcher'
  ;; passes to it's handler
  (lambda (&rest add-watch-params)
    (noether-update-unit buf f start-point len add-watch-params)))


(defun noether--setup-unit (point-state view unit)
  "Setup the given UNIT in respect of VIEW using the POINT-STATE as the boundary."
  (let* ((init-fn (noether--unit-get unit :init))
         (f (noether--unit-get unit :fn))
         (len (noether--unit-get unit :len))
         (label (noether--unit-get unit :label ""))
         (buf (noether--view-get view :buffer))
         (sep (noether--view-get view :separator))
         (var (noether--unit-get unit :var))
         (name (noether--unit-get unit :name))
         (view-name (noether--view-get view :name))
         (start-point (+ point-state (length label)))
         (end-point (+ start-point (noether--unit-get unit :len 0)))
         ;; Just a small trick to make the resulting closure smaller
         (updater (noether--make-updater buf f start-point len)))

    (when (null name)
      (error (format "No :name for unit %s" unit)))

    (when (null f)
      (error (format "No `fn' in %s" unit)))

    (when init-fn
      (funcall init-fn))

    (when var
      ;; Setup the watcher and the watcher remover
      (add-variable-watcher var updater)
      (put view-name :watcher-removers
           (cons (lambda ()
                   ;; We will call this function later during
                   ;; the teardown process
                   (remove-variable-watcher var updater))
                 (get view-name :watcher-removers))))

    (put view-name :updaters
         (cons updater (get view-name :updaters)))

    ;; Move the point to the location of the next unit
    (+ end-point (length (or sep "")))))


(defun noether--reset-view-state (view)
  "Reset the state stored in VIEW.
E.g. the updaters list."
  (put (noether--view-get view :name) :updaters nil))


(defun noether--setup-views (view)
  "Setup the given VIEW by setting up its units."
  (when (not (listp view))
    (error (format "The given value as a view is not a list: %s" view)))

  (noether--reset-view-state view)

  (let ((name (noether--view-get view :name))
        (binding (noether--view-get view :binding)))

    (when (not (null binding))
      (define-key noether-global-mode-map binding
        (lambda () (interactive) (noether-show view))))

    (with-current-buffer (get-buffer-create (noether--view-get view :buffer (format "*%s*" name)))
      (erase-buffer)
      (goto-char 0)
      (insert (get name :initial-content))))

  (seq-reduce
   (lambda (state u)
     (noether--setup-unit state view u))
   (noether--view-get view :units)
   0))

(defun noether--teardown-unit (unit)
  "Tear down the given UNIT by calling the `:deinit' function and removing possible watches."
  (let ((deinit (noether--unit-get unit :deinit (lambda ()))))
    (funcall deinit)))


(defun noether--teardown-views (view)
  "Tear down the given VIEW to avoid any zombie watcher or timer n stuff."
  (let ((name (noether--view-get view :name)))
    (mapc #'noether--teardown-unit (noether--view-get view :units))
    (mapc #'funcall (get name :watcher-removers))
    (kill-buffer
     (noether--view-get view :buffer (format "*%s*" name)))
    (funcall (noether--view-get view :deinit (lambda ())))))


(define-minor-mode noether-global-mode
  "A minor mode that keep tracks of different status blocks.
It reports them back in a status bar like frame."
  :global t
  :lighter " ST42"
  :keymap (make-sparse-keymap)
  (if noether-global-mode
      (mapc #'noether--setup-views noether-views)
    (mapc #'noether--teardown-views noether-views)))


(provide 'noether)
;;; noether.el ends here
