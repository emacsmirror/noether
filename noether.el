;;; noether.el --- A modeline which plays hide and seek  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2023-2025 Sameer Rahmani <lxsameer@gnu.org>
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
(require 'seq)
(require 'posframe)
(require 'subr-x)

(defgroup noether nil
  "The customization group for the noether-mode."
  :group 'convenience)

;; ============================================================================
;; Vars
;; ============================================================================
(eval-when-compile
  (defvar noether-global-mode-map))


(defvar noether-views ()
  "A list of views that noether should manage.

You should adding your views to this var, so noether can activate them
on demand.")


(defvar noether-on-buffer-change-hook ()
  "A hook that runs whenever noether detects focus change on buffers.")


(defvar noether--visible-sticky-views ()
  "An internal list to index the views that must be `visible' and `sticky'.")

(defvar noether--hide-when-minibuffer ()
  "An internal list to index the views that must hide when minibuffer is active.")

(defun noether--adjust-modeline (view)
  "Adjust the VIEW after parent frame resize."
  (noether-show view))

(defvar noether--view-schema
  (list
   :managed? t
   :buffer "*exwm-status*"
   :binding (kbd "C-c 0")
   :separator " "
   :visible? t
   :sticky t
   :timeout 0
   :hide-when-minibuffer? t
   :on-parent-resize #'noether--adjust-modeline
   :frame '()
   :units '()))


(defvar noether--frame-defaults
  (list
   :min-height 1
   :min-width 10
   ;;:position '(0 . 0)
   :border-width 0
   :accewpt-focus nil
   ;;:timeout 10
   :refresh 1))


;; ============================================================================
;; Macros
;; ============================================================================
(defmacro explanation (_ &rest block)
  "A simple macro to add a docstring DOC for a BLOCK of code."
  (declare (indent defun) (doc-string 1))
  `(progn ,@block))


(defmacro noether--unit-get (unit key &optional default)
  "Return the value of the KEY in UNIT or the DEFAULT value if it doesn't exist."
  `(or (plist-get ,unit ,key) ,default))


(defmacro noether--view-get (view key &optional default)
  "Return the value of the KEY in VIEW or the DEFAULT value if it doesn't exist."
  `(or (plist-get ,view ,key) ,default))


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

       (noether-defunit ,name
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


(defmacro noether-defview (name docs &rest body)
  "Create a new view with the given NAME with the given DOCS and BODY.
BODY will be parsed in a way that any starting pair of keyword and value
will be used as the view properties and the rest will be the body of
the show function."
  (declare (doc-string 2) (indent defun))
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


(defmacro noether-defunit (name docs &rest props)
  "Define a unit with the given NAME, DOCS and a set of PROPS.
It will define a function with the given NAME that accepts any
parameter in form of key/values that will override any original
key/value from the original definition."

  (declare (doc-string 2) (indent defun))
  (let* ((parsed-body (noether--extract-props props))
         ;; For now we don't have any use for the body
         (_ (car parsed-body))
         (orig-props (cdr parsed-body)))

    `(defun ,name (&rest f-props)
       ,docs
       (append '(:name ,(intern (format ":%s" (symbol-name name))))
               f-props (list ,@orig-props)))))

;; ============================================================================
;; Helper functions
;; ============================================================================
(defun noether--buffer-focus-change-runner (_)
  "Run a certain hook whenever it detects that focus has change to another buffer.
Users can add a function to the `noether-on-buffer-change-hook' hook to run
some arbitary buffer related code when a focus change event happens."
  (run-hooks 'noether-on-buffer-change-hook))


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
      (put name :posframe (apply #'posframe-show params)))))


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
           (lambda () (string-pad txt len))))))))


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

    (when var
      ;; Setup the watcher and the watcher remover
      (add-variable-watcher var updater)
      (put view-name :watcher-removers
           (cons (lambda ()
                   ;; We will call this function later during
                   ;; the teardown process
                   (remove-variable-watcher var updater))
                 (get view-name :watcher-removers))))

    (when init-fn
      (funcall init-fn))


    (put view-name :updaters
         (cons updater (get view-name :updaters)))

    ;; Move the point to the location of the next unit
    (+ end-point (length (or sep "")))))


(defun noether--reset-view-state (view)
  "Reset the state stored in VIEW.
E.g. the updaters list."
  (put (noether--view-get view :name) :updaters nil))


(explanation
  "In some views that `:visible?' is set to t, the view might have some overlaps
with the minibuffer. For example a modeline similar to `mini-mode-line' that
draws a view on the echo area. For these types of views user can set the value
of `:hide-when-minibuffer?' to t. This way we can use the following functions
and add them to the `minibuffer-setup-hook' and `minibuffer-exit-hook' to hide and
show the views that we already collected at setup time."

  (defun noether--on-minibuffer-enter ()
    "Hide views that set `:hide-when-minibuffer?' to t when entering minibuffer."
    (mapc
     (lambda (view)
       (with-current-buffer (noether--view-get view :buffer)
         (posframe--make-frame-invisible posframe--frame)))

     noether--hide-when-minibuffer))


  (defun noether--on-minibuffer-exit ()
    "Show views that set `:hide-when-minibuffer?' to t when exiting minibuffer."
    (mapc
     (lambda (view)
       (with-current-buffer (noether--view-get view :buffer)
         (posframe--make-frame-visible posframe--frame)))

     noether--hide-when-minibuffer)))


(defun noether--frame-focus-changed ()
  "Check for any view that is defined as `stick' and `visible' and raise them."
  ;; `selected-frame' can be different than the actual focused frame frome the X
  ;; perspective. Soo we need to figure out which frame has the focus, and re-parent
  ;; our `visible' and `sticky' views (technically the frame of a view) and
  ;; raise them
  (dolist (frame (frame-list))
    (pcase (frame-focus-state frame)
      (`t (mapc
           (lambda (view)
             (with-current-buffer (noether--view-get view :buffer)
               ;; `posframe--frame' is buffer local, so it is safe
               ;; to raise it
               (set-frame-parameter posframe--frame 'parent-frame frame)
               (raise-frame posframe--frame)))
           noether--visible-sticky-views)))))


(defun noether--setup-views (view)
  "Setup the given VIEW by setting up its units."
  (when (not (listp view))
    (error (format "The given value as a view is not a list: %s" view)))

  (noether--reset-view-state view)

  (let ((name (noether--view-get view :name))
        (binding (noether--view-get view :binding))
        (visible (noether--view-get view :visible?))
        (sticky (noether--view-get view :sticky?)))

    (when (not (null binding))
      (define-key noether-global-mode-map binding
                  (lambda () (interactive) (noether-show view))))

    (when visible
      (noether-show view))

    ;; We need to take care of `visible' and `sticky' views
    ;; when the frame at focus changes. So let's index them
    (when (and visible sticky)
      (add-to-list 'noether--visible-sticky-views view))

    ;; If the view in question set the `:hide-when-minibuffer?' to t
    ;; then we need a handle to the view for the handler function
    ;; to take care of it.
    (when (noether--view-get view :hide-when-minibuffer?)
      (add-to-list 'noether--hide-when-minibuffer view))

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
  "Tear down the given UNIT by calling the `:deinit' fn.
It removes any possible watch function."
  (let ((deinit (noether--unit-get unit :deinit (lambda ()))))
    (funcall deinit)))


(defun noether-refresh (&optional _)
  "Refresh views.
We need to call this function when ever Emacs resized
or the font size changed."
  (interactive)
  (mapc
   (lambda (v)
     (let ((resize-handler (noether--view-get v :on-parent-resize)))
       (when (not (null resize-handler))
         (funcall resize-handler v))))
   noether-views))


(defun noether--teardown-views (view)
  "Tear down the given VIEW to avoid any zombie watcher or timer n stuff."
  (let ((name (noether--view-get view :name)))
    (mapc #'noether--teardown-unit (noether--view-get view :units))
    (mapc #'funcall (get name :watcher-removers))
    (kill-buffer
     (noether--view-get view :buffer (format "*%s*" name)))
    (funcall (noether--view-get view :deinit (lambda ())))))



(defun noether--enable ()
  "Enable noether by setting up each view and necessary hooks."
  (add-to-list 'window-buffer-change-functions #'noether--buffer-focus-change-runner)
  (add-to-list 'window-selection-change-functions #'noether--buffer-focus-change-runner)
  (add-to-list 'window-size-change-functions #'noether-refresh)
  (add-hook 'minibuffer-setup-hook #'noether--on-minibuffer-enter)
  (add-hook 'minibuffer-exit-hook #'noether--on-minibuffer-exit)

  (add-function :after after-focus-change-function #'noether--frame-focus-changed)
  ;; Technically the argument to the refresh function should be a `frame'
  ;; but since we are not using it and we have to keep it cuz
  ;; `window-size-change-functions' expects it, We just pass true.
  (mapc #'noether--setup-views noether-views))


(defun noether--disable ()
  "Disable noether and clean up after it."
  (delete #'noether--buffer-focus-change-runner window-buffer-change-functions)
  (delete #'noether--buffer-focus-change-runner window-selection-change-functions)
  (delete #'noether-refresh window-size-change-functions)

  (remove-hook 'minibuffer-setup-hook #'noether--on-minibuffer-enter)
  (remove-hook 'minibuffer-exit-hook #'noether--on-minibuffer-exit)

  (setq noether--visible-sticky-views nil)
  (remove-function after-focus-change-function #'noether--frame-focus-changed)

  (mapc #'noether--teardown-views noether-views))


;;;###autoload
(define-minor-mode noether-global-mode
  "A minor mode that keep tracks of different status blocks.
It reports them back in a status bar like frame."
  :global t
  :lighter " N"
  :group 'noether
  :keymap (make-sparse-keymap)
  (if noether-global-mode
      (noether--enable)
    (noether--disable)))


(provide 'noether)
;;; noether.el ends here
