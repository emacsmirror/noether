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
(require 'ert)
(require 'noether)

(ert-deftest test-noether/update ()
  "Update should call the :f function and pass the state correctly."
  (let* ((f (lambda (_) (error "Expected" :type 'ex)))
         (view (list :foo (list :cell '(0 . 2) :unit (list :f f)))))

    (should-error (noether/update view :foo)
                  :type 'ex)
    ))


(provide 'noether-test)
;;; noether-test.el ends here
