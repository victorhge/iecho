;;; iecho-tests.el --- iecho's automatic-tests

;; Copyright (C) 2013 Victor Ren

;; Time-stamp: <2013-10-14 15:20:52 Victor Ren>
;; Author: Victor Ren <victorhge@gmail.com>
;; Version: 0.1
;; X-URL: http://www.emacswiki.org/emacs/todo

;; This file is not part of GNU Emacs, but it is distributed under
;; the same terms as GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is part of iecho.

;;; Code:
(require 'ert)
(require 'iecho)

(ert-deftest iecho-compile-test ()
  (let ((byte-compile-error-on-warn t ))
    (should (byte-compile-file "~/.emacs.d/site-lisp/iecho/iecho.el"))
    (delete-file "~/.emacs.d/site-lisp/iecho/iecho.elc" nil)))

(ert-deftest iecho-mode-base-test ()
  )

(setq elp-function-list '(tooltip-show
                          tooltip-hide
                          buffer-substring
                          ))

;;; iecho-tests.el ends here
