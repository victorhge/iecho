;;; iecho.el --(Inputs ECHO) visualize input events and corresponding commands

;; Copyright (C) 2013 Victor Ren
;;

;; Author: Victor Ren <victorhge@gmail.com>
;; Version: 0.1
;; Keywords: input events command visualize echo

;; This file is *NOT* (yet) part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package is a minor mode which visualizes what you types and
;; corresponding commands. It is useful for feature demos and trainings.  Your
;; audience sees the keys and commands. They will understand your demonstration
;; better.
;;
;;; Installation:

;; Put in your .emacs or site-start.el file the following lines:
;;   (require 'iecho)
;; then
;;   M-x iecho-mode

;;; Compatibility

;; The package is only tested with Cygwin Emacs 24.3.

;; How it works:
;;  Echo input into a buffer in `pre-command-hook'. Show functions that runs in
;; `post-command-hook' display the contents in different ways.

;;; todo
;; * iecho-buffer-mode
;; ** iecho-buffer-mode limits
;; ** keywords
;; **
;; * customize overlay
;; ** face
;; ** position (not only around the point)
;; **
;; * problem
;; ** problems in terminal
;;
;;; Code:

;; todo remove require
(require 'dframe)
(require 'popup)

(defgroup iecho nil
  "Display input events"
  :prefix "iecho-"
  :group 'convenience)

(defcustom iecho-show-function-default 'iecho-in-window
  "The default function to show the input events."
  :type 'function
  :options '(iecho-in-window iecho-in-frame iecho-in-tooltips iecho-in-overlay)
  :group 'iecho)

(defcustom iecho-show-functions '(iecho-in-window iecho-in-frame iecho-in-tooltips iecho-in-overlay)
  "Available show functions."
  :type 'list
  :group 'iecho)

(defcustom iecho-frame-parameters '((minibuffer . nil)
                                    (width . 30)
                                    ;; (height . 20)
                                    (top . 0)
                                    (left . (- 0))
                                    (border-width . 0)
                                    (menu-bar-lines . 0)
                                    (tool-bar-lines . 0)
                                    (unsplittable . t)
                                    (left-fringe . 0)
                                    (right-fringe . 0)
                                    (mode-line . 0)
                                    )
  "Parameters to use when creating the iecho frame in Emacs.
Any parameter supported by a frame may be added."
  :group 'iecho
  :type '(repeat (cons :format "%v"
                       (symbol :tag "Parameter")
                       (sexp :tag "Value"))))

;; todo
(defcustom iecho-overlay-position nil
  "nil means following the cursor."
  :type 'symbol
  :options '(up-right bottom-right)
  :group 'iecho)

(defcustom iecho-lines 1
  "The number of lines of commands to display."
  :type 'integer
  :group 'iecho)

(defface iecho-overlay-face
  '((t (:background "yellow" :foreground "black")))
  "Face for iecho-overlay."
  :group 'iecho)

(defvar iecho-frame nil
  "The frame displaying iecho buffer.")
(defvar iecho-cached-frame nil
  "The frame that was last created, then removed from the display.")
(defvar iecho-buffer nil
  "The buffer for echoing input.")
(defvar iecho-show-function iecho-show-function-default
  "The function to show the input events.")
(defvar iecho-overlay nil
  "The overlay to display the latest commands." )

;;;###autoload
(define-minor-mode iecho-mode
  "Toggle iecho mode.

See also `iecho-show-function'."
  :init-value nil
  :lighter " iecho"
  :global   t
  :group  'iecho
  (if iecho-mode
      (iecho-mode-on)
    (iecho-mode-off)))

(define-derived-mode iecho-buffer-mode fundamental-mode "iecho"
  "Major mode for managing a display of input events and commands.

\\{iecho-buffer-mode-map}"
  (save-excursion
    (setq font-lock-keywords nil) ;; no font-locking please
    (setq truncate-lines nil)
    (make-local-variable 'truncate-partial-width-windows)
    (setq truncate-partial-width-windows nil)
    (make-local-variable 'frame-title-format)
    (setq frame-title-format "iecho"
          case-fold-search nil
          buffer-read-only t)
    (setq buffer-modified-p nil)
    (setq buffer-read-only t)       ; disable undo
    (setq buffer-undo-list t))
  iecho-buffer)

(easy-mmode-defmap iecho-buffer-mode-map
                   '(("\C-c\C-n" . iecho-show-cycling))
                   "Iecho mode keymap."
                   :group 'iecho)

(defun iecho-show-cycling ()
  "Switch to next show function in `iecho-show-functions' to echo input events."
  (interactive)
  (iecho-mode-off)
  (setq iecho-show-function (car (or (cdr (member iecho-show-function iecho-show-functions))
                                 iecho-show-functions)))
  (iecho-mode-on))

(defun iecho-this-command ()
  "Show the input event and command."
  (with-current-buffer iecho-buffer
    (let ((deactivate-mark nil) ; do not deactivate mark in transient-mark-mode
          (inhibit-read-only t)
          (inhibit-modification-hooks t)
          (auto-window-vscroll 1))
      (goto-char (point-max))
      (if (and (eq this-command 'self-insert-command)
               (eq last-command 'self-insert-command))
          nil
        (insert "\n"))
      (insert (mapconcat (lambda (key)
                           (if (or (integerp key) (symbolp key) (listp key))
                               (single-key-description key)
                             (prin1-to-string key nil)))
                         (this-command-keys-vector)
                         " "))
      (insert (if (eq this-command 'self-insert-command)
                  ""
                (concat "  " (if (symbolp this-command)
                                 (symbol-name this-command)
                               (prin1-to-string this-command nil)))))
      )))

(defun iecho-in-window ()
  "Show the input event and command in an separate window."
  (let ((split-width-threshold 0)    ; control how `display-buffer' to split the window
        (split-height-threshold nil))
    (display-buffer iecho-buffer '((display-buffer-reuse-window
                                    display-buffer-pop-up-window)
                                   (window-height . 0.2)
                                   (window-width . 0.3)
                                   ))
    (with-current-buffer iecho-buffer
      (set-window-point (get-buffer-window) (line-beginning-position)))))

;; (setq iecho-show-function 'iecho-in-frame)

(defun iecho-in-frame ()
  "Show the input event and command in an separate frame."
  (with-selected-frame iecho-frame
    (set-window-point (get-buffer-window iecho-buffer) (point-max))))

;; (setq iecho-show-function 'iecho-in-tooltips)
;; todo: too slow
(defun iecho-in-tooltips ()
  "Show input events in tooltips"
  (tooltip-show (with-current-buffer  iecho-buffer
                  (buffer-substring (line-beginning-position -1) (point-max)))))

;; (setq iecho-show-function 'iecho-in-overlay)
(defun iecho-in-overlay ()
  "Show input events and commands in overlay"
  (popup-delete iecho-overlay)
  (let* ((list (iecho-get-commands-list))
         (width (car list))
         (commands-list (cdr list)))
    (setq iecho-overlay (popup-create (point) width iecho-lines
                                      :around t
                                      :face 'iecho-overlay-face
                                      :margin-left 1
                                      :margin-right 1
                                      ))
    (popup-set-list iecho-overlay commands-list)
    (popup-draw iecho-overlay)))

(defun iecho-get-commands-list ()
  " convert buffer-string to list for popup-set-list."
  (let ((commands-list nil)
        (done nil)
        (width 0)
        (i 0))
    (with-current-buffer iecho-buffer
      (save-excursion
        (goto-char (point-max))
        (while (and (< i iecho-lines)  (not done))
          (when (> (- (line-end-position)(line-beginning-position)) width)
            (setq width (- (line-end-position)(line-beginning-position))))
          (setq commands-list (cons (buffer-substring (line-beginning-position) (line-end-position)) commands-list))
          (if (/= 0 (forward-line -1))   ; the first line
              (setq done t)
            (setq i (1+ i)))
          )))
    (cons width commands-list)))

(defun iecho-mode-on ()
  "Turn on iecho mode"
  (interactive)
  (when (not (display-graphic-p))
    (setq iecho-show-functions '(iecho-in-window iecho-in-overlay))
    (setq iecho-show-function 'iecho-in-window))
  (unless (and iecho-buffer (buffer-live-p iecho-buffer))
    (setq iecho-buffer (get-buffer-create " *iecho*"))
    (with-current-buffer iecho-buffer
      (iecho-buffer-mode)))
  (when (eq iecho-show-function 'iecho-in-frame)
    (let ((old-frame (selected-frame))
          (old-buffer (current-buffer)))
      (dframe-frame-mode nil
                         'iecho-frame
                         'iecho-cached-frame
                         'iecho-buffer
                         "iecho"
                         #'iecho-mode
                         iecho-frame-parameters
                         )
      (when (frame-live-p old-frame)
        (raise-frame old-frame))
      (when (buffer-live-p old-buffer)
        (set-buffer old-buffer))))
  (add-hook 'pre-command-hook 'iecho-this-command)
  (add-hook 'post-command-hook iecho-show-function))

(defun iecho-mode-off ()
  "Turn off iecho mode"
  (interactive)
  (remove-hook 'pre-command-hook 'iecho-this-command)
  (remove-hook 'post-command-hook iecho-show-function)

  (let (( window (get-buffer-window iecho-buffer)))
    (if window
      (delete-window window)))
  (when (frame-live-p iecho-frame)
    (delete-frame iecho-frame))
  (popup-delete iecho-overlay))


(provide 'iecho)

;;; iecho.el ends here.
