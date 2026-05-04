;;; pif.el --- Prevent Initial Flash of Light -*- lexical-binding: t -*-

;; Copyright (C) 2025 Oliver Epper

;; Author: Oliver Epper <oliver.epper@gmail.com>
;; Maintainer: Oliver Epper <oliver.epper@gmail.com>
;; Created: 2025
;; Package-Version: 0.0.7
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/oliverepper/pif
;; Keywords: convenience, faces, display, startup, appearance, dark-theme

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; pif.el is designed to prevent the Initial Flash of Light when
;; starting up Emacs.
;;
;; After installing you need to call ~pif-early~ from ~early-init.el~.
;; This hides certain UI elements and sets the position and size for
;; the initial frame.  Next call ~pif APPEARANCE~, where APPEARANCE is
;; either 'light, or 'dark.  This ensures the correct colors are set
;; to prevent the "Initial Flash of Light".
;;
;; On macOS, if your Emacs distribution supports it, invoke this
;; function from the ~ns-system-appearance-change-functions~-hook to
;; automatically apply the current mode (light or dark) based on
;; system settings.
;;
;; In ~init.el~, call ~pif-reset~ to reset the colors and install
;; ~pif-update APPEARANCE~ in the ~kill-emacs-hook~.  This allows pif
;; to save the position, size of the initial-frame, and some colors
;; for the next start.
;;
;; Here's the exact setup that I use:
;;
;; In ~early-init.el~:
;;
;; (add-to-list 'load-path (expand-file-name (locate-user-emacs-file "local/pif")))
;; (require 'pif)
;; (pif-early)
;; (add-hook 'ns-system-appearance-change-functions #'pif)
;;
;;
;; In ~init.el~:
;;
;; (when (featurep 'pif)
;;   (pif-reset)
;;   (remove-hook 'ns-system-appearance-change-functions #'pif)
;;   (add-hook 'kill-emacs-hook (lambda ()
;;                                (pif-update ns-system-appearance))))
;;
;;; Code:

(defgroup pif ()
  "Prevent Initial Flash of Light."
  :group 'convenience)

;;;; User options

(defcustom pif-fallback-light-color "#efe9dd"
  "Fallback color used in light mode if no color was saved."
  :type 'color
  :group 'pif)

(defcustom pif-fallback-dark-color "#1d2235"
  "Fallback color used in dark mode if no color was saved."
  :type 'color
  :group 'pif)

(defcustom pif-enable t
  "Wether to enable or to disable pif."
  :type 'boolean
  :group 'pif)

;;;; Constants

(defconst pif-file-name ".pif.el"
  "Filename for storing PIF-related data within the `user-emacs-directory'.")

;;;; Public API

(defun pif-early ()
  "Hide UI-elements and prepare size and position of the initial frame.

This can be done without knowing if Emacs will start in `light', or
`dark' mode."
  (when pif-enable
      (pif--hide-ui-elements)
      (pif--prepare-frame)))

(defun pif (appearance)
  "Configure colors to prevent the \='Flash of Light\='.

APPEARANCE specifies whether to load the colors for `light' or `dark'
mode."
  (when pif-enable
    (pif--set-colors appearance)))

(defun pif-reset ()
  "Reset all colors to the values before `pif' was called."
  (when pif-enable
    (pif--reset-colors)))

(defun pif-update (appearance &optional frame)
  "Save or update colors, size, and position of the initial frame.

APPEARANCE specifies whether to save the colors for `light' or `dark'
mode.  Call this at your convenience.  The `kill-emacs-hook' might be a
good choice."
  (let* ((frame (or frame (car (visible-frame-list)))))
    (when (and frame (display-graphic-p frame))
      (pif--update-state 'colors appearance (frame-parameter frame 'background-color))
      (pif--update-state 'frame 'width      (frame-parameter frame 'width))
      (pif--update-state 'frame 'height     (frame-parameter frame 'height))
      (pif--update-state 'frame 'left       (frame-parameter frame 'left))
      (pif--update-state 'frame 'top        (frame-parameter frame 'top))
      (setf (alist-get 'width  default-frame-alist) (+ 2 (frame-parameter frame 'width)))
      (setf (alist-get 'height default-frame-alist) (+ 1 (frame-parameter frame 'height)))
      (setf (alist-get 'left   default-frame-alist) (frame-parameter frame 'left))
      (setf (alist-get 'top    default-frame-alist) (frame-parameter frame 'top)))))

;;;; Private Functions

(defun pif--hide-ui-elements ()
  "Hide UI-elements."
  (setq cursor-type nil)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (setq mode-line-format nil))

(defun pif--prepare-frame ()
  "Configure `default-frame-alist'."
  (let ((saved-state (alist-get 'frame (pif--read-state))))
    (when saved-state
      (setq default-frame-alist
            (append `(
                      (width                   . ,(+ 2 (alist-get 'width  saved-state)))
                      (height                  . ,(+ 1 (alist-get 'height saved-state)))
                      (userposition            . t)
                      (left                    . ,(alist-get 'left saved-state))
                      (top                     . ,(alist-get 'top  saved-state))
                      (ns-transparent-titlebar . t))
                    ())))))

(defun pif--set-colors (appearance)
  "Set colors according to APPEARANCE."
  (let* ((saved-state (alist-get 'colors (pif--read-state)))
         (bg (alist-get appearance saved-state
                        (pcase appearance
                          ('light pif-fallback-light-color)
                          ('dark  pif-fallback-dark-color)))))
    (when bg
      ;; default
      (pif--update-state 'colors 'default-background (intern (face-attribute 'default :background)))
      (pif--update-state 'colors 'default-foreground (intern (face-attribute 'default :foreground)))
      (set-face-attribute 'default nil :background bg :foreground bg)
      ;; fringe
      (pif--update-state 'colors 'fringe-background (face-attribute 'fringe :background))
      (pif--update-state 'colors 'fringe-foreground (face-attribute 'fringe :foreground))
      (set-face-attribute 'fringe nil :background bg :foreground bg))))

(defun pif--set-face-attribute (face attribute value)
  "Set the FACE ATTRIBUTE with VALUE.

Translates `upspecified-bg' and `unspecified-fg' to `unspecified'."
  (when value
    (set-face-attribute face nil attribute
                        (if (member value '(unspecified-bg unspecified-fg))
                            'unspecified
                          value))))

(defun pif--reset-colors ()
  "Reset colors to their original values."
  (let ((saved-state (alist-get 'colors (pif--read-state))))
    (when saved-state
      (let ((orig-default-background (alist-get 'default-background saved-state))
            (orig-default-foreground (alist-get 'default-foreground saved-state))
            (orig-fringe-background  (alist-get 'fringe-background  saved-state))
            (orig-fringe-foreground  (alist-get 'fringe-foreground  saved-state)))
        (pif--set-face-attribute 'default :background orig-default-background)
        (pif--set-face-attribute 'default :foreground orig-default-foreground)
        (pif--set-face-attribute 'fringe  :background orig-fringe-background)
        (pif--set-face-attribute 'fringe  :foreground orig-fringe-foreground)))))

(defun pif--read-state ()
  "Read the state from `pif-file-name' in the `user-emacs-directory'."
  (let ((pif-file (expand-file-name pif-file-name user-emacs-directory)))
    (if (file-exists-p pif-file)
        (with-temp-buffer
          (insert-file-contents pif-file)
          (condition-case nil
              (read (current-buffer))
            (error nil)))
      nil)))

(defun pif--write-state (state)
  "Save STATE to `pif-file-name' in the `user-emacs-directory'."
  (with-temp-file (expand-file-name pif-file-name user-emacs-directory)
    (insert (prin1-to-string state))))

(defun pif--update-state (key subkey value)
  "Update then save the VALUE for SUBKEY under KEY."
  (let* ((state (pif--read-state))
         (existing-sublist (alist-get key state))
         (updated-sublist (assoc-delete-all subkey existing-sublist)))
    (setq updated-sublist (cons (cons subkey value) updated-sublist))
    (setq state (assoc-delete-all key state))
    (push (cons key updated-sublist) state)
    (pif--write-state state)))

(provide 'pif)
;;; pif.el ends here
