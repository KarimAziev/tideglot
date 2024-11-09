;;; tideglot.el --- Switch between tide and eglot in JS modes -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/tideglot
;; Version: 0.1.0
;; Keywords: convenience
;; Package-Requires: ((emacs "29.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Helpers for quickly switching between tide and eglot in JS modes

;; This package provides a convenient way to switch between `tide' and `eglot'
;; for JavaScript and TypeScript development in Emacs.

;;; Code:

(declare-function tide-eldoc-function "tide")
(declare-function eglot-shutdown "eglot")
(declare-function eglot-current-server "eglot")

(defcustom tideglot-backend 'tide
  "Backend used for TypeScript integration, either `tide' or `eglot'.

Specifies the backend to use for TypeScript and JavaScript
development.

The available options are:

- `tide': Use the Tide backend.

- `eglot': Use the Eglot backend.

Changing the value will automatically update the buffers and
setup according to the selected backend."
  :type '(radio (const :tag "tide" tide)
          (const :tag "eglot" eglot))
  :group 'tideglot)

(defcustom tideglot-tide-setup-function 'tide-setup
  "Function to set up Tide mode, typically `tide-setup'.

Function to set up Tide mode.

This function is called to initialize Tide mode in JavaScript and
TypeScript buffers. By default, it is set to `tide-setup', but
it can be customized to any function that performs the necessary
setup for Tide mode.

The function should take no arguments and perform all necessary
initialization steps, such as enabling `tide-mode', setting up
syntax checking, and configuring other related modes or hooks."
  :type 'function
  :group 'tideglot)

(defcustom tideglot-after-save-backend-hook nil
  "Hook run after saving the TypeScript integration backend choice.

A hook that runs after changing and optionally saving the TypeScript
integration backend.

Functions added to this hook will be executed after the backend is
changed and the choice is saved. Each function should accept no
arguments. This can be useful for performing additional setup or
cleanup tasks related to the backend change."
  :type 'hook
  :group 'tideglot)


(defcustom tideglot-flymake-eslint-backend 'flymake-eslint-enable
  "Function to enable or disable Flymake ESLint backend.

A function or nil to enable Flymake with ESLint in Eglot-managed
buffers.

If set to a function, it will be called to enable Flymake with
ESLint when Eglot is activated. The function should take no
arguments and configure Flymake to use ESLint for linting.

If set to nil, Flymake will not be configured to use ESLint."
  :type '(radio
          (function :tag "Function")
          (const :tag "None" nil))
  :group 'tideglot)

(defcustom tideglot-js-modes '(js-mode
                               js-ts-mode
                               js-jsx-mode
                               tsx-ts-mode
                               typescript-ts-mode
                               typescript-mode)
  "List of JavaScript for enabling or disabling Tide and Eglot.

A list of major modes for JavaScript and TypeScript files where
Eglot should be enabled or disabled.

Each element in the list should be a symbol representing a major
mode, such as `js-mode', `typescript-mode', or their tree-sitter
equivalents.

This list is used by functions that manage the
activation of Eglot and Tide in the specified modes."
  :group 'tideglot
  :type 'boolean)


(defun tideglot-maybe-maybe-set-mode (mode value)
  "Set minor MODE to VALUE.

If VALUE is positive integer activate MODE if it is not active.

If VALUE is negative integer deactivate MODE if it is active."
  (cond ((and
          (< value 0)
          (symbol-value mode))
         (funcall mode -1))
        ((and (> value 0)
              (not (symbol-value mode)))
         (funcall mode 1))))

(defun tideglot-set-tide-mode (value)
  "Set up or tear down Tide-related modes based on the VALUE provided.

Argument VALUE is an integer that determines whether to enable or disable modes."
  (pcase value
    ((pred (<= 0))
     (require 'tide)
     (when (fboundp tideglot-tide-setup-function)
       (funcall tideglot-tide-setup-function)))
    (_
     (tideglot-maybe-maybe-set-mode 'tide-mode value)
     (tideglot-maybe-maybe-set-mode 'tide-hl-identifier-mode value)
     (tideglot-maybe-maybe-set-mode 'flycheck-mode value)
     (remove-hook 'eldoc-documentation-functions #'tide-eldoc-function t))))

(defun tideglot-eglot-ensure-with-eslint ()
  "Ensure Eglot is running and configure Flymake to use ESLint."
  (require 'eglot)
  (make-local-variable 'eglot-stay-out-of)
  ;; (add-to-list 'eglot-stay-out-of 'flymake-diagnostic-functions)
  (add-hook 'eglot-managed-mode-hook tideglot-flymake-eslint-backend nil t)
  (eglot-ensure))

(defun tideglot-set-eglot-mode (value)
  "Set up or shut down Eglot mode based on the provided VALUE.

Argument VALUE determines whether to enable or disable Eglot mode."
  (pcase value
    ((pred (<= 0))
     (require 'eglot)
     (make-local-variable 'eglot-stay-out-of)
     (add-to-list 'eglot-stay-out-of 'flymake-diagnostic-functions)
     (eglot-ensure)
     (when tideglot-flymake-eslint-backend
       (funcall tideglot-flymake-eslint-backend)))
    (_
     (when-let* ((server
                 (when (fboundp 'eglot-current-server)
                   (eglot-current-server))))
       (tideglot-maybe-maybe-set-mode 'flymake-mode -1)
       (ignore-errors (eglot-shutdown server nil nil nil))))))

(defun tideglot-enable-eglot-in-js-modes ()
  "Enable Eglot and ESLint in specified JavaScript and TypeScript modes."
  (dolist (mode tideglot-js-modes)
    (when-let* ((hook (intern-soft (concat (symbol-name mode) "-hook"))))
      (remove-hook hook tideglot-tide-setup-function)
      (add-hook hook #'tideglot-eglot-ensure-with-eslint))))

(defun tideglot-disable-eglot-in-js-modes ()
  "Disable Eglot and enable Tide in JavaScript and TypeScript modes."
  (dolist (mode tideglot-js-modes)
    (when-let* ((hook (intern-soft (concat (symbol-name mode) "-hook"))))
      (remove-hook hook #'tideglot-eglot-ensure-with-eslint)
      (add-hook hook tideglot-tide-setup-function))))

(defun tideglot-update-buffers (newval)
  "Update JavaScript buffers to use either Tide or Eglot based on NEWVAL.

Argument NEWVAL is the new value to set for the buffer modes."
  (dolist (buff (buffer-list))
    (when (and (buffer-live-p buff))
      (when (memq (buffer-local-value 'major-mode buff) tideglot-js-modes)
        (with-current-buffer buff
          (pcase newval
            ('tide (progn (tideglot-set-eglot-mode -1)
                          (tideglot-set-tide-mode 1)))
            ('eglot (tideglot-set-tide-mode -1)
                    (tideglot-eglot-ensure-with-eslint))))))))


;;;###autoload
(defun tideglot-backend-watcher (_symbol newval &rest _)
  "Watch for change in the backend and update JavaScript buffers accordingly.

Argument NEWVAL is the new value to determine whether to enable Eglot or Tide."
  (pcase newval
    ('eglot (tideglot-enable-eglot-in-js-modes))
    ('tide (tideglot-disable-eglot-in-js-modes)))
  (tideglot-update-buffers newval))


;;;###autoload
(defun tideglot-change-backend (symb)
  "Change the TypeScript integration backend and optionally save the choice.

Argument SYMB is the new backend symbol to switch to."
  (interactive (list
                (let
                    ((options
                      (copy-tree
                       (mapcar #'cddr (cdr
                                       (get 'tideglot-backend 'custom-type))))))
                  (cadr (assoc
                         (completing-read
                          (format "Change %s to "
                                  tideglot-backend)
                          options
                          (lambda (it)
                            (not (eq (cadr it) tideglot-backend))))
                         options)))))
  (if (not (and (not noninteractive)
                (yes-or-no-p "Save for future?")))
      (setq tideglot-backend symb)
    (customize-save-variable 'tideglot-backend symb)
    (run-hooks 'tideglot-after-save-backend-hook))
  (tideglot-backend-watcher nil symb))

;; (add-variable-watcher 'tideglot-backend
;;                       'tideglot-backend-watcher)

(provide 'tideglot)
;;; tideglot.el ends here