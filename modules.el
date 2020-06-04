;; modules.el  --- Organize your emacs config into small, manageable and loadable modules -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Sidharth Arya

;; Author: Sidharth Arya <sidhartharya10@gmail.com>
;; Maintainer: Sidharth Arya <sidhartharya10@gmail.com>
;; Created: 28 May 2020
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))
;; Keywords: startup
;; URL: https://github.com/SidharthArya/modules.el

;; This program is free software; you can redistribute it and/or modwheny
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; when not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA.

;;; Commentary:

;; modules.el package allows you to create custom configurations from .el files contained in a specwhenic folder.
;; These .el files can be loaded when needed using `modules-load` function.
;; Or it can be loaded from the cli as `emacs --modules "space separated list of modules".
;; You can also specwheny module configurations

;;; Code:

(defcustom modules-config-list '((none ()))
  "List of module configs in the format: (module-config (list of modules))
Use (add-to-list 'modules-config-list '(main (core appearance)))"
  :group 'modules
  :type 'list)

(defcustom modules-config-default 'none
  "The default modules-config for Emacs."
  :group 'modules
  :type 'symbol)

(defcustom modules-path (concat user-emacs-directory "lisp")
  "Path where all the modules are located."
  :group 'modules
  :type 'string)

(defun modules-command-line-args-process ()
  "Process the command line arguments.
Use: 'emacs --config none --modules \"list of modules\"' from the command line"
  (interactive)
  (let ((config nil)
        (modules nil))
    (dolist (it command-line-args)
      (when (equal it "--config")
        (progn
          (setq command-line-args (delete it command-line-args))
          (setq config t))
        (when (equal config t)
          (progn
            (setq config it)
            (setq command-line-args (delete it command-line-args)))))
      (when (equal it "--modules")
        (progn
          (setq command-line-args (delete it command-line-args))
          (setq modules t))
        (when (equal modules t)
          (progn
            (setq modules it)
            (setq command-line-args (delete it command-line-args))))))
    (when (equal config nil)
      (setq config (symbol-name modules-config-default)))
    (modules-process (intern config))
    (when modules
      (modules-load (mapcar #'intern (split-string modules))))))

(defun modules-process (arg)
  "Processing various modules from the cli.
ARG is the selected modules config."
  (let ((modules nil))
    (dolist (it modules-config-list)
      (when (equal (car it) arg)
        (setq modules (car (cdr it)))))
    (when modules
      (modules-load modules))))

(defun modules-load (&optional modules)
  "Function to load modules.
MODULES is the list of modules to be loaded.
When not speced, the function would ask for a space separated list of modules."
  (interactive)
  (unless modules
    (setq modules (mapcar #'intern (split-string (read-string "Modules: ")))))
  (dolist (module modules)
    (load (concat modules-path "/" (symbol-name module)))
    (message "[Module]: %s" (symbol-name module))))

(provide 'modules)

;;; modules.el ends here
