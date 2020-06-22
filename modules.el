;;; modules.el --- Organize your emacs config into small and loadable modules -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Sidharth Arya

;; Author: Sidharth Arya <sidhartharya10@gmail.com>
;; Maintainer: Sidharth Arya <sidhartharya10@gmail.com>
;; Created: 28 May 2020
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))
;; Keywords: startup
;; URL: https://github.com/SidharthArya/modules.el

;; This program is free software; you can redistribute it and/or modify
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
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA.

;;; Commentary:

;; modules.el package allows you to create custom configurations from .el files contained in a specific folder.
;; These .el files can be loaded if needed using `modules-load` function.
;; Or it can be loaded from the cli as `emacs --modules "space separated list of modules".
;; You can also specify module configurations

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

(defcustom modules-current-modules  '()
  "Path where all the modules are located."
  :group 'modules
  :type 'string)

(defun modules-command-line-args-process ()
  "Process the command line arguments."
  (interactive)
  (let ((config nil)
        (modules nil))
    (dolist (it command-line-args)
      (if (equal it "--config")
        (progn
          (setq command-line-args (delete it command-line-args))
          (setq config t))
        (if (equal config t)
          (progn
            (setq config it)
            (setq command-line-args (delete it command-line-args)))))
      (if (equal it "--modules")
        (progn
          (setq command-line-args (delete it command-line-args))
          (setq modules t))
        (if (equal modules t)
          (progn
            (setq modules it)
            (setq command-line-args (delete it command-line-args))))))
    (if (equal config nil)
      (setq config (symbol-name modules-config-default)))
    (modules-process (intern config))
    (if modules
      (modules-load (modules-string-to-list modules) 1))))

(defun modules-process (arg)
  "Processing various modules from the cli.
ARG is the selected modules config."
  (let ((modules nil))
    (dolist (it modules-config-list)
      (if (equal (car it) arg)
        (setq modules (car (cdr it)))))
    (if modules
      (modules-load modules 1))))

(defun modules-string-to-list (module)
  "Convert provided MODULE from string to list."
  (mapcar #'intern (split-string module)))

(defun modules-load (modules &optional force)
  "Function to load modules.
MODULES is the list of modules to be loaded.
If not speced, the function would ask for a space separated list of modules.
FORCE is a prefix argument."
  (interactive (list (modules-string-to-list (read-string "Modules: "))
                     (prefix-numeric-value current-prefix-arg)
                    ))
  (message "%s" force)
  (dolist (module modules)
    (let* ((module-name (symbol-name module))
           (module-full (concat modules-path "/" module-name)))
      (if (or (equal force 1) (not (member module-name modules-current-modules)))
          (progn
          (load module-full)
          (add-to-list 'modules-current-modules module-name)
          (message "[Module]: %s" module-name))
        (message "[Module]: Already loaded %s" module-name)
        ))))

(defun modules-loaded-p (modules)
  "Check whether a list of MODULES have been loaded."
  (if (listp modules)
       (not (member nil (mapcar #'modules-loaded-module-p modules)))
    (modules-loaded-module-p modules)
    ))

(defun modules-loaded-module-p (module)
  "Check whether a MODULE is loaded."
      (member (symbol-name module) modules-current-modules)
  )
(provide 'modules)

;;; modules.el ends here
