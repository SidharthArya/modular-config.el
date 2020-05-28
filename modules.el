;; modules.el
(defvar modules-config-list '(
				    (none ())
				    )
  "List of module configs in the format: (module-config (list of modules))
Use (add-to-list 'modules-config-list '(main (core appearance)))")

(defvar modules-config-default 'none
  "The default modules-config for Emacs.")
(defvar modules-path "~/.emacs.d/lisp"
  "Path where all the modules are located.")

(defun command-line-args-process ()
  "Process the command line arguments
   Use emacs --config none --modules \"list of modules\""
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
	  (modules-load (mapcar 'intern (split-string modules))))))

      (defun modules-process (arg)
      "Processing various modules from the cli"
      (let ((modules nil))
      (dolist (it modules-config-list)
      (if (equal (car it) arg)
		(setq modules (car (cdr it)))))
      (if modules
      (modules-load modules))
      ))

      (defun modules-load (&optional modules)
	"Load modules"
	(interactive)
	(if (not modules)
	    (setq modules (mapcar 'intern (split-string (read-string "Modules: ")))))
      (dolist
	  (module modules)
	(load (concat modules-path "/" (symbol-name module)))
	(message "[Module]: %s" (symbol-name module))
	))
(provide 'modules)
