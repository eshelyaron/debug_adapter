;;; dap-swi-prolog.el --- Debug Adapter Protocol mode for SWI-Prolog      -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Eshel Yaron

;; Author: Eshel Yaron <eshelshay.yaron@gmail.com>
;; Keywords: languages

;; Version: 0.1.0

;;; Commentary:

;;; Code:

(require 'dap-mode)

(defcustom dap-swi-prolog-debug-program
    `(,(locate-file "swipl_debug_adapter" exec-path))
  "The path to the SWI-Prolog debug adapter."
  :group 'dap-swi-prolog
  :type '(repeat string))

(defun dap-swi-prolog--populate-start-file-args (conf)
  "Populate CONF with the required arguments."
  (let ((conf (-> conf
                  (dap--put-if-absent :dap-server-path dap-swi-prolog-debug-program)
                  (dap--put-if-absent :type "swi-prolog")
                  (dap--put-if-absent :cwd default-directory)
                  (dap--put-if-absent :module (buffer-file-name))
                  (dap--put-if-absent :goal (read-string "Goal: " nil nil "true"))
                  (dap--put-if-absent :name "SWI-Prolog Debug"))))
    conf))

(dap-register-debug-provider "swi-prolog" #'dap-swi-prolog--populate-start-file-args)

(dap-register-debug-template "SWI-Prolog Run Configuration"
                             (list :type "swi-prolog"
                                   :request "launch"
                                   :name "SWI-Prolog::Run"))

(provide 'dap-swi-prolog)
;;; dap-swi-prolog.el ends here
