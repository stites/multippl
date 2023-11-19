;;; yodel-ts-mode.el --- yodel ts mode             -*- lexical-binding: t; -*-

(require 'treesit)
(require 'sgml-mode)

(setq yodel-ts-mode--keywords
  '("fn" "if" "then" "else" "let" "in" "sample" "exact" "while" )
  )

(setq yodel-ts-mode--keywords-builtins
  '("flip" "normal" "beta" "discrete" "uniform" "normal" "dirichlet" "observe" "iterate")
  )

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html
(setq yodel-ts-font-lock-rules
  `(;; yodel font locking
    :feature delimiter
    :language yodel
    ([ "{" "}" "[" "]" "(" ")" ] @font-lock-bracket-face
     [ "~" "," ";" "<-" "->" ":" "==" ] @font-lock-delimiter-face
     )

   :feature keywords
   :language yodel
   ([,@yodel-ts-mode--keywords] @font-lock-keyword-face)

   :feature builtins
   :language yodel
   ([,@yodel-ts-mode--keywords-builtins] @font-lock-builtin-face)

    :feature types
    :language yodel
    ((ety) @font-lock-type-face
     (sty) @font-lock-type-face
     )

    :feature constants
    :language yodel
    ((float) @font-lock-constant-face
     (bool) @font-lock-constant-face
     (int) @font-lock-constant-face
     )

    :feature comments
    :language yodel
    ((comment) @font-lock-comment-face)

    :feature functions
    :language yodel
    ((efun (identifier) @font-lock-function-name-face)
     (sfun (identifier) @font-lock-function-name-face)
     (sapp (identifier) @font-lock-function-call-face)
     (eapp (identifier) @font-lock-function-call-face)
     )

    :feature variables
    :language yodel
    ((eanf (identifier) @font-lock-variable-use-face)
     (sanf (identifier) @font-lock-variable-use-face)
     (sarg (identifier) @font-lock-variable-name-face)
     (earg (identifier) @font-lock-variable-name-face)
     (eanfprj (identifier) @font-lock-variable-use-face)
     (sanfprj (identifier) @font-lock-variable-use-face)
     (slet       (identifier) @font-lock-variable-name-face)
     (elet       (identifier) @font-lock-variable-name-face)
     (sletsample (identifier) @font-lock-variable-name-face)
     )
  ))

(defun yodel-ts-setup ()
  "Setup for `yodel-ts-mode'."
  (interactive)
  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules
                     yodel-ts-font-lock-rules))
  (setq-local font-lock-defaults nil)
  (setq-local treesit-font-lock-feature-list
              '(
                (comments)
                ;(blocks)
                (builtins)
                (keywords)
                (constants types)
                (variables)
                (functions)
                (delimiter)
                ;(functions variables)
                ))
  ;; (setq-local treesit-simple-imenu-settings
  ;;             `(("Heading" yodel-ts-imenu-node-p nil yodel-ts-imenu-name-function)))

  (setq-local treesit-font-lock-level 10)
  ;; (setq-local treesit-simple-indent-rules
  ;;             `((yodel
  ;;                ;; ((parent-is "fragment") parent-bol 0)
  ;;                ;; ((node-is ,(regexp-opt '("element" "self_closing_tag"))) parent 2)
  ;;                ;; ((node-is "end_tag") parent 0)
  ;;                ;; ((node-is "/") parent 0)
  ;;                ;; ((parent-is "element") parent 2)
  ;;                ;; ((node-is "text") parent 0)
  ;;                ;; ((node-is "attribute") prev-sibling 0)
  ;;                ;; ((node-is ">") parent 0)
  ;;                ;; ((parent-is "start_tag") prev-sibling 0)
  ;;                ;; (no-node parent 0)
  ;;                )))
  (treesit-major-mode-setup))

;;;###autoload
(define-derived-mode yodel-ts-mode sgml-mode "yodel[ts]"
  "Major mode for editing yodel."
  :syntax-table sgml-mode-syntax-table
  (setq-local font-lock-defaults nil)
  (when (treesit-ready-p 'yodel)
    (treesit-parser-create 'yodel)
    (yodel-ts-setup)))

(provide 'yodel-ts-mode)
;;; yodel-ts-mode.el ends here
