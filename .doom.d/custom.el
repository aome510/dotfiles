;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(custom-safe-themes
;;    '("b7e460a67bcb6cac0a6aadfdc99bdf8bbfca1393da535d4e8945df0648fa95fb" "4a8d4375d90a7051115db94ed40e9abb2c0766e80e228ecad60e06b3b397acab" default))
;;  '(package-selected-packages '(ripgrep lsp-mode)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(fixed-pitch ((t (:font #<font-spec nil nil monospace nil nil semi-light nil nil 16 nil nil nil nil> :weight unspecified :slant unspecified :width unspecified)))))
;; (put 'projectile-ripgrep 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f0eb51d80f73b247eb03ab216f94e9f86177863fb7e48b44aacaddbfe3357cf1" default))
 '(safe-local-variable-values
   '((org-download-image-html-width . 500)
     (org-format-latex-header . "\\documentclass{article}
\\usepackage[usenames]{color}
[PACKAGES]
[DEFAULT-PACKAGES]
\\usepackage{accents}
\\newcommand\\undervec[1]{\\underaccent{\\vec}{#1}}
\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}")
     (eval add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]test_output\\'")
     (eval add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]vendor\\'"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
