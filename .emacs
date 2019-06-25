(setq show-paren-style 'expression)
(show-paren-mode t)

(package-initialize)
(exec-path-from-shell-copy-env "LIBRARY_PATH")

(add-to-list 'load-path "~/.emacs.d/page-break-lines-master")
(load-file "~/.emacs.d/page-break-lines-master/page-break-lines.el")

(add-to-list 'load-path "~/.emacs.d/emacs-dashboard-master")
(load-file "~/.emacs.d/emacs-dashboard-master/dashboard.el")


(require 'dashboard)
(dashboard-setup-startup-hook)


;; recentf stuff
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key [f9] 'recentf-open-files)



(defun compile-cc ()
  (interactive)
  (defvar foo)
  (setq foo (concat "mpicc -o "(file-name-sans-extension (file-name-nondirectory (buffer-file-name))) " \"" (buffer-file-name)"\""))
  (setq compile-command foo)
  (call-interactively 'compile))

(defun compile-f ()
  (interactive)
  (defvar foo)
  (setq foo (concat "mpif90 -o "(file-name-sans-extension (file-name-nondirectory (buffer-file-name))) " \"" (buffer-file-name)"\""))
  (setq compile-command foo)
  (call-interactively 'compile))

(defun compile-f-lib ()
  (interactive)
  (defvar foo)
  (setq foo (concat "mpif90 -o "(file-name-sans-extension (file-name-nondirectory (buffer-file-name))) " \"" (buffer-file-name) "\" -fopenmp -lC2 -I ~/.local/share/Library"))
  (setq compile-command foo)
  (call-interactively 'compile))

(defun run-program ()
  (interactive)
  (defvar foo)
  (setq foo (concat "mpirun -np 2 ./"(file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
  (setq compile-command foo)
  (call-interactively 'compile))

(defun run-program-cluster ()
  (interactive)
  (defvar foo)
  (setq foo (concat "mpirun -machinefile hosts -np 2 ./"(file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
  (setq compile-command foo)
  (call-interactively 'compile))


(defun compile-f-lib-plotter ()
  (interactive)
  (defvar foo)
  (setq foo (concat "mpif90 -o "(file-name-sans-extension (file-name-nondirectory (buffer-file-name))) " \"" (buffer-file-name) "\" -fopenmp -lC2 -lPlotter -lAF03GL -lGL -lGLU -lglut -I ~/.local/share/Library"))
  (setq compile-command foo)
  (call-interactively 'compile))


(defun compile-f-plotter ()
  (interactive)
  (defvar foo)
  (setq foo (concat "mpif90 -o "(file-name-sans-extension (file-name-nondirectory (buffer-file-name))) " \"" (buffer-file-name) "\" -lPlotter -lAF03GL -lGL -lGLU -lglut -I ~/.local/share/Library"))
  (setq compile-command foo)
  (call-interactively 'compile))


(defun run-plotter ()
  (interactive)
  (defvar foo)
  (setq foo (concat "./"(file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
  (setq compile-command foo)
  (call-interactively 'compile))

(global-set-key [f4] 'run-program)
(global-set-key [C-f4] 'run-program-cluster)
(global-set-key [f5] 'compile-f)
(global-set-key [C-f5] 'compile-f-lib)
(global-set-key [f6] 'compile-cc)
(global-set-key [f7] 'compile-f-plotter)
(global-set-key [C-f7] 'compile-f-lib-plotter)
(global-set-key [f8] 'run-plotter)

;; Создание пунктов меню 
(define-key-after
  global-map
  [menu-bar build]
  (cons "Build" (make-sparse-keymap "build"))
  'tools )

;; Creating a menu item
(define-key
  global-map
  [menu-bar build compilec]
  '("Compile C" . compile-cc))

(define-key
  global-map
  [menu-bar build compilef]
  '("Compile Fortran" . compile-f))

(define-key
  global-map
  [menu-bar build run]
  '("Run" . run-program))

;; Удаляет пункт из меню панели
(global-unset-key [menu-bar buffer])
;;Удаляет подпункт меню
;;(global-unset-key [menu-bar tools games])

(define-key-after
  global-map
  [menu-bar lib]
  (cons "Library" (make-sparse-keymap "lib"))
  'build )

;; Creating a menu item
(define-key
  global-map
  [menu-bar lib compilec]
  '("Compile" . compile-f-lib))


(define-key
  global-map
  [menu-bar lib run]
  '("Run" . run-program))


(define-key-after
  global-map
  [menu-bar plotter]
  (cons "Plotter" (make-sparse-keymap "plotter"))
  'lib )
(define-key
  global-map
  [menu-bar plotter compilep]
  '("Compile" . compile-f-plotter))
(define-key
  global-map
  [menu-bar plotter runp]
  '("Run" . run-plotter))




(setq x-select-enable-clipboard t)

(electric-pair-mode    1) 
(electric-indent-mode -1)

(setq display-time-24hr-format t) ;; 24-часовой временной формат в mode-line
(display-time-mode             t) ;; показывать часы в mode-line
(size-indication-mode          t) ;; размер файла в %-ах

(setq make-backup-files         nil) ; Don't want any backup files
(setq auto-save-list-file-name  nil) ; Don't want any .saves files
(setq auto-save-default         nil) ; Don't want any auto saving


(add-to-list 'load-path "~/.emacs.d/lisp/")

;;Копировние на узлы кластера
;;Внутри кластера
(defun send-from-claster ()
  (interactive)
  (defvar foo)
  (setq foo (concat "for i in `seq 49 64`; do scp "(file-name-sans-extension(buffer-file-name))" student-pmi@10.128.1.$i:/home/student-pmi/; done"))
    (setq compile-command foo)
  (call-interactively 'compile))
;;С локальной машины
(defun send-from-local ()
 (interactive)
  (defvar foo)
  (setq foo (concat "for i in `seq 49 64`; do scp -P 630$i "(file-name-sans-extension(buffer-file-name))" student-pmi@in.vyatsu.ru:/home/student-pmi/; done" ))
    (setq compile-command foo)
  (call-interactively 'compile))


(global-set-key [C-f9] 'send-from-claster)
(global-set-key [C-f10] 'send-from-local)


(require 'package)
(package-initialize)
(add-to-list'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; http://code.google.com/p/dea/source/browse/trunk/my-lisps/linum%2B.el
(require 'linum+)
(setq linum-format "%d ")
(global-linum-mode 1)


;; built-in
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)


;; built-in
(require 'bs)
(setq bs-configurations '(("files" "^\\*scratch\\*" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)))

(global-set-key (kbd "<f2>") 'bs-show)


;; http://www.emacswiki.org/emacs/AutoComplete
;(add-to-list 'load-path "~/.emacs.d/lisp/auto-complete")
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "/home/student-pmi/.emacs.d/lisp/auto-complete/dict")
;(a

 ;; http://www.emacswiki.org/emacs/SrSpeedbar
 (require 'sr-speedbar)
 (global-set-key (kbd "<f12>") 'sr-speedbar-toggle)


 ;; http://www.emacswiki.org/emacs/Yasnippet
 (add-to-list 'load-path "~/.emacs.d/lisp/yasnippet")
 (require 'yasnippet)
 (yas-global-mode 1)
(yas/load-directory "~/.emacs.d/lisp/yasnippet/snippets")

(load-theme 'wombat)

(require 'font-lock)
(global-font-lock-mode             t) ;; включено с версии Emacs-22. На всякий...
(setq font-lock-maximum-decoration t)


 ;; Options -> Set default font
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-12"))
(set-default-font "DejaVu Sans Mono-12")


(defun reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
        (modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (quail-get-translation
                      (cadr map) (char-to-string to) 1)))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key local-function-key-map
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))

(defadvice read-passwd (around my-read-passwd act)
  (let ((local-function-key-map nil))
    ad-do-it))

 (reverse-input-method 'russian-typewriter)

