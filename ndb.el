;; (defvar ndb-proc nil
;;   "the ndb network 'process'")

;; (defun ndb-sentinel (proc str)
;;   (setq ndb-proc nil)
;;   (message (format "ndb-sentinel: connection closed" str)))

;; (defun ndb-log-callback (svr client msg)
;;   (message (format "ndb-log: %s" msg)))

;; (defun ndb-filter-callback (proc msg)
;;   (message (format "ndb-filter: %s" msg)))

;; (defun ndb-kill ()
;;   (interactive)
;;   (process-send-string ndb-proc "\r\n\r\n")
;;   )

;; (defun ndb-try-connect ()
;;   (setq ndb-proc (or
;;                   ndb-proc
;;                   (make-network-process :name "ndb"
;;                                         :host "localhost"
;;                                         :service 5858
;;                                         :sentinel 'ndb-sentinel
;;                                         :log 'ndb-log-callback
;;                                         :buffer "*ndb*"
;; ;;                                        :filter 'ndb-filter-callback
;;                                         )
;;                   )
;;         )
;;   )

;; (defun ndb-send-cmd (cmd)
;;   "send a command packet to the process"
;;   (ndb-try-connect)
;;   (if (not ndb-proc)
;;       (message "ndb connection not active")
;;     (let
;;         (
;;          (len (length cmd))
;;          (pak)
;;          )
;;       (setq pak (format "Content-Length: %i\r\n%s" len cmd))
;;       (process-send-string ndb-proc pak)
;;       )
;;     )
;;   )
;; (ndb-send-cmd "{\"seq\":1,\"type\":\"request\",\"command\":\"version\"}")

(require 'ansi-color)
(require 'gud)

;; History of argument lists passed to ndb.
(defvar gud-ndb-history nil)

(defvar gud-ndb-marker-regexp "break in \\(.\\|\n\\)?*, \\(/.*/.*\\.js\\) line \\([0-9]+\\) column \\([0-9]+\\)")
(defvar gud-ndb-marker-regexp-file-group 2) ;; not sure what this is
(defvar gud-ndb-marker-regexp-line-group 3)
(defvar gud-ndb-marker-regexp-fnname-group 2)
(defvar gud-ndb-marker-regexp-start "> break in")
(defvar gud-ndb-auto-reconnect t "running node with supervisor means lots of restarts. it is nice to have the debugger reconnect automatically")
;; (string-match gud-ndb-marker-regexp "break in #<Object>.[anonymous](exports=#<Object>, require=function require(path) {\n     return Module._load(path, self);\n   }, module=#<Module>, __filename=/home/abrady/abs/node/tmp.js, __dirname=/home/abrady/abs/node), /home/abrady/abs/node/tmp.js line 3 column 1\n debugger;\n ^\n dbg> ")
;; (string-match gud-ndb-marker-regexp "break in #<Server>.rps_handler(req=#<IncomingMessage>, res=#<ServerResponse>), /home/abrady/rps/server.js line 16 column 5")
;; (string-match gud-ndb-marker-regexp "> break in [anonymous](err=null, file=<!DOCTYPE html>\n <html>\n <head>\n <link rel=\"stylesheet\" type=\"text/css\" h... (length: 634)), /home/abrady/rps/lib/comm.js line 164 column 19")

;; There's no guarantee that Emacs will hand the filter the entire
;; marker at once; it could be broken up across several strings.  We
;; might even receive a big chunk with several markers in it.  If we
;; receive a chunk of text which looks like it might contain the
;; beginning of a marker, we save it here between calls to the
;; filter.
(defun gud-ndb-marker-filter (string)
  (setq gud-marker-acc (concat gud-marker-acc string))
  (let ((output ""))
    ;; Process all the complete markers in this chunk.
    (while (string-match gud-ndb-marker-regexp gud-marker-acc)
      (setq
       ;; Extract the frame position from the marker.
       gud-last-frame
       (let ((file (match-string gud-ndb-marker-regexp-file-group
                                 gud-marker-acc))
             (line (string-to-number
                    (match-string gud-ndb-marker-regexp-line-group
                                  gud-marker-acc))))
         (cons file line))
       ;; Output everything instead of the below
       output (concat output (substring gud-marker-acc 0 (match-end 0)))
       ;;         ;; Append any text before the marker to the output we're going
       ;;         ;; to return - we don't include the marker in this text.
       ;;         output (concat output
       ;;                     (substring gud-marker-acc 0 (match-beginning 0)))
       ;; Set the accumulator to the remaining text.
       gud-marker-acc (substring gud-marker-acc (match-end 0))))
    ;; Does the remaining text look like it might end with the
    ;; beginning of another marker?  If it does, then keep it in
    ;; gud-marker-acc until we receive the rest of it.  Since we
    ;; know the full marker regexp above failed, it's pretty simple to
    ;; test for marker starts.
    (if (string-match gud-ndb-marker-regexp-start gud-marker-acc)
        (progn
          ;; Everything before the potential marker start can be output.
          (setq output (concat output (substring gud-marker-acc
                                                 0 (match-beginning 0))))
          ;; Everything after, we save, to combine with later input.
          (setq gud-marker-acc
                (substring gud-marker-acc (match-beginning 0))))
      (setq output (concat output gud-marker-acc)
            gud-marker-acc ""))
    output))

(defun ndb-gud-break-func (func-name)
  (interactive (list (read-string "func: " (thing-at-point 'sexp))))
  (gud-call (format "break %s()" func-name))
  )

(defadvice gud-sentinel (after restart-ndb (proc msg))
  "potentially restart ndb"
  (if (and 
       (equal (process-buffer proc) "*ndb*")
       gud-ndb-auto-reconnect)
      (ndb)))
;; (ad-activate 'gud-sentinel)

(defun ndb (&optional command-line)
  "Run ndb"
  (interactive)
  (save-excursion
    ;; default to localhost debugging
    (if (not command-line)
        (setq command-line "d8 --remote-debugger"))
    (gud-common-init command-line nil 'gud-ndb-marker-filter)
    (set (make-local-variable 'gud-minor-mode) 'ndb)
    (gud-def gud-break  "break %f:%l"  "\C-b" "Set breakpoint at current line.")
    ;;  (gud-def gud-remove "break clear %f:%l"  "\C-d" "Remove breakpoint at current line")
    (gud-def gud-step   "step"         "\C-s" "Step one source line with display.")
    (gud-def gud-next   "next"         "\C-n" "Step one line (skip functions).")
    (gud-def gud-cont   "continue"     "\C-r" "Continue with display.")
    (gud-def gud-finish "out"          "\C-f" "Finish executing current function.")
    (gud-def gud-up     "up"           "<" "Up one stack frame.")
    (gud-def gud-down   "down"         ">" "Down one stack frame.")
    (gud-def gud-print  "p %e"         "\C-p" "Evaluate PHP expression at point.")
    (gud-def gud-jump   "jump %f:%l"   "\C-j" "jump to the specified line for execution")
    (defalias 'gud-fbreak 'ndb-gud-break-func)
    ;; (setq comint-prompt-regexp "^(.*pdb[+]?) *")
    (setq comint-prompt-regexp "^\\(dbg\\)?> ")
    (setq paragraph-start comint-prompt-regexp)
    (ansi-color-for-comint-mode-on)
    (set-window-dedicated-p nil t)
    (run-hooks 'ndb-mode-hook)
    (gud-call "")
    )
  )

(defun ndb-linenum ()
  "get the current file and linenumber in foo.php:1234. stick it in the kill ring"
  (interactive)
  (let
      ((str (format "%s:%d" (buffer-file-name) (line-number-at-pos (point)))))
    (kill-new str)
    (message str)
    )
  )

(defun ndb-send-line ()
  (interactive)
    (let
        (
         (s)
         )
      (setq s (buffer-substring (line-beginning-position) (line-end-position)))
      (message s)
      (gud-call (format "= %s" s))
      (forward-line 1)
      )
  )

(defun ndb-send-sexp ()
  (interactive)
  (save-excursion
    (let
        (
         (s (thing-at-point 'sexp))
         )
      (message s)
      (gud-call (format "= %s" s))
      )
    )
  )

(provide 'ndb)