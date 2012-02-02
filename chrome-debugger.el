;;;; Interface functions are at the bottom.

(require 'json)

(defcustom chrome-dev-tools-host "localhost"
  "Hostname of the browser running the ChromeDevTools"
  :group 'chrome-dev-tools
  :type 'string)

(defcustom chrome-dev-tools-port 9222
  "Port for the browser running the ChromeDevTools"
  :group 'chrome-dev-tools
  :type 'integer)

(defvar chrome-dev-tools-name "*chrome dev tools*"
  "Name of the chrome-dev-tools network stream")

(defvar chrome-dev-tools-buffer "*chrome dev tools*"
  "Name of the chrome-dev-tools buffer")

(defvar chrome-dev-tools-process nil)

(setq crlf "\r\n")

(defmacro with-chrome-dev-tools (&rest body)
  `(save-excursion
     (if (not (and (processp chrome-dev-tools-process)
                   (process-live-p chrome-dev-tools-process)))
         (setq chrome-dev-tools-process
               (open-network-stream
                chrome-dev-tools-name
                chrome-dev-tools-buffer
                chrome-dev-tools-host
                chrome-dev-tools-port)))
     (set-buffer (get-buffer chrome-dev-tools-buffer))
     ,@body))

(defun chrome-dev-tools-send* (&rest str)
  "Send a literal string to chrome-dev-tools-process."
  (mapc (lambda (str)
          (process-send-string chrome-dev-tools-process str))
        str))

(defun chrome-dev-tools-send (cmd tool &optional dest)
  "Send a command assuming the environment is set correctly."
  (chrome-dev-tools-send*
   "Content-Length: " (+ (length cmd) 2) crlf
   "Tool: " tool crlf)
   (if (dest)
       (chrome-dev-tools-send*
        "Destination: " dest crlf))
   (chrome-dev-tools-send*
    crlf cmd))

(defun chrome-dev-tools-reset ()
  (when (processp chrome-dev-tools-process)
    (delete-process chrome-dev-tools-process)
    (kill-buffer chrome-dev-tools-buffer)))

(defun chrome-dev-tools-startup ()
  (interactive)
  (chrome-dev-tools-reset)
  (with-chrome-dev-tools
   (let ((handshake "ChromeDevToolsHandshake\r\n"))
     (chrome-dev-tools-send* handshake)
     (message "point at %d" (point))
     (if (not (search-forward handshake (length handshake) t))
         (error "handshake with the ChromeDevTools server failed.")))))

;; (chrome-dev-tools-startup)
