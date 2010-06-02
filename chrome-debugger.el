;;;; Interface functions are at the bottom.

(defcustom chrome-debugger-host "localhost"
  "Hostname of the server running the ChromeDevTools"
  :group 'chrome-debugger
  :type 'string)

(defcustom chrome-debugger-port 9222
  "Port for the server running the ChromeDevTools"
  :group 'chrome-debugger
  :type 'integer)

(defvar chrome-debugger-buffer "*chrome debugger*"
  "Name of the chrome-debugger buffer")

(defvar chrome-debugger-stream "*chrome debugger*"
  "Name of the chrome-debugger network stream")

(defmacro with-chrome-debugger-repl (body)
  `(save-excursion
     (let ((chrome-debugger-buffer (get-buffer chrome-debugger-buffer)))
       (if (null chrome-debugger-buffer)
           (open-network-stream
            chrome-debugger-stream
            chrome-debugger-buffer
            chrome-debugger-host
            chrome-debugger-port))
       (set-buffer chrome-debugger-buffer)
       ,@body)))

(defun chrome-debugger-send* (str)
  "Send a literal string to chrome-debugger-process."
  (process-send-string chrome-debugger-stream str))

(defun chrome-debugger-send (cmd tool &optional dest)
  "Send a command assuming the environment is set correctly."
  (chrome-debugger-send* "Content-Length: ")
  (chrome-debugger-send* (+ (length cmd) 2))
  (chrome-debugger-send* crlf)
  (chrome-debugger-send* "Tool: ")
  (chrome-debugger-send* tool)
  (chrome-debugger-send* crlf)
  (if (dest)
      (progn
        (chrome-debugger-send* "Destination: ")
        (chrome-debugger-send* dest)
        (chrome-debugger-send* crlf)))
  (chrome-debugger-send* crlf)
  (chrome-debugger-send* cmd))

(defun chrome-debugger-read* ()
  "Read a response assuming the environment is set correctly."
  
  )

(defun chrome-debugger-handshake ()
  (interactive)
  (with-chrome-debugger-repl
   (let ((text "ChromeDevToolsHandshake"))
    (chrome-debugger-echo* text)
    (if (not (equal text (chrome-debugger-read*)))
        (error "handshake with the ChromeDevTools server failed.")
      t))))

(defun chrome-debugger-send (cmd)
  "Send a command, setting the environment"
  (interactive "sSend: ")
  (with-chrome-debugger-repl
   (chrome-debugger-send* cmd)))
