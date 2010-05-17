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

(defmacro with-chrome-debugger-repl (body)
  `(save-excursion
     (let ((chrome-debugger-buffer (get-buffer chrome-debugger-buffer)))
       (if (null chrome-debugger-buffer)
           (open-network-stream
            chrome-debugger-buffer
            chrome-debugger-buffer
            chrome-debugger-host
            chrome-debugger-port))
       (set-buffer chrome-debugger-buffer)
       ,@body)))

(defun chrome-debugger-echo* (cmd)
  "Echo a string assuming the environment is set correctly."
  (process-send-string chrome-debugger-buffer cmd))

(defun chrome-debugger-send* (cmd tool &optional dest)
  "Send a command assuming the environment is set correctly."
  (chrome-debugger-echo* "Content-Length: ")
  (chrome-debugger-echo* (+ (length cmd) 2))
  (chrome-debugger-echo* crlf)
  (chrome-debugger-echo* "Tool: ")
  (chrome-debugger-echo* tool)
  (chrome-debugger-echo* crlf)
  (if (dest)
      (progn
        (chrome-debugger-echo* "Destination: ")
        (chrome-debugger-echo* dest)
        (chrome-debugger-echo* crlf)))
  (chrome-debugger-echo* crlf))


(defun chrome-debugger-read* (cmd)
  "Read a response assuming the environment is set correctly."
  
  )

(defun chrome-debugger-assert* (predicate)
  "Read a response, and ensure that it passes the predicate."
  (if (not (predicate (chrome-debugger-read*)))
      (error "the ChromeDevTools server returned surprising results")
    t))

(defun chrome-debugger-handshake ()
  (interactive)
  (with-chrome-debugger-repl
   (let ((text "ChromeDevToolsHandshake"))
    (chrome-debugger-send* text)
    (chrome-debugger-assert* (lambda (x) (equal text x))))))

(defun chrome-debugger-send (cmd)
  "Send a command, setting the environment"
  (interactive "sSend: ")
  (with-chrome-debugger-repl
   (chrome-debugger-send* cmd)))
