(require 'utility)
(require 'json)
(require 'url)
(require 'oauth)

(defvar google-consumer-key nil)
(defvar google-consumer-secret nil)
(defvar google-request-url "https://www.google.com/oauth/request_token")
(defvar google-access-url  "https://www.google.com/oauth/access_token") 
(defvar google-user-authorize "https://www.google.com/oauth/authorize")

(defvar google-contacts-url
  "http://www-opensocial.googleusercontent.com/api/people/@me/@self")

(defcustom google-contacts-dir "~/.emacs.d/google-contacts")

(defvar google-contacts-access-token nil)

(defun google-contacts-access-token (&optional token)
  (let ((file (format "%s/%s-token"
                      google-contacts-dir
                      google-contacts-username)))
    (if (not token)
        (if (file-exists-p file) (slurp-file-contents file))
      (progn
        (if (not (file-exists-p google-contacts-dir))
            (mkdir google-contacts-dir t))
        (let-output-file
            file
          (insert
           (format "%s:%s\n"
                   (oauth-t-token token)
                   (oauth-t-token-secret token))))))))

(defun google-contacts-authenticate (username)
  "Get authentication token"
  (if-let* ((token (google-contacts-access-token)))
      (setq google-contacts-access-token
            (make-oauth-access-token 
             :consumer-key google-contacts-consumer-key
             :consumer-secret google-contacts-consumer-secret
             :auth-t (make-oauth-t
                      :token (match-string 1 str)
                      :token-secret (match-string 2 str))))
    (progn
      (setq google-contacts-access-token
            (oauth-authorize-app
             google-contacts-consumer-key
             google-contacts-consumer-secret
             google-contacts-request-url
             google-contacts-access-url
             google-contacts-user-authorize
             (lambda ()
               (let ((token (read-string "Please enter the provided code: ")))
                 (setq access-url
                       (concat access-url "?callback_token=" token))))))
      (google-contacts-access-token google-contacts-access-token)))
  google-contacts-access-token)

(defun google-contacts-find (name)
  
  )
