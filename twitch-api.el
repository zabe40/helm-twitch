;;; twitch-api.el --- An elisp interface for the Twitch.tv API -*- lexical-binding: t -*-

;; Copyright (C) 2015-2016 Aaron Jacobs

;; Author: Aaron Jacobs <atheriel@gmail.com>
;; URL: https://github.com/atheriel/helm-twitch
;; Keywords: games, comm
;; Version: 0.2
;; Package-Requires: ((dash "2.11.0") (emacs "24.3"))

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'url)
(require 'dash)
(require 'json)
(require 'tabulated-list)

(defgroup twitch-api nil
  "An elisp interface for the Twitch.tv API."
  :group 'comm)

(defconst twitch-api-version "0.2"
  "Version of this package to advertise in the User-Agent string.")

(defvar twitch-api-base-url "https://api.twitch.tv/helix/"
  "URL prefix for twitch api calls.")

(defvar twitch-api-oauth-url "https://id.twitch.tv/oauth2/authorize"
  "URL for twitch oauth process.")

(defvar twitch-api-game-filter-id nil
  "The game ID associated with `twitch-api-game-filter'.

A Twitch game ID is a unique identifier for a particular game,
and is used instead of the game name in API calls.")

(defun twitch-api-set-game-filter (symbol game)
  "Set SYMBOL (defaulting to `twitch-api-game-filter') to GAME.
If necessary, invalidate `twitch-api-game-filter-id'."
  (cl-assert (or (null game)
		 (equal 'string (type-of game)))
	     t)
  (setf symbol (or symbol 'twitch-api-game-filter))
  ;; The first time this is called on `twitch-api-game-filter', it
  ;; will not have been assigned a value yet, so in this case just set
  ;; it unconditionally.
  (condition-case err
      (when (not (equal game (symbol-value symbol)))
	(set symbol game)
	(setf twitch-api-game-filter-id nil))
    (void-variable (set symbol game)))
  game)

(defcustom twitch-api-game-filter nil
  "If specified, limits the search to those streaming this game.
Changing this variable invalidates `twitch-api-game-filter-id',
which is handled by `twitch-api-set-game-filter'."
  :group 'twitch-api
  :set #'twitch-api-set-game-filter
  :type '(choice (const :tag "All games" nil)
		 string))

(defvar twitch-api-user-id nil
  "The user ID associated with `twitch-api-username'.

A Twtich user ID is a unique identifier for a twitch user, which
is used instead of the username in API calls. For more
information see URL
`https://dev.twitch.tv/docs/v5#translating-from-user-names-to-user-ids'.")

(defcustom twitch-api-oauth-token nil
  "The OAuth token for the Twitch.tv username in `twitch-api-username'.

To retrieve an OAuth token, call `twitch-api-authenticate', or
visit URL `https://twitchapps.com/tmi/'."
  :group 'twitch-api
  :type 'string)

(defun twitch-api-set-username (symbol username &optional new-oauth-token)
  "Set SYMBOL (defaulting to `twitch-api-username') to USERNAME.
If necessary, invalidate `twitch-api-user-id' and
`twitch-api-oauth-token'.

If NEW-OAUTH-TOKEN is given, set that."
  (cl-assert (or (null username)
		 (equal 'string (type-of username)))
	     t)
  (setf symbol (or symbol 'twitch-api-username))
  ;; The first time this is called on `twitch-api-username' it will
  ;; not have been assigned a value yet, so in this case just set it
  ;; unconditionally.
  (condition-case err
      (when (not (equal username (symbol-value symbol)))
	(set symbol username)
	(setf twitch-api-user-id nil)
	(setf twitch-api-oauth-token new-oauth-token))
    (void-variable (set symbol username)))
  username)

(defcustom twitch-api-username nil
  "A Twitch.tv username, for connecting to Twitch chat.
Changing this variable invalidates `twitch-api-user-id' and
`twitch-api-oauth-token' which are handled by
`twitch-api-set-username'."
  :group 'twitch-api
  :set 'twitch-api-set-username
  :type '(choice (const nil)
		 string))

(defcustom twitch-api-client-id "d6hul5ut8dmqvl6tsa90254yzu8g612"
  "The Client ID for the application.

If you want to use your own, you can register for one at
`https://dev.twitch.tv/console/apps/create'."
  :group 'twitch-api
  :type 'string)

(defcustom twitch-api-curl-binary "curl"
  "Location of the curl program."
  :group 'twitch-api
  :type 'string)

;;;; Utilities

(defun twitch-api--plist-to-url-params (plist)
  "Turn property list PLIST into an HTML parameter string."
  (mapconcat (lambda (entry)
	       (concat (url-hexify-string
			(nth 1 (split-string (format "%s" (nth 0 entry)) ":")))
		       "="
		       (url-hexify-string (format "%s" (nth 1 entry)))))
	     (-partition 2 plist) "&"))

(defun twitch-api-url-from-login (login)
  "Return the url to the twitch stream given by LOGIN."
  (concat "https://twitch.tv/" login))

;;;; Data Structures

(cl-defstruct (twitch-api-stream (:constructor twitch-api-stream--create))
  "A Twitch.tv stream."
  name viewers status game url)

(cl-defstruct (twitch-api-channel (:constructor twitch-api-channel--create))
  "A Twitch.tv channel."
  name followers game url)

;;;; Authentication

(defvar twitch-api-requested-scopes '("user:read:follows" "chat:read" "chat:edit")
  "A list of scopes requested from the user.
For more information see URL
`https://dev.twitch.tv/docs/authentication#scopes'.")

;;;###autoload
(defun twitch-api-authenticate ()
  "Retrieve an OAuth token for a Twitch.tv account with a browser.

For more information see URL
`https://dev.twitch.tv/docs/authentication/getting-tokens-oauth#oauth-implicit-code-flow'."
  (interactive)
  (unless twitch-api-client-id
    (user-error "You must specify a client ID to retrieve an OAuth token"))
  (browse-url
   (concat twitch-api-oauth-url "?"
	   (twitch-api--plist-to-url-params
	    (list :client_id twitch-api-client-id
		  :redirect_uri "http://localhost"
		  :response_type "token"
		  :scope (mapconcat #'identity twitch-api-requested-scopes " ")))))
  (let ((token (read-string "OAuth Token: ")))
    (if (equal token "")
	(user-error "No token supplied. Aborting")
      (setq twitch-api-oauth-token token))))

;;;; API Wrappers

;;;###autoload
(defun twitch-api (endpoint &rest plist)
  "Query the Twitch API at ENDPOINT, returning the resulting JSON
in a property list structure.

Twitch API parameters can be passed in the property list PLIST.
For example:

    (twitch-api \"search/channels\" :query \"flame\" :first 15)

For more information see URL `https://dev.twitch.tv/docs/api'."
  (unless twitch-api-client-id
    (user-error "You must specify a Client ID to query Twitch's API"))
  (unless twitch-api-oauth-token
    (user-error "You must specify an OAuth token to query Twitch's API"))
  (let* ((params (twitch-api--plist-to-url-params plist))
         (api-url (concat twitch-api-base-url endpoint "?" params))
         (curl-opts (list "--compressed" "--silent" "--location" "-D-"))
         (json-object-type 'plist) ;; Decode into a plist.
         (headers
          ;; Use a descriptive User-Agent.
          `(("User-Agent" . ,(format "twitch-api/%s Emacs/%s"
                                     twitch-api-version emacs-version))
            ;; Use new version of API
            ("Accept" . "application/json"))))
    ;; Support setting the method lexically, as with url.el.
    (when url-request-method
      (push (format "-X%s" url-request-method) curl-opts))
    (push `("Authorization" . ,(format "Bearer %s" twitch-api-oauth-token))
          headers)
    (push `("Client-ID" . ,twitch-api-client-id) headers)
    ;; Wrap up arguments to curl.
    (dolist (header headers)
      (cl-destructuring-bind (key . value) header
        (push (format "-H%s: %s" key value) curl-opts)))
    (setq curl-opts (nreverse (cons api-url curl-opts)))
    (with-current-buffer (generate-new-buffer " *twitch-api*")
      (let ((coding-system-for-read 'binary))
        (apply #'call-process twitch-api-curl-binary nil t nil curl-opts))
      (goto-char (point-min))
      ;; Mimic url.el and store the status as a local variable.
      (re-search-forward "^HTTP/[\\.0-9]+ \\([0-9]+\\)")
      (setq-local url-http-response-status (string-to-number (match-string 1)))
      (unless (equal url-http-response-status 204)
        (re-search-forward "^\r\n") ;; End of headers.
        ;; Many Twitch streams have non-ASCII statuses in UTF-8 encoding.
        (decode-coding-region (point) (point-max) 'utf-8)
        (let ((result (json-read)))
          (when (plist-get result ':error)
            ;; According to the Twitch API documentation, the JSON object should
            ;; contain error information of this kind on failure:
            (let ((status (plist-get result ':status))
                  (err    (plist-get result ':error))
                  (errmsg (plist-get result ':message)))
              (error "Twitch.tv API request failed: %d (%s)%s"
                     status err (when errmsg (concat " - " errmsg)))))
          result)))))

(cl-defun twitch-api-ensure-user-id (&optional (username twitch-api-username))
  "Ensure that `twitch-api-user-id' is non-nil.
If it isn't query `twitch-api' for the user ID corresponding to
`twitch-api-username'.

If USERNAME is not specified and `twitch-api-username' is nil,
then the Twitch API will return the user info for the user
associated with `twitch-api-oauth-token'.

For convenience, this also sets `twitch-api-username'.

For more information see URL
`https://dev.twitch.tv/docs/api/reference#get-users'."
  (if twitch-api-user-id
      twitch-api-user-id
    (cl-assert (or username twitch-api-oauth-token))
    (let ((user (if username
		    (twitch-api "users" :login username)
		  (twitch-api "users"))))
      (setf twitch-api-user-id (plist-get (aref (plist-get user :data)
						0)
					  :id))
      (setf twitch-api-username (plist-get (aref (plist-get user :data)
						 0)
					   :login)))))

(cl-defun twitch-api-ensure-game-id (&optional (game twitch-api-game-filter))
  "Ensure that `twitch-api-game-filter-id' is non-nil.
If it isn't query `twitch-api' for the game ID corresponding to
`twitch-api-game-filter'.

For more information see URL
`https://dev.twitch.tv/docs/api/reference#get-games'."
  (cl-assert game)
  (if twitch-api-game-filter-id
      twitch-api-game-filter-id
    (setf twitch-api-game-filter-id (plist-get (aref (plist-get (twitch-api "games" :name game)
								:data)
						     0)
					       :id))))

;;;###autoload
(defun twitch-api-search-streams (search-term &optional limit)
  "Retrieve a list of Twitch streams that match the SEARCH-TERM.

If LIMIT is an integer, pass that along to `twitch-api'.
Additionally, LIMIT must be less than or equal to 100.

This function querys the same endpoint as
`twitch-api-search-channels', but filters for live channels.

For more information on this endpoint see URL
`https://dev.twitch.tv/docs/api/reference#search-channels'."
  (cl-assert (not (equal search-term "")))
  (let ((args (list :query search-term :live_only 'true)))
    (when (integerp limit)
      (cl-assert (<= 1 limit 100) t)
      (setf args (cl-list* :first limit args)))
    (push "search/channels" args)
    (cl-loop for stream across (plist-get (apply #'twitch-api args) :data)
	     collect (twitch-api-stream--create
		      :name    (plist-get stream :broadcaster_login)
		      :status  (replace-regexp-in-string (rx (any ?\C-m ?\C-j)) ""
							 (plist-get stream :title))
		      :game    (plist-get stream :game_name)
		      :url     (twitch-api-url-from-login (plist-get stream :broadcaster_login))))))

;;;###autoload
(defun twitch-api-search-channels (search-term &optional limit)
  "Retrieve a list of Twitch channels that match the SEARCH-TERM.

If LIMIT is an integer, pass that along to `twitch-api'.
Additionally, LIMIT must be less than or equal to 100.

For more information on this endpoint see URL
`https://dev.twitch.tv/docs/api/reference#search-channels'."
  (cl-assert (not (equal search-term "")) t)
  (let ((args (list :query search-term)))
    (when (integerp limit)
      (cl-assert (<= 1 limit 100) t)
      (setf args (cl-list* :first limit args)))
    (push "search/channels" args)
    (cl-loop for channel across (plist-get (apply #'twitch-api args) :data)
	     collect (twitch-api-channel--create
		      :name (plist-get channel :display_name)
		      :game (plist-get channel :game_name)
		      :url  (twitch-api-url-from-login (plist-get channel :broadcaster_login))))))

;;;###autoload
(defun twitch-api-get-followed-streams (&optional limit)
  "Retrieve a list of Twitch streams that match the SEARCH-TERM.

If LIMIT is an integer, pass that along to `twitch-api'.
Additionally, LIMIT must be less than or equal to 100.

For more information on this endpoint see URL
`https://dev.twitch.tv/docs/api/reference#get-followed-streams'."
  (unless twitch-api-oauth-token
    (user-error "You must specify an OAuth token to view your followed streams"))
  (twitch-api-ensure-user-id)
  (let ((args (list :user_id twitch-api-user-id)))
    (when (integerp limit)
      (cl-assert (<= 1 limit 100) t)
      (setf args (cl-list* :first limit args)))
    (push "streams/followed" args)
    (cl-loop for stream across (plist-get (apply #'twitch-api args) :data)
	     collect (twitch-api-stream--create
		      :name    (plist-get stream ':user_name)
		      :viewers (plist-get stream ':viewer_count)
		      :status  (replace-regexp-in-string (rx (any ?\C-m ?\C-j)) ""
							 (plist-get stream ':title))
		      :game    (plist-get stream ':game_name)
		      :url     (twitch-api-url-from-login (plist-get stream :user_login))))))

;;;; Twitch Chat Interaction

(defvar twitch-api-irc-endpoint-url "irc.chat.twitch.tv"
  "The IRC URL initially used to join twitch chat.")

(defvar twitch-api-irc-server-url "tmi.twitch.tv"
  "The IRC URL used after initial connection.")

(defun twitch-api-erc-ignore-004-response (_proc parsed)
  "Ignore \"004\" responses from twitch.tv.

Twitch's IRC server sends a malformed \"004\" response, which
causes ERC to set `erc-server-announced-name' incorrectly.

Example of correctly formed 004 response:
\":calcium.libera.chat 004 <nick> calcium.libera.chat <version> <usermodes> <chanmodes> <chanmodes with parameter>\"

Example of malformed 004 response from twitch:
\":tmi.twitch.tv 004 <nick> :-\"

For more information see URL
`https://datatracker.ietf.org/doc/html/rfc2812#section-5.1', and
URL
`https://dev.twitch.tv/docs/irc/guide#connecting-to-twitch-irc'."
  ;; `erc-server-004-functions' is run with
  ;; `run-hook-with-args-until-success', so returning t prevents other
  ;; functions in the hook from running
  (if (equal (erc-response.sender parsed) twitch-api-irc-server-url)
      t
    nil))

;;;###autoload
(defun twitch-api-open-chat (channel-name)
  "Invokes `erc' to open Twitch chat for a given CHANNEL-NAME.

If this doesn't connect, check if the chat is restricted in any
way, For more information see URL
`https://help.twitch.tv/s/article/how-to-manage-harassment-in-chat'.

For more information on connecting to Twitch Chat with IRC, see
URL `https://dev.twitch.tv/docs/irc/guide'."
  (interactive "sChannel: ")
  (unless twitch-api-oauth-token
    (user-error "Set the variable `twitch-api-oauth-token' to connect to Twitch chat"))
  (unless twitch-api-username
    (twitch-api-ensure-user-id))
  (require 'erc)
  ;; Use negative depth to run before usual hook
  (add-hook 'erc-server-004-functions #'twitch-api-erc-ignore-004-response -75)
  ;; Add closure to `erc-after-connect' and remove after running once.
  ;; We assign the lambda to a symbol-function so it can be
  ;; correctly compared by `equal' in `remove-hook'.
  (let ((fun-symbol (gensym "hook-fun")))
    (setf (symbol-function fun-symbol) `(lambda (server nick)
					  (when (equal server twitch-api-irc-server-url)
					    (unwind-protect
						(erc-join-channel (format "#%s" (downcase ,channel-name)))
					      (remove-hook 'erc-after-connect ',fun-symbol)))))
    (add-hook 'erc-after-connect fun-symbol))
  (erc :server twitch-api-irc-endpoint-url :port 6667
       :nick (downcase twitch-api-username)
       :password (format "oauth:%s" twitch-api-oauth-token)))

;;;; Top Streams Listing

(define-derived-mode twitch-api-top-streams-mode tabulated-list-mode "Top Twitch.tv Streams"
  "Major mode for `twitch-api-list-top-streams'."
  (setq tabulated-list-format
        [("Streamer" 17 t) ("Viewers" 7 twitch-api--sort-by-viewers)
         ("Game" 20 t) ("Status" 0 nil)])
  (setq tabulated-list-sort-key (cons "Viewers" nil))
  (add-hook 'tabulated-list-revert-hook
            'twitch-api--refresh-top-streams nil t))

(defun twitch-api--refresh-top-streams ()
  (setq tabulated-list-entries
        (mapcar (lambda (elt)
                  (list elt (vector (twitch-api-stream-name elt)
                                    (int-to-string
                                     (twitch-api-stream-viewers elt))
                                    (twitch-api-stream-game elt)
                                    (twitch-api-stream-status elt))))
                (twitch-api-search-streams ""))))

(defun twitch-api--sort-by-viewers (s1 s2)
  (> (twitch-api-stream-viewers (car s1))
     (twitch-api-stream-viewers (car s2))))

;;;###autoload
(defun twitch-api-list-top-streams ()
  "Display a list of top streams on Twitch.tv."
  (interactive)
  (let ((buffer (get-buffer-create "*Top Twitch.tv Streams*")))
    (with-current-buffer buffer
      (twitch-api-top-streams-mode)
      (twitch-api--refresh-top-streams)
      (tabulated-list-init-header)
      (tabulated-list-print))
    (display-buffer buffer)
    nil))

(provide 'twitch-api)

;; Local Variables:
;; coding: utf-8
;; End:

;;; twitch-api.el ends here
