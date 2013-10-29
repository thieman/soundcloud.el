(require 'emms)
(require 'json)
(require 'url)
(require 'deferred)
(require 'easymenu)
(require 'string-utils)

(setq sc-client-id "0ef9cd3d5b707698df18f2c22db1714b")
(setq sc-url "http://soundcloud.com")
(setq sc-api "http://api.soundcloud.com")
(setq json-object-type 'hash-table)
(setq url-request-method "GET")
(setq *sc-current-tracks* ())

(defun inl (text) (insert text) (newline))

;;; soundcloud-mode

(defvar soundcloud-mode-map
  (let ((map (make-keymap)))
	(suppress-keymap map t)
	(define-key map (kbd "RET") 'sc-play-track)
	(define-key map (kbd "p") 'sc-play-pause)
	(define-key map (kbd "s") 'sc-search-artist)
	(define-key map (kbd "a") 'sc-load-artist)
	(define-key map (kbd "q") 'sc-quit)
	map))

(define-derived-mode soundcloud-mode special-mode "SoundCloud"
  (buffer-disable-undo)
  (setq truncate-lines t))

(easy-menu-define soundcloud-mode-menu soundcloud-mode-map
  "SoundCloud menu"
  '("SoundCloud"
	["Play Selected Track" sc-play-track t]
	["Play/Pause" sc-play-pause t]
	"---"
	["Load Artist" sc-load-artist t]
	["Search for Artist" sc-search-artist t]
	["Quit" sc-quit t]))

;;;; deferred functions for talking to SoundCloud

(defun get-data-from-request (buf)
  (with-current-buffer buf
	(goto-char (point-min))
	(re-search-forward "^$" nil 'move)
	(setq data (buffer-substring-no-properties (point) (point-max)))
	(kill-buffer buf))
  data)

(defun get-json-from-request (buf)
  (json-read-from-string (get-data-from-request buf)))

(defun get-stream-url (track-id)
  (deferred:$
	(deferred:url-retrieve (format "%s/tracks/%d.json?client_id=%s"
								   sc-api track-id sc-client-id))
	(deferred:nextc it 'get-json-from-request)
	(deferred:nextc it
	  (lambda (json-data)
		(format "%s?client_id=%s" (gethash "stream_url" json-data) sc-client-id)))))

(defun play-track-id (track-id)
  (deferred:$
	(get-stream-url track-id)
	(deferred:nextc it
	  (lambda (stream-url)
		(emms-play-url stream-url)))))

(defun resolve-permalink (permalink)
  (deferred:$
	(deferred:url-retrieve (format "%s/resolve.json?url=%s&client_id=%s"
								   sc-api permalink sc-client-id))
	(deferred:nextc it 'get-json-from-request)
	(deferred:nextc it
	  (lambda (data)
		(gethash "location" data)))))

(defun get-artist-tracks-by-name (artist-name)
  (deferred:$
	(resolve-permalink (format "%s/%s" sc-url artist-name))
	(deferred:nextc it
	  (lambda (resolved)
		(deferred:url-retrieve (replace-regexp-in-string ".json" "/tracks.json" resolved))))
	(deferred:nextc it 'get-json-from-request)))

;;;; the *soundcloud* buffer

(defun switch-to-sc-buffer ()
  (let ((buf (or (get-buffer "*soundcloud*")
				 (generate-new-buffer "*soundcloud*"))))
	(switch-to-buffer buf)))

(defun set-sc-buffer ()
  (let ((buf (or (get-buffer "*soundcloud*")
				 (generate-new-buffer "*soundcloud*"))))
	(set-buffer buf)))

(defun init-sc-buffer ()
  "Turns the current buffer into a fresh SoundCloud buffer."
  (funcall 'soundcloud-mode)
  (let ((inhibit-read-only t))
	(erase-buffer)
	(mapc 'inl '("SoundCloud" "==========" ""))
	(mapc 'inl commands-help)))

(setq commands-help
	  '("Interface" "" "a: go to artist" "s: search for artist" "RET: play selection"
		"q: stop playback and quit" ""
		"Playback" "" "p: play/pause current track"))

(defun draw-sc-artist-buffer (artist-name tracks)
  "Empty the current buffer and fill it with track info for a given artist."
  (let ((inhibit-read-only t))
	(erase-buffer)
	(mapc 'inl (list artist-name (string-utils-string-repeat "=" (length artist-name)) ""))
	(let ((idx 1))
	  (mapc 'track-listing *sc-current-tracks*))
	(goto-line 4)))

(defun track-listing (track)
  "Prints info for a track, followed by a newline."
  (insert (format "%d: %s" idx (gethash "title" track)))
  (newline)
  (setq idx (+ idx 1)))

;;;; interactive commmands

(defun soundcloud ()
  (interactive)
  (switch-to-sc-buffer)
  (init-sc-buffer))

(defun sc-load-artist ()
  (interactive)
  (lexical-let ((artist-name (read-from-minibuffer "Artist name: ")))
	(deferred:$
	  (get-artist-tracks-by-name artist-name)
	  (deferred:nextc it
		(lambda (tracks)
		  (setq *sc-current-tracks* tracks)
		  (switch-to-sc-buffer)
		  (draw-sc-artist-buffer artist-name tracks))))))

(defun sc-play-track ()
  (interactive)
  (move-beginning-of-line)
  (re-search-forward "[0-9]+" nil 'move)
  (setq track-num (buffer-substring-no-properties (line-beginning-position) (point)))
  (insert track-num))

(defun sc-quit ()
  (interactive)
  (emms-stop)
  (kill-buffer "*soundcloud*"))

;(play-track-id 115653060)
