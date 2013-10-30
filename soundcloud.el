(require 'emms)
(require 'emms-playing-time)
(require 'json)
(require 'url)
(require 'deferred)
(require 'easymenu)
(require 'string-utils)

(emms-playing-time 1)

(setq sc-client-id "0ef9cd3d5b707698df18f2c22db1714b")
(setq sc-url "http://soundcloud.com")
(setq sc-api "http://api.soundcloud.com")
(setq json-object-type 'hash-table)
(setq url-request-method "GET")

(setq track-start-line 10)

(setq *sc-current-artist* "")
(setq *sc-current-tracks* ())
(setq *sc-track-num* -1)
(setq *sc-track* nil)
(setq *sc-playing* nil)

(defun inl (text) (insert text) (newline))
(defun safe-next-line ()
  (if (= (line-end-position) (point-max)) nil (next-line)))
(defun delete-line ()
  (let ((start (line-beginning-position))
		(end (progn (safe-next-line) (line-beginning-position))))
	(delete-region start end)))

;;; soundcloud-mode

(defvar soundcloud-mode-map
  (let ((map (make-keymap)))
	(suppress-keymap map t)
	(define-key map (kbd "RET") 'sc-play-track)
	(define-key map (kbd "p") 'sc-pause)
	(define-key map (kbd "f") 'sc-play-next-track)
	(define-key map (kbd "b") 'sc-play-previous-track)
	(define-key map (kbd "s") 'sc-search-artist)
	(define-key map (kbd "a") 'sc-load-artist)
	(define-key map (kbd "q") 'sc-quit)
	map))

(setq sc-mode-keywords
	  '(("[0-9]+\: " . font-lock-variable-name-face)  ;; track numbers
		("\[-+\]" . font-lock-builtin-face)  ;; progress bar
		(".*\n=+\n" . font-lock-constant-face)  ;; headings
		("\\[.*/.*\\]" . font-lock-variable-name-face)))  ;; track timer

(define-derived-mode soundcloud-mode special-mode "SoundCloud"
  (buffer-disable-undo)
  (setq font-lock-defaults '(sc-mode-keywords))
  (setq mode-name "soundcloud")
  (setq truncate-lines t))

(easy-menu-define soundcloud-mode-menu soundcloud-mode-map
  "SoundCloud menu"
  '("SoundCloud"
	["Play Selected Track" sc-play-track t]
	["Play/Pause" sc-pause t]
	["Play Previous Track" sc-play-previous-track t]
	["Play Next Track" sc-play-next-track t]
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
  (let ((data (get-data-from-request buf)))
	(json-read-from-string data)))

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

(defun search-artist-by-query (artist-query)
  (deferred:$
	(deferred:url-retrieve (format "%s/users.json?q=%s&client_id=%s"
								   sc-api artist-query sc-client-id))
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

(defun draw-sc-artist-buffer (tracks)
  "Empty the current buffer and fill it with track info for a given artist."
  (let ((inhibit-read-only t))
	(erase-buffer)
	(draw-now-playing)
	(goto-char (point-max))
	(let ((title-string (format "Tracks by %s" *sc-current-artist*)))
	  (mapc 'inl (list title-string (string-utils-string-repeat "=" (length title-string)) "")))
	(let ((idx 1))
	  (mapc 'track-listing *sc-current-tracks*))
	(goto-char (point-min))
	(dotimes (i (- track-start-line 1)) (next-line))
	(beginning-of-line)))

(defun draw-sc-artist-search-buffer (results)
  "Empty the current buffer and fill it with search info for a given artist."
  (let ((inhibit-read-only t))
	(erase-buffer)
	(draw-now-playing)
	(goto-char (point-max))

(defun draw-now-playing ()
  (with-current-buffer "*soundcloud*"
	(let ((inhibit-read-only t)
		  (current-line (line-number-at-pos)))
	  (goto-char (point-min))
	  (dotimes (i 6) (delete-line))
	  (goto-char (point-min))
	  (mapc 'inl (list "Now Playing" "===========" "" (current-track-detail) (song-progress-bar) ""))
	  (goto-char (point-min))
	  (dotimes (i (- current-line 1)) (next-line))
	  (beginning-of-line))))

(defun song-progress-bar ()
  (if (equal nil *sc-track*)
	  ""
	(let* ((progress (/ (float emms-playing-time) (/ (gethash "duration" *sc-track*) 1000)))
		   (progress-bar-size (- (window-body-width) 2))
		   (completes (min progress-bar-size (floor (* progress-bar-size progress))))
		   (incompletes (- progress-bar-size completes)))
	  (format "[%s%s]"
			  (string-utils-string-repeat "-" completes)
			  (string-utils-string-repeat " " incompletes)))))

(defun track-listing (track)
  "Prints info for a track, followed by a newline."
  (insert (format "%d: %s" idx (gethash "title" track)))
  (newline)
  (setq idx (+ idx 1)))

;;;; private player commands

(defun sc-play-current-track ()
  (setq *sc-playing* t)
  (setq *sc-track* (elt *sc-current-tracks* *sc-track-num*))
  (draw-now-playing)
  (play-track-id (gethash "id" *sc-track*)))

(defun current-track-detail ()
  "Returns string of detailed info for the current track."
  (if (equal nil *sc-track*)
	  "No Track Selected"
	  (format "%s : %s      [ %s / %s ]"
			  (gethash "username" (gethash "user" *sc-track*))
			  (gethash "title" *sc-track*)
			  (string-utils-trim-whitespace emms-playing-time-string)
			  (format-seconds (/ (gethash "duration" *sc-track*) 1000)))))

(defun format-seconds (seconds)
  (format "%02d:%02d" (floor (/ seconds 60)) (mod seconds 60)))

(defun update-now-playing ()
  (deferred:$
	(deferred:wait 500)
	(deferred:nextc it
	  (lambda (x)
		(when (and *sc-playing* (equal nil (active-minibuffer-window)))
		  (draw-now-playing))
		(update-now-playing)))))

(update-now-playing)

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
		  (setq *sc-current-artist* artist-name)
		  (setq *sc-current-tracks* tracks)
		  (setq *sc-track-num* -1)
		  (switch-to-sc-buffer)
		  (draw-sc-artist-buffer tracks))))))

(defun sc-search-artist ()
  (interactive)
  (lexical-let ((artist-query (read-from-minibuffer "Search for artist: ")))
	(deferred:$
	  (search-artist-by-query artist-query)
	  (deferred:nextc it
		(lambda (results)
		  (switch-to-sc-buffer)
		  (draw-sc-artist-search-buffer results))))))

(defun sc-play-track ()
  (interactive)
  (sc-stop)
  (beginning-of-line)
  (re-search-forward "[0-9]+" nil 'move)
  (setq *sc-track-num* (- (string-to-number (buffer-substring-no-properties (line-beginning-position) (point))) 1))
  (sc-play-current-track)
  (beginning-of-line))

(defun sc-pause ()
  (interactive)
  (setq *sc-playing* (not *sc-playing*))
  (emms-pause))

(defun sc-stop ()
  (interactive)
  (setq *sc-playing* nil)
  (emms-stop))

(defun sc-quit ()
  (interactive)
  (setq *sc-playing* nil)
  (emms-stop)
  (kill-buffer "*soundcloud*"))

(defun sc-play-next-track ()
  (interactive)
  (if (or (= -1 *sc-track-num*) (= (length *sc-current-tracks*) (+ 1 *sc-track-num*)))
	(setq *sc-track* nil)
	(progn
	  (setq *sc-track-num* (+ 1 *sc-track-num*))
	  (sc-play-current-track))))

(defun sc-play-previous-track ()
  (interactive)
  (unless (<= *sc-track-num* 0)
	(setq *sc-track-num* (- *sc-track-num* 1))
	(sc-play-current-track)))

;;;; emms hooks

(add-hook 'emms-player-finished-hook 'sc-play-next-track)
