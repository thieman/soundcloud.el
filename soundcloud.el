;;; soundcloud.el --- a SoundCloud client for Emacs

;; Copyright (C) 2013 Travis Thieman

;; GitHub: https://github.com/tthieman/soundcloud.el
;; Author: Travis Thieman <travis.thieman@gmail.com>

;; Package: soundcloud
;; Version: 20131031
;; Package-Requires: ((emms "20131016") (json "1.2") (deferred "20130930") (string-utils "20131022"))
;; Keywords: soundcloud music audio

;; This code is licensed under the WTFPL.

;;            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;;                    Version 2, December 2004

;; Copyright (C) 2013 Travis Thieman <travis.thieman@gmail.com>

;; Everyone is permitted to copy and distribute verbatim or modified
;; copies of this license document, and changing it is allowed as long
;; as the name is changed.

;;            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;;   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION

;;  0. You just DO WHAT THE FUCK YOU WANT TO.

(require 'emms)
(require 'emms-playing-time)
(require 'json)
(require 'url)
(require 'deferred)
(require 'easymenu)
(require 'string-utils)

(emms-playing-time 1)
(add-hook 'emms-player-finished-hook 'sc-play-next-track)

(defvar sc-client-id "0ef9cd3d5b707698df18f2c22db1714b")
(defvar sc-url "http://soundcloud.com")
(defvar sc-api "http://api.soundcloud.com")

(defvar sc-track-start-line 10)

(defvar *sc-last-buffer* nil)
(defvar *sc-current-artist* "")
(defvar *sc-current-tracks* ())
(defvar *sc-search-results* ())
(defvar *sc-track-num* -1)
(defvar *sc-track* nil)
(defvar *sc-playing* nil)

(defun sc-clear-globals ()
  (setq *sc-last-buffer* nil)
  (setq *sc-current-artist* "")
  (setq *sc-current-tracks* ())
  (setq *sc-search-results* ())
  (setq *sc-track-num* -1)
  (setq *sc-track* nil)
  (setq *sc-playing* nil))

(sc-clear-globals)

(defun inl (text) (insert text) (newline))
(defun safe-next-line ()
  (if (= (line-end-position) (point-max)) nil (next-line)))
(defun delete-line ()
  (let ((start (line-beginning-position))
		(end (progn (safe-next-line) (line-beginning-position))))
	(delete-region start end)))

(defun switch-mode (mode-sym)
  (funcall mode-sym))

(setq commands-help
	  '("Interface" "" "a: go to artist" "s: search for artist" "RET: play selection"
		"q: stop playback and quit" ""
		"Playback" "" "p: play/pause current track"))

;;; soundcloud-mode and minor modes

(defvar soundcloud-mode-map
  (let ((map (make-keymap)))
	(suppress-keymap map t)
	(define-key map (kbd "p") 'sc-pause)
	(define-key map (kbd "f") 'sc-play-next-track)
	(define-key map (kbd "b") 'sc-play-previous-track)
	(define-key map (kbd "s") 'sc-search-artist)
	(define-key map (kbd "a") 'sc-load-artist)
	(define-key map (kbd "q") 'sc-quit)
	(define-key map (kbd "RET") 'dispatch-ret)
	map))

;; The normal method of doing this through defining specific RET functions
;; within the derived modes worked in Cocoa Emacs but had weird behavior
;; in XEmacs and Emacs in a console. Hence, dispatch-ret
(defun dispatch-ret ()
  (interactive)
  (cond ((equal major-mode 'soundcloud-player-mode) (sc-play-track))
		((equal major-mode 'soundcloud-artist-search-mode) (sc-goto-artist))))

(defvar soundcloud-player-mode-map
  (let ((map (make-sparse-keymap)))
	(set-keymap-parent map soundcloud-mode-map)
	map))

(defvar soundcloud-artist-search-mode-map
  (let ((map (make-sparse-keymap)))
	(set-keymap-parent map soundcloud-mode-map)
	map))

(setq sc-mode-keywords
	  '((".*\n=+\n" . font-lock-constant-face)  ;; headings
		("[0-9]+\: " . font-lock-variable-name-face)  ;; track numbers
		("\[-+\]" . font-lock-builtin-face)  ;; progress bar
		("\\[.*/.*\\]" . font-lock-variable-name-face)))  ;; track timer

(define-derived-mode soundcloud-mode special-mode "SoundCloud"
  (buffer-disable-undo)
  (setq font-lock-defaults '(sc-mode-keywords))
  (setq truncate-lines t))

(define-derived-mode soundcloud-player-mode soundcloud-mode "SoundCloud Player")
(define-derived-mode soundcloud-artist-search-mode soundcloud-mode "SoundCloud Artist Search")

(easy-menu-define soundcloud-mode-menu soundcloud-mode-map
  "SoundCloud menu"
  '("SoundCloud"
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
	(let ((json-object-type 'hash-table))
	  (json-read-from-string data))))

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
		(emms-play-url stream-url)))
	(deferred:error it
	  (lambda (err)
		(cond
		 ((string-match "^Don't know how to play track" err) (error "emms could not play a stream, make sure mplayer is installed and emms is configured.")))))))

(defun resolve-permalink (permalink)
  (deferred:$
	(deferred:url-retrieve (format "%s/resolve.json?url=%s&client_id=%s"
								   sc-api permalink sc-client-id))
	(deferred:nextc it 'get-json-from-request)
	(deferred:nextc it
	  (lambda (data)
		(gethash "location" data)))
	(deferred:error it
	  (lambda (err)
		(error "Error while resolving artist permalink, try using artist search instead.")))))

(defun get-artist-tracks-by-name (artist-name)
  (deferred:$
	(resolve-permalink (format "%s/%s" sc-url artist-name))
	(deferred:nextc it
	  (lambda (resolved)
		(when (not (equal nil resolved))
		  (deferred:$
			(deferred:url-retrieve (replace-regexp-in-string ".json" "/tracks.json" resolved))
			(deferred:nextc it 'get-json-from-request)))))))

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
  (switch-mode 'soundcloud-mode)
  (let ((inhibit-read-only t))
	(erase-buffer)
	(draw-now-playing)
	(goto-char (point-max))
	(mapc 'inl '("SoundCloud" "==========" ""))
	(mapc 'inl commands-help)))

(defun draw-sc-artist-buffer (tracks)
  "Empty the current buffer and fill it with track info for a given artist."
  (switch-mode 'soundcloud-player-mode)
  (let ((inhibit-read-only t))
	(erase-buffer)
	(draw-now-playing)
	(goto-char (point-max))
	(let ((title-string (format "Tracks by %s (%s)"
								(gethash "username" (gethash "user" (elt *sc-current-tracks* 0)))
								*sc-current-artist*)))
	  (mapc 'inl (list title-string (string-utils-string-repeat "=" (length title-string)) "")))
	(let ((*sc-idx* 1))
	  (mapc 'track-listing *sc-current-tracks*))
	(goto-char (point-min))
	(dotimes (i (- sc-track-start-line 1)) (next-line))
	(beginning-of-line)))

(defun draw-sc-artist-search-buffer (results)
  "Empty the current buffer and fill it with search info for a given artist."
  (switch-mode 'soundcloud-artist-search-mode)
  (let ((inhibit-read-only t))
	(erase-buffer)
	(draw-now-playing)
	(goto-char (point-max))
	(let ((title-string "Search Results"))
	  (mapc 'inl (list title-string (string-utils-string-repeat "=" (length title-string)) "")))
	(let ((*sc-idx* 1))
	  (mapc 'search-listing results))
	(goto-char (point-min))
	(dotimes (i (- sc-track-start-line 1)) (next-line))
	(beginning-of-line)))

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
		   (progress-bar-size (- (window-body-width (get-buffer-window "*soundcloud*")) 2))
		   (completes (min progress-bar-size (floor (* progress-bar-size progress))))
		   (incompletes (- progress-bar-size completes)))
	  (format "[%s%s]"
			  (string-utils-string-repeat "-" completes)
			  (string-utils-string-repeat " " incompletes)))))

(defun track-number-format-string (coll)
  (let ((magnitude (length (number-to-string (length coll)))))
	(concat "%0" (number-to-string magnitude) "d: %s")))

(defun track-listing (track)
  "Prints info for a track, followed by a newline."
  (insert (format (track-number-format-string *sc-current-tracks*)
				  *sc-idx* (gethash "title" track)))
  (newline)
  (setq *sc-idx* (+ *sc-idx* 1)))

(defun search-listing (result)
  "Prints info for a search result, followed by a newline."
  (insert (format (track-number-format-string *sc-search-results*)
				  *sc-idx* (gethash "username" result)))
  (newline)
  (setq *sc-idx* (+ *sc-idx* 1)))

;;;; private player commands

(defun get-current-line-result-number ()
  (beginning-of-line)
  (re-search-forward "[0-9]+" nil 'move)
  (string-to-number (buffer-substring-no-properties (line-beginning-position) (point))))

(defun sc-play-current-track ()
  (setq *sc-playing* t)
  (setq *sc-track* (elt *sc-current-tracks* *sc-track-num*))
  (draw-now-playing)
  (play-track-id (gethash "id" *sc-track*)))

(defun sc-load-artist-by-name (artist-name)
  (lexical-let ((artist-name artist-name))
	(deferred:$
	  (get-artist-tracks-by-name artist-name)
	  (deferred:nextc it
		(lambda (tracks)
		  (if (equal nil tracks)
			  (error (format "Could not find artist %s, try using search instead." artist-name))
			(progn (setq *sc-current-artist* artist-name)
				   (setq *sc-current-tracks* tracks)
				   (setq *sc-track-num* -1)
				   (switch-to-sc-buffer)
				   (draw-sc-artist-buffer tracks))))))))

(defun current-track-detail ()
  "Returns string of detailed info for the current track."
  (if (equal nil *sc-track*)
	  "No Track Selected"
	  (format "%s : %s      [ %s / %s ]"
			  (gethash "username" (gethash "user" *sc-track*))
			  (gethash "title" *sc-track*)
			  (string-utils-trim-whitespace emms-playing-time-string)
			  (format-seconds "%.2m:%.2s" (/ (gethash "duration" *sc-track*) 1000)))))

(defun update-now-playing ()
  (deferred:$
	(deferred:wait 500)
	(deferred:nextc it
	  (lambda (x)
		(when (and *sc-playing* (equal nil (active-minibuffer-window)))
		  (draw-now-playing))
		(update-now-playing)))
	;; TODO: this will keep updater alive, would be good to figure out
	;; why it sometimes breaks and fix it
	(deferred:error it
	  (lambda (err)
		(deferred:wait 2000)
		(update-now-playing)))))

(update-now-playing)

;;;; interactive commmands

;;;###autoload
(defun soundcloud ()
  (interactive)
  (let ((exists (not (equal nil (get-buffer "*soundcloud*")))))
	(if exists
	  (if (equal (buffer-name) "*soundcloud*")
		(switch-to-buffer *sc-last-buffer*)
		(switch-to-sc-buffer))
	  (progn (setq *sc-last-buffer* (current-buffer))
			 (switch-to-sc-buffer)
			 (init-sc-buffer)))))

(defun sc-load-artist ()
  (interactive)
  (lexical-let ((artist-name (read-from-minibuffer "Artist name: ")))
	(sc-load-artist-by-name artist-name)))

(defun sc-search-artist ()
  (interactive)
  (lexical-let ((artist-query (read-from-minibuffer "Search for artist: ")))
	(deferred:$
	  (search-artist-by-query artist-query)
	  (deferred:nextc it
		(lambda (results)
		  (switch-to-sc-buffer)
		  (setq *sc-search-results* results)
		  (draw-sc-artist-search-buffer results))))))

(defun sc-play-track ()
  (interactive)
  (sc-stop)
  (setq *sc-track-num* (- (get-current-line-result-number) 1))
  (sc-play-current-track)
  (beginning-of-line))

(defun sc-goto-artist ()
  (interactive)
  (let ((result-num (get-current-line-result-number)))
	(sc-load-artist-by-name (gethash "permalink" (elt *sc-search-results* (- result-num 1))))
	(beginning-of-line)))

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
  (sc-clear-globals)
  (emms-stop)
  (kill-buffer "*soundcloud*"))

(defun sc-play-next-track ()
  (interactive)
  (when (equal t *sc-playing*)
	(if (or (= -1 *sc-track-num*) (= (length *sc-current-tracks*) (+ 1 *sc-track-num*)))
		(progn (setq *sc-track* nil)
			   (setq *sc-playing* nil))
	  (progn
		(setq *sc-track-num* (+ 1 *sc-track-num*))
		(sc-play-current-track)))))

(defun sc-play-previous-track ()
  (interactive)
  (when (equal t *sc-playing*)
	(unless (<= *sc-track-num* 0)
	  (setq *sc-track-num* (- *sc-track-num* 1))
	  (sc-play-current-track))))

(provide 'soundcloud)

;;; soundcloud.el ends here
