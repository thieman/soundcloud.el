;;; soundcloud.el --- a SoundCloud client for Emacs

;; Copyright (C) 2013 Travis Thieman

;; GitHub: https://github.com/tthieman/soundcloud.el
;; Author: Travis Thieman <travis.thieman@gmail.com>

;; Package: soundcloud
;; Version: 20131209
;; Package-Requires: ((emms "20131016") (json "1.2") (deferred "0.3.1") (string-utils "0.3.2"))
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
(add-hook 'emms-player-finished-hook 'soundcloud-play-next-track)

(defvar soundcloud-client-id "0ef9cd3d5b707698df18f2c22db1714b")
(defvar soundcloud-url "http://soundcloud.com")
(defvar soundcloud-api "http://api.soundcloud.com")

(defvar soundcloud-track-start-line 10)

(defvar *soundcloud-last-buffer* nil)
(defvar *soundcloud-current-artist* "")
(defvar *soundcloud-artist-tracks* (make-hash-table :test 'equal))
(defvar *soundcloud-last-api-result* ())
(defvar *soundcloud-track-num* -1)
(defvar *soundcloud-track* nil)
(defvar *soundcloud-playing* nil)
(defvar *soundcloud-idx* -1)

(defun soundcloud-clear-globals ()
  (setq *soundcloud-last-buffer* nil)
  (setq *soundcloud-current-artist* "")
  (clrhash *soundcloud-artist-tracks*)
  (setq *soundcloud-last-api-result* ())
  (setq *soundcloud-track-num* -1)
  (setq *soundcloud-track* nil)
  (setq *soundcloud-playing* nil)
  (setq *soundcloud-idx* -1))

(soundcloud-clear-globals)

(defun soundcloud-inl (text) (insert text) (newline))
(defun soundcloud-safe-next-line ()
  (if (= (line-end-position) (point-max)) nil (forward-line 1)))
(defun soundcloud-delete-line ()
  (let ((start (line-beginning-position))
        (end (progn (soundcloud-safe-next-line) (line-beginning-position))))
    (delete-region start end)))

(defun soundcloud-switch-mode (mode-sym)
  (funcall mode-sym))

(defun soundcloud-keys (hashtable)
  (let (allkeys)
    (maphash (lambda (kk vv) (setq allkeys (cons kk allkeys))) hashtable)
    allkeys))

;;; soundcloud-mode and minor modes

(defvar soundcloud-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "p") 'soundcloud-pause)
    (define-key map (kbd "f") 'soundcloud-play-next-track)
    (define-key map (kbd "b") 'soundcloud-play-previous-track)
    (define-key map (kbd "s") 'soundcloud-search-artist)
    (define-key map (kbd "a") 'soundcloud-load-artist)
    (define-key map (kbd "q") 'soundcloud-quit)
    (define-key map (kbd "RET") 'soundcloud-dispatch-ret)
    map))

;; The normal method of doing this through defining specific RET functions
;; within the derived modes worked in Cocoa Emacs but had weird behavior
;; in XEmacs and Emacs in a console. Hence, soundcloud-dispatch-ret.
(defun soundcloud-dispatch-ret ()
  (interactive)
  (cond ((equal major-mode 'soundcloud-player-mode) (soundcloud-play-track))
        ((equal major-mode 'soundcloud-artist-search-mode) (soundcloud-goto-artist))))

(defvar soundcloud-player-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map soundcloud-mode-map)
    map))

(defvar soundcloud-artist-search-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map soundcloud-mode-map)
    map))

(defvar soundcloud-mode-keywords
      '((".*\n=+\n" . font-lock-constant-face)  ;; headings
        ("[0-9]+\: " . font-lock-variable-name-face)  ;; track numbers
        ("\[-+\]" . font-lock-builtin-face)  ;; progress bar
        ("\\[.*/.*\\]" . font-lock-variable-name-face)))  ;; track timer

(define-derived-mode soundcloud-mode special-mode "SoundCloud"
  (buffer-disable-undo)
  (setq font-lock-defaults '(soundcloud-mode-keywords))
  (setq truncate-lines t))

(define-derived-mode soundcloud-player-mode soundcloud-mode "SoundCloud Player")
(define-derived-mode soundcloud-artist-search-mode soundcloud-mode "SoundCloud Artist Search")

(easy-menu-define soundcloud-mode-menu soundcloud-mode-map
  "SoundCloud menu"
  '("SoundCloud"
    ["Play/Pause" soundcloud-pause t]
    ["Play Previous Track" soundcloud-play-previous-track t]
    ["Play Next Track" soundcloud-play-next-track t]
    "---"
    ["Load Artist" soundcloud-load-artist t]
    ["Search for Artist" soundcloud-search-artist t]
    ["Quit" soundcloud-quit t]))

;;;; deferred functions for talking to SoundCloud

(defun soundcloud-get-data-from-request (buf)
  (with-current-buffer buf
    (goto-char (point-min))
    (re-search-forward "^$" nil 'move)
    (setq data (buffer-substring-no-properties (point) (point-max)))
    (kill-buffer buf))
  data)

(defun soundcloud-get-json-from-request (buf)
  (let ((data (soundcloud-get-data-from-request buf)))
    (let ((json-object-type 'hash-table))
      (json-read-from-string data))))

(defun soundcloud-get-stream-url (track-id)
  (deferred:$
    (deferred:url-retrieve (format "%s/tracks/%d.json?client_id=%s"
                                   soundcloud-api track-id soundcloud-client-id))
    (deferred:nextc it 'soundcloud-get-json-from-request)
    (deferred:nextc it
      (lambda (json-data)
        (format "%s?client_id=%s" (gethash "stream_url" json-data) soundcloud-client-id)))))

(defun soundcloud-play-track-id (track-id)
  (deferred:$
    (soundcloud-get-stream-url track-id)
    (deferred:nextc it
      (lambda (stream-url)
        (emms-play-url stream-url)))
    (deferred:error it
      (lambda (err)
        (cond
         ((string-match "^Don't know how to play track" err) (error "emms could not play a stream, make sure mplayer is installed and emms is configured.")))))))

(defun soundcloud-resolve-permalink (permalink)
  (deferred:$
    (deferred:url-retrieve (format "%s/resolve.json?url=%s&client_id=%s"
                                   soundcloud-api permalink soundcloud-client-id))
    (deferred:nextc it 'soundcloud-get-json-from-request)
    (deferred:nextc it
      (lambda (data)
        (gethash "location" data)))
    (deferred:error it
      (lambda (err)
        (error "Error while resolving artist permalink, try using artist search instead.")))))

(defun soundcloud-get-artist-tracks-by-name (artist-name)
  (deferred:$
    (soundcloud-resolve-permalink (format "%s/%s" soundcloud-url artist-name))
    (deferred:nextc it
      (lambda (resolved)
        (when (not (equal nil resolved))
          (deferred:$
            (deferred:url-retrieve (replace-regexp-in-string ".json" "/tracks.json" resolved))
            (deferred:nextc it 'soundcloud-get-json-from-request)))))))

(defun soundcloud-search-artist-by-query (artist-query)
  (deferred:$
    (deferred:url-retrieve (format "%s/users.json?q=%s&client_id=%s"
                                   soundcloud-api artist-query soundcloud-client-id))
    (deferred:nextc it 'soundcloud-get-json-from-request)))

;;;; the *soundcloud* buffer

(defun soundcloud-switch-to-buffer ()
  (let ((buf (or (get-buffer "*soundcloud*")
                 (generate-new-buffer "*soundcloud*"))))
    (switch-to-buffer buf)))

(defun soundcloud-buffer-set ()
  (let ((buf (or (get-buffer "*soundcloud*")
                 (generate-new-buffer "*soundcloud*"))))
    (set-buffer buf)))

(defun soundcloud-init-buffer ()
  "Turns the current buffer into a fresh SoundCloud buffer."
  (soundcloud-switch-mode 'soundcloud-mode)
  (let ((inhibit-read-only t))
	(save-excursion
    (erase-buffer)
    (soundcloud-draw-now-playing)
    (goto-char (point-max))
    (mapc 'soundcloud-inl'("SoundCloud" "==========" ""))
    (mapc 'soundcloud-inl'("Interface" "" "a: go to artist" "s: search for artist" "RET: play selection"
                 "q: stop playback and quit" ""
                 "Playback" "" "p: play/pause current track")))))

(defun soundcloud-draw-artist-buffer (tracks)
  "Empty the current buffer and fill it with track info for a given artist."
  (soundcloud-switch-mode 'soundcloud-player-mode)
  (let ((inhibit-read-only t))
	(save-excursion
    (erase-buffer)
    (soundcloud-draw-now-playing)
    (goto-char (point-max))
    (let ((title-string (format "Tracks by %s (%s)"
                                (gethash "username" (gethash "user" (elt (current-artist-tracks) 0)))
                                *soundcloud-current-artist*)))
      (mapc 'soundcloud-inl(list title-string (string-utils-string-repeat "=" (length title-string)) "")))
    (let ((*soundcloud-idx* 1))
      (mapc 'track-listing (current-artist-tracks))))))

(defun soundcloud-draw-artist-search-buffer (results)
  "Empty the current buffer and fill it with search info for a given artist."
  (soundcloud-switch-mode 'soundcloud-artist-search-mode)
  (let ((inhibit-read-only t))
	(save-excursion
	  (erase-buffer)
	  (soundcloud-draw-now-playing)
	  (goto-char (point-max))
	  (let ((title-string "Search Results"))
		(mapc 'soundcloud-inl(list title-string (string-utils-string-repeat "=" (length title-string)) "")))
	  (let ((*soundcloud-idx* 1))
		(mapc 'search-listing results)))))

(defun soundcloud-draw-now-playing ()
  (with-current-buffer "*soundcloud*"
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (dotimes (i 6) (soundcloud-delete-line))
        (goto-char (point-min))
        (mapc 'soundcloud-inl(list "Now Playing" "===========" "" (soundcloud-current-track-detail) (song-progress-bar) ""))))))

(defun song-progress-bar ()
  (if (equal nil *soundcloud-track*)
      ""
    (let* ((progress (/ (float emms-playing-time) (/ (gethash "duration" *soundcloud-track*) 1000)))
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
  (insert (format (track-number-format-string (current-artist-tracks))
                  *soundcloud-idx* (gethash "title" track)))
  (newline)
  (setq *soundcloud-idx* (+ *soundcloud-idx* 1)))

(defun search-listing (result)
  "Prints info for a search result, followed by a newline."
  (insert (format (track-number-format-string *soundcloud-last-api-result*)
                  *soundcloud-idx* (gethash "username" result)))
  (newline)
  (setq *soundcloud-idx* (+ *soundcloud-idx* 1)))

;;;; private player commands

(defun current-artist-tracks ()
  (gethash *soundcloud-current-artist* *soundcloud-artist-tracks*))

(defun random-artist-tracks ()
  (let* ((artists (soundcloud-keys *soundcloud-artist-tracks*))
         (artist (elt artists (random (length artists)))))
    (gethash artist *soundcloud-artist-tracks*)))

(defun get-current-line-result-number ()
  (beginning-of-line)
  (re-search-forward "[0-9]+" nil 'move)
  (string-to-number (buffer-substring-no-properties (line-beginning-position) (point))))

(defun soundcloud-play-current-track ()
  (setq *soundcloud-playing* t)
  (setq *soundcloud-track* (elt (current-artist-tracks) *soundcloud-track-num*))
  (soundcloud-draw-now-playing)
  (soundcloud-play-track-id (gethash "id" *soundcloud-track*)))

(defun soundcloud-load-artist-by-name (artist-name)
  (lexical-let ((artist-name artist-name))
    (deferred:$
      (soundcloud-get-artist-tracks-by-name artist-name)
      (deferred:nextc it
        (lambda (tracks)
          (if (equal nil tracks)
              (error (format "Could not find artist %s, try using search instead." artist-name))
            (progn (setq *soundcloud-current-artist* artist-name)
                   (remhash artist-name *soundcloud-artist-tracks*)
                   (puthash artist-name tracks *soundcloud-artist-tracks*)
                   (setq *soundcloud-track-num* -1)
                   (soundcloud-switch-to-buffer)
                   (soundcloud-draw-artist-buffer tracks))))))))

(defun soundcloud-current-track-detail ()
  "Returns string of detailed info for the current track."
  (if (equal nil *soundcloud-track*)
      "No Track Selected"
      (format "%s : %s      [ %s / %s ]"
              (gethash "username" (gethash "user" *soundcloud-track*))
              (gethash "title" *soundcloud-track*)
              (string-utils-trim-whitespace emms-playing-time-string)
              (format-seconds "%.2m:%.2s" (/ (gethash "duration" *soundcloud-track*) 1000)))))

(defun soundcloud-update-now-playing ()
  (deferred:$
    (deferred:wait 500)
    (deferred:nextc it
      (lambda (x)
        (when (and *soundcloud-playing* (equal nil (active-minibuffer-window)))
          (soundcloud-draw-now-playing))
        (soundcloud-update-now-playing)))
    ;; TODO: this will keep updater alive, would be good to figure out
    ;; why it sometimes breaks and fix it
    (deferred:error it
      (lambda (err)
        (deferred:wait 2000)
        (soundcloud-update-now-playing)))))

(soundcloud-update-now-playing)

;;;; interactive commmands

;;;###autoload
(defun soundcloud ()
  (interactive)
  (let ((exists (not (equal nil (get-buffer "*soundcloud*")))))
    (if exists
      (if (equal (buffer-name) "*soundcloud*")
        (switch-to-buffer *soundcloud-last-buffer*)
        (progn (setq *soundcloud-last-buffer* (current-buffer))
               (soundcloud-switch-to-buffer)))
      (progn (setq *soundcloud-last-buffer* (current-buffer))
             (soundcloud-switch-to-buffer)
             (soundcloud-init-buffer)))))

(defun soundcloud-load-artist ()
  (interactive)
  (lexical-let ((artist-name (read-from-minibuffer "Artist name: ")))
    (soundcloud-load-artist-by-name artist-name)))

(defun soundcloud-search-artist ()
  (interactive)
  (lexical-let ((artist-query (read-from-minibuffer "Search for artist: ")))
    (deferred:$
      (soundcloud-search-artist-by-query artist-query)
      (deferred:nextc it
        (lambda (results)
          (soundcloud-switch-to-buffer)
          (setq *soundcloud-last-api-result* results)
          (soundcloud-draw-artist-search-buffer results))))))

(defun soundcloud-play-track ()
  (interactive)
  (soundcloud-stop)
  (setq *soundcloud-track-num* (- (get-current-line-result-number) 1))
  (soundcloud-play-current-track)
  (beginning-of-line))

(defun soundcloud-goto-artist ()
  (interactive)
  (let ((result-num (get-current-line-result-number)))
    (soundcloud-load-artist-by-name (gethash "permalink" (elt *soundcloud-last-api-result* (- result-num 1))))
    (beginning-of-line)))

(defun soundcloud-pause ()
  (interactive)
  (setq *soundcloud-playing* (not *soundcloud-playing*))
  (emms-pause))

(defun soundcloud-stop ()
  (interactive)
  (setq *soundcloud-playing* nil)
  (emms-stop))

(defun soundcloud-quit ()
  (interactive)
  (soundcloud-clear-globals)
  (emms-stop)
  (kill-buffer "*soundcloud*"))

(defun soundcloud-play-next-track ()
  (interactive)
  (when (equal t *soundcloud-playing*)
    (if (or (= -1 *soundcloud-track-num*) (= (length (current-artist-tracks)) (+ 1 *soundcloud-track-num*)))
        (progn (setq *soundcloud-track* nil)
               (setq *soundcloud-playing* nil))
      (progn
        (setq *soundcloud-track-num* (+ 1 *soundcloud-track-num*))
        (soundcloud-play-current-track)))))

(defun soundcloud-play-previous-track ()
  (interactive)
  (when (equal t *soundcloud-playing*)
    (unless (<= *soundcloud-track-num* 0)
      (setq *soundcloud-track-num* (- *soundcloud-track-num* 1))
      (soundcloud-play-current-track))))

(provide 'soundcloud)

;;; soundcloud.el ends here
