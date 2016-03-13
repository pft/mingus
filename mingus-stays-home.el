;;; mingus-stays-home.el --- Interface for local MPD server

;;                    _                    _
;;  _ __ ___      ___| |_ __ _ _   _ ___  | |__   ___  _ __ ___   ___
;; | '_ ` _ \    / __| __/ _` | | | / __| | '_ \ / _ \| '_ ` _ \ / _ \
;; | | | | | |_  \__ \ || (_| | |_| \__ \ | | | | (_) | | | | | |  __/
;; |_| |_| |_(_) |___/\__\__,_|\__, |___/ |_| |_|\___/|_| |_| |_|\___|
;;                             |___/

;; Copyright (C) 2006-2011, 2015 Niels Giesen <com dot gmail at niels dot giesen, in
;; reversed order>

;; Author: Niels Giesen <pft on #emacs>

;; Contributors (with patches and bug reports): Jeremie Lasalle
;; Ratelle, "Lexa12", Marc Zonzon, Mark Taylor, Drew Adams and Alec
;; Heller

;; Version: Open Letter to Duke, or: 0.33
;; Latest version can be found at http://github.com/pft/mingus/
;; For Changes, please view http://github.com/pft/mingus/commits/master

;; In version 0.33 id3-related stuff has been removed, in favour if
;; the more general taggit.el to be found at
;; http://github.com/pft/elisp-assorted. When taggit has been loaded
;; before will be require'd and some keys bound to it.

;; NEW in 0.23:

;; Removed silly dependencies for calculating time of song (mpd already knows
;; about them: RTFM afore thou proggest pft!).

;; Substituted sentinels for timers.

;; NEW in 0.22:

;; Added ogg and flac support for metadata (the mingus-id3-* functions)

;; Added flac and wav support for `mingus-burn-it'

;; Fixed a shitload of bugs; introduced just as many (hopefully not).

;; Increased number of dependencies to do all this (see below under 'Further
;; requirements').

;; NOTE (at version 0.21) : Version 0.21 is actually the first published version
;; of mingus-stays-home. To provide easy checking whether the versions of
;; mingus.el and mingus-stays-home (should) work together, it was decided to
;; give them concurrent version numbers.

;; Keywords: multimedia, elisp, music, mpd

;; This file is *NOT* part of GNU Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; COMMENTARY:

;; INSTALLATION INSTRUCTIONS:

;; Byte-compile, IN ORDER, repeat: IN ORDER, the files mingus.el and
;; mingus-stays-home.el

;; Then put the following in your .emacs:

;; (add-to-list 'load-path "/path/where/mingus-and-mingus-stays-home-reside")
;; (autoload 'mingus "mingus-stays-home" nil t)

;; Make sure you have a version of mingus.el of the same version number
;; available in your load-path.

;; FUNCTIONALITY

;;; Goal: provide extra functionality to mingus that is only possible when mpd
;;; is run locally, i.e. the frontend has full access to the configuration file
;;; and the filesystem used by mpd.

;;; Extent: currently undecided, but at least:

;;; * integration with other emacs-modes:

;;; ** MINIBUFFER COMPLETION:

;;; using the minibuffer in a better way to add songs or directories, than
;;; mingus-insert previously did. This is done with the function
;;; `mingus-dwim-add'. `mingus-insert' keeps its old behaviour.

;;; ** BURNING CD'S with `mingus-burns'! In DEVELOPMENT stage, but nonetheless
;;; ** usable.

;;; Make sure the directory referred to by the variable
;;; `mingus-burns-tmp-wav-dir' (default: "~/.mingus/tmp") exists. This is where
;;; the intermediate files in the burning process will go.

;;; NOTE mingus-burns currently does NOT check for the capacity of the inserted
;;; disk, nor whether it is writable. You are left to your own devices for
;;; this. The green-redness if the indication bar displays just what is usually
;;; encountered.

;;; INFO: mingus-burns records cd's with the contents of the current
;;; playlist of mpd. It can also just be used to see what the duration
;;; of a playlist and its respective songs is. The playlist-editing
;;; functions are *not* as wide as in the normal playlist buffer
;;; (tagged `*Mingus*'). However, basic support for deletion of song
;;; at point and of songs in region does exist, with respect for the
;;; variables `*mingus-point-of-insertion*' and
;;; `*mingus-marked-list*'. You can add songs without leaving the
;;; *Mingus Burns* buffer as well, by use of `mingus-dwim-add'. Adding
;;; songs leads to a complete recomputation of song duration, which
;;; may take a while, so the fastest way to achieve nice contents for
;;; your CD is to first add a bunch of songs (but not way to much of
;;; course), and then delete the ones you can do without. It looks
;;; more attractive than the default playlist buffer, but because of
;;; the slowness inherent in computing duration, it is not planned to
;;; be a full substitute for it, at least not in the near future.

;;; REQUIREMENTS:

;;; version 0.22 required too much, glad to be able to trim it down again in
;;; version 0.23 after having read some documentation:

;;; decoding:

;;; sox -- for decoding songs to the .wav format.

;;; flac -- for decoding flacs to the .wav format. Must get flac into sox...

;;; burning:

;;; cdrecord (or: wodim, if you stick to Debian guidelines) -- for burning the
;;; cd. Actually, this can be customized with the variables
;;; `mingus-blank-string' and `mingus-burns-format-string'

;;; ** VIEW COVER ART (ach, why not?)

;;; when available, you can use `thumbs' to view cover art from within Mingus:
;;; see `mingus-thumbs' (key: "T")

;;; known bugs: sometimes unicode strings in tags can lead to errors.

;;; For any questions, suggestions and so forth, contact me at the address
;;; stated at the top of this file.

;;; ( use dto's --already deprecated-- golisp-mode to make use of the indexing
;;; in this file: find it at http://dto.freeshell.org/notebook/GoLisp.html )

;;; Code:
(require 'mingus)
;; Even though Mingus already requires cl, in Emacs23 we have to silence the
;; compiler (this might be a regression):
(require 'cl)
(eval-when-compile (load "cl-macs"))
(require 'url)
(require 'taggit nil t)
;;;; {{Update Help Text}}

(setq mingus-help-text
      (replace-regexp-in-string
       "MORE ELABORATE INSTRUCTIONS:"
       "BURNER KEYS:

B                       mingus-burn-it
D                       mingus-burns-decode-playlist
E                       mingus-blank-disk

MORE ELABORATE INSTRUCTIONS:"
       (replace-regexp-in-string
        "U                       mingus-unmark-all"
                (format
                 "U                       mingus-unmark-all%s"
                 (if (featurep 'taggit)
                  "
#                       taggit-interactive
e                       taggit (edit buffer)" ""))
        (replace-regexp-in-string
         "mingus-browser:    3"
         "mingus-browser:    3
mingus-burns:      4\n" mingus-help-text t) t) t))


;;;; {{Update *mingus-header-when-empty*}}
(setq *mingus-header-when-empty* "Press ? for help, 3 for Mingus Browser, 4
for Mingus Burns and 0 for dired\n\nPress 2 to come back here from within
Mingus buffers, M-x mingus from elsewhere.")

(defgroup mingus-stays-home nil
  "Group for customization of variables used when mpd is run on
        the same computer as mingus"
  :group 'mingus)

(eval-when (load)
  (unless (string-match "GNU Emacs 21" (version))
        ;;fixme: make this work in emacs21 too
        (when (featurep 'taggit)
          (define-key-after mingus-playlist-map [menu-bar mingus taggit-interactive]
                '("Set tags" . taggit-interactive) 'dired)
          (define-key-after mingus-playlist-map [menu-bar mingus taggit]
                '("Visit edit buffer for tags" . taggit) 'taggit-interactive)
          (define-key mingus-playlist-map "#" 'taggit-interactive)
          (define-key mingus-playlist-map "e" 'taggit))))

;;;; {{Thumbs}}
(eval-when (compile)
  (when (featurep 'thumbs)
    (defun mingus-thumbs ()
      "In mingus, open a buffer with cover art found in the directory of song at point."
      (interactive)
      (thumbs (_mingus-get-parent-dir)))

    (define-key mingus-playlist-map "T" 'mingus-thumbs)
    (define-key mingus-browse-map "T" 'mingus-thumbs)))

;;;; {{Customization}}
(defgroup mingus-burns nil
  "Customization group for recording cd's with `mingus'"
  :group 'mingus)

(defcustom mingus-burns-tmp-wav-dir "~/.mingus/tmp"
  "Directory to hold temporary .wav files for a recording session.

This directory will be created when it does not exist."
  :group 'mingus-burns
  :type '(file))

(defcustom mingus-burns-format-string
  "wodim dev=%s -eject -pad -audio -speed=%s -fix"
  "Format string for the burning process.
This string can be fed, in order:

mingus-burns-device
mingus-burns-speed

However, you can just as well specify it directly in this string."
  :group 'mingus-burns
  :type '(string))

(defcustom mingus-burns-device "/dev/cdrom"
  "Device name to use for recording"
  :group 'mingus-burns
  :type '(choice (file :tag "File (such as /dev/cdrom)")
                 (string :tag "Description (such as ATA:1,0,0)")))

(defcustom mingus-burns-speed 2
  "Speed of cd-recording device"
  :group 'mingus-burns
  :type 'number)

(defcustom mingus-blank-string
  "wodim -eject blank=all"
  "Command with which to blank a cd."
  :group 'mingus-burns
  :type 'string)


;;;; {{Keymap}}
(defconst mingus-burnin-map (copy-keymap mingus-global-map)
  "Burnin keymap for `mingus'")

(define-key mingus-burnin-map " " 'scroll-up)
(define-key mingus-burnin-map "\C-m" 'mingus-burns-play)
(define-key mingus-burnin-map "d" 'mingus-burns-del)

(define-key mingus-burnin-map "B" 'mingus-burn-it)
(define-key mingus-burnin-map "D" 'mingus-burns-decode-playlist)
(define-key mingus-burnin-map "E" 'mingus-blank-disk)

(define-key mingus-burnin-map [menu-bar mingus sep-playlist-editing]
  '(menu-item "--"))
(define-key mingus-burnin-map [menu-bar mingus unset]
  '("Unset Insertion Point" . mingus-unset-insertion-point))
(define-key mingus-burnin-map [menu-bar mingus sep4]
  '(menu-item "--"))
(define-key mingus-burnin-map [menu-bar mingus burn]
  '(menu-item "Burn CD" mingus-burn-it :burnin "Burn a cd with current contents of the playlist"))
(define-key mingus-burnin-map [menu-bar mingus decode]
  '(menu-item "Decode Playlist" mingus-burns-decode-playlist :burnin "Decode current contents of the playlist to .wav files"))
(define-key mingus-burnin-map [menu-bar mingus blank]
  '(menu-item "Blank Disk" mingus-blank-disk :burnin "Blank disk"))
(define-key mingus-burnin-map [menu-bar mingus sep3]
  '(menu-item "--"))

(define-key mingus-burnin-map [menu-bar mingus browser]
  '(menu-item "Browser" mingus-browse :burnin "go to browser"))
(define-key mingus-burnin-map [menu-bar mingus playlist]
  '(menu-item "Playlist" mingus :burnin "go to playlist"))

(define-key mingus-burnin-map "0" 'mingus-dired-file)
(define-key mingus-burnin-map [menu-bar mingus dired]
  '(menu-item "Dired" mingus-dired-file :burnin "look song up in dired"))

(define-key mingus-playlist-map "4" 'mingus-burns)

(define-key mingus-playlist-map [menu-bar mingus burner]
  '(menu-item "Burner" mingus-burns))

(define-key mingus-browse-map "4" 'mingus-burns)
(define-key mingus-help-map [menu-bar mingus burner]
  '(menu-item "Burner" mingus-burns))

(define-key mingus-help-map "4" 'mingus-burns)
(define-key mingus-browse-map [menu-bar mingus burner]
  '(menu-item "Burner" mingus-burns))

;;;; {{Generic Functions}}
;; Replace with: (?)
(defun mingus-read-entire-metadata ()
  (mpd-get-songs mpd-inter-conn "playlistinfo")
  ;; (mapcar (lambda (sublist)
  ;;           (mapcar (lambda (item)
  ;;                     (cond
  ;;                      ((eq item 'Pos) 'Pos)
  ;;                      ((symbolp item)
  ;;                       (intern-soft (concat ":" (downcase (symbol-name item)))))
  ;;                      (t item)))
  ;;                   sublist))
  ;;         (mpd-get-songs mpd-inter-conn "playlistinfo"))
  )

(defun mingus-what-type (string)
  "Return symbol, based on extension"
  (string-match "\\.\\([^.]*\\)$" string)
  (intern-soft (downcase (match-string 1 string))))

(defun mingus-burns-get-name-for-shell ()
  (shell-quote-argument
   (mingus-burns-get-name)))

(defun mingus-burns-get-name ()
  (format "%s%s" mingus-mpd-root
          (getf (mingus-get-details) 'file)))

;; keep me
(defun mingus-burns-color-bar (pos-beg-from-bol pos-end-from-bol color)
  (put-text-property  (+ pos-beg-from-bol (point-at-bol))
                      (+ pos-end-from-bol (point-at-bol))
                      'face `(          ;(background-color . "#000000")
                              (foreground-color . ,color)
                              (weight . "bold"))))

;; (defun mingus-burns-remove-non-local-files (contents)
;;   (mapconcat 'identity (remove-if (lambda (item) (string-match "^#[0-9]+) http://" item))  (split-string contents "\n")) "\n"))

;; STILL NEED TO GET SOMETHING FROM THIS
;; (defun mingus-burns ()
;;   "Go to the buffer in `mingus' where recording takes place."
;;   (interactive)
;;   (switch-to-buffer "*Mingus Burns*")
;;   (setq major-mode 'mingus-burns)
;;   (setq mode-name "Mingus-burns")
;;   (use-local-map mingus-burnin-map)
;;   (let ((buffer-read-only nil)
;;      (new-contents (shell-command-to-string "mpc --format \"%file%\" playlist")))
;;     (if (string-match "^#[0-9]+) http://" new-contents)
;;      (message "There is a non-local file (a stream) in the playlist. Please remove before using mingus-burns.")
;;     (when (or (= (point-max) (point-min)) ;if there's an empty-buffer or the playlist has changed in the mean time
;;            (not (equal new-contents *mingus-buffer-contents*)))
;;       (erase-buffer)
;;       (cond ((< 1 (length new-contents))
;;           (message "Computing lengths, this may take a while")
;;           (insert (setq *mingus-buffer-contents* new-contents))
;;           (mingus-burns-invisible)
;;           (mingus-compute-buffer-length))
;;          (t (insert "Press ? for help, 2 for Mingus Playlist, 3 for Mingus Browser and 0 for Dit.red\n\nPress 4 from within Mingus buffers to come back here, M-x mingus-burns from elsewhere.\n\nPlaylist is empty, please add some songs.\n\nYou can do so with either mingus-dwim-add, with mingus-browse or from within dired.")
;;             (goto-char (point-min)))))))
;;   (setq buffer-read-only t))

;; throw me away
(defun mingus-burns-del ()
  "Delete song at point."
  (interactive)
  (block nil
    (save-excursion
      (let* ((buffer-read-only nil)
             (length-of-song-at-p (getf (mingus-get-details) 'Time))
             (min:secs (mingus-sec->min:sec (or length-of-song-at-p (return nil)))))
      (if (null min:secs) (message "Nothing to delete")
        (mpd-delete mpd-inter-conn (1- (mingus-line-number-at-pos)))
        (mingus-reset-point-of-insertion)
        (delete-region (point-at-bol) (point-at-bol 2))
        (decf (get '*mingus-b-session* :total-time) length-of-song-at-p)
        (mingus-goto-line (- (mingus-line-number-at-pos (point-max)) 2))
        (delete-region (point) (point-max))
        (mingus-2-burns-bar (get '*mingus-b-session* :total-time))))
    (forward-line -1))))

(defun mingus-burns-play ()
  (interactive)
  (mingus-play (plist-get (get-text-property (point-at-bol) 'metadata) 'Pos)))

;;;; {{Recording}}
(defvar *mingus-b-session* nil)

(defun mingus-b-the-cd ()
  "Perfom the act of burning a cd from mpd playlist
Use M-x mingus-decode-playlist if you just want to decode the files."
  (message "Mingus-a-Burning... C-x b *Mingus-Output* to watch the process.")
  (set-process-sentinel
   (apply 'start-process-shell-command "mingburn" "*Mingus-Output*"
         (format mingus-burns-format-string mingus-burns-device mingus-burns-speed)
         (mapcar
          (lambda (item)
            (shell-quote-argument (mingus-dec-transl-src->dest (getf item 'file))))
          *mingus-b-session*))
   'mingus-b-ask-to-keep-session))

(defun mingus-b-ask-to-keep-session (&optional process event) ;a sentinel
  (when
          (string-match "^exited abnormally with code " event)
        (switch-to-buffer "*Mingus-Output*")
        (and (not (eq (process-status process) 'exit))
                 (stop-process process))
        (error "Something happened that should not have. Inspect *Mingus-Output* buffer for any hints"))
  (put '*mingus-b-session* :burn nil)   ;always reset to not go burning!
  (if (y-or-n-p "Remove temporary wave files?")
      (mapc 'delete-file
            (mapcar (lambda (item)
                      (mingus-dec-transl-src->dest (getf item 'file)))
                    *mingus-b-session*)))
  (unless (y-or-n-p "Keep session data? ")
    (setq *mingus-b-session* nil)))

(defun mingus-burn-it ()
  "Burn a disk from current sessiondata"
  (interactive)
  (unless  (and *mingus-b-session*
                (y-or-n-p "Still got an old session lying, do you want to use this? "))
    (setq *mingus-b-session* (mingus-read-entire-metadata)))
  (put '*mingus-b-session* :burn t)
  (mingus-dec-list))

(defun mingus-burns-decode-playlist ()
  "Decode current playlist and put the resulting wave files in the directory `mingus-tmp-wav-dir'"
  (interactive)
  (setq *mingus-b-session* (mingus-read-entire-metadata))
  (put '*mingus-b-session* :burn nil)
  (mingus-dec-list))

(defun mingus-dec-list (&optional process event)
  "Decode contents referred to by *mingus-b-session* and put the resulting wave files in the directory `mingus-tmp-wav-dir'."
  (when
          (and event
                   (string-match "^exited abnormally with code " event))
        (switch-to-buffer "*Mingus-Output*")
        (error "Something happened that should not have. Inspect *Mingus-Output* buffer for any hints"))
  (let* ((data *mingus-b-session*)
         (file (mingus-cdr-down-sessiondata data)))
    (message "Abort with M-x mingus-dec-abort")
    (sit-for 2)                         ;so people can see the message above
    (cond
     (file
      (mingus-dec-file-rel-to-mpd-root file)
      (set-process-sentinel (get-process "mingdec")
                            'mingus-dec-list))
     (t (message "Decoding finished!")
        (if (get '*mingus-b-session* :burn)
            (mingus-b-the-cd))))))

(defun mingus-dec-abort ()
  "Abort a decoding session."
  (interactive)
  (let ((sessiondata *mingus-b-session*))
    (setq *mingus-b-session* nil)
    (kill-process (get-process "mingdec"))
    (message "Aborting decoding process...")
    (sit-for 2)
    (put '*mingus-b-session* :burnp nil)
    (if (y-or-n-p "Keep session data? ")
        (setq *mingus-b-session* sessiondata))))

(defun mingus-cdr-down-sessiondata (data)
  "Recursively find the first non-existing destination pathname from DATA."
  (cond ((null data) nil)
        (t (if (file-exists-p (mingus-dec-transl-src->dest (getf (car data) 'file)))
               (mingus-cdr-down-sessiondata (cdr data))
             (getf (car data) 'file)))))

;; translation functions
(defun mingus-transl-mpd->realroot (file)
  "Return absolute path of FILE, which is a file in de mpd database in filesystem."
  (expand-file-name (format "%s%s" mingus-mpd-root file)))

(defun mingus-dec-transl-src->dest (name)
  "Return NAME, stripped of its parent and concatenated to `mingus-burns-tmp-wav-dir'"
  (concat (expand-file-name mingus-burns-tmp-wav-dir) "/"
          (replace-regexp-in-string "^.*/" ""
                                    (replace-regexp-in-string  "\\.\\([^.]*\\)$" "wav" name nil nil 1))))

(defun mingus-dec-file-rel-to-mpd-root (file)
  "Take FILE, which is relative to the mingus-mpd-root, and decode it into `mingus-tmp-wav-dir'."
  (let ((src (mingus-transl-mpd->realroot file))
        (dest (mingus-dec-transl-src->dest file)))
    (mingus-dec src dest)))

(defun mingus-dec (src dest &optional p)
  "Decode music file SRC to DEST.
Both filename are absolute paths in the filesystem"
  (interactive p)
  (when (not (file-exists-p mingus-burns-tmp-wav-dir))
        (make-directory mingus-burns-tmp-wav-dir t))
  (unless (and (not p)(file-exists-p dest))
    (case (mingus-what-type src)
      (flac (message "Decoding %s to %s" src dest)
            (start-process "mingdec" "*Mingus-Output*" "flac" "-sd" src "-o" dest))
      (wav  (make-symbolic-link src dest)
            (start-process "mingdec" "*Mingus-Output*" "flac")) ;just a dummy so we will have a process!
      (t (message "Decoding %s to %s" src dest)
         (start-process "mingdec" "*Mingus-Output*" "sox" "-V" src "-t" ".wav" dest)))))

(defun mingus-burn-mode ()
  "Mingus burning mode
\\{mingus-burnin-map}"
  (setq major-mode 'mingus-burn-mode)
  (setq mode-name "Mingus-burns")
  (setq buffer-undo-list t)
  (use-local-map mingus-burnin-map)
  (setq buffer-read-only t))

(defun mingus-burns ()
  (interactive)
  (switch-to-buffer "*Mingus Burns*")
  (mingus-burn-mode)
  (let* ((data (mingus-read-entire-metadata))
         (total-time 0)
         buffer-read-only
         (httpp (member* nil data :test (lambda (elt item)
                                          (null (getf item 'Time))))))
    (if httpp
        (progn
          (message "There is a non-local file in the playlist (%s);\nPlease remove it as I am (still) too stupid to handle this situation " (getf (car httpp) 'file))
          (bury-buffer "*Mingus Burns*"))

      (put '*mingus-b-session* :total-time total-time)
      (erase-buffer)
      (mapc
       (lambda (item)
         (insert (format "%5s %s\n" (mingus-sec->min:sec (getf item 'Time))
                         (truncate-string-to-width
                          (replace-regexp-in-string "\\(.*/\\)+" ""
                                                    (or
                                                     (getf item 'Title)
                                                     (getf item 'file))
                                                    t t 1)
                          (- (window-width) 7) nil 32 "…")))
         (forward-line -1)
         (mingus-burns-color-bar 0 5 "orange")
         (mingus-burns-color-bar 5 (- (point-at-eol) (point-at-bol)) "lightblue")
         (put-text-property (point-at-bol) (point-at-eol) 'details item)

         (incf total-time (getf item 'Time))
                                        ;        (incf total-time (min:sec->secs (getf item 'Time)))
         (forward-line))
       data)
      (put '*mingus-b-session* :total-time total-time)
      (mingus-2-burns-bar total-time)))
  (goto-char (point-min)))

(defun mingus-2-burns-bar (seconds)
  "Make a time-line bar at the bottom of the *Mingus Burns* buffer."
  (let* ((total-seconds seconds)
         (buffer-read-only nil)
         (border (format "%s30%s50%s70%s75%s80%s"
                         (make-string 29 ?\-)
                         (make-string 18 ?\-)
                         (make-string 18 ?\-)
                         (make-string 3 ?\-)
                         (make-string 3 ?\-)
                         (make-string (max (- (window-width) 82) 0) ?\-)))
         (window-width (window-width))
         (string (format "%s\n%s %s \n%s"
                         border         ;bar representing total cd time
                         (make-string (min (/ total-seconds 60) (- window-width 9)) ?|)
                                        ;bar representing percentage filled
                         (multiple-value-bind (min sec)
                             (floor* total-seconds 60)
                           (format "%d:%s" min (if (< sec 10)
                                                   (format "0%d" sec)
                                                 (format "%d" sec))))
                         (make-string (- window-width 1) ?\-))))
                                        ;bar representing total cd time
    (insert string)
    (mingus-goto-line (- (mingus-line-number-at-pos (point-max)) 2))
    ;;(goto-line (+ (mingus-playlist-length) 1))
    (dotimes (foo 3)
      (mingus-burns-color-bar 0 35 "darkgreen")
      (mingus-burns-color-bar 35 75 "green")
      (mingus-burns-color-bar 75 80 "orange")
      (mingus-burns-color-bar 80 (- (point-at-eol) (point-at-bol)) "red")
      (forward-line 1))))

(defun mingus-blank-disk ()
  (interactive)
  (start-process-shell-command "mingblank" "*Mingus-Output*" mingus-blank-string)
  (message "Blanking disk")
  (set-process-sentinel (get-process "mingblank") 'mingus-blank-sentinel))

(defun mingus-blank-sentinel (process event)
  (when (null (process-status "mingblank"))
    (message "Disk blanked")))

;;;; Drag 'n' Drop
;; These functions should return the action done (move, copy, link or
;; private); for now, simply return action unaltered.
(defun mingus-add-url (url action)
  (mingus-add (mingus-url-to-relative-file url))
  action)

(defun mingus-url-to-relative-file (url)
  (mingus-abs->rel (url-unhex-string (mingus-url-to-absolute-file url))))

(defun mingus-url-to-absolute-file (url)
  (string-match "^file://\\(.*\\)\\'" url)
  (match-string 1 url))

(defun mingus-inject-dnd-action (action)
  (set (make-local-variable 'dnd-protocol-alist)
       (delete (assoc "^file:///" dnd-protocol-alist) dnd-protocol-alist))
  (push `("^file:///" . ,action) dnd-protocol-alist))

(defadvice mingus (after mingus-dnd-injection activate)
  (mingus-inject-dnd-action 'mingus-add-url))

(defun mingus-browse-url (url action)
  (let* ((file (url-unhex-string (mingus-url-to-absolute-file url)))
         (file-relative (mingus-abs->rel file)))
    (if (file-directory-p file)
        (mingus-browse-to-dir file-relative)
      (mingus-browse-to-file file-relative))
  action))

(defadvice mingus-browse (after mingus-dnd-injection activate)
  (mingus-inject-dnd-action 'mingus-browse-url))

(provide 'mingus-stays-home)
;;; mingus-stays-home ends here
