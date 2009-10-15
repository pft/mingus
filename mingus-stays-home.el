;;; mingus-stays-home.el ---

;;                    _                    _                          
;;  _ __ ___      ___| |_ __ _ _   _ ___  | |__   ___  _ __ ___   ___ 
;; | '_ ` _ \    / __| __/ _` | | | / __| | '_ \ / _ \| '_ ` _ \ / _ \
;; | | | | | |_  \__ \ || (_| | |_| \__ \ | | | | (_) | | | | | |  __/
;; |_| |_| |_(_) |___/\__\__,_|\__, |___/ |_| |_|\___/|_| |_| |_|\___|
;;                             |___/                                  

;; Copyright (C) 2006  Niels Giesen <nielsgiesen at ibbu dot nl>

;; Author: Niels Giesen <nielsgiesen at ibbu dot nl> <pft on #emacs>

;; Version: orange was the color of her dress, or: 0.21

;; NOTE: Version 0.21 is actually the first published version of mingus-stays-home. To
;; provide easy checking whether the versions of mingus.el and mingus-stays-home
;; (should) work together, it was decided to give them concurrent version numbers.

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

;;; Commentary:

;; INSTALLATION INSTRUCTIONS:

;; Put the following in your .emacs:

;;  (require 'mingus-stays-home)

;; If you already had a (require 'mingus) in your .emacs, REPLACE this with the code
;; above. Make sure you have a version of mingus.el of the same version number
;; available.

;;; goal: provide extra functionality to mingus that is only possible when mpd is run
;;; locally, i.e. the frontend has full access to the configuration file and the
;;; filesystem used by mpd.

;;; extent: currently undecided, but at least:

;;; * integration with other emacs-modes:

;;; ** DIRED:

;;; Ability to snap to the file location of a song instantly in `dired', so as to
;;; perform file management or other actions on these files easily (such as removal,
;;; movement or renaming), or just to check wtfs '3.ogg' actually refers to.

;;; You might want to change the `dired-mode-map' so that it will play well with Mingus.
;;; If you want to, you can set the variable `mingus-dired-add-keys' to t; this can be
;;; done with `mingus-customize'. It will set "SPC" to `mingus-dired-add', "C-u SPC" to
;;; `mingus-dired-add-and-play' and add an item for `mingus-dired-add' to the menu-bar
;;; in dired. `mingus-dwim-add' and `mingus-dwim-add-and-play' (see below) calls
;;; mingus-dired-add when in dired, so binding this to a global key might be a nice
;;; solution too.

;;; ** MINIBUFFER COMPLETION: 

;;; using the minibuffer in a better way to add songs or directories, than mingus-insert
;;; previously did. This is done with the function `mingus-dwim-add'. `mingus-insert'
;;; keeps its old behaviour.

;;; ** ID3-TAGGING: 

;;; see `mingus-id3-set' and `mingus-id3-erase' (only to be called from within `mingus')

;;; ** BURNING CD'S with `mingus-burns'! In DEVELOPMENT stage, but nonetheless usable.

;;; Make sure the directory referred to by the variable `mingus-burns-tmp-wav-dir'
;;; (default: "~/.mingus/tmp") exists. This is where the intermediate files in the
;;; burning process will go.

;;; NOTE mingus-burns currently does NOT check for the capacity of the inserted disk,
;;; nor whether it is writable. You are left to your own devices for this. The
;;; green-redness if the indication bar displays just what is usually
;;; encountered. 

;;; INFO: mingus-burns records cd's with the contents of the current playlist of mpd. It
;;; can also just be used to see what the duration of a playlist and its respective
;;; songs (ONLY if in ogg- or mp3 format) is. Currently the playlist-editing functions
;;; are *not* as wide as in the normal playlist buffer (tagged `*Mingus*'). However,
;;; basic support for deletion of song at point and of songs in region does exist, with
;;; respect for the variables `*mingus-point-of-insertion*' and
;;; `*mingus-marked-list*'. You can add songs without leaving the *Mingus Burns* buffer
;;; as well, by use of mingus-dwim-add. Adding songs leads to a complete recomputation
;;; of song duration, which may take a while, so the fastest way to achieve nice
;;; contents for your CD is to first add a bunch of songs (but not way to much of
;;; course), and then delete the ones you can do without. It looks more attractive than
;;; the default playlist buffer, but because of the slowness inherent in computing
;;; duration, it is not planned to be a full substitute for it, at least not in the near
;;; future.

;;; Further requirements:

;;; ogginfo -- for computing duration of ogg vorbis files
;;; mp3info -- for computing duration of mp3 files

;;; sox -- for decoding songs to the .cdr format.
;;; cdrecord -- for burning the cd

;;; ** VIEW COVER ART (ach, why not?)

;;; when available, you can use `thumbs' to view cover art from within Mingus: see
;;; `mingus-thumbs' (key: "T")

;;; For any questions, suggestions and so forth, contact nielsgiesen at ibbu dot nl

;;; (use dto's golisp-mode to make use of the indexing in this file: find it at http://dto.freeshell.org/notebook/GoLisp.html )

;; {{index}}
;;
;; <<index>>
;; <<Update Help Text>>
;; <<Update *mingus-header-when-empty*>>
;; <<Generic Functions>>
;; <(function mingus-make-alist)>
;; <(function mingus-get-config-option)>
;; <(customization group mingus-stays-home)>
;; <(customization variable mingus-mpd-config-file)>
;; <<Shell>>
;; <(function mingus-get-filename-from-playlist)>
;; <(function mingus-get-filename-from-playlist-for-shell)>
;; <(function mingus-get-filename-from-browser-for-shell)>
;; <(function mingus-get-single-filename-from-browser-for-shell)>
;; <(function mingus-get-single-filename-for-shell)>
;; <(function mingus-get-filename-for-shell)>
;; <<Dired>>
;; <(function mingus-retrieve-symlinks)>
;; <(function mingus-replace-absolute-path-with-relative-one)>
;; <(function mingus-dired-add)>
;; <(customization variable mingus-dired-add-keys)>
;; <(function mingus-dired-file)>
;; <(function mingus-get-parent-dirs)>
;; <(function mingus-get-parent-dir-from-browser)>
;; <(function mingus-get-parent-dir-from-playlist)>
;; <<Minibuffer>>
;; <(function mingus-dwim-add)>
;; <<Id3>>
;; <<Id3 - Datastructures>>
;; <(variable *mingus-id3-items*)>
;; <(structure (id3-item))>
;; <(function mingus-id3-get-item)>
;; <(variable *mingus-id3-song-history*)>
;; <(variable *mingus-id3-artist-history*)>
;; <(variable *mingus-id3-album-history*)>
;; <(variable *mingus-id3-genre-history*)>
;; <(variable *mingus-id3-year-history*)>
;; <(variable *mingus-id3-comment-history*)>
;; <<Id3 - Functions>>
;; <(function mingus-make-genre-alist)>
;; <(function mingus-id3-erase)>
;; <(function mingus-id3-set)>
;; <(customization variable mingus-id3-format)>
;; <(customization variable teststese)>
;; <(function mingus-read-id3-elt)>
;; <<Thumbs>>
;; <(function mingus-thumbs)>
;; <<Mingus Burns>>
;; <<Generic Stuff>>
;; <<Global Variables>>
;; <(variable *mingus-playlist-duration*)>
;; <(variable *mingus-buffer-contents*)>
;; <<Process-Related Vars>>
;; <(variable *mingus-decoding-process-list*)>
;; <(variable *mingus-burns-args*)>
;; <(variable *mingus-waiting-for-burning*)>
;; <(variable *mingus-burns-delete-cdrs-question-timer*)>
;; <<Customization>>
;; <(customization group mingus-burns)>
;; <(customization variable mingus-burns-tmp-wav-dir)>
;; <(customization variable mingus-burns-device)>
;; <(customization variable mingus-burns-speed)>
;; <<Keymap>>
;; <<Generic Functions>>
;; <(function mingus-burns-kind-of-file)>
;; <(function mingus-get-length)>
;; <(function mingus-burns-get-name-for-shell)>
;; <(function mingus-burns-get-name)>
;; <(function mingus-get-length-in-seconds)>
;; <(function mingus-display-time)>
;; <(function mingus-burns-bar)>
;; <(function mingus-burns-color-bar)>
;; <(function mingus-compute-buffer-length)>
;; <(function mingus-burns)>
;; <(function mingus-burns-invisible)>
;; <(function mingus-burns-del)>
;; <(function mingus-burns-del-region)>
;; <<Decoding>>
;; <(function mingus-burns-decode-file-at-point)>
;; <(function mingus-burns-decode)>
;; <(function mingus-burns-decode-playlist)>
;; <(function mingus-burns-make-destination-name)>
;; <<Recording>>
;; <(function mingus-burns-check-if-still-decoding)>
;; <(function mingus-burns-make-args)>
;; <(function mingus-burn-it)>
;; <(function mingus-burn-the-cd)>
;; <(function mingus-burns-ask-to-delete-cdrs)>

;;; Code:



(require 'mingus)


;;;; {{Update Help Text}}


(setq mingus-help-text 
      (replace-regexp-in-string 
       "MORE ELABORATE INSTRUCTIONS:" 
       "BURNER KEYS:

B                       mingus-burn-it
D                       mingus-burns-decode-playlist

MORE ELABORATE INSTRUCTIONS:"  
       (replace-regexp-in-string 
	"U                       mingus-unmark-all"
	"U                       mingus-unmark-all
#                       mingus-id3-set
e                       mingus-id3-erase"  
	(replace-regexp-in-string 
	 "mingus-browser:    3" 
	 "mingus-browser:    3
mingus-burns:      4
mingus-dired-file: 0\n" mingus-help-text t) t) t))


;;;; {{Update *mingus-header-when-empty*}}


(setq *mingus-header-when-empty* "Press ? for help, 3 for Mingus Browser, 4 for Mingus Burns and 0 for dired\n\nPress 2 to come back here from within Mingus buffers, M-x mingus from elsewhere.")


;;;;  {{Generic Functions}}


(defun mingus-make-alist (list)
  "Make an alist out of a flat list, whereby every pair is reversed."
  (if (endp list)
      nil
    (cons (cons (cadr list) (car list))
	  (mingus-make-alist (cddr list)))))


(defun mingus-get-config-option (file option)
  (with-temp-buffer
    (insert-file-contents file)
    (re-search-forward (format "%s +" option))
    (goto-char (re-search-forward "\""))
    (buffer-substring-no-properties (point) (1- (re-search-forward "\"")))))


(defgroup mingus-stays-home nil
  "Group for customization of variables used when mpd is run on
        the same computer as mingus"
  :group 'mingus)


(defcustom mingus-mpd-config-file "~/.mpdconf"
  "File used by mpd as a configuration file"
  :group 'mingus-stays-home
  :type '(string))


(defconst mingus-mpd-root (expand-file-name (concat (mingus-get-config-option mingus-mpd-config-file "music_directory") "/")) "Root for mpd")

;;;; {{Shell}}
;;; functions for retrieving the true filenames for interaction with a shell and
;;; `dired'; this needs reviewing for consistency and use


;; as a list..
(defun mingus-get-filename-from-playlist (&optional list)
  "In `mingus', retrieve filenames from the playlist and put them in a list"
  (let ((list (or list (list (line-number-at-pos)))))
    (with-temp-buffer
      (insert (shell-command-to-string "mpc playlist --format %file%"))
      (goto-char (point-min))
      (mapcar (lambda (list)
		(concat mingus-mpd-root
			(buffer-substring-no-properties (goto-char (re-search-forward (format "#%d) " list)) )(point-at-eol))))
	      (sort list #'<)))))


;; as one well-quoted string..
(defun mingus-get-filename-from-playlist-for-shell (&optional integer)
  "In `mingus', return a shell-quoted string filenames from playlist"
  (let ((list (or (when integer (list integer)) (sort (copy-list mingus-marked-list) #'<) (list (line-number-at-pos)))))
    (with-temp-buffer
      (insert (shell-command-to-string "mpc playlist --format %file%"))
      (goto-char (point-min))
      (mapconcat
       (lambda (number)
	 (let ((song (buffer-substring-no-properties (goto-char (re-search-forward (format "#%d) " number)))(point-at-eol))))
	   (unless (looking-at "http://")
	     (shell-quote-argument (concat mingus-mpd-root song))))) list " "))))


(defun mingus-get-filename-from-browser-for-shell ()
  (list (mingus-get-single-filename-from-browser-for-shell)))


(defun mingus-get-single-filename-from-browser-for-shell ()
  (concat mingus-mpd-root (buffer-substring-no-properties (point-at-bol)(point-at-eol))))


(defun mingus-get-single-filename-for-shell (&optional integer)
  (case major-mode
    ('mingus-browse-mode
     (shell-quote-argument (mingus-get-single-filename-from-browser-for-shell)))
    ('mingus-playlist-mode
     (mingus-get-filename-from-playlist-for-shell))))


(defun* mingus-get-filenames-from-browser-for-shell (&optional beg end)
  "Get everything under the region, sloppily."
  (interactive "r")
  (let ((res nil)
	(beg (if mark-active beg (point-at-bol)))
	(end (if mark-active end (point-at-eol))))
    (goto-char (or beg (point-at-bol)))
    (while (< (point) end)
      (setf res (cons (concat mingus-mpd-root (buffer-substring-no-properties (point-at-bol) (point-at-eol))) res))
      (forward-line 1)) res))


(defun mingus-get-filename-for-shell (&optional integer)
  (case major-mode
    ('mingus-browse-mode
     (mingus-get-filename-from-browser-for-shell))
    ('mingus-playlist-mode
     (mingus-get-filename-from-playlist (list integer)))))


;;;; {{Dired}}
;; get all symlinks under mingus-mpd-root:
(defun mingus-retrieve-symlinks ()
  "Retrieve all symlinks under mpd root, and make an alist out of them.
To be used for passing the right data to mpd in dired."
  (cons (cons mingus-mpd-root "")
	(mingus-make-alist
	 (remove "" (split-string (shell-command-to-string (format "symlinks -crv %s" mingus-mpd-root))
				  (format "\\(\\(\n*relative:\\|\n*other_fs:\\|\n*dangling:\\| ->\\) \\|%s\\|\n\\)" mingus-mpd-root))))))


(defun mingus-replace-absolute-path-with-relative-one (string)
  "In STRING, replace the absolute path with the associated symlink under `mingus-mpd-root';
If found, also quote it. Used in `mingus-dired-add'"
  (let ((found-association (assoc* string (mingus-retrieve-symlinks) :test
				   '(lambda (place-of-file parent-dir) ;; (string-match parent-dir place-of-file)
				      ;; account for relative interpretation by the shell of symlinks
				      (string-match (replace-regexp-in-string "\\.\\./" "" parent-dir) place-of-file)))))
    (if found-association
	(shell-quote-argument
	 (concat (cdr found-association) (substring string ;;(length (car found-association)
						    ;; account for relative interpretation by the shell of symlinks
						    (apply (if (string-match "\\.\\./" (car found-association)) '1+ '+)
							   ;; account for relative interpretation by the shell of symlinks
							   (length (replace-regexp-in-string "\\.\\./" "" (car found-association))) nil)))))))

					   
(defun mingus-dired-add ()
  "In `dired', add marked files or file at point to the mpd playlist;

If mpd is unwary of these files, ask whether to make a symlink.
Create a symlink, and optionally update database.

If updating takes too long, it might be that this does not work in one go.
Try again at a later time if this happens."
  (interactive)
  (let ((old-length (mingus-playlist-length)))
    (mingus-add (mapconcat 'mingus-replace-absolute-path-with-relative-one (dired-get-marked-files) " "))
    ;; make sure mingus-add has indeed added the stuff:
    (if (= old-length (mingus-playlist-length))
	;; if not, ask some questions
	(when (yes-or-no-p (format "Something has gone wrong. Possibly mpd does not know about the file.
Do you want to symlink the parent directory? : " ))
	  (shell-command-to-string (format "ln -s %s %s" (shell-quote-argument (dired-current-directory))
					   (shell-quote-argument mingus-mpd-root)))
	  
	  (when (yes-or-no-p "Update the database ? : ")
	    (mingus-update)
	    (message "This might take a while, repeat `mingus-dired-add' when that rattling sound of your harddisk has subsided"))))))

	  
;; create function mingus-dired-add-and-play
(mingus-and-play mingus-dired-add mingus-dired-add-and-play)

;; make sure mingus-dired-add handles point-of-insertion and mingus-marked-list
(mingus-insertion-advice mingus-dired-add)


;; alright, a customization is in place for interfering with dired's defaults:
(defcustom mingus-dired-add-keys nil
  "Add keys for interaction to dired-mode-map;

the file \"mingus-stays-home.elc\" needs to be reloaded for a change here to have effect."
  :group 'mingus
  :type '(boolean))

(eval-when (load)
  (when mingus-dired-add-keys
  (define-key dired-mode-map " " 'mingus-dired-add)
  (define-key dired-mode-map "\C-u " 'mingus-dired-add-and-play)
  (unless (string-match "GNU Emacs 21" (version)) ;fixme: make this work in emacs21 too
    (define-key-after dired-mode-map [menu-bar operate mingus]
      '("Add to Mingus"  . mingus-dired-add) 'query-replace))))


(defun mingus-dired-file ()
  "Open dired with parent dir of song at point."
  (interactive)
  (dired (mingus-get-parent-dirs)) "-al")

(defun mingus-get-parent-dirs ()
  "Get parent dir of song at point."
  (case major-mode
    ('mingus-browse-mode
     (mingus-get-parent-dir-from-browser))
    (t
     (mingus-get-parent-dir-from-playlist))))

(defun mingus-get-parent-dir-from-browser ()
  "Get parent dir of the song at point in `mingus-browse'."
  (let ((string (concat mingus-mpd-root (buffer-substring-no-properties (point-at-bol)(point-at-eol)))))
    (string-match "/.*/" string)
    (match-string 0 string)))


 (defun mingus-get-parent-dir-from-playlist ()
  "Get  parent dir of the song at point in `mingus'."  
  (let ((number (mingus-line-number-at-pos)))
    (with-temp-buffer
      (insert (shell-command-to-string "mpc playlist --format %file%"))
      (goto-char (point-min))
      (let ((string (concat mingus-mpd-root
			    (buffer-substring-no-properties (goto-char (or (re-search-forward (format "#%d) " number) nil t) (point-min)))(point-at-eol)))))
	    (if  (looking-at "http://")
		  (error "Not a local file!")
	      (string-match "/.*/" string)
	      (match-string 0 string))))))
  

;; add some keys to the various modes for dired look-ups
(define-key mingus-playlist-map "0" 'mingus-dired-file)
(define-key mingus-browse-map "0" 'mingus-dired-file)
(define-key mingus-help-map "0" #'(lambda ()
				    (interactive)
				    (dired mingus-mpd-root)))

(define-key mingus-playlist-map "#" 'mingus-id3-set)
(define-key mingus-playlist-map "e" 'mingus-id3-erase)

(eval-when (load) 
  (unless (string-match "GNU Emacs 21" (version)) ;fixme: make this work in emacs21 too
    (define-key-after mingus-playlist-map [menu-bar mingus dired]
      '("Dired file" . mingus-dired-file) 'repeat)
    (define-key-after mingus-playlist-map [menu-bar mingus id3-set]
      '("Set ID3 tag" . mingus-id3-set) 'dired)
    (define-key-after mingus-playlist-map [menu-bar mingus id3-erase]
      '("Erase ID3 tag" . mingus-id3-erase) 'id3-set)))

;;;; {{Minibuffer}}

(defun mingus-dwim-add ()
  "In `dired', add marked files or file at point to the mpd playlist.
In other modes, use the minibuffer.


If mpd is unwary of these files, ask whether to make a symlink.
Create a symlink, and optionally update database.

If updating takes too long, it might be that this does not work in one go.
Try again at a later time if this happens."
  (interactive)
  (let ((old-length (mingus-playlist-length)))
    (case major-mode
      ('dired-mode
       (mingus-add (mapconcat 'mingus-replace-absolute-path-with-relative-one (dired-get-marked-files) " ")))
      (t
       (mingus-add (mingus-replace-absolute-path-with-relative-one (expand-file-name (read-file-name "Add: "))))))
    ;; make sure mingus-add has indeed added the stuff:
    (if (= old-length (mingus-playlist-length))
	;; if not, ask some questions
	(when (yes-or-no-p (format "Something has gone wrong. Possibly mpd does not know about the file.
Do you want to symlink the parent directory? : " ))
	  (shell-command-to-string (format "ln -s %s %s" (shell-quote-argument (dired-current-directory))
					   (shell-quote-argument mingus-mpd-root)))
	  
	  (when (yes-or-no-p "Update the database ? : ")
	    (mingus-update)
	    (message "This might take a while, repeat `mingus-dired-add' when that rattling sound of your harddisk has subsided")))
      (case major-mode
	('mingus-burns (mingus-burns))))))

(mingus-and-play mingus-dwim-add mingus-dwim-add-and-play)    
;;;; {{Id3}}


;;;; {{Id3 - Datastructures}}


(defvar *mingus-id3-items*
  '(artist album song track year comment genre)
  "A list for storing information on handling id3-related functions and data for `mingus-id3-set';

Its property list is of great importance.")


;; create a structure for use in the plist of `*mingus-id3-items*'
(defstruct (id3-item)
  (minibuffer-string nil)
  (option-string nil)
  (list-or-list-function)
  (history-list nil))


(defun mingus-id3-get-item (item)
  "Shortcut function to get at an item in the plist of `*mingus-id3-items*'"
  (get '*mingus-id3-items* item))


;; fill up the plist of `*mingus-id3-items*'
(progn

  (put '*mingus-id3-items* 'track
       (make-id3-item
	:minibuffer-string "Track number/(optional: total tracks): "
	:option-string "-T"
	:list-or-list-function
	'(lambda ()
	   (save-excursion
	     (beginning-of-line)
	     (let (list)
	       (while
		   (re-search-forward "[0-9]+" (- (point-at-eol) 1) t)
		 (push (match-string-no-properties 0) list)) list)))
	:history-list '*mingus-id3-track-history*))

  (put '*mingus-id3-items* 'genre
       (make-id3-item
	:minibuffer-string "Genre: "
	:option-string "-g"
	:list-or-list-function
	'mingus-make-genre-alist
	:history-list '*mingus-id3-genre-history*))

  (put '*mingus-id3-items* 'artist
       (make-id3-item
	:minibuffer-string "Artist: "
	:option-string "-a"
	:list-or-list-function
	'(lambda ()
	   (save-excursion
	     (widen)
	     (end-of-line)
	     (let ((beg (or (and (re-search-backward "/" (point-at-bol) t) (1+ (point)))
			    (progn (beginning-of-line) (point)))))
	       (list
		(if (re-search-forward " *- *" (point-at-eol) t)
		    (buffer-substring-no-properties beg (match-beginning 0)) "")))))
	:history-list '*mingus-id3-artist-history*))

  (put '*mingus-id3-items* 'song
       (make-id3-item
	:minibuffer-string "Songtitle: "
	:option-string "-t"
	:list-or-list-function
	'(lambda ()
	   (save-excursion
	     (end-of-line)
	     (re-search-backward "\\." (- (point-at-eol) 5) t)
	     (let ((pos (point)))
	       (list
		(buffer-substring-no-properties
		 (if (re-search-backward  " *- *" (point-at-bol) t)
		     (progn
		       (re-search-forward "[0-9]+ " (point-at-eol) t)
		       (match-end 0))
		   (point-at-bol)) pos)))))
	:history-list '*mingus-id3-song-history*))

  (put '*mingus-id3-items* 'year
       (make-id3-item
	:minibuffer-string "Year: "
	:option-string "-y"
	:list-or-list-function
	'(lambda ()
	   (save-excursion
	     (beginning-of-line)
	     (if (re-search-forward "[0-9]\\{4\\}" (point-at-eol) t)
		 (list (match-string-no-properties 0)))))
	:history-list '*mingus-id3-year-history*))

  (put '*mingus-id3-items* 'comment
       (make-id3-item
	:minibuffer-string "Comment: "
	:option-string "-c"
	:list-or-list-function
	'(lambda ()
	   (list))
	:history-list '*mingus-id3-comment-history*))

  (put '*mingus-id3-items* 'album
       (make-id3-item
	:minibuffer-string "Album: "
	:option-string "-A"
	:list-or-list-function
	'(lambda ()
	   (save-excursion
	     (beginning-of-line)
	     (re-search-forward " *- *" (- (point-at-eol) 5) t)
	     (let ((pos (point)))
	       (list
		(buffer-substring-no-properties
			(or (and (re-search-forward  " *- *" (point-at-eol) t) (match-beginning 0)) pos)
		    pos)))))
	:history-list '*mingus-id3-album-history*)))


;; id3-history variables (as completing-read needs them to be referred to as a symbol, I cannot hide them)
(defvar *mingus-id3-song-history* nil
  "History of id3-songs for use in Mingus")
(defvar *mingus-id3-artist-history* nil
  "History of id3-artists for use in Mingus")
(defvar *mingus-id3-album-history* nil
  "History of id3-albums for use in Mingus")
(defvar *mingus-id3-genre-history* nil
  "History of id3-genres for use in Mingus")
(defvar *mingus-id3-year-history* nil
  "History of id3-years for use in Mingus")
(defvar *mingus-id3-comment-history* nil
  "History of id3-comments for use in Mingus")



;;;; {{Id3 - Functions}}
(defun mingus-make-genre-alist ()
  "Make an alist of all known id3 genres with their respective canonical numbers"
  (let ((count -1))
    (mapcar (lambda (genre)
	      (incf count)
	      (cons (downcase genre) count))
	    (split-string (shell-command-to-string "id3v2 -L") "\\([ \n0-9]+: \\|[\f\t\n\r\v]+\\)"))))



(defun mingus-id3-erase ()
  "Erase id3 contents from song at point, in either `mingus' or `mingus-browse'"
  (interactive)
  (shell-command (format "id3v2 -D %s" (shell-quote-argument (car (mingus-get-filename-for-shell (mingus-line-number-at-pos)))))))


(defun mingus-id3-set (&optional value)
  "Set id3 tags in `mingus' or `mingus-browse';

The effect will only be shown after a `mingus-update'"
  (interactive)
  (shell-command (format "id3v2 %s %s"
			  (mapconcat #'(lambda (item) item)
				     (mapcar (lambda (option)
					       (concat (id3-item-option-string (mingus-id3-get-item option))
						       " \"" (apply mingus-id3-format (list (mingus-read-id3-elt option))) "\"")) 
					     *mingus-id3-items*) " ")
			 (format "%s" (shell-quote-argument (car (mingus-get-filename-for-shell (mingus-line-number-at-pos))))))))

(defcustom mingus-id3-format 'downcase
  "Format used by `mingus-id3-set' for formatting id3-tags;

valid options are:
downcase,
upcase,
capitalize"
  :group 'mingus
  :type '(choice 
	  (const :tag "downcase" downcase)
	  (const :tag "upcase" upcase)
	  (const :tag "capitalize" capitalize)))


(defun mingus-read-id3-elt (item)
  "Try to retrieve info of type ITEM of song at point for `mingus-id3-set';

ITEM must be one of the elements in the variable `*mingus-id3-items*'"
  (let* ((list (funcall (id3-item-list-or-list-function (mingus-id3-get-item item))))
	 (result (completing-read-allow-spaces (id3-item-minibuffer-string (mingus-id3-get-item item))
				  list
				  nil nil (car list) (id3-item-history-list (mingus-id3-get-item item)))))
    (if (eq item 'genre)		;one special case for genres. Otherwise, I'd have to make
					;this whole function a method
	(number-to-string (cdr (assoc result (mingus-make-genre-alist))))
      result)))

;;;; {{Thumbs}}


(eval-when (compile)
  (when (featurep 'thumbs)
    (defun mingus-thumbs ()
      "In mingus, open a buffer with cover art found in the directory of song at point."
      (interactive)
      (thumbs (mingus-get-parent-dirs)))
  
    (define-key mingus-playlist-map "T" 'mingus-thumbs)
    (define-key mingus-browse-map "T" 'mingus-thumbs)))


;;;; {{Mingus Burns}}

;;;; {{Generic Stuff}}

;;;; {{Global Variables}}


(defvar *mingus-playlist-duration* 0 "Duration of playlist contents")
(defvar *mingus-buffer-contents* nil)

;;;; {{Process-Related Vars}}

(defvar *mingus-decoding-process-list* nil "List of mingus decoding processes")
(defvar *mingus-burns-args* nil "Arguments for the final call to cdrecord")
(defvar *mingus-waiting-for-burning* nil "Timer checking whether we can begin burning")
(defvar *mingus-burns-delete-cdrs-question-timer* nil "Timer checking whether to pose the question of deleting temporary .cdr files")

;;;; {{Customization}}

(defgroup mingus-burns nil
  "Customization group for recording cd's with `mingus'"
  :group 'mingus)
  
  
(defcustom mingus-burns-tmp-wav-dir "~/.mingus/tmp"
  "Directory to hold temporary .wav files for a recording session."
  :group 'mingus-burns
  :type '(file))


(defcustom mingus-burns-device "/dev/cdrom"
  "Device name to use for recording"
  :group 'mingus-burns
  :type '(choice (file :tag "File (such as /dev/cdrom)")
		 (string :tag "Description (such as ATA:1,0,0)")))


(defcustom mingus-burns-speed 2
  "Speed of cd-recording device"
  :group 'mingus-burns
  :type 'number)


;;;; {{Keymap}}

(defconst mingus-burnin-map (copy-keymap mingus-global-map)
  "Burnin keymap for `mingus'")

(define-key mingus-burnin-map " " 'scroll-up)
(define-key mingus-burnin-map "\C-m" '(lambda () (interactive)
					(condition-case nil (mingus-play (number-to-string (mingus-line-number-at-pos))))))
(define-key mingus-burnin-map "d" 
	'(lambda () (interactive)
			   (if (mingus-mark-active)
			       (call-interactively 'mingus-burns-del-region)
			     (mingus-burns-del))))

(define-key mingus-burnin-map "B" 'mingus-burn-it)
(define-key mingus-burnin-map "D" 'mingus-burns-decode-playlist)

(define-key mingus-burnin-map [menu-bar mingus sep-playlist-editing]
  '(menu-item "--"))
(define-key mingus-burnin-map [menu-bar mingus unset]
  '("Unset Insertion Point" . mingus-unset-insertion-point))
(define-key mingus-burnin-map [menu-bar mingus sep4]
  '(menu-item "--"))
(define-key mingus-burnin-map [menu-bar mingus burn]
  '(menu-item "Burn CD" mingus-burn-it :burnin "Burn a cd with current contents of the playlist"))
(define-key mingus-burnin-map [menu-bar mingus decode]
  '(menu-item "Decode Playlist" mingus-burns-decode-playlist :burnin "Decode current contents of the playlist to .cdr files"))
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

(defun mingus-burns-kind-of-file (file)
  (cond
   ((string-match "\\.[oO][Gg]\\{2\\}$" file) 'ogg)
   ((string-match "\\.[mM][pP]3$" file) 'mp3)))


(defun mingus-get-length ()
  "Get the length of a track; 

Return a list, whereof CAR is minutes and CDR is seconds.
Works only with ogg and mp3"
  (if (looking-at ".*\\ogg$")
      (multiple-value-bind (min sec)
	  (mapcar 'string-to-number (split-string (shell-command-to-string (format "ogginfo %s|grep Playback|sed 's/\tPlayback length: //'|sed 's/.[0-9]*s//'|sed 's/m//'" (mingus-burns-get-name-for-shell))) "[^0-9]")) (list min sec))
    (floor* (string-to-number (shell-command-to-string (format "mp3info -p %%S %s" (mingus-burns-get-name-for-shell)))) 60)))


(defun mingus-burns-get-name-for-shell ()
  (beginning-of-line)
  (shell-quote-argument
   (mingus-burns-get-name)))


(defun mingus-burns-get-name ()
  (beginning-of-line)
  (format "%s%s" mingus-mpd-root
	  (buffer-substring-no-properties 
	   (or (re-search-forward "^[0-9]+:[0-9]\\{2\\} " (point-at-eol) t)
	       (point-at-bol)) (point-at-eol))))


(defun mingus-get-length-in-seconds ()
  (multiple-value-bind (min sec) (mingus-get-length)
    (+ (* 60 min) sec)))


(defun mingus-display-time ()
  (interactive)
  (let (buffer-read-only)
    (beginning-of-line)
    (insert 
     (multiple-value-bind (min sec) 
	 (mingus-get-length)
       (format "%d:%s " min (if (< sec 10)
			       (format "0%d" sec)
			     (format "%d" sec)))))
    (mingus-burns-color-bar 0 5 "orange")
    (mingus-burns-color-bar 5 (- (point-at-eol) (point-at-bol)) "lightblue")))


(defun mingus-burns-bar ()
  (interactive)
  (let* ((total-seconds *mingus-playlist-duration*)
	 (buffer-read-only nil)
	 (border (format "%s30%s50%s70%s75%s80%s" (make-string 29 ?\-) (make-string 18 ?\-) (make-string 18 ?\-)(make-string 3 ?\-) (make-string 3 ?\-) (make-string (max (- (window-width) 82) 0) ?\-)))
	 (window-width (window-width))
	 (string (format "%s\n%s %s \n%s"
			 border ;bar representing total cd time
			 (make-string (min (/ total-seconds 60) (- window-width 9)) ?|) ;bar representing percentage filled
			 (multiple-value-bind (min sec)
			     (floor* total-seconds 60)
			   (format "%d:%s" min (if (< sec 10)
						   (format "0%d" sec)
						 (format "%d" sec))))
			 (make-string (- window-width 1) ?\-))))	;bar representing total cd time
      (insert string)
      (goto-line (+ (mingus-playlist-length) 1))
      (dotimes (foo 3)
	(mingus-burns-color-bar 0 35 "darkgreen")
	(mingus-burns-color-bar 35 75 "green")
	(mingus-burns-color-bar 75 80 "orange")
	(mingus-burns-color-bar 80 (- (point-at-eol) (point-at-bol)) "red")
	(forward-line 1))))
      

(defun mingus-burns-color-bar (pos-beg-from-bol pos-end-from-bol color)
  (put-text-property  (+ pos-beg-from-bol (point-at-bol))
		      (+ pos-end-from-bol (point-at-bol))
		      'face `((background-color . "#000000")
			      (foreground-color . ,color)
			      (weight . "bold"))))


(defun mingus-compute-buffer-length ()
  (interactive)
  (font-lock-mode -1)
  (setq *mingus-playlist-duration* 0)
  (save-excursion
    (let ((playlist-length (mingus-playlist-length)))
      (goto-char (point-min))
      (while (< (mingus-line-number-at-pos) 
		playlist-length)
	(setq *mingus-playlist-duration* (+ *mingus-playlist-duration* (mingus-get-length-in-seconds)))
	(mingus-display-time)
	(forward-line 1))
      (setq *mingus-playlist-duration* (+ *mingus-playlist-duration* (mingus-get-length-in-seconds)))
      (mingus-display-time)
    (goto-char (point-max))
    (let (buffer-read-only)
	  (mingus-burns-bar)))))


(defun mingus-burns ()
  "Go to the buffer in `mingus' where recording takes place."
  (interactive)
  (switch-to-buffer "*Mingus Burns*")
  (setq major-mode 'mingus-burns)
  (use-local-map mingus-burnin-map)
  (let ((buffer-read-only nil)
	(new-contents (shell-command-to-string "mpc --format \"%file%\" playlist")))
    (when (or (= (point-max) (point-min)) ;if there's an empty-buffer or the playlist has changed in the mean time
	      (not (equal new-contents *mingus-buffer-contents*))) 
      (erase-buffer)
      (cond ((< 1 (length new-contents))
	     (message "Computing lengths, this may take a while")
	     (insert (setq *mingus-buffer-contents* new-contents))
	     (mingus-burns-invisible)
	     (mingus-compute-buffer-length))
	    (t (insert "Press ? for help, 2 for Mingus Playlist, 3 for Mingus Browser and 0 for Dired\n\nPress 4 from within Mingus buffers to come back here, M-x mingus-burns from elsewhere.\n\nPlaylist is empty, please add some songs.\n\nYou can do so with either mingus-dwim-add, with mingus-browse or from within dired.")
	       (goto-char (point-min))))))
    (setq buffer-read-only t))

(defun mingus-burns-invisible ()
  (save-excursion
    (while		
	(re-search-backward "^#[0-9]+) " nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (not (eobp))
      (put-text-property
       (point-at-bol)
       (or (re-search-forward ".*/" (point-at-eol) t 1) (point-at-bol))
       'invisible t)
      (forward-line 1))))


(defun mingus-burns-del ()
  (interactive)
  (save-excursion
    (let ((buffer-read-only nil)
	  (length-of-song-at-p (mingus-get-length-in-seconds)))
      (shell-command (format "mpc del %s" (mingus-line-number-at-pos)))
      (mingus-reset-point-of-insertion)
      (delete-region (point-at-bol) (point-at-bol 2))
      (mingus-remove-current-line-from-marked-list)
      (setq *mingus-playlist-duration* (- *mingus-playlist-duration* length-of-song-at-p))
      (goto-line (1+ (mingus-playlist-length)))
      (delete-region (point) (point-max))
      (mingus-burns-bar)
      (setq *mingus-buffer-contents* (shell-command-to-string "mpc --format \"%file%\" playist"))))
  (forward-line -1))


(defun mingus-burns-del-region (beg end)
  (interactive "r")
  (save-excursion
    (goto-char end)
    (if (bolp)
	(forward-line -1))
    (while (and (> (point) (1- beg)) (not (bobp)))
	(mingus-burns-del))
    (when (= beg (point-min))
      (goto-char (point-min))
      (mingus-burns-del))))


;;;; {{Decoding}}


(defun mingus-burns-decode-file-at-point ()
  (let ((name (mingus-burns-make-destination-name)))
  (unless (file-exists-p name)
    (mingus-burns-decode (mingus-burns-get-name) name))))


(defun mingus-burns-decode (file destination)
   (push (start-process "mingus-decoder" "*Mingus-Output*" "sox" "-V" file "-t" ".cdr" destination) *mingus-decoding-process-list*))


(defun mingus-burns-decode-playlist ()
  "In `mingus-burns', decode the whole playlist to files in .cdr format.
The destination files are put in the directory referenced by the variable `mingus-burns-tmp-wav-dir'"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (<= (mingus-line-number-at-pos)
	      (mingus-playlist-length))
      (mingus-burns-decode-file-at-point)
      (forward-line 1))
    (message "Decoding...")))

(defun mingus-burns-make-destination-name ()
  "In `mingus-burns', construct a sounding .cdr destination name from the name of the song at point."
  (beginning-of-line)
  (let ((begin-of-source-name (or (re-search-forward ".*/" (point-at-eol) t) (point-at-bol))))
    (concat (expand-file-name mingus-burns-tmp-wav-dir) "/"
    (buffer-substring-no-properties begin-of-source-name (- (point-at-eol) 4)) ".cdr")))

;;;; {{Recording}}


(defun mingus-burns-check-if-still-decoding ()
  "Check whether any decoding process called by `mingus-burns-decode-file-at-point' is still running"
  (setq *mingus-decoding-process-list* (remove-if '(lambda (item) 
						    (eq (process-status item) 'exit)) *mingus-decoding-process-list*)))


(defun mingus-burns-make-args ()
  "Make a list of all .cdr files related to playlist contents"
  (goto-char (point-min))
  (mapcar '(lambda (item)
	     (prog1 (mingus-burns-make-destination-name) (forward-line 1)))  (make-list (mingus-playlist-length) 1)))


(defun mingus-burn-it ()
  "In `mingus-burns', record a CD; output of the process goes to the buffer \`*Mingus-Output*\'"
  (interactive)
  (when (eq major-mode 'mingus-burns)
    (setq *mingus-burns-args* (mingus-burns-make-args))
    (mingus-burns-decode-playlist)
    (setq *mingus-waiting-for-burning*
	  (run-with-timer 0 10 
			  #'(lambda ()
			      (unless (mingus-burns-check-if-still-decoding)
				(mingus-burn-the-cd)
				(cancel-timer *mingus-waiting-for-burning*)
				(setq *mingus-waiting-for-burning* nil)

				(setq *mingus-burns-delete-cdrs-question-timer*
				      (run-with-timer 10 10 'mingus-burns-ask-to-delete-cdrs))))))))


(defun mingus-burn-the-cd ()
  (message "Mingus-a-Burning... C-x b *Mingus-Output* to watch the process")
  (apply 'start-process "mingus-burn-it" "*Mingus-Output*" "cdrecord" (format "dev=%s" mingus-burns-device) "-eject" "-pad" "-audio" *mingus-burns-args*))


(defun mingus-burns-ask-to-delete-cdrs ()
  (when (null (process-status "mingus-burn-it"))
    (if (yes-or-no-p (format "Delete temporary .cdr-files from %s? " (expand-file-name mingus-burns-tmp-wav-dir)))
	(mapcar 'delete-file *mingus-burns-args*))
    (cancel-timer *mingus-burns-delete-cdrs-question-timer*)
    (setq timer-list (remove *mingus-burns-delete-cdrs-question-timer* timer-list))))



(provide 'mingus-stays-home)
;;; mingus-stays-home ends here

   		     
