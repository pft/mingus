;;; mingus.el --- 

;;            _                       
;;  _ __ ___ (_)_ __   __ _ _   _ ___ 
;; | '_ ` _ \| | '_ \ / _` | | | / __|
;; | | | | | | | | | | (_| | |_| \__ \
;; |_| |_| |_|_|_| |_|\__, |\__,_|___/
;;                    |___/           
;; -----------------------------------------------------------
;; MPD Interface that's No Garbage, but (just) Utterly Stylish 
;; -----------------------------------------------------------
;; ....................but actually named after a man so named
;; 

;; Copyright (C) 2006  Niels Giesen <nielsgiesen at ibbu dot nl>

;; Author: Niels Giesen <nielsgiesen at ibbu dot nl> <pft on #emacs>
;; Version: Better Git It In Your Soul, or: 0.0
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
;; 
;; Put something like the following in your .emacs file:

;; make sure you have mpc installed. This is just a wrapper. 

;; (add-to-list 'load-path "/path/to/where/you/put/mingus.el") 
;; (require 'mingus)

;; Design issues:
;; 
;; mingus is a wrapper around the mpc command line tool, and works via
;; shell-commands. As such, it is probably slower than clients using
;; modules or so, but has the advantage of easiness of installation:
;; all one needs is mpc (and mpd, if run locally). Currently mingus
;; uses playlist id's, instead of song id's. This makes it a little
;; more fragile for simultaneous use by different users.

;; No editing of id3 tags is provided. This is, first, because mpd is
;; designed to be run in a network as a server (although it can be
;; used on a single system, which, in fact, is what I do); as such,
;; clients to mpd are unaware of mpd's root dir, and possibly/probably
;; do not have write permissions on the music files. Second, I
;; personally do not see the use of id3 tags. They are to me a clumsy
;; way to provide information better handled in the
;; filename/filesystem, and as everyone holds different thoughts on
;; the content of those tags, e.g. what constitutes a 'genre' or
;; whether stuff should be down/upcased. Editing filenames is just way
;; more straightforward.

;; The interface is roughly based on that on ncmpc, but simpler, and
;; better :) (at the expence of a bit of speed though) .  Many
;; keybindings are alike, except for some notoriously vi-style-ones.
;; Some significant features (main reasons to write this stuff) :

;; MARKING
;; Notice specifically the possibility to mark multiple songs in the
;; playlist for movement or deletion (by pressing the spacebar one
;; toggles the mark at the current line; if there is a region, it
;; marks all songs in the region.) Unmarking all marks happens with a
;; single "u".

;; INSERTION POINT
;; Another nice feature is "mingus-set-insertion-point" (Key: "i") :
;; mark a song after which you would like your next insertions to take
;; place. Then go inserting. Unset this behaviour with "u"
;; (mingus-unset-insertion-point), and songs will be added to the end
;; of the playlist again. Notice that this can take quite some time
;; (the more songs, the longer you will have to wait, as this is done
;; by calling mpc move, which unfortunately take sonly two arguments).

;; NB: right now these two functions are mutually exclusive.

;; For those already familiar with mpd/mpc, and have set that up, you're done now.

;; If you get a message like 
 
;; MPD_HOST and/or MPD_PORT environment variables are not set
;; error: problems getting a response from "localhost" on port 6600 : Connection refused

;; there are two options: 

;; 1. you want to run locally, so run mpd
;; first. Do so from somewhere else or simply evaluate (mingus-start-daemon).
;; On some configurations of mpd this must be done as root.

;; For those unfamiliar with mpd, to set it up, put something like the following in 
;; ~/.mpdconf (this is for when run a user)

;; port		             "6600"
;; music_directory     "/your/music/directory"
;; playlist_directory  "~/playlists"
;; log_file       	   "~/.mpd.log"
;; error_file          "~/.mpd.err"
;;
;; then run mpd

;; 2. you want to connect to a remote host, but have not set the
;; environment variables MPD_HOST and/or MPD_PORT. Do so by calling
;; (mingus-set-variables-interactively) (settings lost when emacs
;; restarted) or by means of customization ((mingus-customize) or
;; (customize-group 'mingus), after which you can call
;; (mingus-set-variables) again)
p
;; Known bugs: 

;; * a file name cannot have a " (double quotes) in it. Do not know
;; how to fix that, so if anyone feels so inclined... You CAN query
;; your database (M-x mingus-query-regexp " RET) to know if you are in
;; the possession of such files, so you can adjust their names. The
;; only way to insert such files is by inserting their parent
;; directory.

;; * when mingus-info is left on for a while, it will exceed
;; max-lisp-eval-depth. This is nothing to worry about, although I am
;; interested in how to circumvent this.

;;; Code:
(require 'cl)
(require 'dired)

(defgroup mingus nil "Group customization for mingus mpd interface"
  :group 'applications)

(defcustom mingus-mpd-env-set-p nil 
  "Whether to set environment variables from emacs.\nDo not set when nil.\nDo set when t.\nDefault: nil.\nThese variables are set when mingus.el(c) is loaded or when mingus-set-variables is called."
  :group 'mingus
  :type '(boolean))

(defcustom mingus-mpd-host "localhost"
  "Setting for environment variable MPD_HOST"
  :group 'mingus
  :type '(string))

(defcustom mingus-mpd-port 6600 "Setting for environment variable MPD_PORT"
  :group 'mingus
  :type '(integer))

(when mingus-mpd-env-set-p 
  (setenv "MPD_HOST" mingus-mpd-host)
  (setenv "MPD_PORT" (number-to-string mingus-mpd-port)))

(defun mingus-set-variables-interactively ()
  "In Mingus, set environment variables for mpd connection according to input, defaulting to mingus-mpd-host and mingus-mpd-port. Do not use this for customizing these values; use mingus-customize instead."
  (interactive)
  (setenv "MPD_HOST" (read-string "MPD_HOST: " mingus-mpd-host))
  (setenv "MPD_PORT" (number-to-string (read-number "MPD_PORT: " mingus-mpd-port))))

(defun mingus-customize ()
  (interactive)
  (customize-group 'mingus))

(defvar mingus-version "Better Git It In Your Soul, or: 0.0")
(defvar mingus-song-regexp "^.+\.\\([Mm][Pp]3\\|[Oo][Gg][Gg]\\|[fF][lL][aA][cC]\\|[wW][aA][vV]\\)")

(defvar mingus-help-text 
  "           _                       
 _ __ ___ (_)_ __   __ _ _   _ ___ 
| '_ ` _ \\| | '_ \\ / _` | | | / __|
| | | | | | | | | | (_| | |_| \\__ \\
|_| |_| |_|_|_| |_|\\__, |\\__,_|___/
                   |___/             
=====================================================
MPD Interface, Nice, GPL'ed, User-friendly and Simple
=====================================================
.........but actually just named after Charles Mingus

REFCARD: (see further down for more elaborate instructions)

Those familiar  with dired-mode should find themselves at home; 
those familiar with ncmpc too, AMAP that is

MAIN CONTROLS:

mingus-help:     \"1\"
mingus-playlist: \"2\"
mingus-browser:  \"3\"

Global keys:

g/q       mingus-git-out			 	  		
s 		 		mingus-stop				 			
p 		 		mingus-toggle			 			
> 		 		mingus-next				 			
< 		 		mingus-prev				 			
+,right	  mingus-vol-up			 	  
-,left  	mingus-vol-down	    	
b 		 		mingus-seek-backward	
f         mingus-seek-forward																	 
%         mingus-seek-percents
$         mingus-seek-from-start
C 		 		mingus-clear									 
L 		 		mingus-load-all			
I 		 		mingus-info					
v 		 		mingus-show-version		              
? 		 	 	mingus-help						
z 		 		mingus-random					
Z 		 		mingus-shuffle			
r 		 		mingus-repeat		    
Q         mingus-query

Playlist keys:

d,C-d,
<delete>, C-w  mingus-del						 
D         mingus-del-marked					 
M     		mingus-move-all						 
C-l   		mingus-goto-current-song	 
C-k   		mingus-move-up						 
C-j   		mingus-move-down					 
RET   		mingus-play								 
SPC,m  		mingus-mark								 
U         mingus-unmark-all
!         run a command on the marked songs

Browser keys:					 
                                 
RET       mingus-down-dir-or-play-song 	
:,^  			mingus-dir-up							 	 	
SPC 			mingus-insert                	
P   			mingus-insert-and-play        

MORE ELABORATE INSTRUCTIONS:

Requirements:
emacs22
 (it might perhaps work with emacs21 if you define the functions read-number,
 while-no-input and line-number-at-pos)
cl-macs.el and dired.el (included in emacs)
mpc
acces to mpd, running either locally or on another server

Getting started:

This help is always available with the command mingus-help, or
the keys \"?\" or \"1\" from the buffers *Mingus* or *Mingus
Browser*

When mpd is already playing a playlist, the command M-x mingus
will show this playlist; when not so, load a playlist with \"l\",
or make a new one with M-x mingus-browse (default key: \"3\").

Starting mpd:                    mingus-start-mpd-daemon
Providing environment variables: mingus-set-variables-interactively 
                                 (see also mingus-customize)

SELECTION OF SONGS:

Browsing:  command (mingus-browse) key: \"3\"

movement and insertion:

SPACEBAR always inserts everything under point or region

\"P\"        same as SPACEBAR, and plays the inserted song(s) instantly

RET        same as SPACEBAR, exept when on a directory and mark not active, then descend into dir.

\"^\" or \":\" go up a directory

Minibuffer browsing:

\"a\"       insert a file or directory through the use of the minibuffer; 
          follow instructions there provided

Playlist loading:

\"l\"        load playlist
\"o\"        as \"l\", but clear playlist first (prompts)

Querying: 

\"Q\"        query the database for artist, album, filename, title, or regexp on filename
           (type read from minibuffer)
\"M-%\"      query for regexp on filename

Results are shown in the *Mingus Browser* buffer, where all commands for browsing are available

PLAYING CONTROLS: 

see the refcard, and documentation of various commands, just try
them out. They should be quite self-evident, but let me know when
they are not. Not every command is (already) mapped to a
key, so M-x mingus- TAB to your delight to find everything.

PLAYLIST EDITING:

Deletion:

on marked songs: see section \"Marking\"

\"\\C-d\", \"d\", \"\\C-w\" or DEL 

delete single file, or region when there is a region; 

NB: this leaves the marking of other songs intact. As such it can
be slow, esp. when the region is large; it is then highly
recommended to mark the songs first, and then issue the command
mingus-del-marked (until I rewrite this function :])

Movement:

of marked songs: see section \"Marking\"

of single song: 

\"\\C-k\"                 Move song up one position
\"\\C-j\"                 Move song up down position

Marking:

Marking songs is useful for movement or deletion of multiple songs in or from the playlist;
first mark them, then delete or move them (to point).

\"m\" or SPACEBAR       (un)mark a song, or region, when there is a region
\"D\" (upcased)         delete marked songs (this will have the same effect as 
                      mingus-del when there are no marked songs)
\"M\"                   move marked songs to point
\"!\"                   get prompted for an operation on the marked songs

Point of insertion:

with mingus-set-insertion-point you can specify where new
insertions from the insertion commands from the *Mingus Browser*
buffer or from minibuffer-insertion will take place. Otherwise
the insertions will take place at the end of the playlist.

\"i\"                   set insertion point
\"u\"                   unset insertion point (available from everywhere)
\"\\C-u i\"              show current insertion point and move point there

Saving your playlist:

what about \"\\C-x \\C-s\", can you memorize that?


=================================================
AUTHOR:  Niels Giesen 
CONTACT: nielsgiesen at ibbu dot nl
")

(defconst mingus-font-lock-keywords
  (list
   '("^\* .*" . font-lock-warning-face)))

(defconst mingus-browse-font-lock-keywords
  (list 
   `(,mingus-song-regexp . mingus-song-face) ;dired-ignored-face)
   '(".*" . font-lock-type-face)))

(defconst mingus-help-font-lock-keywords
  (list 
   '("mingus[a-z-]*" . font-lock-function-name-face)
   '("^[A-Z ]+:" . font-lock-warning-face)
   '("^[A-Z][a-z ]+:" . font-lock-constant-face)
   '("=" . font-lock-variable-name-face)))

(defconst mingus-global-map
  (let ((mingus-global-map (make-keymap)))
    (define-key mingus-global-map "k" (lambda () (interactive) (forward-line -1)))
    (define-key mingus-global-map "q" 'mingus-git-out)
    (mapcar (lambda (key)
	      (define-key mingus-global-map key 'mingus-git-out)) '("g" "q"))
    (define-key mingus-global-map "Q" 'mingus-query)
    (define-key mingus-global-map "\M-%" 'mingus-query-regexp)
    (define-key mingus-global-map "\\" 'mingus-last-query-results)
    (define-key mingus-global-map "j" 'forward-line)
    (define-key mingus-global-map "s" 'mingus-stop)
    (define-key mingus-global-map "@" 'mingus-update)
    (define-key mingus-global-map "p" 'mingus-toggle)
    (define-key mingus-global-map "%" 'mingus-seek-percents)
    (define-key mingus-global-map ">" 'mingus-next)		
    (define-key mingus-global-map "<" 'mingus-prev)		
    (mapcar (lambda (key) (define-key mingus-global-map key 'mingus-vol-up))
	    '("+" [(right)] "*"))
    (mapcar (lambda (key) (define-key mingus-global-map key 'mingus-vol-down))
	    '("-" [(left)] "/"))
    (define-key mingus-global-map "b" 'mingus-seek-backward)		
    (define-key mingus-global-map "f" 'mingus-seek)		
    (define-key mingus-global-map "$" 'mingus-seek-from-start)
    (define-key mingus-global-map "x" 'mingus-crossfade)		
    (define-key mingus-global-map "C" 'mingus-clear)		    
    (define-key mingus-global-map "c" 'mingus-crop)		
    (define-key mingus-global-map "L" 'mingus-load-all)		
    (define-key mingus-global-map "I" 'mingus-info)		
    (define-key mingus-global-map "v" 'mingus-show-version)		
    (define-key mingus-global-map "z" 'mingus-random)		
    (define-key mingus-global-map "Z" 'mingus-shuffle)		
    (define-key mingus-global-map "r" 'mingus-repeat)		
    (define-key mingus-global-map "u" 'mingus-unset-insertion-point)		
    (define-key mingus-global-map "l" 'mingus-load-playlist)		
    (define-key mingus-global-map "R" 'mingus-remove-playlist)		
    (mapcar (lambda (key) (define-key mingus-global-map key 'mingus-help))
	    '("H" "?" "1"))
    (define-key mingus-global-map "a" (lambda () (interactive) (mingus-insert t)))		
    (define-key mingus-global-map "P" 'mingus-insert-and-play)
    (define-key mingus-global-map "\C-x\C-s" 'mingus-save-playlist)
    (define-key mingus-global-map "o" 'mingus-open-playlist)
    (define-key mingus-global-map "2" 'mingus)				
    (define-key mingus-global-map "3" 'mingus-browse)				
    mingus-global-map) 
  "Global keymap for mingus")

(defconst mingus-help-map
  (let ((mingus-help-map (copy-keymap mingus-global-map)))
    (define-key mingus-help-map " " 'scroll-up)
    mingus-help-map)
  "Help keymap for Mingus")

(defconst mingus-playlist-map
  (let ((mingus-playlist-map (copy-keymap mingus-global-map)))
    ;;deletion keys
    (mapcar (lambda (key)
	      (define-key mingus-playlist-map key (lambda () (interactive) 
						    (if mark-active
							(call-interactively 'mingus-del-region)
						      (mingus-del-marked))))) '("D" "\C-w"))

    (mapcar (lambda (key) (define-key mingus-playlist-map key 
			    '(lambda () (interactive) 
			       (if mark-active
				   (call-interactively 'mingus-del-region)
				 (mingus-del)))))
	    '("d" "\C-d"))
    ;;movement keys
    (define-key mingus-playlist-map "M" 'mingus-move-all)		
    (define-key mingus-playlist-map "\C-k" 'mingus-move-up)		
    (define-key mingus-playlist-map "\C-j" 'mingus-move-down)				
    ;;marking keys
    (define-key mingus-playlist-map "U" 'mingus-unmark-all)
    (mapcar (lambda (key)
	      (define-key mingus-playlist-map key (lambda () (interactive) 
						    (if mark-active
							(call-interactively 'mingus-mark-region)
						      (mingus-mark)))))
	    '("m" " "))
    (define-key mingus-playlist-map "i" 'mingus-set-insertion-point)
    (define-key mingus-playlist-map "!" (lambda ()
					  (interactive)
					  (if (or mingus-marked-list)
					      (progn
						(let ((command (read-key-sequence "! on marked songs (D: deletion M: Move here)" )))
						  (cond ((string-match "d\\|D" command)
							 (mingus-del-marked))
							((string-match "m\\|M" command)
							 (mingus-move-all))
							(t nil))))
					    (message "No marked songs"))))


    ;; miscellaneous keys
    (define-key mingus-playlist-map "\r" 'mingus-play)
    (define-key mingus-playlist-map "\C-l" 'mingus-goto-current-song)		
    mingus-playlist-map)
  "Playlist keymap for mingus")

(defconst mingus-browse-map
  (let ((mingus-browse-map (copy-keymap mingus-global-map)))
    (define-key mingus-browse-map "\r" 'mingus-down-dir-or-play-song)				
    (mapcar (lambda (key) (define-key mingus-browse-map key 'mingus-dir-up))
	    '(":" "^"))

    (define-key mingus-browse-map " " 'mingus-insert)      
;    (define-key mingus-browse-map "P" 'mingus-insert-and-play)
    mingus-browse-map)
  "Browse keymap for mingus") 

(defun mingus-show-version ()
  (interactive)
  (message "Version of mingus: %s" mingus-version))

(defun mingus-help ()
  "Help screen for mingus."
  (interactive)
  (switch-to-buffer "*Mingus Help*")
  (set (make-local-variable 'font-lock-defaults) '(mingus-help-font-lock-keywords))
  (font-lock-mode t)
  (when (string= (buffer-string) "")
    (use-local-map mingus-help-map)
    (insert mingus-help-text)
    (setq buffer-read-only t))
  (goto-char (point-min)))

;;;some generic functions:
(defun mingus-min-and-sec-to-sec (&optional time)
  "Convert minutes and seconds to seconds, in format min:sec (a string) .\nIf time not provided, read from minibuffer. If incorrect string, return nil."
  (let* ((time (or time (read-from-minibuffer "Minutes:seconds: ")))
	 (mins (progn (if (string-match "[0-9]:+" time)
			  (string-to-number (substring (match-string 0 time) 0 -1)))))
	 (secs (progn (if (string-match ":[0-9]+" time)
			  (string-to-number (substring (match-string 0 time) 1))))))
    (if (and (null secs) (null mins))
	nil 
      (+ (or secs 0) (* 60 (or mins 0))))))

(defun mingus-delete-line ()
  "Delete line at point."
  (delete-region (point-at-bol 1) (point-at-bol 2))
  (when (eobp) 
    (delete-region (point-at-bol) (point-at-eol 0))
    (beginning-of-line)))

(defun mingus-strip-last-line ()
  (let (pos (point))
    (goto-char (point-max))
    (delete-region (point-at-bol) (point-at-eol 0))
    (goto-char pos)))
  
(defun mingus-git-out (&optional x)
  (interactive)
  (while (member (buffer-name) '("*Mingus Help*" "*Mingus*" "*Mingus Browser*"))
    (bury-buffer)))

(defun mingus-playlist-mode ()
  "Mingus playlist mode;
see function mingus-help for instructions."
  (font-lock-mode t)
  (use-local-map mingus-playlist-map)
  (set (make-local-variable 'font-lock-defaults) '(mingus-font-lock-keywords))
  (set (make-local-variable 'point-of-insertion) nil)
  (setq major-mode 'mingus-playlist-mode)
  (setq mode-name "MINGUS-PLAYLIST")
  (run-hooks 'mingus-playlist-hook))

(defun mingus-browse-mode ()
  "Mingus major mode"
  (kill-all-local-variables)
  (use-local-map mingus-browse-map)
  (set (make-local-variable 'font-lock-defaults) '(mingus-browse-font-lock-keywords))
  (setq major-mode 'mingus-browse-mode)
  (setq mode-name "MINGUS-BROWSE")
  (run-hooks 'mingus-browse-hook)
  (set (make-local-variable 'positions) nil))

(defvar mingus-header-height 0)
(defvar mingus-marked-list nil)

(defun mingus ()
  "MPD Interface by Niels Giesen, Useful and Simple, or actually just named after that great bass player"
  (interactive)
  (progn
    (if (bufferp (get-buffer "*Mingus*"))
	(switch-to-buffer "*Mingus*")
      (switch-to-buffer "*Mingus*")
      (mingus-playlist-mode)
      (font-lock-mode t)
      (setq buffer-read-only t))
    (let ((buffer-read-only nil))
      (mingus-playlist)
      (goto-line (or (mingus-cur-song-number) 1)))))

(defmacro mingus-make-fn (name command &optional docstring &rest body)
  (funcall
   (lambda ()
     `(defun ,name ()
	,docstring
	(interactive)
	(let ((buffer-read-only nil))
	  (shell-command (concat "mpc " ,command))
	  ,@body)))))

(defun mingus-start-daemon ()
  "Start mpd daemon for Mingus."
  (interactive)
  (shell-command "mpd"))

(mingus-make-fn mingus-shuffle "shuffle" "Shuffle mpd playlist." (save-window-excursion (mingus)))
(mingus-make-fn mingus-update "update" "Update mpd database.")
(mingus-make-fn mingus-repeat "repeat" "Toggle mpd repeat mode." (mingus-info))
(mingus-make-fn mingus-random "random" "Toggle mpd random mode." (mingus-info))
(mingus-make-fn mingus-toggle "toggle" "Toggle mpd pause/play mode." (mingus-info))
(mingus-make-fn mingus-pause "pause" "Pause mpd." (mingus-info))
(mingus-make-fn mingus-prev "prev" "Play previous song in mpd playlist." (mingus-info))
(mingus-make-fn mingus-next "next" "Play next song in mpd playlist." 
		(when (string=
		       "*Mingus*" (buffer-name))
		  (mingus-info)))
(mingus-make-fn mingus-info "" "Show mpd info." (progn (while-no-input (sleep-for 1)
								       (mingus-info))))
(mingus-make-fn mingus-stop "stop" "Tell mpd to stop playing.")
(mingus-make-fn mingus-vol-up "volume +1" "Tell mpd to increase volume." (mingus-info))
(mingus-make-fn mingus-vol-down "volume -1" "Tell mpd to decrease volume." (mingus-info))

(defmacro mingus-advice (func-name buffer-name &optional docstring) ;should make this dependent on a keyword
  (funcall
   (lambda ()
     `(defadvice ,func-name (around mingus-around-advise activate)
	,docstring
	(if (string= ,buffer-name (buffer-name))
	    ad-do-it
	  (message ,(format "Not in %s buffer" buffer-name)))))))

(mingus-advice mingus-goto-current-song "*Mingus*")
(mingus-advice mingus-del-region "*Mingus*")
(mingus-advice mingus-mark-region "*Mingus*")
(mingus-advice mingus-move-down "*Mingus*")
(mingus-advice mingus-set-insertion-point "*Mingus*")
(mingus-advice mingus-move-up "*Mingus*")
(mingus-advice mingus-down-dir-or-play-song "*Mingus Browser*")

(mapcar 'ad-activate '(mingus-goto-current-song 
		       mingus-del-region 
		       mingus-down-dir-or-play-song 
		       mingus-move-down
		       mingus-move-up 
		       mingus-set-insertion-point))

(defun* mingus-seek (amount &optional percentage from-start)
  "Seek song played by mpd in seconds or percentage.
Take optional AMOUNT argument, specifying movement forward or backward movement.
Defaults to 10 seconds.
When PERCENTAGE is specified, seek to PERCENTAGE of song.
If PERCENTAGE is specified and AMOUNT is negative, seek PERCENTAGE backwards."
  (interactive "p")
  (if (string= "seek amount" (substring (shell-command-to-string
					 (concat "mpc seek " (if (or (minusp amount) percentage from-start) "" "+") 
						 (number-to-string (if (and (null from-start)(= 1 amount)) 10 amount)) (if percentage "%"))) 0 11))
      (message "Seek amount would seek past the end of the song")
    (mingus-info)))

(defun mingus-seek-percents (amount)
  "Seek song played by mpd in percentage."
  (interactive "p")
  (cond ((= 1 amount)
	 (message "Usage: give prefix argument to specify absolute percentage of song.\n(eg: C-u 40 %% seeks to the point at 40%% of current song)\nNegative argument seeks backward.\n(eg: C-u -10 %% to seek backward 10 percent)"))
	(t
	 (mingus-seek amount t))))

(defun mingus-seek-from-start (amount)
  "Seek to PREFIX seconds from start of current song played by mpd."
  (interactive "p")
  (if (= 1 amount)
      (message "Usage: seek to PREFIX seconds from start of current song.\n(eg: C-U 30 seeks to thirtieth second of song)")
    (mingus-seek amount nil t)))

(defun mingus-seek-min-sec ()
  "Seek to minute:second point in song."
  (interactive)
  (mingus-seek-from-start
   (mingus-min-and-sec-to-sec (read-from-minibuffer "Minutes and seconds (eg 2:30): "))))

(defun mingus-seek-backward (amount)
  (interactive "p")
  (mingus-seek (- 0 (if (= 1 amount) 10 amount))))

(defun mingus-crossfade (p)
  "Set crossfade time for mpd;
prefix argument of 0 sets crossfade off."
  (interactive "P")
  (shell-command (concat "mpc crossfade " (if p (number-to-string (if (listp p) (car p) p)) "")))
  (if p (message "crossfade: %d" p)))
  

(defmacro mingus-make-fn-shell-after (name command &optional docstring &rest body) ;should make this dependent on a keyword
  (funcall
   (lambda ()
     `(defun ,name ()
	,docstring
	(interactive)
	(let ((buffer-read-only nil))
	  ,@body
	  (shell-command (concat "mpc " ,command)))))))

(defun mingus-cur-line (&optional stringify)
  "In Mingus, return number of song under point"
  (if stringify 
      (number-to-string	(line-number-at-pos))
    (line-number-at-pos)))

(defun mingus-playlist ()	
  "Insert current playlist into buffer \"*Mingus*\"."
  (switch-to-buffer "*Mingus*")
  (let ((buffer-read-only nil))
    (let ((line (line-number-at-pos)))
      (erase-buffer)
      (cond ((string= "" (shell-command-to-string "mpc playlist"))
	     (insert "Press ? for help, 3 for Mingus Browser"))
	    (t 
	     (insert (substring (shell-command-to-string "mpc playlist") 0 -1))
	     (let ((int '(0)))
	       (while
		   (re-search-backward "^#[0-9]+) \\(.+/\\|\\)" nil t)
		 (replace-match "")))
	     (mapcar (lambda (list)
		       (goto-line list)
		       (insert "* ")) mingus-marked-list)
	     (goto-line line))))))

(defun mingus-unmark-all ()
  "In Mingus, unmark all marked songs in mpd playlist"
  (interactive)
  (save-excursion
    (let ((buffer-read-only nil))										
      (setq mingus-marked-list nil)
      (goto-char (point-min))
      (while (re-search-forward "^* " (point-max) 1)
	(replace-match ""))))
  (message "No songs marked anymore"))

(defun mingus-cur-song-number ()
  "Return number of song currently played by mpd.
Return nil if no song playing."
  (let ((mess (shell-command-to-string "mpc"))) 
    (when	(string-match "^\\[\\(paused\\] \\|playing\\]\\) #\\([0-9]+\\)" mess)
      (string-to-number (substring mess (match-beginning 2)(match-end 0))))))
		
(defun mingus-goto-current-song ()			
  "In Mingus, move point to currently playing song."
  (interactive)
  (goto-line (or (mingus-cur-song-number) 1)))

(defun mingus-playlist-length ()
  "Return length of current mpd playlist."
  (if (string= "" (shell-command-to-string "mpc playlist")) 
      0
    (with-temp-buffer 
      (insert (shell-command-to-string "mpc playlist")) 
      (goto-char (point-max)) 
      (string-to-number (buffer-substring-no-properties
			 (1+ (re-search-backward "^#" nil t))
			 (re-search-forward "[0-9]+" nil t))))))

(defun mingus-insert-and-play ()
  "In Mingus Browse, insert stuff under point or region in mpd playlist and play instantly."
  (interactive)
  (let ((point (1+ (or (save-window-excursion
												 (switch-to-buffer "*Mingus*")
												 (caar point-of-insertion))(mingus-playlist-length)))))
    (mingus-insert)
    (mingus-play (number-to-string point))))

(defun mingus-volume ()
  "Return mpd volume as string."
  (shell-command-to-string "mpc volume"))

(defun mingus-move-one (from to)
  "Move mpd playlist id FROM to mpd playlist position TO."
  (shell-command (format "mpc move %s %s" from to)))

(defun mingus-move-up ()
  "In Mingus, move song at point up one position, visually."
  (interactive)
  (when (< 1 (line-number-at-pos))
    (let ((buffer-read-only nil))
      (cond ((and point-of-insertion (= (line-number-at-pos) (caar point-of-insertion)))
	     (decf (caar point-of-insertion)))
	    ((and point-of-insertion (= (line-number-at-pos) (1+ (caar point-of-insertion))))
	     (incf (caar point-of-insertion))))
      (mingus-move-one (line-number-at-pos) (1- (line-number-at-pos)))
      (transpose-lines 1)
      (forward-line -2)
      (message "Moved 1 song up."))))

(defun mingus-move-down ()
  "In Mingus, move song at point down one position, visually."
  (interactive)
  (when (> (mingus-playlist-length) (line-number-at-pos))
    (let ((buffer-read-only nil))
      (cond ((and point-of-insertion (= (line-number-at-pos) (caar point-of-insertion)))
	     (incf (caar point-of-insertion)))
	    ((and point-of-insertion (= (line-number-at-pos) (1- (caar point-of-insertion))))
	     (decf (caar point-of-insertion))))
      (mingus-move-one (line-number-at-pos)(1+ (line-number-at-pos)))
      (forward-line 1)
      (transpose-lines 1)
      (forward-line -1)
      (message "Moved 1 song down."))))

(defun mingus-move (list pos &optional after-insert)
  "In Mingus, move LIST of songs to position POS.
POS increments when number of a song is greater, thereby preserving order."
  (let* ((poi (mingus-get-insertion-number))
	 (less-than-poi (count-if (lambda (item) (< item poi)) list))
	 (more-than-poi (- (length list) less-than-poi))
	 (novy (mingus-get-insertion-number))
	 (poi-itself-moved-p (if (and point-of-insertion (member novy list)) t)))
    (when (and point-of-insertion (not poi-itself-moved-p))
      (cond ((and (< poi pos)
		  (< 0 less-than-poi))
	     (setq novy (- poi less-than-poi)))
	    ((and (> poi pos)
		  (> more-than-poi 0))
	     (setq novy (+ poi more-than-poi)))))
    (let ((count1 0)
	  (newpos (- (line-number-at-pos)
		     (count-if (lambda (item) (> (line-number-at-pos) item)) list))))
      (let ((to (list pos))
	    (count (list count1)))
	(mapcar #'(lambda (from)
		    (cond ((> from (car to))
			   (shell-command (concat "mpc move " (number-to-string from) " " (number-to-string (car to))))
			   (incf (car to)))
			  ((< from (car to))
			   (shell-command (concat "mpc move " (number-to-string (- from (car count))) " " (number-to-string (1- (car to)))))
			   (incf (car count))))) 
		(sort list #'<)))
      ;;wip
      (unless after-insert
	(setq mingus-marked-list nil)
	)
      ;;wip
      (mingus)
      (cond (poi-itself-moved-p 
	     (mingus-unset-insertion-point))
	    ((/= novy (mingus-playlist-length))
	     (goto-line novy)
	     (mingus-set-insertion-point)))
      (goto-line newpos))))



(defun mingus-move-all ()
  "In Mingus, move all marked songs to current position in buffer."
  (interactive)
  (let ((length (length mingus-marked-list)))
    (if (= length 0)
	(message "No marked songs")
      (let ((line (line-number-at-pos)))
	(mingus-move mingus-marked-list (1+ (line-number-at-pos)))
	(goto-line line)
	(message "Moved %d song%s" length 
		 (if (< 1 length) "s" ""))))))

(defun mingus-mark ()
  "In Mingus, mark a song for movement or deletion.
Unmark song when already marked.
To mark a region, use mingus-mark-region."
  (interactive)
  (let ((buffer-read-only nil))
    (if (= (point) (point-max))
	(forward-line -1))
    (progn
      (if (not mingus-marked-list)
	  (setq mingus-marked-list nil))
      (cond ((not (member (mingus-cur-line) mingus-marked-list))
	     (push (mingus-cur-line) mingus-marked-list)
	     (beginning-of-line)
	     (insert "* "))
	    (t (setq mingus-marked-list (remove (mingus-cur-line) mingus-marked-list))
	       (beginning-of-line)
	       (delete-char 2)))
      (forward-line 1))))

(defun mingus-mark-region (beg end)
  "In Mingus, mark a region for movement or deletion."
  (interactive "r")
  (let ((buffer-read-only nil)
	(int 0))
    (if (= (point) (point-max))
	(forward-line -1))
    (goto-char beg)
    (while (< (point) end)
      (if (not (member (mingus-cur-line) mingus-marked-list))
	  (progn
	    (push (mingus-cur-line) mingus-marked-list)
	    (beginning-of-line)
	    (insert "* ")
	    (incf end 2)))
      (forward-line))))

(mingus-make-fn mingus-del (format "del %s" (mingus-cur-line t)) "Delete song under line."
		(let ((buffer-read-only nil))
		  (cond ((and point-of-insertion (= (mingus-get-insertion-number) (line-number-at-pos)))
			 (mingus-unset-insertion-point))
			((and point-of-insertion (> (mingus-get-insertion-number) (line-number-at-pos)))
			 (decf (caar point-of-insertion))))
		  (mingus-delete-line)
		  (setq mingus-marked-list (nreverse (mapcar (lambda (list)
							       (cond ((< (mingus-cur-line) list) 
								      (1- list))
								     (t list)))(remove (mingus-cur-line) mingus-marked-list))))))


(defun mingus-del-region (beg end)
  "In Mingus, delete region.
Leave mingus-marked-list intact."
  (interactive "r")
  ;;no need for consuming computation and bindings when whole buffer is selected
  (if (and (= beg (point-min)) (= end (point-max)))
      (mingus-clear t)
    (let* ((buffer-read-only nil)
	   (beg (line-number-at-pos beg))
	   (end (if (bolp) (line-number-at-pos end) (1+ (line-number-at-pos end))))
	   (howmanysongs (- end beg))
	   (deletestring "mpc del "))
      ;; make the shell-command-string
      (dotimes (count howmanysongs)
	(setq deletestring (concat deletestring " " (number-to-string (+ beg count)))))
      ;; delete the files
      (shell-command deletestring)
      ;; remove all songs that are deleted from mingus-marked-list
      (setq mingus-marked-list
	    (remove-if (lambda (item) (and (<= beg item) (> end item))) mingus-marked-list))
      ;; substract howmanysongs from the marked songs that have become at a lower place in playlist due to deletion
      (setq mingus-marked-list (mapcar (lambda (item) 
					 (if (<= end item) 
					     (- item howmanysongs)
					   item)) mingus-marked-list))
      ;; rebuild the playlist according to new specs
      (mingus)
      ;; reset insertion point
      (cond ((and (or point-of-insertion)(<= end (caar point-of-insertion)))
	     (goto-line (- (caar point-of-insertion) howmanysongs))
	     (mingus-set-insertion-point))
	    ((and (or point-of-insertion)(<= beg (caar point-of-insertion)))
	     (mingus-unset-insertion-point)))
      ;; bring point to beginning of former region
      (goto-line beg))))

(defun mingus-del-marked ()
  "Delete songs marked in *Mingus* buffer"
  (interactive)
  (let ((buffer-read-only nil)
	(cur-line (line-number-at-pos)))
    (if mingus-marked-list
	(when	(yes-or-no-p (format "Remove %d marked songs? " (length mingus-marked-list)))
	  (progn
	    (shell-command (let ((delete-string "mpc del"))
			     (mapcar 
			      (lambda (list)
				(setq delete-string (concat delete-string " " (number-to-string list))))
			      mingus-marked-list)
			     delete-string))
	    (delete-matching-lines "^* " (point-min) (point-max))
	    (cond ((and point-of-insertion (member (mingus-get-insertion-number) mingus-marked-list))
		   (mingus-unset-insertion-point))
		  (point-of-insertion
		   (goto-line
		    (- (mingus-get-insertion-number) (count-if (lambda (item) (< item (mingus-get-insertion-number))) mingus-marked-list)))
		   (mingus-set-insertion-point)))
	    (goto-line (- cur-line (count-if (lambda (item) (> cur-line item)) mingus-marked-list)))
	    (setq mingus-marked-list nil)))
      (progn
	(shell-command (concat "mpc del " (mingus-cur-line t)))
	(mingus-delete-line)		;this will have an effect on
					;mingus-move: either make actions
					;mutually exclusive, or take
					;these effects into account
	(setq mingus-marked-list (nreverse (mapcar (lambda (list)
						     (cond ((< (line-number-at-pos) list) 
							    (1- list))
							   (t list)))(remove (line-number-at-pos) mingus-marked-list)))))))
  (when (eobp)
    (delete-region (point-at-bol) (point-at-eol 0))
    (beginning-of-line)))

(defun mingus-del-other-songs ()
  "In the *Mingus* buffer, delete all songs but the marked ones from playlist."
  (interactive)
  (if mingus-marked-list
      (let ((delete-string "mpc del")
	    ;; calculate new position for point
	    (newpos (length (member (line-number-at-pos) (setq mingus-marked-list (sort mingus-marked-list #'>)))))
	    ;; calculate new position of insertion point
	    (posfrom (length (member (caar point-of-insertion) mingus-marked-list))))
	(mapcar 
	 (lambda (list)
	   (setq delete-string 
		 (concat delete-string " " (number-to-string list)))
	   list)
	 (let ((result nil))
	   (dotimes (int (1+ (count-lines (point-min) (point-max))) result)
	     (unless (or (= 0 int) (member int mingus-marked-list))
	       (push int result)))))
	(shell-command delete-string)
	(setq mingus-marked-list nil)
	(call-interactively 'mingus)
	(if  (zerop posfrom)
	    (mingus-unset-insertion-point)
	  (goto-line posfrom)
	  (mingus-set-insertion-point))
	(goto-line newpos)
	(when (/= newpos (line-number-at-pos))
	  (goto-char (point-min)))
	(message "Other songs deleted"))))

(defun mingus-play (&optional position)										
  "Start playing the mpd playlist, only if not yet playing.
When called with argument POSITION, play playlist id POSITION."
  (interactive)
  (shell-command 
   (concat "mpc play " 
	   (or position
	       (if (string= "*Mingus*" (buffer-name))
		   (mingus-cur-line t)))))
  (mingus-info))

(defun mingus-play-pos (position)
  "Play song in mpd playlist at position specified by prefix argument."
  (interactive "p")
  (mingus-play (number-to-string position)))

(defun mingus-clear (&optional dontask)
  "Clear mpd playlist;
Does prompting."
  (interactive)
  (if (or dontask (yes-or-no-p "Clear the playlist? "))
      (progn (shell-command "mpc clear ")
	     (with-current-buffer "*Mingus*"				
	       (let ((buffer-read-only nil))
		 (erase-buffer)
		 (mingus-unset-insertion-point)
		 (setq mingus-marked-list nil)
		 (message "Playlist cleared"))))
    (message "Playlist not cleared")))

(defun mingus-load-all ()
  "Load all songs in mpd database into mpd playlist."
  (interactive)
  (when (yes-or-no-p "Load the WHOLE mpd database? " )
    (shell-command (concat "mpc clear&&mpc ls|mpc add" ) nil)
    (save-window-excursion
      (mingus))))

(defun mingus-crop ()
  "Crop mpd playlist "
  (interactive)
  (let ((buffer-read-only nil))
    (shell-command "mpc crop")
    (save-window-excursion
      (mingus))))

(defun mingus-add (string) 
  "In Mingus, add a song." 
  (shell-command (concat "mpc add " "\"" string "\""))
  (mingus))

(defun mingus-browse ()
  "Switch to buffer *Mingus Browser* and start your Mingus browsing experience."
  (interactive)
  (if (bufferp (get-buffer "*Mingus Browser*"))
      (switch-to-buffer "*Mingus Browser*")
    (progn
      (switch-to-buffer "*Mingus Browser*")
      (mingus-browse-mode)
      (setq buffer-read-only t)
      (let ((buffer-read-only nil))	
	(erase-buffer)		     ;only if not yet in browsing mode
	(goto-char (point-min))
					;				(mingus-header "Browse: ")
	(mingus-down-dir-or-play-song)))))

(defun mingus-browse-invisible ()
  "Hide $PWD in file and directory names in *Mingus Browser* buffer."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (put-text-property	
       (point-at-bol) 
       (or (re-search-forward ".*/" (point-at-eol) t 1) (point-at-bol)) 
       'invisible t)
      (forward-line 1))))

(defun mingus-songp ()
  "In *Mingus Browser* buffer, check whether current line contains a song name."
  (save-excursion
    (beginning-of-line)
    (re-search-forward mingus-song-regexp (point-at-eol) t)))
(defun* mingus-add-song-at-p (&optional beg end)
  "Add song or directory at point.
If active region, add everything under the region, sloppily."
					;	(interactive "r")
  (let ((song (buffer-substring-no-properties
	       (if beg (progn (goto-char beg) (point-at-bol)) (point-at-bol)) ;sloppy region
	       (if end (progn (goto-char end) (point-at-eol (if (bolp) 0 1))) (point-at-eol)))))	
					;if point is at bol, do not count the current line
    (with-temp-buffer
      (insert song)
      (goto-char (point-min))
      (while (re-search-forward "^" (point-max) t)
	(replace-match "\""))
      (goto-char (point-min))
      (while (re-search-forward "\n" (point-max) t)
	(replace-match "\" "))
      (goto-char (point-max))
      (insert "\"")
      (shell-command (format "mpc add %s"  (buffer-string))))))

(defun mingus-down-dir-or-play-song ()
  "In *Mingus Browser* buffer, descend into dir at point, or play song at point"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (mingus-songp) 
	(mingus-insert)
      (progn
	(push (line-number-at-pos) positions)
	(mingus-ls (concat "\"" 
			   (buffer-substring-no-properties 
			    (point-at-bol)(point-at-eol)) "\""))))))

(defun mingus-ls (string)
  "List songs/dirs in directory STRING in dedicated *Mingus Browser* buffer."
  (switch-to-buffer "*Mingus Browser*")
  (save-excursion)
  (let ((buffer-read-only nil))
    (erase-buffer)
    (insert (substring (shell-command-to-string (concat "mpc ls "  string)) 0 -1))
    (setq mode-name (if (string= "" string) "top dir" string))
    (mingus-browse-invisible)))

(defun mingus-dir-up () 
  "In Mingus-Browse, go up one directory level."
  (interactive)
  (end-of-line)				
  (let ((buffer-read-only nil)
	(goal (buffer-substring-no-properties 
	       (or (re-search-backward "/" (point-at-bol) t 1) (point))(point-at-bol))))
    (end-of-line)
    (if (re-search-backward "/" (point-at-bol) t 2)
	(progn
	  (mingus-ls 
	   (concat "\"" 
		   (buffer-substring-no-properties (point-at-bol) (point)) 
		   "\"" )))
      (progn
	(mingus-ls "")))
    (re-search-backward goal)))

(defun mingus-browse-back ()
  (interactive)
  (if (not (string= "*Mingus Browser*" (buffer-name)))
      (message "Not in *Mingus Browser* buffer")
    (let ((buffer-read-only nil))
      (erase-buffer)
      (insert-string (pop prvscrn)))))

(defun* mingus-insert (&optional read)
  "In Mingus-Browse, insert anything under point or region into mpd playlist."
  (interactive)
  (save-window-excursion
    (let ((end-of-playlist (1+ (mingus-playlist-length)))
	  (buffer-read-only nil))
      (if (or read (not (string= "*Mingus Browser*" (buffer-name))))
	  (mingus-add-read-input)
	(if mark-active 
	    (mingus-add-song-at-p (mark)(point))
	  (mingus-add-song-at-p)))
      (let* ((howmanysongs (- (1+ (mingus-playlist-length)) end-of-playlist))
	     (song (if (< 1 howmanysongs) "songs" "song")))
	(switch-to-buffer "*Mingus*")
	(if (or point-of-insertion)
	    (progn					
	      (mingus-move 
	       (do* ((num end-of-playlist (1+ num))
		     (result (list num) (push num result)))
		   ((= (car result) (mingus-playlist-length)) result))
	       (1+ (caar point-of-insertion)) t) 
	      ;;let's handle mingus-marked-list gracefully:
	      (if point-of-insertion
		  (let ((number-of-songs (1+ (- (mingus-playlist-length) end-of-playlist))))
		    (setq mingus-marked-list
			  (mapcar (lambda (list) (if (> list (caar point-of-insertion))
						     (incf list number-of-songs) list)) mingus-marked-list))))

	      (message "%d %s added after %s (%d)" howmanysongs song (cadar point-of-insertion) (caar point-of-insertion) ))
	  (message "%d %s added at end of playlist." howmanysongs song)))))
  (if (string= "*Mingus*" (buffer-name))
      (mingus)
    (unless mark-active (forward-line 1))))

(defun* mingus-set-insertion-point (&optional p)
  "In Mingus, set point-of-insertion for new songs.
They will be added after this point.
Prefix argument shows value of point-of-insertion, and moves there."
  (interactive "P")
  (cond ((string= "*Mingus*" (buffer-name))
	 (cond ((null p)
		(set 'point-of-insertion (list (list (line-number-at-pos)
						     (buffer-substring-no-properties 
						      (point-at-bol) (point-at-eol))))))
	       (point-of-insertion 
		(goto-line (caar point-of-insertion))))
	 (message "Point-of-insertion set at %s" (or (cadar point-of-insertion) "end of playlist (unset)")))
	(t (message "Not in \"*Mingus*\" buffer"))))

					;fixme do something with text-properties here once I find out how to...

(defun mingus-set-insertion-point-at-currently-playing-song ()
  (interactive)
  (save-window-excursion
    (switch-to-buffer "*Mingus*")
    (mingus-goto-current-song)
    (mingus-set-insertion-point)))

(defun mingus-get-insertion-number (&optional stringify)
  (if point-of-insertion
      (if stringify (number-to-string (caar point-of-insertion))
	(caar point-of-insertion))
    (if stringify (number-to-string (mingus-playlist-length))
      (mingus-playlist-length))))

(defun mingus-unset-insertion-point ()
  "Unset Mingus' point-of-insertion."
  (interactive)
  (save-window-excursion
    (switch-to-buffer "*Mingus*")
    (set 'point-of-insertion nil)
    (message "Point-of-insertion unset")))

(defun mingus-load-playlist (&optional open)
  "Load an mpd playlist.
Append playlist to current playlist."
  (interactive)
	(let ((lst (shell-command-to-string "mpc lsplaylists")))
		(if (string= "" lst)
				(message "No playlist present")
			(if open (mingus-clear))
			(let ((playlist (completing-read (format "%s playlist: " (if open "Open" "Load"))
																			 (split-string (substring lst 0 -1) "\n+")  nil t)))
				(if (string= "" playlist)
						(message "No playlist selected")
					(progn
						(shell-command (concat "mpc load " playlist))
						(message (format "Playlist %s %s" playlist  (if open "opened" "loaded")))
						(mingus)))))))

(defun mingus-open-playlist ()
  "Open an mpd playlist. 
Replace current playlist."
  (interactive)
  (mingus-load-playlist t))
				 
(defun mingus-save-playlist ()
  "Save an mpd playlist"
  (interactive)
  (let* ((lst (shell-command-to-string "mpc lsplaylists"))
				 (playlist (completing-read "Save playlist as: "
																		(if (string= "" lst) 
																				nil
																			(split-string (substring lst 0 -1) "\n+")) nil nil)))
    (if (string= "" playlist)
				(message "No name for playlist provided, won't save...")
      (progn
				(shell-command (concat "mpc save " playlist))
				(message "Playlist saved as %s" playlist)))))

(defun mingus-remove-playlist ()
  "Remove an mpd playlist"
  (interactive)
  (let ((list (shell-command-to-string "mpc lsplaylists")))
    (cond ((string= "" list)
	   (message "No playlist to remove"))
	  (t
	   (let ((playlist (completing-read "Remove playlist: "
					    (split-string (substring list 0 -1) "\n+") nil nil)))
	     (if (string= "" playlist)
		 (message "No name for playlist provided, won't remove")
	       (progn
		 (shell-command (concat "mpc rm " playlist))
		 (message "Playlist %s removed" playlist))))))))

(defun mingus-add-read-input (&optional string)
  "Add song or dir to mpd playlist using minibuffer input"
  (interactive)
  (let* ((string (if string string ""))
	 (res-one "")
	 (res (completing-read 
	       (format "Add: %s%s" string (if (string= "" string) "" "/"))
	       (split-string 
		(with-temp-buffer 
		  (insert
		   (substring (shell-command-to-string (concat "mpc ls " "\"" string "\"")) 0 -1))
		  (goto-char (point-min))
		  (while (re-search-forward "^.*/" (point-max) t)
		    (replace-match ""))
		  (buffer-string))
		"\n+") nil t)))
    (if	(string-match mingus-song-regexp res)
	(mingus-add (concat string "/" res))
      (if (string-match "n\\| " (read-key-sequence "Descend into dir? (\"n\" or SPACE to insert whole dir) "))
	  (mingus-add (concat string "/" res))
	(mingus-add-read-input (concat string (if (not (string= "" string)) "/" "") res))))))

(defun mingus-add-read-input-and-play ()
  "Add song or dir to mpd playlist using minibuffer input and play it instantly"
  (interactive)
  (let ((length (mingus-playlist-length)))
    (mingus-insert t)
    (save-excursion
      (switch-to-buffer "*Mingus*")
      (mingus-play (number-to-string (1+ (if point-of-insertion
					     (mingus-get-insertion-number)
					   length)))))))

;;;Searching section
(defun mingus-query ()
  "Query the mpd database for a string or regexp (in case of regexp on filename); 
Show results in dedicated *Mingus Browser* buffer for further selection."
  (interactive)
  (let* ((type)
	 (buffer (buffer-name))
	 (pos (point))
	 (query
	  (read-from-minibuffer 
	   (capitalize (format "%s: " 
			       (set 'type (completing-read "Search type: "
							   '("album" "artist" "filename" "title" "regexp on filename") nil t)))))))
    (mingus-query-do-it type query)))

(defun mingus-query-regexp ()
  "Query the filenames in the mpd database with a regular expression; 
Show results in dedicated *Mingus Browser* buffer for further selection."
  (interactive)
  (let ((buffer (buffer-name))
	(pos (point)))
    (mingus-query-do-it "regexp on filename" (read-from-minibuffer "Regexp search on filename in mpd database: "))))

(defun mingus-query-do-it (type query)
  "Perform the query provided by either mingus-query or mingus-query-regexp"
  (switch-to-buffer "*Mingus Browser*")
  (setq buffer-read-only t)
  (mingus-browse-mode)
  (let ((buffer-read-only nil)
	(prev (buffer-string)))
    (erase-buffer)
    (cond ((string-match "regexp on filename" type)
	   (insert (shell-command-to-string "mpc listall"))
	   (keep-lines query (point-min) (point-max)))
	  (t (insert
	      (shell-command-to-string (format "mpc search %s %s" type query)))))
    (mingus-browse-invisible)
    (goto-char (point-min))
    (mingus-revert-from-query)))

(defun mingus-revert-from-query ()
  "Restore previous situation when a mingus query did not return any results."
  (cond ((eobp)
	 (insert prev)
	 (switch-to-buffer buffer)
	 (goto-char pos)
	 (message "No hits!"))
	(t 
	 (setq mode-name "QUERY RESULTS")
	 (set (make-local-variable 'mingus-last-query-results) (buffer-string)))))

(defun mingus-last-query-results ()
  "Show last query results again in dedicated *Mingus Browser* buffer"
  (interactive)
  (cond ((save-window-excursion 
					 (switch-to-buffer "*Mingus Browser*") 
					 (not (boundp 'mingus-last-query-results)))
				 (message "No succesful search yet"))
				(t (switch-to-buffer "*Mingus Browser*")
					 (let ((buffer-read-only nil))
						 (erase-buffer)
						 (insert mingus-last-query-results)
						 (goto-char (point-min))))))

(defalias 'mingus-search 'mingus-query)
	
(provide 'mingus)
;;; mingus.el ends here

