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
;; Version: orange was the color of her dress, or: 0.21
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

;; make sure you have mpc installed. This is just a wrapper.

;; 1. When you install both the main mingus AND mingus-stays-home (recommended when the
;; computer running the mpd service is the same as the one from which mingus is being
;; run, see more on the reasons for installing mingus-stays-home (id3 tagging,
;; cd-burning, integration with dired and the shell etc etc in the file
;; mingus-stays-home.el)) :

;; byte-compile, IN ORDER, repeat: IN ORDER, the files mingus.el and
;; mingus-stays-home.el

;; Add the following to your .emacs:

;; (add-to-list 'load-path "/path/to/where/you/put/all/your/mingus/files/")
;; (require 'mingus-stays-home)

;; 2. Mingus only (so NO mingus-stays-home) :

;; byte-compile the file mingus.el

;; Add the following to your .emacs:

;; (add-to-list 'load-path "/path/to/where/you/put/mingus.el/")
;; (require 'mingus)

;; Design issues:
;;
;; mingus is a wrapper around the mpc command line tool, and works via
;; shell-commands. As such, it is probably slower than clients using modules or so, but
;; has the advantage of easiness of installation: all one needs is mpc (and mpd, if run
;; locally). Currently mingus uses playlist id's, instead of song id's. This makes it a
;; little more fragile for simultaneous use by different users.

;; No editing of id3 tags is provided in mingus itself. This is, first, because mpd is
;; designed to be run in a network as a server (although it can be used on a single
;; system, which, in fact, is what I do); as such, clients to mpd are unaware of mpd's
;; root dir, and possibly/probably do not have write permissions on the music
;; files. Second, I personally do not see the use of id3 tags. They are to me a clumsy
;; way to provide information better handled in the filename/filesystem, and as everyone
;; holds different thoughts on the content of those tags, e.g. what constitutes a
;; 'genre' or whether stuff should be down/upcased. Editing filenames is just way more
;; straightforward.

;; If you DO use mingus-stays-home, ID3-editing IS provided. `mingus-id3-set' tries to
;; guess the values for artist, song, track number, and album from the name encountered
;; in the playlist; I still think ID3 is stupid, but it was fun coding it....

;; The interface is roughly based on that on ncmpc, but simpler, and better :) (at the
;; expence of a bit of speed though) .  Many keybindings are alike, except for some
;; notoriously vi-style-ones.  Some significant features (main reasons to write this
;; stuff) :

;; MARKING Notice specifically the possibility to mark multiple songs in the playlist
;; for movement or deletion (by pressing the spacebar one toggles the mark at the
;; current line; if there is a region, it marks all songs in the region.) Unmarking all
;; marks happens with a single "u".

;; INSERTION POINT Another nice feature is "mingus-set-insertion-point" (Key: "i") :
;; mark a song after which you would like your next insertions to take place. Then go
;; inserting. Unset this behaviour with "u" (mingus-unset-insertion-point), and songs
;; will be added to the end of the playlist again. Notice that this can take quite some
;; time (the more songs, the longer you will have to wait, as this is done by calling
;; mpc move, which unfortunately take sonly two arguments).

;; NB: right now these two functions are mutually exclusive.

;; For those already familiar with mpd/mpc, and have set that up, you're done now.

;; If you get a message like

;; MPD_HOST and/or MPD_PORT environment variables are not set
;; message: problems getting a response from "localhost" on port 6600 : Connection refused

;; there are two options:

;; 1. you want to run locally, so run mpd
;; first. Do so from somewhere else or simply evaluate (mingus-start-daemon).
;; On some configurations of mpd this must be done as root.

;; For those unfamiliar with mpd, to set it up, put something like the following in
;; ~/.mpdconf (this is for when run a user)

;; port                "6600"
;; music_directory     "/your/music/directory"
;; playlist_directory  "~/playlists"
;; log_file            "~/.mpd.log"
;; message_file        "~/.mpd.err"
;;
;; then run mpd

;; 2. you want to connect to a remote host, but have not set the
;; environment variables MPD_HOST and/or MPD_PORT. Do so by calling
;; (mingus-set-variables-interactively) (settings lost when emacs
;; restarted) or by means of customization ((mingus-customize) or
;; (customize-group 'mingus), after which you can call
;; (mingus-set-variables) again)

;; NEW in mingus 0.21: `mingus-wake-up-call'; fixed the lisp-max-eval-depth error
;; message when leaving mingus-info on for a while; allowing spaces in minibuffer
;; operations, such as loading and saving of playlists, radio-streams and the like, but
;; most of all: inclusion of mingus-stays-home, which provides nice integration
;; features. See that file for more information. Emacs21 compatablity, except fro parts
;; of mingus-stays-home.

;; Known bugs:

;; * a file name cannot have a " (double quotes) in it. Do not know how to fix that, so
;; if anyone feels so inclined... You CAN query your database (M-x mingus-query-regexp "
;; RET) to know if you are in the possession of such files, so you can adjust their
;; names (with mingus-stays-home installed: press 0 (zero) to go to dired to do so). The
;; only way to insert such files is by inserting their parent directory.

;;; Code:

(require 'cl)
(require 'dired)

(defvar *mingus-point-of-insertion* nil "Insertion point for mingus")
(defvar *mingus-positions* nil "cursor positions to be retained in *Mingus Browser*")
(defvar *mingus-header-when-empty* "Press ? for help, 3 for Mingus Browser" "Header to show when the playlist is empty")

(defgroup mingus nil "Group customization for mingus mpd interface"
  :group 'external
  :group 'multimedia
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

(defcustom mingus-seek-amount 10
  "Default amount of seconds or percents to seek by when using `mingus-seek'."
  :group 'mingus
  :type '(integer))

(when mingus-mpd-env-set-p
  (setenv "MPD_HOST" mingus-mpd-host)
  (setenv "MPD_PORT" (number-to-string mingus-mpd-port)))

;; some emacs21 compatibility:
(if (not (fboundp 'read-number))
    (defun read-number (prompt &optional default)
      (let ((n nil))
        (when default
	  (setq prompt
		(if (string-match "\\(\\):[ \t]*\\'" prompt)
		    (replace-match (format " (default %s)" default) t t prompt 1)
		  (replace-regexp-in-string "[ \t]*\\'"
					    (format " (default %s) " default)
					    prompt t t))))
        (while
            (progn
              (let ((str (read-from-minibuffer prompt nil nil nil nil
					       (and default
						    (number-to-string default)))))
                (setq n (cond
                         ((zerop (length str)) default)
                         ((stringp str) (read str)))))
              (unless (numberp n)
                (message "Please enter a number.")
                (sit-for 1)
                t)))
        n)))

(defun mingus-set-variables-interactively ()
  "In Mingus, set environment variables for mpd connection according to input, defaulting to `mingus-mpd-host' and `mingus-mpd-port'. Do not use this for customizing these values; use `mingus-customize' instead."
  (interactive)
  (setenv "MPD_HOST" (read-string "MPD_HOST: " mingus-mpd-host))
  (setenv "MPD_PORT" (number-to-string (read-number "MPD_PORT: " mingus-mpd-port))))

(defun mingus-customize ()
  (interactive)
  (customize-group 'mingus))

(defvar mingus-version "Orange Was The Color Of Her Dress, or: 0.21")
(defvar mingus-song-extension-regexp "\.\\([Mm][Pp]3\\|[Oo][Gg][Gg]\\|[fF][lL][aA][cC]\\|[wW][aA][vV]\\)")
(defvar mingus-song-regexp (concat "^.+" mingus-song-extension-regexp))
(defvar mingus-stream-regexp "http:[^<>'\"?{}() ]+\.\\([Mm][Pp]3\\|[Oo][Gg][Gg]\\|[fF][lL][aA][cC]\\|[wW][aA][vV]\\|[0-9][0-9][0-9][0-9]\\)")

(defvar mingus-last-query-results nil
  "Holds last results of mingus-query")

(make-variable-buffer-local 'mingus-last-query-results)

(defvar mingus-help-text
  (format
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

Version: %s

REFCARD: (see further down for more elaborate instructions)

Those familiar with dired-mode should find themselves at home;
those familiar with ncmpc too, AMAP that is

MAIN CONTROLS:

mingus-help:       1
mingus-playlist:   2 
mingus-browser:    3

Global keys:

p                       mingus-toggle (toggle play/pause)
>                       mingus-next
<                       mingus-prev
g/q                     mingus-git-out
s                       mingus-stop
?,1,H                   mingus-help
+,right,*, C-<mouse-4>  mingus-vol-up
-,left,/,  C-<mouse-5>  mingus-vol-down
a                       mingus-insert
~                       mingus-add-stream
`                       mingus-add-podcast
b,S-<mouse-5>           mingus-seek-backward
f,S-<mouse-4>           mingus-seek-forward
%%                       mingus-seek-percents
$                       mingus-seek-from-start
I                       mingus-info
c                       mingus-crop
C                       mingus-clear
L                       mingus-load-all
z                       mingus-random
Z                       mingus-shuffle
r                       mingus-repeat
C-x C-s                 mingus-save-playlist
R                       mingus-remove-playlist
l                       mingus-load-playlist
o                       mingus-open-playlist
Q                       mingus-query
M-%%                     mingus-query-regexp
\\                       mingus-last-query-results
k                       forward-line -1
j                       forward-line
v                       mingus-show-version


Playlist keys:

d,C-d,
<delete>, C-w           mingus-del
D                       mingus-del-marked
O                       mingus-del-other-songs
M                       mingus-move-all
C-l                     mingus-goto-current-song
C-k                     mingus-move-up
C-j                     mingus-move-down
RET,[mouse-3]           mingus-play
SPC,m,[mouse-2]         mingus-mark
U                       mingus-unmark-all
!                       run a command on the marked songs

Browser keys:

RET, [mouse-1]          mingus-down-dir-or-play-song
:,^, [mouse-3]          mingus-dir-up
SPC  [mouse-2]          mingus-insert
P                       mingus-insert-and-play

MORE ELABORATE INSTRUCTIONS:

Requirements:

- cl-macs.el
- dired.el (included in emacs)
- mpc acces to mpd, either locally or on another server.

- Emacs22

- Issues with emacs21:

although the function line-number-at-pos is replaced with a
custom mingus-line-number-at-pos, and the call to
`while-no-input' is left out when running emacs21, and whereas
for the previously unsupported read-number, I simply replicated
its function definition from the emacs22 subr.el, annoying issues
now have crept up so that you have to call C-g whenever switching
to and fro the mingus-buffers. The rest /seems/ to work somewhat
now.

Getting started:

This help is always available with the command mingus-help, or
the keys ? or 1 from the buffers *Mingus* or *Mingus
Browser*

When mpd is already playing a playlist, the command M-x mingus
will show this playlist; when not so, load a playlist with l,
or make a new one with M-x mingus-browse (default key: 3).

Starting mpd:                    mingus-start-mpd-daemon
Providing environment variables: mingus-set-variables-interactively
                                 (see also mingus-customize)

SELECTION OF SONGS:

Browsing:  command: mingus-browse key: 3

movement and insertion:

SPACEBAR always inserts everything under point or region

P          same as SPACEBAR, and plays the inserted song(s) instantly

RET        same as SPACEBAR, exept when on a directory and mark not active, then descend into dir.

^  or :    go up a directory

Minibuffer browsing:

a          insert a file or directory through the use of the minibuffer;
           follow instructions there provided

Playlist loading:

l          load playlist

Streaming audio:

Mpd supports streaming audio.  Aside from the fact that one can
always save a link in a playlist, this provides a way to take
one's own presets with you as a client, as streaming audio does
not require storage of songs on the server. Mingus takes
advantage of this fact by providing the customizable variables
`mingus-stream-alist' and `mingus-podcast-alist': alists of
conses whose key is a pretty name, and whose value is the url of
the respective radio stream or podcast file.

~          load an audio stream, read from minibuffer, with completion from the customizable variable `mingus-stream-alist';
           defaults to link (in w3m and possibly in gnus html mail buffers) or url under point.

`          same as ~, but loads all podcasts found in a link. Completion provided by the customizable variable `mingus-podcast-alist'.
           actually ~ will load a podcast too if a podcast is under point. However, to provide two separate variables for completion,
           this option is provided separately.

Making sure an insertion is instantly played:

If any of the insertion commands is prefinged, they will play the insertion instantly after insertion.

C-u a      mingus-insert-and-play
C-u l      mingus-load-playlist-and-play
C-u ~      mingus-add-stream-and-play
C-u `      mingus-add-podcast-and-play

Querying:

Q          query the mpd database for artist, album, filename, title, or regexp on filename
           (type read from minibuffer)
M-%%        query for regexp on filename
\\          show last query results again


Results are shown in the *Mingus Browser* buffer, where all commands for browsing are available

PLAYING CONTROLS:

see the refcard, and documentation of various commands, just try
them out. They should be quite self-evident, but let me know when
they are not. Not every command is (already) mapped to a
key, so M-x mingus- TAB to your delight to find everything.

PLAYLIST EDITING:

Deletion:

on marked songs: see section `Marking'

C-d, d, C-w or DEL

delete single file, or region when there is a region;

NB: this leaves the marking of other songs intact. As such it can
be slow, esp. when the region is large; it is then highly
recommended to mark the songs first, and then issue the command
mingus-del-marked (until I rewrite this function :])

Movement:

of marked songs: see section `Marking'

of single song:

C-k                 Move song up one position
C-j                 Move song up down position

Marking:

Marking songs is useful for movement or deletion of multiple songs in or from the playlist;
first mark them, then delete or move them (to point).

m or SPACEBAR       (un)mark a song, or region, when there is a region
D (upcased)         delete marked songs (this will have the same effect as
                    mingus-del when there are no marked songs)
M                   move marked songs to point
!                   get prompted for an operation on the marked songs

Point of insertion:

with mingus-set-insertion-point you can specify where new
insertions from the insertion commands from the *Mingus Browser*
buffer or from minibuffer-insertion will take place. Otherwise
the insertions will take place at the end of the playlist.

i                   set insertion point
u                   unset insertion point (available from everywhere)
C-u i               show current insertion point and move point there

Saving your playlist:

what about C-x C-s, can you memorize that?


=================================================
AUTHOR:  Niels Giesen
CONTACT: nielsgiesen at ibbu dot nl
" mingus-version))

(defconst mingus-font-lock-keywords
  (list
   '("^\* .*" . font-lock-warning-face)))

(eval-when (load)
  (if (string-match "GNU Emacs 22" (version))
      (defconst mingus-browse-font-lock-keywords
	(list
	 '(".*" . font-lock-type-face)
	 `(,mingus-song-regexp . dired-ignored-face)))))

(defconst mingus-help-font-lock-keywords
  (list
   '("mingus[a-zA-Z 0-9-]*" . font-lock-function-name-face)
   '("^[A-Z ]+:" . font-lock-warning-face)
   '("^[A-Z][a-z ]+:" . font-lock-constant-face)
   '("=" . font-lock-variable-name-face)
   '("`.+'")))

(defconst mingus-global-map (make-keymap) "Global keymap for `mingus'")

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
(define-key mingus-global-map "a" 'mingus-insert)
(define-key mingus-global-map "P" 'mingus-insert-and-play)
(define-key mingus-global-map "~" 'mingus-add-stream)
(define-key mingus-global-map "`" 'mingus-add-podcast)
(define-key mingus-global-map "\C-x\C-s" 'mingus-save-playlist)
(define-key mingus-global-map "2" 'mingus)
(define-key mingus-global-map "3" 'mingus-browse)
(define-key mingus-global-map "w" 'mingus-wake-up-call)
(define-key mingus-global-map (if (featurep 'xemacs)[(control button5)][C-mouse-5]) 'mingus-vol-down)
(define-key mingus-global-map (if (featurep 'xemacs)[(control button4)][C-mouse-4]) 'mingus-vol-up)
(define-key mingus-global-map (if (featurep 'xemacs)[(shift button5)][S-mouse-5]) 'mingus-seek-backward)
(define-key mingus-global-map (if (featurep 'xemacs)[(shift button4)][S-mouse-4]) 'mingus-seek)

;; build the menu
(define-key mingus-global-map [menu-bar mingus]
  (cons "Mingus" (make-sparse-keymap "mingus")))

(define-key mingus-global-map [menu-bar mingus customization]
  (cons  "Customization"  (make-sparse-keymap "mingus customization")))
(define-key mingus-global-map [menu-bar mingus customization port]
  '(menu-item "MPD PORT" (lambda () (interactive) (customize-variable 'mingus-mpd-port)) :help "Port for connecting to mpd server"))
(define-key mingus-global-map [menu-bar mingus customization host]
  '(menu-item "MPD HOST" (lambda () (interactive) (customize-variable 'mingus-mpd-host)) :help "Host to connect to"))
(define-key mingus-global-map [menu-bar mingus customization seek-amount]
  '("Seek Amount" . (lambda () (interactive) (customize-variable 'mingus-seek-amount))))
(define-key mingus-global-map [menu-bar mingus customization stream-alist]
  '(menu-item "Streams"  (lambda () (interactive) (customize-variable 'mingus-stream-alist)) :help "Customize stream presets"))
(define-key mingus-global-map [menu-bar mingus customization podcast-alist]
  '(menu-item "Podcasts" (lambda () (interactive) (customize-variable 'mingus-podcast-alist)) :help "Customize podcast presets"))
(define-key mingus-global-map [menu-bar mingus customization all]
  '(menu-item "All" mingus-customize :help "customize all mingus variables"))

(define-key mingus-global-map [menu-bar mingus sep1]
  '(menu-item "--"))

(define-key mingus-global-map [menu-bar mingus query-regexp]
  '(menu-item "Query regexp"  mingus-query-regexp :help "Query the mpd database with a regexp"))
(define-key mingus-global-map [menu-bar mingus query]
  '(menu-item "Query" mingus-query :help "Query the mpd database"))
(define-key mingus-global-map [menu-bar mingus sep-above-query]
  '(menu-item "--"))

(define-key mingus-global-map [menu-bar mingus streams]
  (cons "Streams and Podcasts" (make-sparse-keymap "mingus streams")))
(define-key mingus-global-map [menu-bar mingus streams podcast-alist]
  '(menu-item "Customize Podcasts" (lambda () (interactive) (customize-variable 'mingus-podcast-alist)) :help "Customize podcast presets"))
(define-key mingus-global-map [menu-bar mingus streams stream-alist]
  '(menu-item "Customize Streams"  (lambda () (interactive) (customize-variable 'mingus-stream-alist)) :help "Customize stream presets"))
(define-key mingus-global-map [menu-bar mingus streams sep]
  '(menu-item "--"))
(define-key mingus-global-map [menu-bar mingus streams podcast-and-play]
  '("Add Podcast and Play"       . mingus-add-podcast-and-play))
(define-key mingus-global-map [menu-bar mingus streams podcast]
  '("Add Podcast"       . mingus-add-podcast))
(define-key mingus-global-map [menu-bar mingus streams stream-and-play]
  '("Add Stream and Play"  . mingus-add-stream-and-play))
(define-key mingus-global-map [menu-bar mingus streams stream]
  '("Add Stream"       . mingus-add-stream))

(define-key mingus-global-map [menu-bar mingus playlists]
  (cons "Playlist loading/saving/removing" (make-sparse-keymap "mingus playlists")))
(define-key mingus-global-map [menu-bar mingus playlists remove]
  '("Remove"       . mingus-remove-playlist))
(define-key mingus-global-map [menu-bar mingus playlists save]
  '("Save"       . mingus-save-playlist))
(define-key mingus-global-map [menu-bar mingus playlists load-and-play]
  '("Load entire database" . mingus-load-all))
(define-key mingus-global-map [menu-bar mingus playlists load-all]
  '("Load and Play" . mingus-load-playlist-and-play))
(define-key mingus-global-map [menu-bar mingus playlists load]
  '("Load" . mingus-load-playlist))

(define-key mingus-global-map [menu-bar mingus sep-above-playlists-and-streams]
  '(menu-item "--"))

(define-key mingus-global-map [menu-bar mingus repeat]
  '("Repeat (toggle)"          . mingus-repeat))
(define-key mingus-global-map [menu-bar mingus shuffle]
  '("Shuffle"          . mingus-shuffle))
(define-key mingus-global-map [menu-bar mingus random]
  '("Random (toggle)"          . mingus-random))

(define-key mingus-global-map [menu-bar mingus clear]
  '("Clear Playlist"          . mingus-clear))
(define-key mingus-global-map [menu-bar mingus crop]
  '(menu-item "Crop Playlist"  mingus-crop :help "Delete all but the playing song"))

(define-key mingus-global-map [menu-bar mingus vol-up]
  '("Vol up" . mingus-vol-up))
(define-key mingus-global-map [menu-bar mingus vol-down]
  '("Vol down" . mingus-vol-down))

(define-key mingus-global-map [menu-bar mingus seek]
  '("Seek Forward"          . mingus-seek))
(define-key mingus-global-map [menu-bar mingus seek-back]
  '("Seek Backward"          . mingus-seek-backward))

(define-key mingus-global-map [menu-bar mingus next]
  '("Next"          . mingus-next))
(define-key mingus-global-map [menu-bar mingus previous]
  '("Previous"      . mingus-prev))
(define-key mingus-global-map [menu-bar mingus stop]
  '("Stop"          . mingus-stop))
(define-key mingus-global-map [menu-bar mingus play]
  '("Play"          . mingus-play))
(define-key mingus-global-map [menu-bar mingus toggle]
  '("Toggle play/pause"  . mingus-toggle))

(defconst mingus-help-map (copy-keymap mingus-global-map)
  "Help keymap for `mingus'")
(define-key mingus-help-map " " 'scroll-up)
(define-key mingus-help-map [menu-bar mingus sep-playlist-editing]
  '(menu-item "--"))
(define-key mingus-help-map [menu-bar mingus unset]
  '("Unset Insertion Point" . mingus-unset-insertion-point))
(define-key mingus-help-map [menu-bar mingus sep3]
  '(menu-item "--"))
(define-key mingus-help-map [menu-bar mingus browser]
  '(menu-item "Browser" mingus-browse :help "go to browser"))
(define-key mingus-help-map [menu-bar mingus playlist]
  '(menu-item "Playlist" mingus :help "go to playlist"))


(defconst mingus-playlist-map (copy-keymap mingus-global-map)
  "Playlist keymap for `mingus'")

;;deletion keys
(mapcar (lambda (key)
	  (define-key mingus-playlist-map key (lambda () (interactive)
						(if (mingus-mark-active)
						    (call-interactively 'mingus-del-region)
						  (mingus-del-marked))))) '("D" "\C-w"))

(mapcar (lambda (key) (define-key mingus-playlist-map key
			'(lambda () (interactive)
			   (if (mingus-mark-active)
			       (call-interactively 'mingus-del-region)
			     (mingus-del)))))
	'("d" "\C-d"))
(define-key mingus-playlist-map "O" 'mingus-del-other-song)
;;movement keys
(define-key mingus-playlist-map "M" 'mingus-move-all)
(define-key mingus-playlist-map "\C-k" 'mingus-move-up)
(define-key mingus-playlist-map "\C-j" 'mingus-move-down)
;;marking keys
(define-key mingus-playlist-map "U" 'mingus-unmark-all)
(mapcar (lambda (key)
	  (define-key mingus-playlist-map key (lambda () (interactive)
						(if (mingus-mark-active)
						    (call-interactively 'mingus-mark-region)
						  (mingus-mark)))))
	'("m" " "))
(define-key mingus-playlist-map "i" 'mingus-set-insertion-point)
(define-key mingus-playlist-map "t" 'mingus-toggle-marked)
(define-key mingus-playlist-map "!" (lambda ()
				      (interactive)
				      (if (or mingus-marked-list)
					  (progn
					    (let ((command (read-key-sequence "! on marked songs (D: deletion M: Move here O: delete the non-marked songs)" )))
					      (cond ((string-match "d\\|D" command)
						     (mingus-del-marked))
						    ((string-match "m\\|M" command)
						     (mingus-move-all))
						    ((string-match "o\\|O" command)
						     (mingus-del-other-songs))
						    (t nil))))
					(message "No marked songs"))))
;; miscellaneous keys
(define-key mingus-playlist-map "\r" 'mingus-play)
(define-key mingus-playlist-map "\C-l" 'mingus-goto-current-song)
;; menu keys
(define-key mingus-playlist-map [menu-bar mingus sep-playlist-editing]
  '("---" . separador))
(define-key mingus-playlist-map [menu-bar mingus playlist-editing]
  (cons "Playlist Editing" (make-sparse-keymap "mingus playlist editing")))
(define-key mingus-playlist-map [menu-bar mingus playlist-editing mingus-toggle-marked]
  '("Toggle Marked Songs"          . mingus-toggle-marked))
(define-key mingus-playlist-map [menu-bar mingus playlist-editing mingus-unmark-all]
  '("Unmark All Songs"          . mingus-unmark-all))
(define-key mingus-playlist-map [menu-bar mingus playlist-editing del-other]
  '("Delete Unmarked Songs"          . mingus-del-other-songs))
(define-key mingus-playlist-map [menu-bar mingus playlist-editing del-marked]
  '("Delete Marked Songs or Song at Point"          . mingus-del-marked))
(define-key mingus-playlist-map [menu-bar mingus playlist-editing move]
  '("Move Marked Songs"          . mingus-move-all))
(define-key mingus-playlist-map [menu-bar mingus playlist-editing mark]
  '("Mark Region or (un)Mark Line"          . (lambda () (interactive) (if (mingus-mark-active)
									   (call-interactively 'mingus-mark-region)
									 (mingus-mark)))))
(define-key mingus-playlist-map [menu-bar mingus playlist-editing uns-ins-point]
  '("Unset Point of Insertion"          . mingus-unset-insertion-point))
(define-key mingus-playlist-map [menu-bar mingus playlist-editing ins-point]
  '("Set Point of Insertion"          . mingus-set-insertion-point))
(define-key mingus-playlist-map [menu-bar mingus playlist-editing del-region]
  '("Delete Region" . mingus-del-region))
(define-key mingus-playlist-map [menu-bar mingus sep3]
  '(menu-item "--"))
(define-key mingus-playlist-map [menu-bar mingus help]
  '(menu-item "Help" mingus-help :help "go to help"))
(define-key mingus-playlist-map [menu-bar mingus browser]
  '(menu-item "Browser" mingus-browse :help "go to browser"))
;; mouse keys
(define-key mingus-playlist-map (if (featurep 'xemacs) [button2] [mouse-2])
  (lambda (ev) (interactive "e") (if (mingus-mark-active)
				     (call-interactively (quote mingus-mark-region))
				   (mouse-set-point ev)
				   (mingus-mark))))
(define-key mingus-playlist-map (if (featurep 'xemacs) [button3] [mouse-3]) 'mingus-play)
					;               (define-key mingus-playlist-map (if (featurep 'xemacs) [button1] [mouse-1]) 'mingus-play)

(defconst mingus-browse-map     (copy-keymap mingus-global-map)
  "Browse keymap for `mingus'")

(define-key mingus-browse-map "\r" 'mingus-down-dir-or-play-song)
(define-key mingus-browse-map (if (featurep 'xemacs) [button1] [mouse-1]) 'mingus-down-at-mouse)
(define-key mingus-browse-map (if (featurep 'xemacs) [button2] [mouse-2]) 'mingus-insert-at-mouse)
(define-key mingus-browse-map (if (featurep 'xemacs) [button3] [mouse-3]) 'mingus-dir-up)
(mapcar (lambda (key) (define-key mingus-browse-map key 'mingus-dir-up))
	'(":" "^"))
(define-key mingus-browse-map [menu-bar mingus sep-playlist-editing]
  '("---" . separador))
(define-key mingus-browse-map [menu-bar mingus unset]
  '("Unset Insertion Point" . mingus-unset-insertion-point))
(define-key mingus-browse-map [menu-bar mingus sep3]
  '(menu-item "--"))
(define-key mingus-browse-map [menu-bar mingus help]
  '(menu-item "Help" mingus-help :help "go to help"))
(define-key mingus-browse-map [menu-bar mingus playlist]
  '(menu-item "Playlist" mingus :help "go to playlist"))
(define-key mingus-browse-map " " 'mingus-insert)


(defun mingus-line-number-at-pos (&optional pos)
  "Return (narrowed) buffer line number at position POS.
If POS is nil, use current buffer location.
This is an exact copy of line-number-at-pos for use in emacs21."
  (let ((opoint (or pos (point))) start)
    (save-excursion
      (goto-char (point-min))
      (setq start (point))
      (goto-char opoint)
      (forward-line 0)
      (1+ (count-lines start (point))))))

(defun mingus-mark-operation ()
  (interactive)
  (if (or mingus-marked-list)
      (progn
	(let ((command (read-key-sequence "! on marked songs (D: deletion M: Move here)" )))
	  (cond ((string-match "d\\|D" command)
		 (mingus-del-marked))
		((string-match "m\\|M" command)
		 (mingus-move-all))
		(t nil))))
    (message "No marked songs")))

;;;; {{mouse functions}}
(defun mingus-insert-at-mouse (ev)
  "Insert song or dir at mouse."
  (interactive "e")
  (mouse-set-point ev)
  (mingus-insert))

(defun mingus-down-at-mouse (ev)
  "Insert song or dir at mouse."
  (interactive "e")
  (mouse-set-point ev)
  (mingus-down-dir-or-play-song))

(defun mingus-show-version ()
  (interactive)
  (message "Version of mingus: %s" mingus-version))

(defun mingus-help ()
  "Help screen for `mingus'."
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

;; thanks to piyo-w3m--read-query-smart and offby1, but this one saves the former state of the minibuffer keymap
(defun completing-read-allow-spaces (prompt table &optional predicate require-match initial-input hist def inherit-input-method)
  "Use `completing-read'. But allow space input and let case be of no importance."
  (let* ((completion-ignore-case t)
	 (former-function (cdr (assoc 32 minibuffer-local-completion-map)))) ;save former function of space character
    (setcdr (assoc 32 minibuffer-local-completion-map) 'self-insert-command) ; change space character to simply insert a space
    (unwind-protect
	(completing-read prompt table predicate require-match initial-input hist def inherit-input-method)
      (setcdr (assoc 32 minibuffer-local-completion-map) former-function)))) ;change back the space character to its former value

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

(defun mingus-switch-to-playlist ()
  (switch-to-buffer "*Mingus*")
  (mingus-playlist-mode))

(defun mingus-switch-to-browser ()
  (switch-to-buffer "*Mingus Browser*")
  (mingus-browse-mode))

(defun mingus-git-out (&optional x)
  "Bury all Mingus buffers."
  (interactive)
  (while (member (buffer-name) '("*Mingus Help*" "*Mingus*" "*Mingus Browser*" "*Mingus Burns*")) ;future stuff
    (bury-buffer)))

(defun mingus-playlist-mode ()
  "Mingus playlist mode;
see function `mingus-help' for instructions."
  (font-lock-mode t)
  (use-local-map mingus-playlist-map)
  (set (make-local-variable 'font-lock-defaults) '(mingus-font-lock-keywords))
					;  (unless (boundp '*mingus-point-of-insertion*) (set (make-local-variable '*mingus-point-of-insertion*) nil))
					; fixme check if this is ok
  (setq major-mode 'mingus-playlist-mode)
  (setq mode-name "MINGUS-PLAYLIST")
  (font-lock-mode t)								;wip
  (setq buffer-read-only t)                                                     ;wip
  (run-hooks 'mingus-playlist-hook))

(defun mingus-browse-mode ()
  "Mingus major mode"
  (let ((res mingus-last-query-results))
    (kill-all-local-variables)
    (use-local-map mingus-browse-map)
    (set (make-local-variable 'font-lock-defaults) '(mingus-browse-font-lock-keywords))
    (setq major-mode 'mingus-browse-mode)
    (setq mode-name "MINGUS-BROWSE")
    (run-hooks 'mingus-browse-hook)
    (set (make-local-variable '*mingus-positions*) nil)
    (setq buffer-read-only t)
    (setq mingus-last-query-results res)))

(defvar mingus-header-height 0)
(defvar mingus-marked-list nil)
(defvar mingus-wake-up-call nil)

(defun mingus ()
  "MPD Interface by Niels Giesen, Useful and Simple, or actually just named after that great bass player"
  (interactive)
  (mingus-switch-to-playlist)
  (let ((buffer-read-only nil))
    (mingus-playlist)
    (goto-line (or (mingus-cur-song-number) 1))))

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
  "Start mpd daemon for `mingus'."
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

(defun mingus-info ()
  "Show mpd info."
  (interactive)
  (or
   (and (fboundp 'while-no-input)	
        (while-no-input
          (dotimes (cnt most-positive-fixnum) ;no recursion here anymore, so no busy cursor anymore, nor violation of max-lisp-eval-depth!
            (sit-for 1)
	    (shell-command "mpc"))))
   (shell-command "mpc")))

(mingus-make-fn mingus-stop "stop" "Tell mpd to stop playing.")
(mingus-make-fn mingus-vol-up "volume +1" "Tell mpd to increase volume." (mingus-info))
(mingus-make-fn mingus-vol-down "volume -1" "Tell mpd to decrease volume." (mingus-info))

(defmacro mingus-advice (func-name buffer-name &optional docstring)
					;fixme: should make this dependent on a keyword
  (funcall
   (lambda ()
     `(defadvice ,func-name (around mingus-around-advice activate)
        ,docstring
        (if (string= ,buffer-name (buffer-name))
            ad-do-it
          (message ,(format "Not in %s buffer" buffer-name)))))))

(defun mingus-insertion-point-set-p ()
  (save-window-excursion
    (mingus-switch-to-playlist)
    (if (boundp '*mingus-point-of-insertion*)(caar *mingus-point-of-insertion*))))

(defun mingus-get-insertion-number (&optional stringify)
  (save-window-excursion
    (mingus-switch-to-playlist)
    (if *mingus-point-of-insertion*
	(if stringify (number-to-string (caar *mingus-point-of-insertion*))
	  (caar *mingus-point-of-insertion*))
      (if stringify (number-to-string (mingus-playlist-length))
	(mingus-playlist-length)))))

(mingus-advice mingus-toggle-marked "*Mingus*")
(mingus-advice mingus-goto-current-song "*Mingus*")
(mingus-advice mingus-del-region "*Mingus*")
(mingus-advice mingus-mark-region "*Mingus*")
(mingus-advice mingus-move-down "*Mingus*")
(mingus-advice mingus-set-insertion-point "*Mingus*")
(mingus-advice mingus-move-up "*Mingus*")
(mingus-advice mingus-mark "*Mingus*")
(mingus-advice mingus-down-dir-or-play-song "*Mingus Browser*")

(mapcar 'ad-activate '(mingus-goto-current-song
                       mingus-del-region
                       mingus-down-dir-or-play-song
                       mingus-move-down
                       mingus-move-up
                       mingus-set-insertion-point))



(defmacro mingus-insertion-advice (func-name)
  "Restore `mingus-marked-list', and move inserted songs to *mingus-point-of-insertion* after insertion."
  `(defadvice ,func-name (around mingus-insertion-advice activate)
     (let ((end-of-playlist (1+ (mingus-playlist-length)))
	   (insertion-point (if (mingus-insertion-point-set-p)
				(mingus-get-insertion-number))))
       (when  ad-do-it
	 (save-window-excursion
	   (mingus-switch-to-playlist)
	   (let* ((howmanysongs (- (1+ (mingus-playlist-length)) end-of-playlist))
		  (song (if (< 1 howmanysongs) "songs" "song"))
		  (number-of-songs (1+ (- (mingus-playlist-length) end-of-playlist))))
	     ;;back out when nothing is inserted:
	     (unless (= (mingus-playlist-length) (1- end-of-playlist))
	       (message "Processing request...")
	       (if insertion-point
		   (progn
		     ;;move all just inserted songs to their destination:
		     (mingus-move
		      (do* ((num end-of-playlist (1+ num))
			    (result (list num) (push num result)))
			  ((= (car result) (mingus-playlist-length)) result))
		      (1+ insertion-point) t)
		     ;;let's handle mingus-marked-list gracefully:
		     (setq mingus-marked-list
			   (mapcar (lambda (list) (if (> list insertion-point)
						      (incf list number-of-songs) list)) mingus-marked-list))
		     ;; some informative message:
		     (message "%d %s added at %s" howmanysongs song (cadar *mingus-point-of-insertion*)))
		 (message "%d %s added at end of playlist." howmanysongs song)) (mingus))))))))

(mingus-insertion-advice mingus-add-stream)
(mingus-insertion-advice mingus-add-podcast)
(mingus-insertion-advice mingus-insert)

(defmacro mingus-and-play (func-name new-func-name)
  "Macro for constructing functions from mingus insertion commands so that the new functions (\"*-and-play\" functions)
play the first thing they have inserted after such insertion. "
  `(defun ,new-func-name ()
     (interactive)
     (let ((mingus-playing-point (1+ (mingus-get-insertion-number))))
       (,func-name)
       (mingus-play (number-to-string mingus-playing-point)))))

(mingus-and-play mingus-add-stream mingus-add-stream-and-play)
(mingus-and-play mingus-add-podcast mingus-add-podcast-and-play)
(mingus-and-play mingus-insert mingus-insert-and-play)
(mingus-and-play mingus-load-playlist mingus-load-playlist-and-play)
(mingus-and-play mingus-load-all mingus-load-all-and-play)

(defun* mingus-seek (amount &optional percentage from-start)
  "Seek song played by mpd in seconds or percentage.
Take optional AMOUNT argument, specifying movement forward or backward movement.
Defaults to variable mingus-seek-amount.
When PERCENTAGE is specified, seek to PERCENTAGE of song.
If PERCENTAGE is specified and AMOUNT is negative, seek PERCENTAGE backwards."
  (interactive "p")
  (if (string= "seek amount" (substring (shell-command-to-string
                                         (concat "mpc seek " (if (or (minusp amount) percentage from-start) "" "+")
                                                 (number-to-string (if (and (null from-start)(= 1 amount)) mingus-seek-amount amount)) (if percentage "%"))) 0 11))
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
  "Seek song played by mpd in seconds or percentage backwards."
  (interactive "p")
  (mingus-seek (- 0 (if (= 1 amount) mingus-seek-amount amount))))

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

;;;; {{xemacs compatability}}
(when (featurep 'xemacs)
  (defun mingus-line-number-at-pos ()
    (line-number)))

(if (featurep 'xemacs)
    (defun mingus-mark-active ()
      (mark))
  (defun mingus-mark-active ()
    mark-active))

(defun mingus-cur-line (&optional stringify)
  "In Mingus, return number of song under point"
  (if stringify
      (number-to-string (mingus-line-number-at-pos))
    (mingus-line-number-at-pos)))

(defun mingus-playlist ()
  "Insert current playlist into buffer \"*Mingus*\"."
  (erase-buffer)
  (if (string= "" (shell-command-to-string "mpc playlist"))
      (insert *mingus-header-when-empty*)
    (insert (substring (shell-command-to-string "mpc --format \"[%artist% [- %album% ]]&[- %title%]|[%file%]\" playlist") 0 -1))
    (while (re-search-backward "^#[0-9]+) \\(.+/\\|\\)" nil t) (replace-match ""))
    (mapcar (lambda (list)
	      (goto-line list)
	      (insert "* ")) mingus-marked-list)))

(defun mingus-unmark-all ()
  "In Mingus, unset `mingus-marked-list'."
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
    (when       (string-match "^\\[\\(paused\\] \\|playing\\]\\) #\\([0-9]+\\)" mess)
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

(defun mingus-volume ()
  "Return mpd volume as string."
  (shell-command-to-string "mpc volume"))

(defun mingus-move-one (from to)
  "Move mpd playlist id FROM to mpd playlist position TO."
  (shell-command (format "mpc move %s %s" from to)))

(defun mingus-move-up ()
  "In Mingus, move song at point up one position, visually."
  (interactive)
  (when (< 1 (mingus-line-number-at-pos))
    (let ((buffer-read-only nil))
      (cond ((and *mingus-point-of-insertion* (= (mingus-line-number-at-pos) (caar *mingus-point-of-insertion*)))
             (decf (caar *mingus-point-of-insertion*)))
            ((and *mingus-point-of-insertion* (= (mingus-line-number-at-pos) (1+ (caar *mingus-point-of-insertion*))))
             (incf (caar *mingus-point-of-insertion*))))
      (mingus-move-one (mingus-line-number-at-pos) (1- (mingus-line-number-at-pos)))
      (transpose-lines 1)
      (forward-line -2)
      (message "Moved 1 song up."))))

(defun mingus-move-down ()
  "In Mingus, move song at point down one position, visually."
  (interactive)
  (when (> (mingus-playlist-length) (mingus-line-number-at-pos))
    (let ((buffer-read-only nil))
      (cond ((and *mingus-point-of-insertion* (= (mingus-line-number-at-pos) (caar *mingus-point-of-insertion*)))
             (incf (caar *mingus-point-of-insertion*)))
            ((and *mingus-point-of-insertion* (= (mingus-line-number-at-pos) (1- (caar *mingus-point-of-insertion*))))
             (decf (caar *mingus-point-of-insertion*))))
      (mingus-move-one (mingus-line-number-at-pos)(1+ (mingus-line-number-at-pos)))
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
         (poi-itself-moved-p (if (and *mingus-point-of-insertion* (member novy list)) t)))
    (when (and *mingus-point-of-insertion* (not poi-itself-moved-p))
      (cond ((and (< poi pos)
                  (< 0 less-than-poi))
             (setq novy (- poi less-than-poi)))
            ((and (> poi pos)
                  (> more-than-poi 0))
             (setq novy (+ poi more-than-poi)))))
    (let ((count1 0)
          (newpos (- (mingus-line-number-at-pos)
                     (count-if (lambda (item) (> (mingus-line-number-at-pos) item)) list))))
      (let ((to (list pos))
            (count (list count1)))
        (mapcar #'(lambda (from)
                    (cond ((> from (car to))
			   (start-process "mpc" nil "mpc" "move" (number-to-string from)(number-to-string (car to)))
			   ;;                  (shell-command (concat "mpc move " (number-to-string from) " " (number-to-string (car to))))
			   (incf (car to)))
			  ((< from (car to))
			   (start-process "mpc" nil "mpc" "move" (number-to-string (- from (car count)))(number-to-string (1- (car to))))
			   ;;                         (shell-command (concat "mpc move " (number-to-string (- from (car count))) " " (number-to-string (1- (car to)))))
                           (incf (car count)))))
                (sort list #'<)))
      ;;wip
      (unless after-insert
        (setq mingus-marked-list nil))
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
      (let ((line (mingus-line-number-at-pos)))
        (mingus-move mingus-marked-list (1+ (mingus-line-number-at-pos)))
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

(defun mingus-toggle-marked ()
  (interactive)
  (setq mingus-marked-list (let ((result nil))
			     (dotimes (int (1+ (mingus-playlist-length)) ;; (1+ (count-lines (point-min) (point-max)))
					   result)
			       (unless (or (= 0 int) (member int mingus-marked-list))
				 (push int result)))))
  (mingus))

(mingus-make-fn mingus-del (format "del %s" (mingus-cur-line t)) "Delete song under line."
                (let ((buffer-read-only nil))
                  (mingus-reset-point-of-insertion)
                  (mingus-delete-line)
                  (mingus-remove-current-line-from-marked-list)))

(defun mingus-reset-point-of-insertion ()
  "Reset the variable `*mingus-point-of-insertion*' according to the situation where the song at point will have been deleted."
  (cond ((and *mingus-point-of-insertion* (= (mingus-get-insertion-number) (mingus-line-number-at-pos)))
	 (mingus-unset-insertion-point))
	((and *mingus-point-of-insertion* (> (mingus-get-insertion-number) (mingus-line-number-at-pos)))
	 (decf (caar *mingus-point-of-insertion*)))))

(defun mingus-remove-current-line-from-marked-list ()
  "Remove line at point from mingus-marked-list, and move other marks to their new places."
  (setq mingus-marked-list (nreverse (mapcar (lambda (list)
					     (cond ((< (mingus-cur-line) list)
						    (1- list))
						   (t list)))(remove (mingus-cur-line) mingus-marked-list)))))

(defun mingus-del-region (beg end)
  "In Mingus, delete region.
Leave `mingus-marked-list' intact."
  (interactive "r")
  ;;no need for consuming computation and bindings when whole buffer is selected
  (if (and (= beg (point-min)) (= end (point-max)))
      (mingus-clear t)
    (let* ((buffer-read-only nil)
           (beg (mingus-line-number-at-pos beg))
           (end (if (bolp) (mingus-line-number-at-pos end) (1+ (mingus-line-number-at-pos end))))
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
      (cond ((and (or *mingus-point-of-insertion*)(<= end (caar *mingus-point-of-insertion*)))
             (goto-line (- (caar *mingus-point-of-insertion*) howmanysongs))
             (mingus-set-insertion-point))
            ((and (or *mingus-point-of-insertion*)(<= beg (caar *mingus-point-of-insertion*)))
             (mingus-unset-insertion-point)))
      ;; bring point to beginning of former region
      (goto-line beg))))

(defun mingus-del-marked ()
  "Delete songs marked in *Mingus* buffer"
  (interactive)
  (let ((buffer-read-only nil)
        (cur-line (mingus-line-number-at-pos)))
    (if mingus-marked-list
        (when   (yes-or-no-p (format "Remove %d marked songs? " (length mingus-marked-list)))
          (progn
            (shell-command (let ((delete-string "mpc del"))
                             (mapcar
                              (lambda (list)
                                (setq delete-string (concat delete-string " " (number-to-string list))))
                              mingus-marked-list)
                             delete-string))
            (delete-matching-lines "^* " (point-min) (point-max))
            (cond ((and *mingus-point-of-insertion* (member (mingus-get-insertion-number) mingus-marked-list))
		   (mingus-unset-insertion-point))
		  (*mingus-point-of-insertion*
		   (goto-line
		    (- (mingus-get-insertion-number) (count-if (lambda (item) (< item (mingus-get-insertion-number))) mingus-marked-list)))
		   (mingus-set-insertion-point)))
            (goto-line (- cur-line (count-if (lambda (item) (> cur-line item)) mingus-marked-list)))
            (setq mingus-marked-list nil)))
      (progn
	(shell-command (concat "mpc del " (mingus-cur-line t)))
	(mingus-delete-line)            ;this will have an effect on
					;mingus-move: either make actions
					;mutually exclusive, or take
					;these effects into account
	(setq mingus-marked-list (nreverse (mapcar (lambda (list)
						     (cond ((< (mingus-line-number-at-pos) list)
							    (1- list))
							   (t list)))(remove (mingus-line-number-at-pos) mingus-marked-list)))))))
  (when (eobp)
    (delete-region (point-at-bol) (point-at-eol 0))
    (beginning-of-line)))



(defun mingus-del-other-songs ()
  "In the *Mingus* buffer, delete all songs but the ones in `mingus-marked-list' playlist."
  (interactive)
  (if mingus-marked-list
      (let ((delete-string "mpc del")
            ;; calculate new position for point
            (newpos (length (member (mingus-line-number-at-pos) (setq mingus-marked-list (sort mingus-marked-list #'>)))))
            ;; calculate new position of insertion point
            (posfrom (length (member (caar *mingus-point-of-insertion*) mingus-marked-list))))
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
        (when (/= newpos (mingus-line-number-at-pos))
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

(defun mingus-load-all (&optional and-play)
  "Load all songs in mpd database into mpd playlist."
  (interactive "P")
  (if and-play (mingus-load-all-and-play)
    (when (yes-or-no-p "Load the WHOLE mpd database? " )
      (shell-command (concat "mpc clear&&mpc ls|mpc add" ) nil)
      (save-window-excursion
	(mingus)))))

(defun mingus-crop ()
  "Crop mpd playlist "
  (interactive)
  (let ((buffer-read-only nil))
    (if (string-match "You need to be playing to crop the playlist" (shell-command-to-string "mpc crop"))
	(message "You need to be playing to crop the playlist")
      (mingus-unset-insertion-point)
      (setq mingus-marked-list nil)
      (save-window-excursion
	(mingus))
      (message "Playlist cropped and *mingus-point-of-insertion* unset"))))

(defun mingus-add (string &optional mingus-url)
  "In Mingus, add a song."
  (shell-command (format "mpc add %s" (if (boundp 'mingus-url) string (shell-quote-argument string))))
  (save-window-excursion (mingus)))

(defcustom mingus-stream-alist
  '(("brazillbient lounge" . "http://freeteam.nl:9000/brasill.ogg")
    ("Radio Tzara" . "http://radio.indymedia.org:8000/tzara.ogg")
    ("Enemy Combatant Radio" . "http://radio.indymedia.org:8000/ecr.ogg")
    ("New College of California Internet Radio (american)" . "http://radio.indymedia.org:8000/newcollege.ogg")
    ("ralacoco" . "http://orelha.radiolivre.org:8000/ralacoco")
    ("radiolivre" . "http://orelha.radiolivre.org:8000/radiolivre.ogg")
    ("CRo 1 - Radiozurnal (czech)" . "http://amp1.cesnet.cz:8000/cro1-256.ogg")
    ("CRo 2 - Praha (czech)" . "http://amp1.cesnet.cz:8000/cro2-256.ogg")
    ("CRo 3 - Vltava czech)" . "http://amp1.cesnet.cz:8000/cro3-256.ogg"))
  "Alist of radio stations to be used by the function `mingus-add-stream'"
  :group 'mingus
  :type '(alist))

(defcustom mingus-podcast-alist ()
  "Alist of podcasts to be used by the function `mingus-add-podcast'"
  :group 'mingus
  :type '(alist))

(defun mingus-add-stream (&optional and-play)
  "Add a url-stream to the mpd playlist.
When point is at the beginning of a url, add that url;
In w3m, add the link under point;
Completion is provided by the entries in `mingus-stream-alist' to choose from.
With prefix argument, instantly play the insertion."
  (interactive "P")
  (if and-play (mingus-add-stream-and-play)
    (let ((url (completing-read-allow-spaces "Url: " mingus-stream-alist nil nil
				(mingus-extract-url))))
      (case (mingus-playlist-type (setq url (or (cdr (assoc url mingus-stream-alist))
						(assoc url mingus-stream-alist)
						url)))
	(m3u (mingus-add (mingus-site-to-string)))
	(pls (mingus-add-podcast))
	(t (mingus-add url t))))))

(defun mingus-extract-url ()
  "Return url at point;
If no url at point, return nil."
  (let ((url (and (not (member major-mode '(mingus-playlist-mode mingus-browse-mode)))
		  (or (plist-get (text-properties-at (point)) 'w3m-href-anchor)
		      (thing-at-point-url-at-point)))))
    (when (not (null url))
      (if (string-match "\?" url)
	  (replace-match "\\?" nil t url)
	url))))

(defun mingus-playlist-type (url)
  (cond ((string-match ".*\.[mM]3[Uu]" (or url "something absolutely different")) 'm3u)
	((string-match ".*\.\\([pP][lL][sS]\\|[aA][sS][xX]\\)" (or url "something absolutely different")) 'pls)
	(t nil)))

(defun mingus-site-to-string (&optional p)
  (let ((url (or p (mingus-extract-url))))
    (when (not (null url))
      (shell-command-to-string (format "w3m -dump %s" (or p (mingus-extract-url)))))))

(defun mingus-add-podcast (&optional and-play)
  "Add all streams in podcast at point.
Actually it tries to retrieve any stream from a given url.
The variable `mingus-podcast-alist' can be used for input selection.
With prefix argument, play directly."
  (interactive "P")
  (if and-play (mingus-add-podcast-and-play)
    (do* ((item nil (substring (match-string 0 xml) 0))
	  (res nil (if (not (member item res)) (push item res) res))
	  (xml (or (mingus-site-to-string)(mingus-site-to-string (let ((url (completing-read "Add a podcast: " mingus-podcast-alist)))
								   (or (cdr (assoc url mingus-podcast-alist))
								       (assoc url mingus-podcast-alist)
								       url)))))
	  (count 0 nil))
	((not (string-match mingus-stream-regexp xml (or count (match-end 0))))
	 (if (null res) (message "No valid podcast or empty podcast")
	   (do ((end-result (car res) (concat end-result '(? ) (cadr res)))
		(res res (cdr res)))
	       ((null res) (mingus-add end-result))
	     nil))))))

(defun mingus-browse ()
  "Switch to buffer *Mingus Browser* and start your Mingus browsing experience."
  (interactive)
  (if (bufferp (get-buffer "*Mingus Browser*"))
      (mingus-switch-to-browser)
    (progn
      (mingus-switch-to-browser)
      (let ((buffer-read-only nil))
	(erase-buffer)			;only if not yet in browsing mode
	(goto-char (point-min))
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
                                        ;       (interactive "r")
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
      (shell-command (format "mpc add %s"  (buffer-string)) t)))) ;silent adding by t

(defun mingus-down-dir-or-play-song ()
  "In *Mingus Browser* buffer, descend into dir at point, or play song at point"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (mingus-songp)
        (mingus-insert)
      (progn
        (push (mingus-line-number-at-pos) *mingus-positions*)
        (mingus-ls (concat "\""
                           (buffer-substring-no-properties
                            (point-at-bol)(point-at-eol)) "\""))))))

(defun mingus-ls (string)
  "List songs/dirs in directory STRING in dedicated *Mingus Browser* buffer."
  (mingus-switch-to-browser)
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

(defun* mingus-insert (&optional and-play)
  "In *Mingus Browser* buffer, insert anything under point or region into mpd playlist.
Anywhere else, call `mingus-add-read-input'.
With prefix argument, instantly play the insertion."
  (interactive "P")
  (if and-play (mingus-insert-and-play)
    (if (not (eq major-mode 'mingus-browse-mode))
	(mingus-add-read-input)
      (if (mingus-mark-active)
	  (mingus-add-song-at-p (mark)(point))
	(mingus-add-song-at-p)))
    (if (eq major-mode 'mingus-playlist-mode)
	(mingus)
      (unless (mingus-mark-active) (forward-line 1)))))

(defun* mingus-set-insertion-point (&optional p)
  "In Mingus, set *mingus-point-of-insertion* for new songs.
They will be added after this point.
Prefix argument shows value of *mingus-point-of-insertion*, and moves there."
  (interactive "P")
  (cond ((string= "*Mingus*" (buffer-name))
         (cond ((null p)
                (set '*mingus-point-of-insertion* (list (list (mingus-line-number-at-pos)
							      (buffer-substring-no-properties
							       (point-at-bol) (point-at-eol))))))
               (*mingus-point-of-insertion*
                (goto-line (caar *mingus-point-of-insertion*))))
         (message "*mingus-point-of-insertion* set at %s" (or (cadar *mingus-point-of-insertion*) "end of playlist (unset)")))
        (t (message "Not in \"*Mingus*\" buffer"))))
					;fixme do something with text-properties here once I find out how to...

(defun mingus-set-insertion-point-at-currently-playing-song ()
  (interactive)
  (save-window-excursion
    (mingus-switch-to-playlist)
    (mingus-goto-current-song)
    (mingus-set-insertion-point)))



(defun mingus-unset-insertion-point ()
  "Unset Mingus' *mingus-point-of-insertion*."
  (interactive)
  (save-window-excursion
    (mingus-switch-to-playlist)
    (set '*mingus-point-of-insertion* nil)
    (message "*mingus-point-of-insertion* unset")))

(defun mingus-mpc-load (playlist)
  (shell-command (concat "mpc load " playlist)))

(defun mingus-load-playlist (&optional and-play)
  "Load an mpd playlist.
Append playlist to current playlist.
With prefix argument, instantly play the insertion."
  (interactive "P")
  (if and-play (mingus-load-playlist-and-play)
    (let ((lst (shell-command-to-string "mpc lsplaylists")))
      (if (string= "" lst)
	  (message "No playlist present")
	(let* ((playlist  (completing-read-allow-spaces "Load playlist: "
					 (split-string (substring lst 0 -1) "\n+")  nil t))
	      (quoted-playlist (shell-quote-argument playlist)))
	  (if (string= "" playlist)
	      (message "No playlist selected")
	    (progn
	      (mingus-mpc-load quoted-playlist)
	      (message (format "Playlist %s loaded" playlist))
	      (mingus))))))))

(defun mingus-save-playlist ()
  "Save an mpd playlist"
  (interactive)
  (let* ((lst (shell-command-to-string "mpc lsplaylists"))
	 (playlist (completing-read-allow-spaces "Save playlist as: "
				    (if (string= "" lst)
					nil
				      (split-string (substring lst 0 -1) "\n+")) nil nil))
	 (quoted-playlist (shell-quote-argument playlist)))
    (if (string= "" playlist)
	(message "No name for playlist provided, won't save...")
      (shell-command (format "mpc rm %s" quoted-playlist))
      (shell-command (format "mpc save %s"  quoted-playlist))
      (message "Playlist saved as %s" playlist))))

(defun mingus-remove-playlist ()
  "Remove an mpd playlist"
  (interactive)
  (let ((list (shell-command-to-string "mpc lsplaylists")))
    (cond ((string= "" list)
           (message "No playlist to remove"))
          (t
           (let* ((playlist  (completing-read-allow-spaces "Remove playlist: "
							  (split-string (substring list 0 -1) "\n+") nil nil))
		 (quoted-playlist (shell-quote-argument playlist)))
             (if (string= "" playlist)
                 (message "No name for playlist provided, won't remove")
               (progn
                 (shell-command (concat "mpc rm " quoted-playlist))
                 (message "Playlist %s removed" playlist))))))))

(defun mingus-add-read-input (&optional string)
  "Add song or dir to mpd playlist using minibuffer input"
					;  (interactive)
  (let* ((string (if string string ""))
	 (res-one "")
	 (res (completing-read-allow-spaces
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
    (if (or (string-match mingus-song-regexp res)(string-match "n\\| " (read-key-sequence "Descend into dir? (\"n\" or SPACE to insert whole dir) ")))
	(shell-command-to-string (concat "mpc add " "\"" string (if (string= string "") "" "/") res "\""))
      (mingus-add-read-input (concat string (if (not (string= "" string)) "/" "") res)))))

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
                               (set 'type (completing-read-allow-spaces "Search type: "
                                                           '("album" "artist" "filename" "title" "regexp on filename") nil t)))))))
    (mingus-query-do-it type query pos buffer)))

(defun mingus-query-regexp ()
  "Query the filenames in the mpd database with a regular expression;
Show results in dedicated *Mingus Browser* buffer for further selection."
  (interactive)
  (let ((buffer (buffer-name))
        (pos (point)))
    (mingus-query-do-it "regexp on filename" (read-from-minibuffer "Regexp search on filename in mpd database: ") pos buffer)))

(defun mingus-query-do-it (type query pos buffer)
  "Perform the query provided by either `mingus-query' or `mingus-query-regexp'"
  (mingus-switch-to-browser)
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
    (mingus-revert-from-query pos prev buffer)))

(defun mingus-revert-from-query (pos prev buffer)
  "Restore previous situation when `mingus-query-do-it' did not return any results."
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
           (mingus-switch-to-browser)
           (null mingus-last-query-results))
         (message "No succesful search yet"))
        (t (switch-to-buffer "*Mingus Browser*")
           (setq mode-name "QUERY RESULTS")
           (let ((buffer-read-only nil))
             (erase-buffer)
             (insert mingus-last-query-results)
             (goto-char (point-min))))))

(defalias 'mingus-search 'mingus-query)

;;;; {{Wake up call}}
(defun mingus-date-to-sec-from-epoch (datestring)
  (apply #'encode-time
	 (mapcar #'string-to-number 
		 (list 
		  (substring datestring 12 14)	;seconds
		  (substring datestring 10 12)	;minute
		  (substring datestring 8 10)	;day
		  (substring datestring 6 8)	;month
		  (substring datestring 4 6)		
		  (substring datestring 0 4))))) 


(defun mingus-wake-up-call (&optional p)
  "Set a time for mingus to start playing. 
TIME will be interpreted to always lie in the future.
With prefix argument, cancel the wake-up call.

The timer-object is referenced to by the variable `mingus-wake-up-call'"
  (interactive "P")
  (cond ((and p (timerp mingus-wake-up-call)) 
	 (cancel-timer mingus-wake-up-call)
	 (message "Wake-up call cancelled"))
	(t
	 (setq
	  mingus-wake-up-call (run-at-time 
			       (let ((time (mingus-date-to-sec-from-epoch (concat 
									   (format-time-string "%Y%m%d")
									   (mingus-pad-with-zeros (read-from-minibuffer "Hour: ") 2)
									   (mingus-pad-with-zeros (read-from-minibuffer "Minute: ") 2) "00"))))
				 (if (time-less-p time (current-time)) (time-add (days-to-time 1) time) time)) 
			       nil 'mingus-play))
	 (message (format "%sake sure you have a playlist set before dozing off!" 
			  (if (=  0 (mingus-playlist-length)) "Playlist is empty, m" "M"))))))


(defun mingus-pad-with-zeros (el pad-width)
  "Pad EL up to PAD-WIDTH characters with zeros on the left"
  (let* ((need (- pad-width (length el))))
    (if (> need 0)
	(concat (make-string need ?0) el)
      el)))

(provide 'mingus)
;;; mingus.el ends here

