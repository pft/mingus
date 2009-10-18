Mingus is a frontend for GNU Emacs to the Music Player daemon. The
interface closely, though not strictly, resembles that of ncmpc, which
I had been using before (the main drawback of ncmpc for me was it’s
apparent lack of support for unicode). It has some advantages over
other clients, mainly in the field of playlist editing. This is
achieved through marking and regions as you are used too in emacs and
in dired. Mingus also provides a point-of-insertion for inserting new
songs. This can be handy to drop a song after the currently playing
one (enqueue), but is more flexible. Mingus is distributed under the
GPL.

w3m
===
Another plus is its integration with emacs-w3m , in the sense that,
when bumping into a sexy podcast or radio-stream while browsing in
the browser of browsers, you have the ability to add any stream at
point (and play it directly, if so desired).

Dired
=====
Jump from Mingus to song at point in Dired, and, vice versa, add songs
from Dired to the playlist.

Mingus stays home
=================
If mingus stays home, i.e. the daemon mpd is run from the same
computer as the client, mingus-stays-home.el provides some tagging
support for ogg, flac and mp3, an experimental cd-burning tool
(mingus-burns).

Prerequisites
=============
libmpdee.el by R. Ramkumar. Currently mingus runs on GNU Emacs22 and
GNU Emacs 23. If you also want to use mingus-stays-home.el, the
programs flac, metaflac, vorbiscomment, sox, id3v2 and cdrecord (or
any other command-line cd-burning tool taking files as arguments) might
be needed.

ToDo
====
mingus.el
---------
- Add text-properties (fringe-bitmap?) for point-of-insertion
- Add a search buffer In this search buffer, you would be able to
   filter songs by their attributes. It would have a tabular layout,
   and you could safe-sort the songs. Maybe as-you-type, maybe not.

mingus-stays-home.el
--------------------
- add tagging for multiple files in one go (apply one field of choice to one or more files)
   For this I am looking into [LibTag](http://developer.kde.org/~wheeler/taglib.html) . Best bet would be to write a
   command-line program in C++ (maybe even one spitting out lists :)
   ), so that I do not have to worry about people having this or that
   interpreter language that *has got* ffi's.

   So now I am learning C++...


