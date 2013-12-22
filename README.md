soundcloud.el
=============

SoundCloud client in yo' Emacs. Available now on [MELPA](https://github.com/milkypostman/melpa) as `soundcloud`.

<img src="http://i.imgur.com/ZyTcbzD.png" height="600">

## Installation

#### mplayer

    brew install mplayer  # for os x
    sudo apt-get install mplayer  # for ubuntu

#### emacs

`soundcloud` is available on [MELPA](https://github.com/milkypostman/melpa).

    M-x package-install <RET> soundcloud <RET>
    
#### emms config

If you've never used emms before, you'll need to add these lines to your `init.el`

    (require 'emms-setup)
    (emms-standard)
    (emms-default-players)

## Getting Started

    M-x soundcloud

    a: go to an artist using their permalink name
    s: search for an artist by name
    p: play/pause
    f: next song
    b: previous song
    q: quit

## Features

 * Play tracks of a specific artist
 * Search for artists by name
 * Compatible out of the box with most EMMS commands, e.g. emms-seek-to

#### Author

 * [Travis Thieman](https://twitter.com/thieman)
