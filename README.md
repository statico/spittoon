Spittoon
========

Spittoon is a comic strip generator. It is an implementation of the [Microsoft Comic Chat](http://en.wikipedia.org/wiki/Microsoft_Comic_Chat) algorithm described in the paper, [Comic Chat (Reprinted from SIGGRAPH '96 Proceedings)](http://kurlander.net/DJ/Pubs/SIGGRAPH96.pdf).

![example comic strip](http://github.com/statico/spittoon/raw/master/examples/example-output.png)

I wrote this as an experiment to learn Ruby in 2005. I had planned on making an ongoing comic strip, but it turns out that I'm not very funny. I've now released it as open-source.

Getting Started
---------------

First, install [RMagick](http://rmagick.rubyforge.org/).

Next, `cd` to `examples/` and run:

    ruby -I ../lib ../bin/make_comic.rb -c config.yaml -s spec -o example.png

Voila -- a comic has been generated.

...or not. Spittoon *may crash* on you. If it does, just re-run the generator. Spittoon picks variations at random and sometimes they simply don't work. When something doesn't fit, Spittoon bails. (Yes, I know this isn't great, but I had always planned on running this from a terminal where I could re-run the command easily.)

Chat Spec Scripts
-----------------

I called them "specs" because originally they were YAML files of "chat specifications." This became a pain in the ass so I created a simpler but more magical text format.

For example:

    a->b: hey! (happy/exclaiming)
    b: hi! (positive)
    c

This is a one-panel comic with three characters. The characters will be chosen at random (see `characters` in `config.yaml`) and assigned to `a`, `b`, and `c`. For specific characters you can use their names directly, e.g. `alice->bob`.

`a->b` means that `a` will be looking at `b`. The direction of the other characters will be determined by who is speaking. It *is* possible to cram four or more characters in a panel and have multiple groups chatting with one another.

Order matters. The characters will be placed in the order defined in the spec. This makes things a little hairy when you want to have one character monologuing, but Spittoon tries to figure it out.

`(happy/exlaiming)` picks the `happy` face and `exclaiming` pose from the artwork directory. You can specify exact faces and poses or you can specify a set which is named in `config.yaml`, such as `positive` or `negative`. You can specify just a face with `(facename)` and just the pose with `(/posename)`.

To create a new panel, simply re-use a character name. Since the same character can't appear twice in the same panel, this is the signal that a new panel is necessary.

Narration is possible, meaning you can include a rectangular chat balloon which isn't pointing toward anyone. Unfortunately, it still needs to be attached to a character:

    robin* back at the bat cave...
    batman: are those steaks ready yet?

See the spec in `examples/spec` for another example.

Customization
-------------

The basic idea of Spittoon was to write a minimal chat script (ideally being able to paste from IRC with few modifications) and have a comic be generated with random variations. It's supposed to be mostly hands-off.

There are a lot of options in `examples/config.yaml`. They're mostly self-obvious.

Panel layouts, however, are hard-coded. How many panels you get in the strip is determined by the chat script -- more panels are added when a text balloon can't fit in a single strip. A 3-panel strip is always rendered vertically.

Want better fonts? A great source for free and commercial comic fonts is [Blambot Comic Fonts](http://www.blambot.com/fonts_dialogue.shtml). A lot of comic fonts are dual-weight -- lowercase text produces normal-weight letters and uppercase text produces bold letters.

Development
-----------

I am not actively using or maintaining Spittoon, but I welcome your patches. Fork at your leisure.
