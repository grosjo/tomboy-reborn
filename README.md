Tomboy Reborn
=============

What is this?
-------------

This project intends to provide a "drop-in", straightforward replacement of deprecated , but extremely usefull Gnome Tomboy

This effort came after reviewing existing efforts (tomboy-ng, QOwnNotes, ..), but all lacking teh basic features of legacy Tomboy : Simplicity, security and network synchornisation.

This is a fork from tomboy-ng, after obvious mis-alignement of users need and software objectives


Prerequisites
-------------

Tomboy Reborn is built on Linux, Windows or Mac using

    Free Pascal Compiler - https://freepascal.org/ - version 3.0.4 or later.
    Lazarus - https://www.lazarus-ide.org/ - version 2.0.6 (earlier versions may have some specific bugs, can be patched but easier to use 2.0.6). If you use Lazarus Fixes no patches are required.
    kcontrols - https://bitbucket.org/tomkrysl/kcontrols - apply the kmemo_paste_text patch in tomboy-ng's patches directory. Failure to apply will mess with copying and pasting formatted text.


Compiling
---------
From Lazarus, built the "project"


Usage
-----
You will need to install the "Grauphel" plugin in your ownCloud/NextCloud instance


Status
------
- NextCloud sync working. Need further testing
- Daily usage working


Debugging/Support
-----------------

Please submit requests/bugs via the [GitHub issue tracker](https://github.com/grosjo/tomboy-reborn/issues).



Thanks
------

Thank you to @davidbannon for letting me dig into its own software to built this one
Thank you to @cweiske for his extremely usefull piece of software on NextCloud side
