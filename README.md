Loker
=====

Loker is:

* *(in progress)* library for parsing (POSIX) UNIX Shell scripts
* *(planned)* program for finding errors and bugs in Shell scripts through static analysis
* *(planned)* interpreter and compiler for Shell scripts

Note that Loker is not supposed to be used as interactive shell.

Loker is free software, distributed under the MIT license. See LICENSE file for
details.

Adherence to The Standard
-------------------------
*Shell Command Language* is one of those few programming languages that have
[specification, or The Standard][standard].

Today many shell interpreters add ad-hoc, informally specified, incompatible
extensions to the language. While this may be convenient in interactive shells,
we believe that shell scripts should be portable between different
implementations and thus adhere to The Standard.

To aid developers in writing portable shell scripts, Loker strictly checks a
program against The Standard. Loker does not support any language features that
are not required by The Standard.

Why the name?
-------------
The project is named after Eli Loker, a character of the *Lie to Me* series who
is committed to always telling the truth. Likewise, if your "shell script" does
not really conform to The Standard, Loker is not going to lie to you about that.

Hacking
-------
Loker is in the early stage of development and your help is appreciated. If you
have any questions, suggestions or patches, please send them to our [mailing
list][ml]. (More preferred way of submitting patches, though, is via forking the
repository on github or otherwise sharing your git repository.)

See also
--------

* [GitHub repository][repo]
* [Mailing list][ml]
* [Issues][issues]
* [Ohloh page][ohloh]
* [The Standard][standard]

[repo]: http://github.com/feuerbach/loker
[ml]: https://groups.google.com/group/loker-sh
[issues]: http://github.com/feuerbach/loker/issues
[ohloh]: https://www.ohloh.net/p/loker
[standard]: http://www.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html
