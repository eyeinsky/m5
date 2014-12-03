m5
==

! Under development -- won't work until this warning is removed.  
! The details of the features explained below will probably change a bit.


m5 is a general purpose macro processor inspired by [GNU
m4](https://www.gnu.org/software/m4/manual/m4.html).


## Introduction

An example is perhaps the best introduction, consider a file 'readme.m5' to be
the following:

```
This text goes to the 'stdout' stream, directed to standard out by default.

Regular text is passed on unmodified, given that no defined macros are
mentioned.

hey name = Hey, name!

By way of the '=' character, the previous line defines a macro 'hey' which takes one
argument. 'hey you' is expanded to "Hey, you!", unless escaped by TODO.

=> otherStream

With '=>' another stream ('otherStream') is introduced, and lines following the
declaration are now sent to there -- including the empty line preceeding this
paragraph.

All such defined streams must be mapped on the commandline when invoking the
processor.

The newline of the last line of a output stream's definition is ignored -- such
as the newline that follows the empty line immediately below. Thus, the 'dnl'
trick as in m4 is not required in macro definitions.

= blockMacro a b c

This is a block macro definition. Until the next stream- or macro-block, everything
here will be in the current macro's body. 

The macro takes three arguments 'a', 'b' and 'c', which are expanded to their
values when the macro is called.

The macro is defined after its definition has ended, i.e blockMacro one two three
is not expanded within its own body.

=> stdout

'stdout' stream continues and the current text is added to it. If 'blockMacro'
without the quotes were mentioned, it would be replaced by its contents. 

An important detail in expansion is that, as the current idea goes, arguments
are only taken from only the same line. Given 'blockMacro x y\n', 'a' would
equal 'x', 'b' 'y', but 'c' would be replaced by an empty string in the
expansion.

```

TODO: write about escaping, command line usage, and also of how tokenization and
hence, expanstion, works, etc.
