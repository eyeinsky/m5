m5
==

! Under development -- won't work until this warning is removed.

! The details of the features explained below will probably change a bit. The
readme is not fully finished yet.

m5 is a macro processor inspired by [GNU
m4](https://www.gnu.org/software/m4/manual/m4.html).

An example is perhaps the best introduction:

```
This text goes to the 'stdout' stream, directed to standard out by default.

Regular text is passed on unmodified, given that no defined macros are
mentioned.

hey name = Hey, name!

The previous defines a macro 'hey' which takes one argument. 'hey you' is
expanded to "hey you", unless escaped by TODO.

=> otherStream

This text is sent to the 'otherStream' (including the first empty line). All
defined streams must be mapped on the commandline when invoking the processor.

'hey you' still expands to hey you -- right here, in the definition.

The newline of the last line of a output stream's definition is ignored -- such
as the newline that follows the empty line immediately below.

= blockMacro a b c

This is a block macro definition. Until the next stream- or macro-block, everything
will go to the current macro's body. 

The macro takes three arguments 'a', 'b' and 'c', which are expanded in the body
to their values.

The macro is defined after its definition has ended, i.e blockMacro one two
three is not expanded within its own body.  This behavior vmight change when a
case expression is added.

=> stdout

'stdout' stream continues -- current text is added to the stream. If 'blockMacro'
without the quotes were mentioned, it would be replaced by its contents. 

An important detail in expansion is that, as the current idea goes, arguments
are only taken from only the same line. Given 'blockMacro x y\n', 'a' would
equal 'x', 'b' 'y', but 'c' would be replaced by an empty string. 
```

TODO: write about escaping, command line usage, and also of how tokenization and
hence, expanstion, works, etc.
