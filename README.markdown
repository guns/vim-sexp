<pre>
             o8o
             '"'
 oooo    ooooooo ooo. .oo.  .oo.        .oooo.o  .ooooo. oooo    ooooo.ooooo.
  `88.  .8' `888 `888P"Y88bP"Y88b      d88(  "8 d88' `88b `88b..8P'  888' `88b
   `88..8'   888  888   888   888 8888 `"Y88b.  888ooo888   Y888'    888   888
    `888'    888  888   888   888      o.  )88b 888    .o .o8"'88b   888   888
     `8'    o888oo888o o888o o888o     8""888P' `Y8bod8P'o88'   888o 888bod8P'
                                                                     888
                                                                    o888o
</pre>

## Introduction

Vim-sexp brings the Vim philosophy of _precision editing_ to S-expressions.

## Requirements

* Vim 7.3+

* [vim-repeat][] (optional)

  Enables use of the `.` command for repeating change operations in vim-sexp,
  as well as repeating builtin operations with vim-sexp's text objects.

## Definitions

A COMPOUND FORM is a region of text delimited by a pair of `(` and `)`,
`[` and `]`, or `{` and `}`.

A STRING is a contiguous region of text whose syntax name matches the Vim
pattern `\vstring|regex|pattern`.

A COMMENT is a region of text whose syntax name matches the Vim pattern
`comment`. Line comments that are indented but on successive lines are
considered to be a single comment.

MACRO CHARACTERS are the special set of leading characters expanded by a Lisp
reader at read-time. These differ by language, and default to Scheme's macro
character set in an unknown FileType.

An ELEMENT is the

* current STRING if the cursor is in a STRING
* current COMMENT if the cursor is in a COMMENT
* current COMPOUND FORM if the cursor is on a paired structural bracket
* current sequence of MACRO CHARACTERS and the following ELEMENT if the cursor is on a MACRO CHARACTER
* current contiguous sequence of non-whitespace, non-bracket characters otherwise

An ELEMENT always includes any leading MACRO CHARACTERS.

## Mappings

All default mappings can be changed via the `g:sexp_mappings` variable. Every
mapping is available as a `<Plug>` mapping, which may be used for any purpose.

Users who desire more explicit, opt-in configuration should refer to
`:help sexp-explicit-mappings`.

Comprehensive documentation is available at `:help vim-sexp`. The following is
a brief summary.

### Text Object Selections (visual, operator-pending)

Text object selections refer to text _around_ the cursor.

* The `af` and `if` objects select COMPOUND FORMS.
* The `aF` and `iF` objects select top-level COMPOUND FORMS.
* The `as` and `is` objects select STRINGS.
* The `ae` and `ie` objects select ELEMENTS.

### Text Object Motions (normal, visual, operator-pending)

Text object motions move the cursor in normal mode, extend selections in
visual mode, and refer to text defined by the movement of the cursor in
operator-pending mode.

* The `(` and `)` motions move the cursor to the nearest paired structural bracket.
* The `<M-b>` and `<M-w>` motions move the cursor ELEMENT-wise, ending on an element head. Analogous to builtin `b` and `w` motions.
* The `g<M-e>` and `<M-e>` motions move the cursor ELEMENT-wise, ending on an element tail. Analogous to builtin `ge` and `e` motions.
* The `[[` and `]]` motions move the cursor to an adjacent top-level ELEMENT.
* The `[e` and `]e` mappings select an adjacent ELEMENT.

### Indent Commands (normal)

* `==` indents the current COMPOUND FORM without moving the cursor
* `=-` indents the current top-level COMPOUND FORM without moving the cursor

### Wrap Commands (normal, visual)

Wrap commands wrap the current COMPOUND FORM, ELEMENT, or visual selection and
place the cursor at the head or tail of the newly created COMPOUND FORM.

If `g:sexp_insert_after_wrap` is set (true by default), insert mode is entered
after wrapping.

* `<LocalLeader>i` and `<LocalLeader>I` wrap the current COMPOUND FORM with `(` and `)`.
* `<LocalLeader>[` and `<LocalLeader>]` wrap the current COMPOUND FORM with `[` and `]`.
* `<LocalLeader>{` and `<LocalLeader>}` wrap the current COMPOUND FORM with `{` and `}`.
* `<LocalLeader>W` and `<LocalLeader>w` wrap the current ELEMENT with `(` and `)`.
* `<LocalLeader>e[` and `<LocalLeader>e]` wrap the current ELEMENT with `[` and `]`.
* `<LocalLeader>e{` and `<LocalLeader>e}` wrap the current ELEMENT with `{` and `}`.

### List Manipulation (normal, visual)

List manipulation commands change the structure of COMPOUND FORMS. If these
commands are called from visual mode, the selection is used in place of the
current COMPOUND FORM or ELEMENT.

* `<LocalLeader>@` splices the current COMPOUND FORM into its parent.
* `<LocalLeader>o` raises the current COMPOUND FORM to replace the enclosing COMPOUND FORM.
* `<LocalLeader>O` raises the current ELEMENT to replace the enclosing COMPOUND FORM.
* `<M-k>` and `<M-j>` swap the position of the current COMPOUND FORM with a sibling ELEMENT.
* `<M-h>` and `<M-l>` swap the position of the current ELEMENT with a sibling ELEMENT.
* `<M-S-j>` and `<M-S-k>` emit the terminal ELEMENTS of the current COMPOUND FORM.
* `<M-S-h>` and `<M-S-l>` capture adjacent ELEMENTS into the current COMPOUND FORM.

The last two commands are also known as `barfage` and `slurpage` in [paredit.el][].

### Cursor Insertion (normal)

* `<LocalLeader>h` inserts the cursor at the head of the current COMPOUND FORM
* `<LocalLeader>l` inserts the cursor at the tail of the current COMPOUND FORM

If inserting at the head, a space is conditionally appended after the opening
bracket so that any typed characters will be separated from the next element.

### Insert Mode Mappings (insert)

Vim-sexp does intelligent bracket and double quote insertion like
[paredit.el][]. Unlike ParEdit, deletion of brackets that would cause an
imbalance is not prevented, except in the limited case of `<BS>` below.

* `(`, `[`, and `{` produce a closing bracket and also insert spaces as
  necessary to separate the new COMPOUND FORM from adjacent non-compound
  ELEMENTS.
* `)`, `]`, and `}` insert closing brackets when the corresponding opening
  bracket is unpaired. If the opening bracket is paired, jump to the next
  paired instance of the closing bracket.
* `"` inserts a pair of double quotes, unless the cursor is currently in
  a STRING.
* `<BS>` deletes an adjacent pair of `()`, `[]`, `{}`, and `""` when deleting
  the opening bracket or quote. Normal backspace otherwise.

These insert mode mappings can be disabled with:

```vim
let g:sexp_enable_insert_mode_mappings = 0
```

[vim-repeat]: https://github.com/tpope/vim-repeat
[paredit.el]: http://www.emacswiki.org/emacs/ParEdit
