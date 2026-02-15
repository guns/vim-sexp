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

* Vim 8.0+

* [vim-repeat][] (optional)

  Enables use of the `.` command for repeating change operations in vim-sexp,
  as well as repeating builtin operations with vim-sexp's text objects.

## Treesitter Support

Although Treesitter is not a requirement, vim-sexp will use it to achieve significant performance gains if it's available.

:help sexp-treesitter-support

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
* The `ac` and `ic` objects select Nth CHILD ELEMENT from **start** of current COMPOUND FORM.
* The `aC` and `iC` objects select Nth CHILD ELEMENT from **end** of current COMPOUND FORM.

### Text Object Motions (normal, visual, operator-pending)

Text object motions move the cursor in normal mode, extend selections in
visual mode, and refer to text defined by the movement of the cursor in
operator-pending mode.

* The `(` and `)` motions move the cursor to the nearest paired structural bracket.
* The `<M-b>` and `<M-w>` motions move the cursor ELEMENT-wise, ending on an element head. Analogous to builtin `b` and `w` motions.
* The `g<M-e>` and `<M-e>` motions move the cursor ELEMENT-wise, ending on an element tail. Analogous to builtin `ge` and `e` motions.
* The `[[` and `]]` motions move the cursor to an adjacent top-level ELEMENT.
* The `[e` and `]e` mappings select an adjacent ELEMENT.

### Flow Motions (normal, visual)

Flow motions move the cursor in normal mode and move (not extend) the selection in visual mode. Unlike text object motions, flow motions are completely unconstrained by list structure, permitting the cursor to move freely in and out of compound forms.

Note: Since the application of delete operators across list boundaries could destroy structural integrity, flow-motions are not provided in operator-pending mode.

There are 2 types of flow motions:
1. "list" motions land only on brackets
1. "leaf" motions land only on non-list elements (e.g., atoms, strings and comments).

#### List flow commands
* The `<M-]>` motion moves the cursor forward by open brackets
* The `<M-[>` motion moves the cursor backward by close brackets
* The `<M-}>` motion moves the cursor forward by close brackets
* The `<M-{>` motion moves the cursor backward by open brackets

Hint: Square bracket commands tend to move down and into lists, curly braces up and out. If you picture a top-level form as a tree, `<M-]>`/`<M-[>` perform forwards/backwards depth-first recursive descents, and `<M-{>`/`<M-}>` may be used to "rewind" the descent.

#### Leaf flow commands
* The `<M-S-b>` motion moves the cursor backward to leaf head
* The `<M-S-w>` motion moves the cursor forward to leaf head
* The `<M-S-g>` motion moves the cursor backward to leaf tail
* The `<M-S-e>` motion moves the cursor forward to leaf tail

### Indent Commands (normal, visual)

* `==` indents the current COMPOUND FORM or visual selection without moving the cursor
* `=-` indents the current top-level COMPOUND FORM without moving the cursor (normal mode only)
* `<M-=>` indents and removes extra whitespace from the current COMPOUND FORM or visual selection without moving the cursor
* `<M-->` indents and removes extra whitespace from the current top-level COMPOUND FORM without moving the cursor (normal mode only)

If `g:sexp_indent_does_clean` is set (false by default), the `==` and `=-` commands remove extra whitespace before performing indent. If you set this option, you may wish to unmap `<M-=>` and `<M-->` to avoid creating redundant mappings.

If `g:sexp_indent_aligns_comments` is set (false by default), the `==` and `=-` commands also trigger re-alignment of end-of-line comments, as described in the subsequent section. If you set this option, you may wish to unmap `<LocalLeader>a` and `<LocalLeader>A` to avoid creating redundant mappings.

### Comment Alignment Commands (normal, visual)

* `<LocalLeader>a` aligns end-of-line comments in the current COMPOUND FORM or visual selection without moving the cursor
* `<LocalLeader>A` aligns end-of-line comments in the current top-level COMPOUND FORM without moving the cursor (normal mode only)

**Note:** Vim-sexp uses a weighted-cost dynamic programming algorithm to perform comment alignment, based on weights and thresholds you can easily customize.

:help sexp-comment-alignment

### Smart Paste Commands (normal, visual)

A family of commands and operators for pasting or replacing from registers, optimized for use with _S-expressions_.
Unlike builtin put operators such as `p` and `P`, these commands will keep your Lisp code properly formatted and will never create unbalanced forms.
In fact, you should rarely need to make any adjustments at all after a smart put.
This is because the smart paste engine analyzes the target context to determine whether to insert a newline at each end of the put text, and performs an automatic re-indent after the put on the smallest range guaranteed to preserve correct indentation.
Once you've used the smart paste commands, you will probably not have much use for Vim's builtin put commands in your Lisp buffers, and thus, may wish to replace the default `<LocalLeader>` keybindings with more convenient bindings that override the builtins.
For an easy way to accomplish this...
```
:help sexp-regput-overriding-builtins
```

The smart-paste commands come in two varieties:
* Normal and visual mode commands that target the element or list at the cursor position.
* Normal mode _operators_ whose targets are remote S-expressions, selected with one of the following:
  * a builtin or sexp _object_
  * a builtin or sexp _motion_
  * a target specified with a _telescopic motion_ (`:help sexp-operator-telescopic-motion`)

**Note:** A few of the commands listed below are given intentionally long (3 character) default mappings because they are essentially shortcuts for something that can be accomplished with the more general put/replace operators.
Feel free to give these commands more convenient mappings if you find them useful, or unmap them altogether if you prefer the operators.

#### Put register before/after (normal)
* ["x] `<LocalLeader>p` puts `[count]` copies of register `x` after the current element.
* ["x] `<LocalLeader>P` puts `[count]` copies of register `x` before the current element.

**Note:** It is possible to configure whether these commands put _into_ or _around_ a list whose bracket is _under_ the cursor.
```
:help sexp-regput-behavior-on-bracket
```

#### Replace selection with register (visual)
* ["x] `<LocalLeader>p` replaces the visual selection with `[count]` copies of register `x`.
* ["x] `<LocalLeader>P` idem, but doesn't update the unnamed register

#### Replace current element with register (normal)
* ["x] `<LocalLeader><LocalLeader>p` replaces the current element with `[count]` copies of register `x`.
* ["x] `<LocalLeader><LocalLeader>P` idem, but doesn't update the unnamed register

**Note:** Syntactic sugar for applying the replace operator to the current element: e.g., `<LocalLeader><LocalLeader>p` = `<M-p>ie`

#### Put register into current list (normal)
* ["x] `<LocalLeader><p` puts register into current list, just before the `[count]`th child from head.
* ["x] `<LocalLeader>>p` puts register into current list, just after the `[count]`th child from tail.

**Note:** Syntactic sugar for applying the put operator to an _inner child_ object with a `[count]`: e.g., `3<LocalLeader>>p` = `<p2iC`

#### Replace operator (normal)
* ["x] `<M-p>` replaces the S-expression(s) selected by the operator's motion/object with register `x`.
* ["x] idem, but doesn't update the unnamed register

#### Put operator (normal)
* ["x] `<p` puts register `x` before the element selected by the operator's motion/object
* ["x] `>p` puts register `x` after the element selected by the operator's motion/object

#### Put/Replace Operator Examples
Although `<LocalLeader>p` and `<LocalLeader>P` will probably be your go-to commands for the most typical use cases, the put/replace _operators_ provide a powerful mechanism for putting and replacing forms _at a distance_.
The following examples illustrate just a few of the possibilities...

| Command | Result |
| ------- | ------ |
| `<M-p>af`  | Replace current list |
| `<M-P>ie`  | Replace current element without updating unnamed register (`@"`) |
| `<M-p>2ic` | Replace second child of current list |
| `<M-p>3E`  | Replace current and next two elements |
| `<piC`  | Put before final element of current list (equivalent to `2<LocalLeader>>p`) |
| `>p3E`  | Put after the second element beyond the current element |

#### "Telescopic" Mode Example
The preceding examples work with the default options.
The following example (borrowed from the help) requires the following option settings:

```
" Enable telescopic motion for motions outside current level.
let g:sexp_regput_tele_motion = 1
" Put/replace operators preserve original cursor position.
let g:sexp_regput_curpos_op == 2
```

          (foo (bar)
               (|baz))

To swap "foo" and "baz" without moving the cursor (indicated by `|`), execute the following sequence of commands...

| Command | Result |
| ------- | ------ |
| yie            | copy current element into unnamed register (`@"`)|
| <M-p>?foo<CR>  | replace target of backwards search, updating unnamed register (`@"`) |
| <M-p>ie        | replace current element with unnamed register (`@"`) |

Although use of the unnamed register to swap words is idiomatic Vim, telescopic motions streamline the pattern by obviating the need to move the cursor back and forth between the elements being swapped and by handling any required re-indentation.

```
:help sexp-operator-telescopic-motion
```

### Clone Commands (normal, visual)

* `<LocalLeader>c` inserts copy(s) of current list or visual selection before cursor without moving cursor

**Note:** Simple heuristics are used to decide whether to perform a single or multi-line clone: i.e., whether to insert a space or a newline between the cloned elements.
If you find that the default logic doesn't always do what you expect, you can add mappings for the single and multi-line command variants to your `g:sexp_mappings` override.

:help sexp-clone-logic

If `g:sexp_clone_does_indent` is set (true by default) and cloned text spans multiple lines, all elements involved in the clone (both original and copies) will be indented.

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
* `<M-?>` _convolutes_ the current COMPOUND FORM, splicing the tail of the current list into the current list's parent and moving the head of the current list to the head of a new list containing the parent of the current list.

The `Emit` and `capture` commands are known as `barfage` and `slurpage` in [paredit.el][].

### Auto-Indent

Although vim-sexp contains explicit commands for re-indenting forms, re-indenting after a command that alters form structure is a common enough use
case that vim-sexp provides an auto-indent capability.
When enabled, commands such as clone, emit/capture, raise, and splice will perform an auto-reindent of
the affected region.
As with an explicitly requested indent, auto-indent uses option settings to determine whether to perform excess whitespace cleanup and/or trailing comment alignment.

For details on enabling/disabling auto-indent, whitespace cleanup and comment alignment...

:help sexp-auto-indent

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
