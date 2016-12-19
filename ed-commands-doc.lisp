#|

;;; **********************************************************************
;;; Copyright (c) 92, 93 Hallvard Traetteberg. All rights reserved.
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted and may be copied as long as 
;;; no fees or compensation are charged for use, copying, or accessing
;;; this software and all copies of this software include this copyright
;;; notice.  Suggestions, comments and bug reports are welcome.  Please 
;;; address email to: Hallvard.Tretteberg@si.sintef.no
;;; **********************************************************************

;;;
;;; Text Editor
;;;
;;; Look in the file site-specific.lisp to see how keys map to commands.
;;;
;;; Point is normal insertion point,
;;; Mark is mark you can set,
;;; Region is text between point and mark.
;;;
;;; Point can be set by clicking in window, drag to set mark.
;;; M-x gives access to editing commands in the minibuffer.
;;;


Refresh
  Refreshes the window.

Layout
  Lays out the text, i.e. computes line wraps and refreshes window.

Exit
Exit Ed
  Exits the editor.

;;; Point movement commands:

Previous Line, Next line
Backward Char, Forward Char
Beginning Of Line, End Of Line
Beginning Of Text, End Of Text

;;; Modification commands:

Insert <string>
  Inserts string at point.

Replace Region <string>
  Replaces region by <string>.

Transform Region <symbol>
  Makes a string from region, runs function bound to <symbol> on the string,
  and replaces region by result.

Tabulate
  Tries to make column after point line up with column in line above.
  Try it!

;;; Deletion commands:

Delete Region,
Delete Char, Delete Char Backward,
Delete To Beginning, Delete To End

;;; Mark commands:

Swap Point And Mark
  Exchanges point and mark.

Set Mark
  Sets the mark to point.

Unset Mark
  Unsets the mark.

;;;

Yank
  Takes the last deleted text and inserts it at point.

Undo
Undo Replace
Knay
  Undoes the last editing command.

;;; Searching commands:

Search Backward <string> &
Search Forward <string>
  Moves point to nearest occurence of <string>,
  looking backwards or forwards, respectively.

;;; File commands:

Find File <pathname>
  Inserts text in file denoted by <pathname> at point.

Save File <pathname>
  Saves all text in editor to <pathname>.

;;; Lisp commands:

Eval
Eval Form
  Evals the form after point.

Eval Form And Exit
  Evals form after point and exits the editor.

Identity 
Macroexpand 1
Macroexpand
  Runs corresponding lisp functions on form after point,
  printing the result in the editor.

Identity To Minibuffer
Eval To Minibuffer
Macroexpand 1 To Minibuffer
Macroexpand To Minibuffer
  Runs corresponding lisp functions on form after point,
  printing the result in the minibuffer

Eval Buffer
  Evals all the forms in the editor.

|#