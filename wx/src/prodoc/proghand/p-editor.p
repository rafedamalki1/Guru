/* p-editor.p */

/* This procedure provides the user with a simple editor. */
DEFINE VAR ccol      AS INTEGER LABEL "Column: " NO-UNDO.
DEFINE VAR crow      AS INTEGER LABEL "Row: " NO-UNDO.
DEFINE VAR cpstr     AS CHARACTER NO-UNDO.
DEFINE VAR i	     AS CHARACTER VIEW-AS EDITOR
	SIZE 71 BY 15 SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL NO-UNDO.
DEFINE VAR pos1      AS INTEGER LABEL "Start: " NO-UNDO.
DEFINE VAR pos2      AS INTEGER LABEL "End: " NO-UNDO.
DEFINE VAR restr     AS CHARACTER FORMAT "x(20)" 
	LABEL "Replacement: " NO-UNDO.
DEFINE VAR statlog   AS LOGICAL NO-UNDO.
DEFINE VAR title-str AS CHARACTER.
DEFINE VAR tmp	     AS CHAR FORMAT "x(20)" LABEL "File name: " NO-UNDO.
DEFINE VAR tstr      AS CHARACTER FORMAT "x(20)" 
	LABEL "String: " NO-UNDO.
DEFINE VAR tstr2     AS CHARACTER FORMAT "x(20)" 
	LABEL "Replacement: " NO-UNDO.
FORM i WITH FRAME edit-frame.
FORM
    tmp
    WITH FRAME file-frame OVERLAY ROW 6 COLUMN 26
	TITLE title-str VIEW-AS DIALOG-BOX.
FORM
    tstr
    WITH FRAME search-frame OVERLAY ROW 6 COLUMN 26
  TITLE "Search" VIEW-AS DIALOG-BOX.
FORM
    tstr tstr2
    WITH FRAME rep-frame OVERLAY ROW 6 COLUMN 20 
	TITLE "Replace" VIEW-AS DIALOG-BOX.
FORM
    restr
    WITH FRAME res-frame OVERLAY ROW 6 COLUMN 26 
	TITLE "Replace" VIEW-AS DIALOG-BOX.

FORM
    pos1 pos2
    WITH FRAME pos-frame OVERLAY ROW 6 COLUMN 26 
	TITLE "Offset" VIEW-AS DIALOG-BOX.

FORM
    crow ccol
    WITH FRAME cur-frame OVERLAY ROW 6 COLUMN 26
	TITLE "Position Cursor" VIEW-AS DIALOG-BOX.

/* Disable GO and F4 keys within editor. */

ON GO, END-ERROR OF i IN FRAME edit-frame
    RETURN NO-APPLY.
/* Define pulldown menus. */

DEFINE SUB-MENU filmenu
    MENU-ITEM fnew LABEL "New"
    MENU-ITEM fopen LABEL "Open . . ."
    MENU-ITEM finsert LABEL "Insert . . ."
    MENU-ITEM fsave LABEL "Save . . .".

DEFINE SUB-MENU editmenu
    MENU-ITEM esearch LABEL "Search . . ."
    MENU-ITEM ereplace LABEL "Search and Replace . . ."
    RULE
    MENU-ITEM esetsel LABEL "Select . . ."
    MENU-ITEM ecopy LABEL "Copy"
    MENU-ITEM epaste LABEL "Paste"
    MENU-ITEM erepsel LABEL "Replace Selection . . ."
    MENU-ITEM edesel LABEL "Deselect"
    RULE
    MENU-ITEM edeline LABEL "Delete Current Line"
    MENU-ITEM edelchar LABEL "Delete Character".

DEFINE SUB-MENU cursmenu
    MENU-ITEM cloc  LABEL "Location"
    MENU-ITEM cset  LABEL "Move . . .".

DEFINE SUB-MENU quitmenu
    MENU-ITEM qquit LABEL "Exit".

/* Define menu bar. */

DEFINE MENU mainbar MENUBAR
    SUB-MENU filmenu LABEL "File"
    SUB-MENU editmenu LABEL "Edit"
    SUB-MENU cursmenu LABEL "Cursor"
    SUB-MENU quitmenu LABEL "Quit".

/* Define actions for menu items. */

ON CHOOSE OF MENU-ITEM fnew  /* Clear editor */
    ASSIGN i:SCREEN-VALUE IN FRAME edit-frame = "".



ON CHOOSE OF MENU-ITEM fopen /* Open new file */
    DO:
	title-str = "Open file".
	UPDATE tmp WITH FRAME file-frame.
	statlog = i:READ-FILE(tmp) IN FRAME edit-frame .
	IF NOT statlog
	THEN MESSAGE "Could not open" tmp.
    END.

ON CHOOSE OF MENU-ITEM finsert	/* Insert file */
    DO:
	title-str = "Insert file".
	UPDATE tmp WITH FRAME file-frame.
	statlog = i:INSERT-FILE(tmp) IN FRAME edit-frame.
	IF NOT statlog
	THEN MESSAGE "Could not insert file" tmp.
    END.

ON CHOOSE OF MENU-ITEM fsave  /* Save to file */
    DO:
	title-str = "Save to file".
	UPDATE tmp WITH FRAME file-frame.
	statlog = i:SAVE-FILE(tmp) IN FRAME edit-frame.
	IF NOT statlog
	THEN MESSAGE "Could not save file" tmp.
    END.

ON CHOOSE OF MENU-ITEM esearch /* Search for string */
    DO:
	UPDATE tstr WITH FRAME search-frame.
	statlog = i:SEARCH(tstr, FIND-NEXT-OCCURRENCE + 
			    FIND-SELECT) IN FRAME edit-frame.
	IF NOT statlog
	THEN MESSAGE "Could not find" tstr.
    END.

ON CHOOSE OF MENU-ITEM ereplace /* Search and replace */
    DO:
	UPDATE tstr tstr2 WITH FRAME rep-frame.
	statlog = i:REPLACE(tstr, tstr2, 
		FIND-NEXT-OCCURRENCE + FIND-GLOBAL) IN FRAME edit-frame.
	IF NOT statlog
	THEN MESSAGE "Could not replace" tstr "with" tstr2.
    END.



ON CHOOSE OF MENU-ITEM esetsel /* Set selection */
    DO:
	/* If text is currently selected, then initialize
	   the positions based on the current selection. */
	ASSIGN pos1 = i:SELECTION-START IN FRAME edit-frame
	       pos2 = i:SELECTION-END IN FRAME edit-frame.

	/* If text is not currently selected, then 
	   initialize to the current cursor position. */
	IF pos2 = 0
	THEN ASSIGN pos1 = i:CURSOR-OFFSET 
			     IN FRAME edit-frame
		    pos2 = pos1.
	UPDATE pos1 pos2 WITH FRAME pos-frame.
	statlog = i:SET-SELECTION(pos1, pos2)
		IN FRAME edit-frame.
	IF NOT statlog
	THEN MESSAGE "Could not set selection from" pos1 "to" pos2.
    END.

ON CHOOSE OF MENU-ITEM ecopy /* Copy selected text */
    DO:
	cpstr = i:SELECTION-TEXT IN FRAME edit-frame.
    END.

ON CHOOSE OF MENU-ITEM epaste /* Paste text */
    DO :
	statlog = i:INSERT-STRING(cpstr) 
		      IN FRAME edit-frame.
	IF NOT statlog
	THEN MESSAGE "Paste failed for string" cpstr.
    END.

ON CHOOSE OF MENU-ITEM erepsel /* Replace selected text */
    DO :
	UPDATE restr WITH FRAME res-frame.
	statlog = i:REPLACE-SELECTION-TEXT(restr) IN FRAME edit-frame.
    END.

ON CHOOSE OF MENU-ITEM edesel /* Deselect */
    DO:
	statlog = i:CLEAR-SELECTION() IN FRAME edit-frame.
	IF NOT statlog
	THEN MESSAGE "Could not deselect.".
    END.


ON CHOOSE OF MENU-ITEM edeline /* Delete line */
    DO:
	statlog = i:DELETE-LINE() IN FRAME edit-frame.
	IF NOT statlog
	THEN MESSAGE "Could not delete current line.".
    END.
ON CHOOSE OF MENU-ITEM edelchar /* Delete character */
    DO:
	statlog = i:DELETE-CHAR() IN FRAME edit-frame.
	IF NOT statlog
	THEN MESSAGE "Could not delete character.".
    END.

ON CHOOSE OF MENU-ITEM cloc /* Display cursor location */
    DO:
	MESSAGE "Line: " 
		i:CURSOR-LINE IN FRAME edit-frame + 1
	     "Column: " 
		i:CURSOR-CHAR IN FRAME edit-frame + 1.
    END.

ON CHOOSE OF MENU-ITEM cset /* Set cursor location */
    DO:
	UPDATE crow ccol WITH FRAME cur-frame.
	ASSIGN i:CURSOR-OFFSET IN FRAME edit-frame =
		i:CONVERT-TO-OFFSET(crow - 1, ccol - 1).
    END.

ON CHOOSE OF MENU-ITEM qquit /* Exit */
    DO:
	HIDE ALL NO-PAUSE.
	STOP.
    END.

/* Set up the menu bar for the default window. */

ASSIGN DEFAULT-WINDOW:MENUBAR = MENU mainbar:HANDLE.

/* Allow user to edit text (and select from
   the menu bar at any time along the way).	*/

UPDATE i NO-LABEL WITH FRAME edit-frame.
