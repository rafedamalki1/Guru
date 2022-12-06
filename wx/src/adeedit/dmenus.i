/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/

/*---------------------------------------------------------------------------
  dmenus.i
  DEFINE MENUS for Editor
----------------------------------------------------------------------------*/

/* The conditional menu accelerator definitions below are used to support
   menu accelerators in both GUI and TTY environments.  The TTY accelerators
   follow the keyfunction mappings defined in adeedit/dbftrigs.i.
*/

&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN

/* File */
&SCOPED-DEFINE NEW-ACCEL        "SHIFT-F3"
&SCOPED-DEFINE OPEN-ACCEL       "F3"
&SCOPED-DEFINE CLOSE-ACCEL      "F8"
&SCOPED-DEFINE PWIN-ACCEL       "CTRL-F3"
&SCOPED-DEFINE SAVE-ACCEL       "F6"
&SCOPED-DEFINE SAVEAS-ACCEL     "SHIFT-F6"
/* Edit */
&SCOPED-DEFINE UNDO-ACCEL       "CTRL-Z"
&SCOPED-DEFINE CUT-ACCEL        "CTRL-X"
&SCOPED-DEFINE COPY-ACCEL       "CTRL-C"
&SCOPED-DEFINE PASTE-ACCEL      "CTRL-V"
/* Search */
&SCOPED-DEFINE FIND-ACCEL       "CTRL-F"
&SCOPED-DEFINE FIND-NEXT-ACCEL  "F9"
&SCOPED-DEFINE FIND-PREV-ACCEL  "SHIFT-F9"
&SCOPED-DEFINE REPLACE-ACCEL    "CTRL-R"
&SCOPED-DEFINE GOTOLINE-ACCEL   "CTRL-G"
/* Buffers */
&SCOPED-DEFINE LIST-ACCEL       "CTRL-L"
&SCOPED-DEFINE NEXT-ACCEL       "F7"
&SCOPED-DEFINE PREV-ACCEL       "SHIFT-F7"
/* Compile */
&SCOPED-DEFINE RUN-ACCEL        KBLABEL("GO")
&SCOPED-DEFINE CHECK-ACCEL      "SHIFT-F2"
&SCOPED-DEFINE DEBUG-ACCEL      "SHIFT-F4"
&SCOPED-DEFINE CMSG-ACCEL       "CTRL-E"

&ELSE

/* File */
&SCOPED-DEFINE NEW-ACCEL        KBLABEL("NEW")
&SCOPED-DEFINE OPEN-ACCEL       KBLABEL("GET")
&SCOPED-DEFINE CLOSE-ACCEL      KBLABEL("CLEAR")
&SCOPED-DEFINE SAVE-ACCEL       KBLABEL("PUT")
&SCOPED-DEFINE SAVEAS-ACCEL     KBLABEL("SAVE-AS")
/* Edit */
&SCOPED-DEFINE CUT-ACCEL        KBLABEL("CUT")
&SCOPED-DEFINE COPY-ACCEL       KBLABEL("COPY")
&SCOPED-DEFINE PASTE-ACCEL      KBLABEL("PASTE")
/* Search */
&SCOPED-DEFINE FIND-ACCEL       KBLABEL("FIND")
&SCOPED-DEFINE FIND-NEXT-ACCEL  KBLABEL("FIND-NEXT")
&SCOPED-DEFINE FIND-PREV-ACCEL  KBLABEL("FIND-PREVIOUS")
&SCOPED-DEFINE REPLACE-ACCEL    KBLABEL("REPLACE")
&SCOPED-DEFINE GOTOLINE-ACCEL   KBLABEL("GOTO")
/* Buffers */
&SCOPED-DEFINE LIST-ACCEL       KBLABEL("BREAK-LINE")
&SCOPED-DEFINE NEXT-ACCEL       KBLABEL("NEXT-FRAME")
&SCOPED-DEFINE PREV-ACCEL       KBLABEL("PREV-FRAME")
/* Compile */
&SCOPED-DEFINE RUN-ACCEL        KBLABEL("GO")
&SCOPED-DEFINE CHECK-ACCEL      KBLABEL("COMPILE")
&SCOPED-DEFINE DEBUG-ACCEL      ?
&SCOPED-DEFINE CMSG-ACCEL       ?

&ENDIF


DEFINE SUB-MENU mnu_File
  MENU-ITEM _New       LABEL "&New      " ACCELERATOR {&NEW-ACCEL}
  MENU-ITEM _Open      LABEL "&Open...  " ACCELERATOR {&OPEN-ACCEL}
  MENU-ITEM _Close     LABEL "&Close    " ACCELERATOR {&CLOSE-ACCEL}
&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
  RULE
  MENU-ITEM _New_PW    LABEL "New Procedure &Window" ACCELERATOR {&PWIN-ACCEL}
&ENDIF
  RULE
  MENU-ITEM _Save      LABEL "&Save      "       ACCELERATOR {&SAVE-ACCEL}
  MENU-ITEM _Save_as   LABEL "Save &As..." ACCELERATOR {&SAVEAS-ACCEL}
  RULE
  MENU-ITEM _Print     LABEL "&Print"
  RULE
  MENU-ITEM _Exit      LABEL "E&xit"
  .

DEFINE SUB-MENU mnu_Edit
&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
  MENU-ITEM _Undo           LABEL "&Undo"             ACCELERATOR {&UNDO-ACCEL}
  RULE
&ENDIF
  MENU-ITEM _Cut            LABEL "Cu&t             " ACCELERATOR {&CUT-ACCEL}
  MENU-ITEM _Copy           LABEL "&Copy            " ACCELERATOR {&COPY-ACCEL}
  MENU-ITEM _Paste          LABEL "&Paste           " ACCELERATOR {&PASTE-ACCEL}
  RULE
  MENU-ITEM _Insert_File    LABEL "&Insert File...  "
  MENU-ITEM _Field_Selector LABEL "Insert Fie&lds..."
  .

  DEFINE SUB-MENU mnu_Search
  MENU-ITEM _Find      LABEL "&Find...      " ACCELERATOR {&FIND-ACCEL}
  MENU-ITEM _Find_Next LABEL "Find &Next    " ACCELERATOR {&FIND-NEXT-ACCEL}
  MENU-ITEM _Find_Prev LABEL "Find &Previous" ACCELERATOR {&FIND-PREV-ACCEL}
  MENU-ITEM _Replace   LABEL "&Replace...   " ACCELERATOR {&REPLACE-ACCEL}
  RULE
  MENU-ITEM _Goto_Line LABEL "&Goto Line... " ACCELERATOR {&GOTOLINE-ACCEL}
  .

DEFINE SUB-MENU mnu_Buffer
  MENU-ITEM _BuffList    LABEL "&List...        " ACCELERATOR {&LIST-ACCEL}
  MENU-ITEM _Next        LABEL "&Next Buffer    " ACCELERATOR {&NEXT-ACCEL}
  MENU-ITEM _Prev        LABEL "&Previous Buffer" ACCELERATOR {&PREV-ACCEL}
  &IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
  MENU-ITEM _BufFont     LABEL "&Font..."
  &ENDIF
  MENU-ITEM _BufSettings LABEL "&Information..."
  .

DEFINE SUB-MENU mnu_Compile
  MENU-ITEM _Run          LABEL "&Run               " ACCELERATOR {&RUN-ACCEL}
  MENU-ITEM _Check_Syntax LABEL "&Check Syntax      " ACCELERATOR {&CHECK-ACCEL}
&IF OPSYS <> "VMS" &THEN
  MENU-ITEM _Debug 	  LABEL "&Debug             " ACCELERATOR {&DEBUG-ACCEL}
&ENDIF
  RULE
  MENU-ITEM _Comp_Msgs    LABEL "Compiler &Messages..." ACCELERATOR {&CMSG-ACCEL}
  .

/* ADE Standard Tools Menu Include. */
{ adecomm/toolmenu.i &EXCLUDE_EDIT=yes }

&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
DEFINE SUB-MENU mnu_Options
  MENU-ITEM _Editor_Opts  LABEL "&Preferences..."
  MENU-ITEM _Menu_Accels  LABEL "&Menu Accelerators..."
  MENU-ITEM _DefFont      LABEL "Default &Font..."
  RULE
  MENU-ITEM _Save_Settings_Exit LABEL "&Save Settings on Exit" TOGGLE-BOX
  .
&ENDIF

DEFINE SUB-MENU mnu_Help SUB-MENU-HELP
&IF ( "{&WINDOW-SYSTEM}" <> "TTY" ) &THEN
    MENU-ITEM _Help_Topics   LABEL "&Help Topics"
    RULE
&ENDIF
    MENU-ITEM _Menu_Messages LABEL "M&essages..."
    MENU-ITEM _Menu_Recent   LABEL "&Recent Messages..."
&IF ( "{&WINDOW-SYSTEM}" = "TTY" ) &THEN
    MENU-ITEM _Keyboard      LABEL "&Keyboard..."
&ENDIF
    RULE
    MENU-ITEM _About         LABEL "&About Procedure Editor"
    .

DEFINE MENU mnb_ProEdit
  MENUBAR
  SUB-MENU mnu_File    LABEL "&File"
  SUB-MENU mnu_Edit    LABEL "&Edit"
  SUB-MENU mnu_Search  LABEL "&Search"
  SUB-MENU mnu_Buffer  LABEL "&Buffer"
  SUB-MENU mnu_Compile LABEL "&Compile"
  SUB-MENU mnu_Tools   LABEL "&Tools"
&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
  SUB-MENU mnu_Options LABEL "&Options"
&ENDIF
  SUB-MENU mnu_Help    LABEL "&Help"
  .

