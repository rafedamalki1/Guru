/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/

/******************************************************************************

Procedure: _pwmbar.p

Syntax   :
    RUN adecomm/_pwmbar.p
        ( INPUT  p_Popup    /* Create Popup Menu */,
          OUTPUT p_HMenubar /* Menubar Handle */ ).

Purpose  :          
    Create a PROGRESS Procedure Windows Menubar and return its handle.
    Default is POPUP = NO, but caller can change that before realizing
    the menubar or pass p_Popup = TRUE.

Description:
       
       
Parameters:
    INPUT  p_Popup          - Logical - Create as popup = TRUE.
    OUTPUT p_hMenubar       - Handle of created PW Menubar.

Notes :

Author: John Palazzo

Date  : January, 1994

*****************************************************************************/


DEFINE INPUT  PARAMETER p_Popup    AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER p_hMenubar AS HANDLE  NO-UNDO.

/* Procedure Window application-global contstants. */
{ adecomm/_pwglob.i }

/* Define various handles.  */
DEFINE VARIABLE h_menu    AS WIDGET NO-UNDO.  /*... Menu-bar             */
DEFINE VARIABLE h_submenu AS WIDGET NO-UNDO.  /*... Sub-Menu             */
DEFINE VARIABLE h_subm    AS WIDGET NO-UNDO.  /*... Sub-Menu             */
DEFINE VARIABLE h         AS WIDGET NO-UNDO.  /*... generic handle       */

/* MAIN */
DO ON STOP UNDO, LEAVE:

/* Create a MENU-BAR */
CREATE MENU h_menu IN WIDGET-POOL {&PW_Pool}
ASSIGN
  POPUP-ONLY = p_Popup
  &IF "{&WINDOW-SYSTEM}" BEGINS "MS-WIN" &THEN
  TRIGGERS:
    ON MENU-DROP PERSISTENT RUN adecomm/_pwemdrp.p.
  END TRIGGERS
  &ENDIF
  .

ASSIGN p_hMenubar = h_menu
       h_submenu  = h_menu.
       

IF NOT p_Popup THEN
DO:
    RUN FileMenu.
    RUN EditMenu.
    RUN SearchMenu.    
    RUN CompileMenu.
    RUN HelpMenu.
END.
ELSE
DO:
    RUN CompileMenu.
    RUN InsertMenu.
    RUN CreateRule.
    RUN CutCopyPaste.
    RUN CreateRule.
    RUN KeywordHelp.
END.

PROCEDURE FileMenu.

/* Create a File sub-menu... */
CREATE SUB-MENU h_submenu IN WIDGET-POOL {&PW_Pool}
ASSIGN
  LABEL  = "&File"
  PARENT = h_menu.
/*... add menu-items.  ..*/

CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool}
ASSIGN
  LABEL  = "&New"
  ACCELERATOR = "Shift-F3"
  PARENT = h_submenu
  TRIGGERS:
    ON CHOOSE PERSISTENT RUN adecomm/_pwfile.p ("NEW").
  END TRIGGERS.

CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool}
ASSIGN
  LABEL  = "&Open..."
  ACCELERATOR = "F3"
  PARENT = h_submenu
  TRIGGERS:
    ON CHOOSE PERSISTENT RUN adecomm/_pwfile.p ("OPEN").
  END TRIGGERS.

RUN CreateRule.

CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool}
ASSIGN
  LABEL  = "New Procedure &Window"
  ACCELERATOR = "Ctrl-F3"
  PARENT = h_submenu
  TRIGGERS:
    ON CHOOSE PERSISTENT RUN adecomm/_pwfile.p ("NEW-PWIN").
  END TRIGGERS.

RUN CreateRule.

CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool}
ASSIGN
  LABEL  = "&Save"
  ACCELERATOR = "F6"
  PARENT = h_submenu
  TRIGGERS:
    ON CHOOSE PERSISTENT RUN adecomm/_pwfile.p ("SAVE").
  END TRIGGERS.

CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool}
ASSIGN
  LABEL  = "Save &As..."
  ACCELERATOR = "Shift-F6"
  PARENT = h_submenu
  TRIGGERS:
    ON CHOOSE PERSISTENT RUN adecomm/_pwfile.p ("SAVE-AS").
  END TRIGGERS.

RUN CreateRule.

CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool}
ASSIGN
  LABEL  = "&Print"
  PARENT = h_submenu
  TRIGGERS:
    ON CHOOSE PERSISTENT RUN adecomm/_pwfile.p ("PRINT").
  END TRIGGERS.

RUN CreateRule.

CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool}
ASSIGN
  LABEL  = "&Close Window"
  PARENT = h_submenu
  TRIGGERS:
    ON CHOOSE PERSISTENT RUN adecomm/_pwfile.p ("CLOSE").
  END TRIGGERS.

END PROCEDURE. /* FileMenu */


PROCEDURE EditMenu.
    /* Create an Edit sub-menu... */
    CREATE SUB-MENU h_submenu IN WIDGET-POOL {&PW_Pool}
    ASSIGN
      LABEL  = "&Edit"
      PARENT = h_menu
    &IF "{&WINDOW-SYSTEM}" BEGINS "MS-WIN" &THEN
      TRIGGERS:
        ON MENU-DROP PERSISTENT RUN adecomm/_pwemdrp.p.
      END TRIGGERS
    &ENDIF
      .
    
    RUN CutCopyPaste.
    RUN CreateRule.
    RUN InsertMenu.
END PROCEDURE. /* EditMenu */

PROCEDURE CutCopyPaste.
  /*... add menu-items.  ..*/
  IF NOT p_Popup THEN
  DO:
    CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool}
    ASSIGN
      LABEL       = "&Undo"
      ACCELERATOR = IF p_Popup THEN "" ELSE "Ctrl-Z"
      PARENT      = h_submenu
      TRIGGERS:
        ON CHOOSE PERSISTENT RUN adecomm/_pwedit.p ("UNDO").
      END TRIGGERS.
    RUN CreateRule.
  END.

  CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool}
  ASSIGN
    LABEL       = "Cu&t"
    ACCELERATOR = IF p_Popup THEN "" ELSE "Ctrl-X"
    PARENT      = h_submenu
    TRIGGERS:
      ON CHOOSE PERSISTENT RUN adecomm/_pwedit.p ("CUT").
    END TRIGGERS.
  CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool}
  ASSIGN
    LABEL       = "&Copy"
    ACCELERATOR = IF p_Popup THEN "" ELSE "Ctrl-C"
    PARENT      = h_submenu
    TRIGGERS:
      ON CHOOSE PERSISTENT RUN adecomm/_pwedit.p ("COPY").
    END TRIGGERS.
  CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool}
  ASSIGN
    LABEL       = "&Paste"
    ACCELERATOR = IF p_Popup THEN "" ELSE "Ctrl-V"
    PARENT      = h_submenu
    TRIGGERS:
      ON CHOOSE PERSISTENT RUN adecomm/_pwedit.p ("PASTE").
    END TRIGGERS.
END PROCEDURE.

PROCEDURE InsertMenu.  
  IF NOT p_Popup THEN
      ASSIGN h_subm = h_submenu.
  ELSE
  DO:
    /* Create an Edit->Insert sub-menu... */
    CREATE SUB-MENU h_subm IN WIDGET-POOL {&PW_Pool}
    ASSIGN
      LABEL  = "&Insert"
      PARENT = h_submenu
      .
  END.
  
  IF NOT p_Popup THEN
  DO:
      CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool}
      ASSIGN
        LABEL       = "&Insert File..."
        PARENT      = h_subm
        TRIGGERS:
          ON CHOOSE PERSISTENT RUN adecomm/_pwedit.p ("INSERT-FILE").
        END TRIGGERS.
          
      CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool}
      ASSIGN
        LABEL       = "Insert Fie&lds..."
        PARENT      = h_subm
        TRIGGERS:
          ON CHOOSE PERSISTENT RUN adecomm/_pwedit.p ("INSERT-FIELDS").
        END TRIGGERS.
  END.
  ELSE
  DO:
      CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool}
      ASSIGN
        LABEL       = "&Database Fields..."
        PARENT      = h_subm
        TRIGGERS:
          ON CHOOSE PERSISTENT RUN adecomm/_pwedit.p ("INSERT-FIELDS").
        END TRIGGERS.
    
      CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool}
      ASSIGN
        LABEL       = "&File Contents..."
        PARENT      = h_subm
        TRIGGERS:
          ON CHOOSE PERSISTENT RUN adecomm/_pwedit.p ("INSERT-FILE").
        END TRIGGERS.
  END.
  
  IF NOT p_PopUP THEN
    RUN CreateRule.
  
  CREATE SUB-MENU h_subm IN WIDGET-POOL {&PW_Pool}
  ASSIGN
    LABEL  = "&Format Selection"
    PARENT = h_submenu.
  
  CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool}
  ASSIGN
    LABEL       = "&Indent"
    PARENT      = h_subm
    TRIGGERS:
      ON CHOOSE PERSISTENT RUN adecomm/_pwedit.p ("INDENT-SELECTION").
    END TRIGGERS.
  CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool}
  ASSIGN
    LABEL       = "&Unindent"
    PARENT      = h_subm
    TRIGGERS:
      ON CHOOSE PERSISTENT RUN adecomm/_pwedit.p ("UNINDENT-SELECTION").
    END TRIGGERS.
  CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool}
  ASSIGN
    LABEL       = "&Comment"
    PARENT      = h_subm
    TRIGGERS:
      ON CHOOSE PERSISTENT RUN adecomm/_pwedit.p ("COMMENT-SELECTION").
    END TRIGGERS.
  CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool}
  ASSIGN
    LABEL       = "Unc&omment"
    PARENT      = h_subm
    TRIGGERS:
      ON CHOOSE PERSISTENT RUN adecomm/_pwedit.p ("UNCOMMENT-SELECTION").
    END TRIGGERS.

END PROCEDURE. /* InsertMenu */

  
PROCEDURE SearchMenu.
  /* Create a Search sub-menu... */
  CREATE SUB-MENU h_submenu IN WIDGET-POOL {&PW_Pool} ASSIGN
    LABEL  = "&Search"
    PARENT = h_menu.
  /*... add menu-items.  ..*/
  CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool} ASSIGN
    LABEL       = "&Find..."
    ACCELERATOR = "Ctrl-F"
    PARENT      = h_submenu
    TRIGGERS:
      ON CHOOSE PERSISTENT RUN adecomm/_pwsrch.p ("FIND").
    END TRIGGERS.
  CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool} ASSIGN
    LABEL       = "Find &Next"
    ACCELERATOR = "F9"
    PARENT      = h_submenu
    TRIGGERS:
      ON CHOOSE PERSISTENT RUN adecomm/_pwsrch.p ("FIND-NEXT").
    END TRIGGERS.
  CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool} ASSIGN
    LABEL       = "Find &Previous"
    ACCELERATOR = "Shift-F9"
    PARENT      = h_submenu
    TRIGGERS:
      ON CHOOSE PERSISTENT RUN adecomm/_pwsrch.p ("FIND-PREV").
    END TRIGGERS.
  CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool} ASSIGN
    LABEL       = "&Replace..."
    ACCELERATOR = "Ctrl-R"
    PARENT      = h_submenu
    TRIGGERS:
      ON CHOOSE PERSISTENT RUN adecomm/_pwsrch.p ("REPLACE").
    END TRIGGERS.
  CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool} ASSIGN
    SUBTYPE  = "RULE"
    PARENT   = h_submenu.
  CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool} ASSIGN
    LABEL       = "&Goto Line..."
    ACCELERATOR = "Ctrl-G"
    PARENT      = h_submenu
    TRIGGERS:
      ON CHOOSE PERSISTENT RUN adecomm/_pwsrch.p ("GOTO-LINE").
    END TRIGGERS.
  
END PROCEDURE. /* SearchMenu */

PROCEDURE CompileMenu.

  IF NOT p_Popup THEN
  DO:
    /* Create a Compile sub-menu... */
    CREATE SUB-MENU h_submenu IN WIDGET-POOL {&PW_Pool} ASSIGN
      LABEL  = "&Compile"
      PARENT = h_menu.
    /*... add menu-items.  ..*/
  END.

  CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool} ASSIGN
    LABEL       = "&Run"
    ACCELERATOR = IF p_Popup THEN "" ELSE "F2"
    PARENT      = h_submenu
    TRIGGERS:
      ON CHOOSE PERSISTENT RUN adecomm/_pwcomp.p ("RUN").
    END TRIGGERS.
    
  CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool} ASSIGN
    LABEL       = "Chec&k Syntax"
    ACCELERATOR = IF p_Popup THEN "" ELSE "Shift-F2"
    PARENT      = h_submenu
    TRIGGERS:
      ON CHOOSE PERSISTENT RUN adecomm/_pwcomp.p ("CHECK-SYNTAX").
    END TRIGGERS.
  /*
  RUN CreateRule.
  CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool} ASSIGN
    LABEL       = "Compiler &Messages"
    ACCELERATOR = "Ctrl-E"
    PARENT      = h_submenu
    TRIGGERS:
      /* ON CHOOSE PERSISTENT RUN adecomm/_pwcomp.p ("COMP-MESSAGES"). */
    END TRIGGERS.
  */  
END PROCEDURE. /* CompileMenu */

PROCEDURE HelpMenu.

/* Create a Help sub-menu... */
  CREATE SUB-MENU h_submenu IN WIDGET-POOL {&PW_Pool} ASSIGN
    LABEL  = "&Help"
    SUB-MENU-HELP = TRUE
    PARENT = h_menu.
  /*... add menu-items.  ..*/
  CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool} ASSIGN
    LABEL       = "&Help Topics"
    ACCELERATOR = ""
    PARENT      = h_submenu
    TRIGGERS:
      ON CHOOSE PERSISTENT RUN adecomm/_pwhelp.p ("TOPICS").
    END TRIGGERS.
  CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool} ASSIGN
    SUBTYPE  = "RULE"
    PARENT   = h_submenu.
  CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool} ASSIGN
    LABEL       = "M&essages..."
    ACCELERATOR = ""
    PARENT      = h_submenu
    TRIGGERS:
      ON CHOOSE PERSISTENT RUN adecomm/_pwhelp.p ("MESSAGES").
    END TRIGGERS.
  CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool} ASSIGN
    LABEL       = "&Recent Messages..."
    ACCELERATOR = ""
    PARENT      = h_submenu
    TRIGGERS:
      ON CHOOSE PERSISTENT RUN adecomm/_pwhelp.p ("RECENT-MESSAGES").
    END TRIGGERS.
END PROCEDURE. /* HelpMenu */

PROCEDURE KeywordHelp.

  /* Create Keyword Help menu-item... */
  CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool} ASSIGN
    LABEL       = "Key&word Help"
    ACCELERATOR = ""
    PARENT      = h_submenu
    TRIGGERS:
      ON CHOOSE PERSISTENT RUN adecomm/_pwhelp.p ( INPUT "KEYWORD-HELP" ).
    END TRIGGERS.
END PROCEDURE.


PROCEDURE CreateRule.
  CREATE MENU-ITEM h IN WIDGET-POOL {&PW_Pool} ASSIGN
    SUBTYPE  = "RULE"
    PARENT   = h_submenu.
END PROCEDURE.


END. /* MAIN */
