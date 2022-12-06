/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/

/******************************************************************************

Procedure: _pwcreat.p

Syntax   :

RUN adecomm/_pwcreat.p ( INPUT  p_Parent_ID /* Parent ID   */ ,
                         INPUT  p_Name      /* Window Name */ ,
                         INPUT  p_Title     /* Title       */ ,
                         INPUT  p_Menubar   /* Menubar     */ ,
                         OUTPUT p_Window    /* Window H    */ ).

Purpose  :          
    Create a Persistent Procedure Window with Menubar, frame, and
    editor widget, and additional Attributes used to hold persistent data.
    This also attaches default triggers for WINDOW-CLOSE, WINDOW-RESIZE,
    and HELP.  Override them if you wish in the calling procedure.

Description:

       
Parameters:

Notes :

Author: John Palazzo

Date  : February, 1994

*****************************************************************************/

DEFINE INPUT  PARAMETER p_Parent_ID AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER p_Name      AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER p_Title     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER p_Menubar   AS WIDGET    NO-UNDO.
DEFINE OUTPUT PARAMETER p_Window    AS WIDGET    NO-UNDO.

/* ADE Standards Include. */
{ adecomm/adestds.i }
IF NOT initialized_adestds
THEN RUN adecomm/_adeload.p.

/* Procedure Window application-global contstants. */
{ adecomm/_pwglob.i }

/* Procedure Window Attribute definitions and procedures. */
{ adecomm/_pwattr.i }

/* Define various handles needed to reference the various widgets.  */
DEFINE VARIABLE h_frame   AS WIDGET NO-UNDO.  /*... Frame in the  window */
DEFINE VARIABLE h_ed      AS WIDGET NO-UNDO.  /*... the editor itself    */

/** ******
 ** Step 1: Create the Window for the editor and specifiy its attributes.
 ** ******
 **/

CREATE WINDOW p_Window IN WIDGET-POOL {&PW_Pool}
ASSIGN
  PRIVATE-DATA   = IF p_Parent_ID = ? THEN "?" ELSE p_Parent_ID
  NAME           = p_Name
  TITLE          = p_Title
  MENU-BAR       = p_Menubar
  WIDTH          = FONT-TABLE:GET-TEXT-WIDTH(FILL("0", 85), editor_font)
  HEIGHT         = (SESSION:HEIGHT * 0.66)
  MAX-WIDTH      = ?
  MAX-HEIGHT     = ?
  VIRTUAL-WIDTH  = SESSION:WIDTH
  VIRTUAL-HEIGHT = SESSION:HEIGHT
  MIN-WIDTH      = 1 /* Zero is not acceptable to UIM. */
  MIN-HEIGHT     = 1 /* Zero is not acceptable to UIM. */
  RESIZE         = YES
  SCROLL-BARS    = NO
  MESSAGE-AREA   = NO
  STATUS-AREA    = NO
  SENSITIVE      = YES
  VISIBLE        = YES
  TRIGGERS:
    /* Attach default triggers. */
    ON WINDOW-CLOSE   PERSISTENT RUN adecomm/_pwclose.p.
    ON WINDOW-RESIZED PERSISTENT RUN adecomm/_pwresz.p.
    ON HELP           PERSISTENT RUN adecomm/_pwhelp.p ( INPUT "KEYWORD" ).
  END.

/** ******
 ** Step 2: Create an "Editor" Frame for the Procedure Window.
 ** ******
 **/
CREATE FRAME h_frame IN WIDGET-POOL {&PW_Pool}
ASSIGN
  X = 0
  Y = 0
  BOX        = NO
  SCROLLABLE = NO
  WIDTH      = p_Window:WIDTH 
  HEIGHT     = p_Window:HEIGHT 
  PARENT     = p_Window
  VISIBLE    = YES 
  SENSITIVE  = YES.
  
/** ******
 ** Step 3: Create an editor widget for the Frame of the Procedure Window.
 ** ******
 **/
CREATE EDITOR h_ed IN WIDGET-POOL {&PW_Pool}
ASSIGN
  X = 0
  Y = 0
  WIDTH       = h_frame:WIDTH -
                  ( h_Frame:BORDER-LEFT + h_Frame:BORDER-RIGHT )
  HEIGHT      = h_frame:HEIGHT -
                  ( h_Frame:BORDER-TOP  + h_Frame:BORDER-BOTTOM )
  SCROLLBAR-V = YES
  SCROLLBAR-H = YES
  FONT        = editor_font         /* From adestds.i */
  FGCOLOR     = std_ed4gl_fgcolor   /* From adestds.i */
  BGCOLOR     = std_ed4gl_bgcolor   /* From adestds.i */
  AUTO-INDENT = TRUE
  LARGE       = TRUE
  PROGRESS-SOURCE = YES  /* No-Op on GUI, but ok to have. */
  FRAME       = h_frame
  VISIBLE     = YES
  SENSITIVE   = YES
  TRIGGERS:
    ON TAB,CTRL-TAB
        PERSISTENT RUN adecomm/_pwedit.p ( INPUT "TAB" ).
    ON BACK-TAB,SHIFT-TAB
        PERSISTENT RUN adecomm/_pwedit.p ( INPUT "BACK-TAB" ).
    ON END-ERROR
        PERSISTENT RUN adecomm/_pwedit.p ( INPUT "END-ERROR" ).
  END TRIGGERS.

/* Clear Undo state. */
ASSIGN h_ed:EDIT-CAN-UNDO = FALSE.

/** ******
 ** Step 4: Create the additional PW Attributes used for persistent data.
 ** ******
 **/
/* Use the Editor Frame to hold these PW's Search Attributes.  We placed
   these in FILL-IN widgets of their own because they can be multi-line
   text strings.  Because of the EOL characters, this could be tricky to
   store properly in a delimited-list.
*/
RUN CreateFrameAttr ( INPUT h_frame , {&PW_Find_Text} ).
RUN CreateFrameAttr ( INPUT h_frame , {&PW_Replace_Text} ).

/* Additional PW Attributes are stored in comma-list in the 
   ED:PRIVATE-DATA
*/
RUN InitAttr ( INPUT h_ed ).       
