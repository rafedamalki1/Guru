/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/
/* $Id: _chscolr.p,v 1.20 97/02/04 16:50:00 ross Exp $ */

/*----------------------------------------------------------------------------

File: _chscolor.p

Description: This dialog serves as a common color chooser.  The tricky point
             to represent is the DEFAULT color.  A button should show the
             default color of its frame for the default, while a frame or
             window should show the System Default.

Input Parameters: ipTitle        - Title for the dialog
                  cipMessage     - Message to write at bottom of screen
                                   (e.g."Button color has no effect on Windows")
		  iipDfltBgColor - color number to use as default background
		  iipDfltFgColor -  color number to use as default foreground
Input-Output Parameters:
                    iiopBgColor - background color number
		    iiopFgColor - foreground color number
Output Parameters:  pressed_OK  - FALSE if cancelled dialog box.

Author: Ravi-Chandar Ramalingam

Date Created: 31 March 1993

Modifications:
  9/15/93 wood  Added explicit default colors (iipDfltBgColor etc). 
  7/18/05 wood  Allow editting of color < 16 (bug # 95-06-30-052)
---------------------------------------------------------------------------- 
Test code:
def var iBgC AS INTEGER NO-UNDO.
def var iFgC AS INTEGER NO-UNDO.
def var pressed_OK  AS LOGICAL.
RUN adecomm/_chscolr.p ("Title", "message", ?, ?,
  INPUT-OUTPUT ibgc,
  INPUT-OUTPUT ifgc,
  OUTPUT pressed_OK).
MESSAGE ibgc ifgc pressed_OK.
---------------------------------------------------------------------------- */

&GLOBAL-DEFINE WIN95-BTN YES
{adecomm/commeng.i}   /* Help pre-processor directives                       */
{adecomm/adestds.i}

/******************************* Definitions *********************************/
DEFINE INPUT        PARAMETER ipTitle AS CHARACTER NO-UNDO. 
DEFINE INPUT        PARAMETER cipMessage AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER iipDfltBgColor AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER iipDfltFgColor AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iiopBgColor AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iiopFgColor AS INTEGER NO-UNDO.
DEFINE       OUTPUT PARAMETER pressed_OK  AS LOGICAL.

&Global-define SKP &IF "{&WINDOW-SYSTEM}" = "OSF/Motif" &THEN SKIP &ELSE &ENDIF

DEFINE BUTTON bOk LABEL "OK" {&STDPH_OKBTN} AUTO-GO.
DEFINE BUTTON bCancel LABEL "Cancel" {&STDPH_OKBTN} AUTO-END-KEY.
DEFINE BUTTON bHelp LABEL "&Help" {&STDPH_OKBTN}.

DEFINE BUTTON bfcEdit LABEL "Edit..." SIZE 9 BY 1.
DEFINE BUTTON bbcEdit LIKE bfcEdit.

DEFINE BUTTON bSave LABEL "&Save Color Settings":C20 SIZE 22 BY {&H_OKBTN} MARGIN-EXTRA DEFAULT.

DEFINE BUTTON ifcLeftArrow IMAGE-UP FILE "btn-left-arrow".
DEFINE BUTTON ifcRightArrow IMAGE-UP FILE "btn-right-arrow".

DEFINE BUTTON ibcLeftArrow LIKE ifcLeftArrow.
DEFINE BUTTON ibcRightArrow LIKE ifcRightArrow.

DEFINE VARIABLE lChanged AS LOGICAL NO-UNDO.
DEFINE VARIABLE lSaved AS LOGICAL NO-UNDO.
DEFINE VARIABLE highest-color AS INTEGER NO-UNDO.

DEFINE VARIABLE tFgTitle AS CHARACTER
	INITIAL "Foreground Colors: " FORMAT "x(22)".
DEFINE VARIABLE tBgTitle AS CHARACTER
	INITIAL "Background Colors: " FORMAT "x(22)".

DEFINE RECTANGLE rFgHilite SIZE-PIXELS 1 BY 1 
                           FGCOLOR ? BGCOLOR ? EDGE-PIXELS 3 NO-FILL.
DEFINE RECTANGLE rBgHilite LIKE rFgHilite.       

DEFINE VARIABLE iBgColor AS INTEGER NO-UNDO.
DEFINE VARIABLE iFgColor AS INTEGER NO-UNDO.

DEFINE VARIABLE tSample AS CHARACTER FORMAT "X(7)" NO-UNDO.
DEFINE RECTANGLE rSample SIZE-CHAR 20 BY 1 EDGE-PIXELS 2 BGCOLOR 0.
DEFINE VARIABLE tMessage AS CHARACTER FORMAT "X(65)" VIEW-AS TEXT.

/* standard button rectangle */
&IF {&OKBOX} &THEN
DEFINE RECTANGLE rHeavyRule {&STDPH_OKBOX}.
&ENDIF

DEFINE VARIABLE tfc1 AS CHARACTER FORMAT "X(3)" 
       VIEW-AS TEXT SIZE 3 BY 1.
DEFINE VARIABLE tfc2 LIKE tfc1.
DEFINE VARIABLE tfc3 LIKE tfc1.
DEFINE VARIABLE tfc4 LIKE tfc1.
DEFINE VARIABLE tfc5 LIKE tfc1.
DEFINE VARIABLE tfc6 LIKE tfc1.
DEFINE VARIABLE tfc7 LIKE tfc1.
DEFINE VARIABLE tfc8 LIKE tfc1.
DEFINE VARIABLE tfc9 LIKE tfc1.
DEFINE VARIABLE tfc10 LIKE tfc1.
DEFINE VARIABLE tfc11 LIKE tfc1.
DEFINE VARIABLE tfc12 LIKE tfc1.
DEFINE VARIABLE tfc13 LIKE tfc1.
DEFINE VARIABLE tfc14 LIKE tfc1.
DEFINE VARIABLE tfc15 LIKE tfc1.
DEFINE VARIABLE tfc16 LIKE tfc1.

DEFINE VARIABLE tbc1 LIKE tfc1.
DEFINE VARIABLE tbc2 LIKE tfc1.
DEFINE VARIABLE tbc3 LIKE tfc1.
DEFINE VARIABLE tbc4 LIKE tfc1.
DEFINE VARIABLE tbc5 LIKE tfc1.
DEFINE VARIABLE tbc6 LIKE tfc1.
DEFINE VARIABLE tbc7 LIKE tfc1.
DEFINE VARIABLE tbc8 LIKE tfc1.
DEFINE VARIABLE tbc9 LIKE tfc1.
DEFINE VARIABLE tbc10 LIKE tfc1.
DEFINE VARIABLE tbc11 LIKE tfc1.
DEFINE VARIABLE tbc12 LIKE tfc1.
DEFINE VARIABLE tbc13 LIKE tfc1.
DEFINE VARIABLE tbc14 LIKE tfc1.
DEFINE VARIABLE tbc15 LIKE tfc1.
DEFINE VARIABLE tbc16 LIKE tfc1.

DEFINE VARIABLE tFgDefault LIKE tfc1.
DEFINE VARIABLE tBgDefault LIKE tfc1.

DEFINE RECTANGLE rfc0 SIZE-CHAR 3.2 BY 1 EDGE-PIXELS 1.
DEFINE RECTANGLE rfc1 LIKE rfc0.
DEFINE RECTANGLE rfc2 LIKE rfc0.
DEFINE RECTANGLE rfc3 LIKE rfc0.
DEFINE RECTANGLE rfc4 LIKE rfc0.
DEFINE RECTANGLE rfc5 LIKE rfc0.
DEFINE RECTANGLE rfc6 LIKE rfc0.
DEFINE RECTANGLE rfc7 LIKE rfc0.
DEFINE RECTANGLE rfc8 LIKE rfc0.
DEFINE RECTANGLE rfc9 LIKE rfc0.
DEFINE RECTANGLE rfc10 LIKE rfc0.
DEFINE RECTANGLE rfc11 LIKE rfc0.
DEFINE RECTANGLE rfc12 LIKE rfc0.
DEFINE RECTANGLE rfc13 LIKE rfc0.
DEFINE RECTANGLE rfc14 LIKE rfc0.
DEFINE RECTANGLE rfc15 LIKE rfc0.

DEFINE RECTANGLE rbc0 LIKE rfc0.
DEFINE RECTANGLE rbc1 LIKE rfc0.
DEFINE RECTANGLE rbc2 LIKE rfc0.
DEFINE RECTANGLE rbc3 LIKE rfc0.
DEFINE RECTANGLE rbc4 LIKE rfc0.
DEFINE RECTANGLE rbc5 LIKE rfc0.
DEFINE RECTANGLE rbc6 LIKE rfc0.
DEFINE RECTANGLE rbc7 LIKE rfc0.
DEFINE RECTANGLE rbc8 LIKE rfc0.
DEFINE RECTANGLE rbc9 LIKE rfc0.
DEFINE RECTANGLE rbc10 LIKE rfc0.
DEFINE RECTANGLE rbc11 LIKE rfc0.
DEFINE RECTANGLE rbc12 LIKE rfc0.
DEFINE RECTANGLE rbc13 LIKE rfc0.
DEFINE RECTANGLE rbc14 LIKE rfc0.
DEFINE RECTANGLE rbc15 LIKE rfc0.

DEFINE RECTANGLE rfcDefault LIKE rfc0 EDGE-CHAR 1.6.
DEFINE RECTANGLE rbcDefault LIKE rfc0.

DEFINE FRAME frColorEdit
  SKIP({&TFM_WID})
  rSample AT 30
  tSample AT ROW-OF rSample COL-OF rSample + 7 VIEW-AS TEXT NO-LABEL SKIP(1)

  tFgTitle VIEW-AS TEXT NO-LABEL AT 2 SKIP(0.4)
  ifcLeftArrow AT 2 SPACE ifcRightArrow SPACE(3)
  rfcDefault SPACE(0.5)
  rfc0 SPACE(0.5) rfc1 SPACE(0.5)
  rfc2 SPACE(0.5) rfc3 SPACE(0.5) rfc4 SPACE(0.5)
  rfc5 SPACE(0.5) rfc6 SPACE(0.5) rfc7 SPACE(0.5)
  rfc8 SPACE(0.5) rfc9 SPACE(0.5) rfc10 SPACE(0.5)
  rfc11 SPACE(0.5) rfc12 SPACE(0.5) rfc13 SPACE(0.5)
  rfc14 SPACE(0.5) rfc15 SPACE(0.5) bfcEdit SKIP(0.2)
  rFgHilite tfgDefault AT ROW-OF rFgHilite COL-OF rfcDefault + 1
  tfc1 AT ROW-OF rFgHilite COL-OF rfc0 + .5
  tfc2 AT ROW-OF rFgHilite COL-OF rfc1 + .5
  tfc3 AT ROW-OF rFgHilite COL-OF rfc2 + .5
  tfc4 AT ROW-OF rFgHilite COL-OF rfc3 + .5
  tfc5 AT ROW-OF rFgHilite COL-OF rfc4 + .5
  tfc6 AT ROW-OF rFgHilite COL-OF rfc5 + .5
  tfc7 AT ROW-OF rFgHilite COL-OF rfc6 + .5
  tfc8 AT ROW-OF rFgHilite COL-OF rfc7 + .5
  tfc9 AT ROW-OF rFgHilite COL-OF rfc8 + .5
  tfc10 AT ROW-OF rFgHilite COL-OF rfc9 + .5
  tfc11 AT ROW-OF rFgHilite COL-OF rfc10 + .3
  tfc12 AT ROW-OF rFgHilite COL-OF rfc11 + .3
  tfc13 AT ROW-OF rFgHilite COL-OF rfc12 + .3
  tfc14 AT ROW-OF rFgHilite COL-OF rfc13 + .3 
  tfc15 AT ROW-OF rFgHilite COL-OF rfc14 + .3
  tfc16 AT ROW-OF rFgHilite COL-OF rfc15 + .3 SKIP({&VM_WIDG})

  tBgTitle VIEW-AS TEXT NO-LABEL AT 2 SKIP(0.4)
  ibcLeftArrow AT 2 SPACE ibcRightArrow SPACE(3)
  rbcDefault SPACE(0.5)
  rbc0 SPACE(0.5) rbc1 SPACE(0.5)
  rbc2 SPACE(0.5) rbc3 SPACE(0.5) rbc4 SPACE(0.5)
  rbc5 SPACE(0.5) rbc6 SPACE(0.5) rbc7 SPACE(0.5)
  rbc8 SPACE(0.5) rbc9 SPACE(0.5) rbc10 SPACE(0.5)
  rbc11 SPACE(0.5) rbc12 SPACE(0.5) rbc13 SPACE(0.5)
  rbc14 SPACE(0.5) rbc15 SPACE(0.5) bbcEdit SKIP(0.2)
  rBgHilite tBgDefault AT ROW-OF rBgHilite COL-OF rbcDefault + 1
  tbc1 AT ROW-OF rBgHilite COL-OF rbc0 + .5
  tbc2 AT ROW-OF rBgHilite COL-OF rbc1 + .5 
  tbc3 AT ROW-OF rBgHilite COL-OF rbc2 + .5
  tbc4 AT ROW-OF rBgHilite COL-OF rbc3 + .5
  tbc5 AT ROW-OF rBgHilite COL-OF rbc4 + .5
  tbc6 AT ROW-OF rBgHilite COL-OF rbc5 + .5
  tbc7 AT ROW-OF rBgHilite COL-OF rbc6 + .5
  tbc8 AT ROW-OF rBgHilite COL-OF rbc7 + .5
  tbc9 AT ROW-OF rBgHilite COL-OF rbc8 + .5
  tbc10 AT ROW-OF rBgHilite COL-OF rbc9 + .5
  tbc11 AT ROW-OF rBgHilite COL-OF rbc10 + .3
  tbc12 AT ROW-OF rBgHilite COL-OF rbc11 + .3
  tbc13 AT ROW-OF rBgHilite COL-OF rbc12 + .3
  tbc14 AT ROW-OF rBgHilite COL-OF rbc13 + .3
  tbc15 AT ROW-OF rBgHilite COL-OF rbc14 + .3
  tbc16 AT ROW-OF rBgHilite COL-OF rbc15 + .3
  tMessage AT 2
  {adecomm/okform.i
      &BOX    = "rHeavyRule"
      &STATUS = "no"
      &OK     = "bOK"
      &CANCEL = "bCancel"
      &OTHER  = "space({&HM_DBTNG}) bSave"
      &HELP   = "bHelp"}
WITH NO-LABELS TITLE ipTitle VIEW-AS DIALOG-BOX DEFAULT-BUTTON bOK WIDTH 87.5.

/* **************************** THREE-D Support *************************** */       
/* In 3D, we want to change some of the characteristics of the widgets
   to show better in this environment. Changes include:
      - Fill Default FGCOLOR with black
      - Fill Default BGCOLOR with grey (?)
      - Make Hilight box solid black
 */
IF SESSION:THREE-D THEN ASSIGN 
  rfcDefault:EDGE-PIXELS = 1
  rfcDefault:BGCOLOR     = 0
  rfcDefault:FILLED      = YES
  
  rbcDefault:EDGE-PIXELS = 2
  rbcDefault:FILLED      = YES

  rFgHilite:EDGE-PIXELS  = 1  
  rFgHilite:FILLED       = YES
  
  rBgHilite:EDGE-PIXELS  = 1
  rBgHilite:FILLED       = YES
  .
/**************************** Internal Procedures ***************************/

PROCEDURE EditColor.
  DEFINE INPUT PARAMETER iipColorNumber AS INTEGER NO-UNDO.  
  
  DEF VAR lvAccepted AS LOGICAL NO-UNDO.
  DEF VAR old-red AS INTEGER NO-UNDO. 
  DEF VAR old-green AS INTEGER NO-UNDO. 
  DEF VAR old-blue AS INTEGER NO-UNDO. 
  
  IF iipColorNumber > -1 AND iipColorNumber < 256 THEN DO:
    ASSIGN lvAccepted = COLOR-TABLE:SET-DYNAMIC(iipColorNumber, TRUE)
           old-red    = COLOR-TABLE:GET-RED-VALUE(iipColorNumber)
           old-green  = COLOR-TABLE:GET-GREEN-VALUE(iipColorNumber)
           old-blue   = COLOR-TABLE:GET-BLUE-VALUE(iipColorNumber)
           . 
    SYSTEM-DIALOG COLOR iipColorNumber UPDATE lvAccepted. 
    /* If anything changed, then note the fact. */
    IF lvAccepted AND
       ((old-red   ne COLOR-TABLE:GET-RED-VALUE(iipColorNumber)) OR
        (old-green ne COLOR-TABLE:GET-GREEN-VALUE(iipColorNumber)) OR
        (old-blue  ne COLOR-TABLE:GET-BLUE-VALUE(iipColorNumber)) 
       )
    THEN ASSIGN lchanged      = YES
                highest-color = MAX(highest-color, iipColorNumber).
  END.
  ELSE
    MESSAGE "Color number" iipColorNumber "is not valid." SKIP
	    "Valid color numbers are from 0 - 255."
	VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END PROCEDURE.

PROCEDURE GetStartingColorNumber.
  DEFINE INPUT PARAMETER iipColorNumber AS INTEGER.
  DEFINE OUTPUT PARAMETER iopColorStart AS INTEGER.

  ASSIGN iopColorStart = TRUNCATE(iipColorNumber / 16, 0) * 16.
END PROCEDURE.

PROCEDURE BoxRectangle.
  DEFINE INPUT PARAMETER whipRectangleHandle AS WIDGET-HANDLE.
  DEFINE INPUT PARAMETER whipBoxHandle AS WIDGET-HANDLE.
  DEFINE INPUT PARAMETER iipColorNumber AS INTEGER.

  ASSIGN whipBoxHandle:HIDDEN = TRUE
	 whipBoxHandle:X = whipRectangleHandle:X - 3
	 whipBoxHandle:Y = whipRectangleHandle:Y - 4
	 whipBoxHandle:WIDTH-PIXELS = whipRectangleHandle:WIDTH-PIXELS + 6
	 whipBoxHandle:HEIGHT-PIXELS = whipRectangleHandle:HEIGHT-PIXELS + 8
         whipBoxHandle:HIDDEN = FALSE.
END PROCEDURE.

PROCEDURE SetSampleForeground.
  DEFINE INPUT PARAMETER iipFgColor AS INTEGER.

  DO WITH FRAME frColorEdit: 
    /* Use the default color if the value is UNKNOWN. */    
    IF iipFgColor = ? THEN iipFgColor = iipDfltFgColor.
    ASSIGN bfcEdit:SENSITIVE = yes
           rSample:FGCOLOR = iipFgColor
	   tSample:FGCOLOR = iipFgColor.
  END.
END PROCEDURE.

PROCEDURE SetSampleBackground.
  DEFINE INPUT PARAMETER iipBgColor AS INTEGER.

  DO WITH FRAME frColorEdit:
    /* Use the default color if the value is UNKNOWN. */    
    IF iipBgColor = ? THEN iipBgColor = iipDfltBgColor. 
    IF iipBgColor eq ?
    THEN rSample:FILLED  = NOT (FRAME frColorEdit:THREE-D).
    ELSE rSample:FILLED  = YES.
    ASSIGN rSample:BGCOLOR = iipBgColor
	   tSample:BGCOLOR = iipBgColor.  
  END.
END PROCEDURE.

PROCEDURE InitializeColors.
  DEFINE VARIABLE ivfgColStart AS INTEGER INITIAL 0.
  DEFINE VARIABLE ivbgColStart AS INTEGER INITIAL 0.
  DEFINE VARIABLE dummy AS LOGICAL.

  DO WITH FRAME frColorEdit:
    IF COLOR-TABLE:NUM-ENTRIES < 16 THEN /* verify 16 color entries */
      ASSIGN COLOR-TABLE:NUM-ENTRIES = 16.

    ASSIGN FRAME frColorEdit:HIDDEN = TRUE
           highest-color = COLOR-TABLE:NUM-ENTRIES.
    RUN SetSampleForeground( iFgColor ).
    IF iFgColor = ? 
    THEN RUN SelectForegroundRectangle( ? ).
    ELSE DO:
      RUN GetStartingColorNumber( iFgColor, OUTPUT ivfgColStart ).
      RUN SelectForegroundRectangle(iFgColor - ivfgColStart).
    END.
    RUN SetForegroundRectangleColors( ivfgColStart ).
    dummy = rFgHilite:MOVE-TO-BOTTOM().
    IF iBgColor = ? 
    THEN RUN SelectBackgroundRectangle( ? ).
    ELSE DO:
      RUN GetStartingColorNumber( iBgColor, OUTPUT ivbgColStart ).
      RUN SelectBackgroundRectangle(iBgColor - ivbgColStart).
    END.
    RUN SetBackgroundRectangleColors( ivbgColStart ).
    dummy = rBgHilite:MOVE-TO-BOTTOM().
    /* Hide the frame and set the colors of the "default" boxes.  The
       default boxes might be a different color than ? (eg. if we
       are choosing the color of a fill-in, the defaults will be the
       frame defaults.  So fill in the Foreground rectangle with the
       FGCOLOR - this will only matter if it is not ?.  If it is ?
       then we can only see the default FGCOLOR on its edge */
    ASSIGN rbcDefault:BGCOLOR = IF (FRAME frColorEdit:THREE-D AND 
                                    iipDfltBgColor eq ?)
                                THEN 8
                                ELSE iipDfltBgColor
           rfcDefault:FGCOLOR = iipDfltFgColor  
           FRAME frColorEdit:HIDDEN = FALSE.    

    /* Fill the Default FGCOLOR rectangle with its appropriate value.
       This needs to be done after the frame has been unhidden. */
   RUN SetSampleBackground( iBgColor ). 
   IF NOT (FRAME frColorEdit:THREE-D AND iipDfltFgColor eq ?)
   THEN rfcDefault:BGCOLOR = iipDfltFgColor.   
  END.
END PROCEDURE.

PROCEDURE SelectForegroundRectangle.
  DEFINE INPUT PARAMETER iipColorNumber AS INTEGER.
  DEFINE VARIABLE whRectangle AS WIDGET-HANDLE NO-UNDO.

  DO WITH FRAME frColorEdit:
    CASE iipColorNumber:
      WHEN ? THEN
        ASSIGN whRectangle = rfcDefault:HANDLE.
      WHEN  0 THEN
        ASSIGN whRectangle = rfc0:HANDLE.
      WHEN  1 THEN
        ASSIGN whRectangle = rfc1:HANDLE.
      WHEN  2 THEN
        ASSIGN whRectangle = rfc2:HANDLE.
      WHEN  3 THEN
        ASSIGN whRectangle = rfc3:HANDLE.
      WHEN  4 THEN
        ASSIGN whRectangle = rfc4:HANDLE.
      WHEN  5 THEN
        ASSIGN whRectangle = rfc5:HANDLE.
      WHEN  6 THEN
        ASSIGN whRectangle = rfc6:HANDLE.
      WHEN  7 THEN
        ASSIGN whRectangle = rfc7:HANDLE.
      WHEN  8 THEN
        ASSIGN whRectangle = rfc8:HANDLE.
      WHEN  9 THEN
        ASSIGN whRectangle = rfc9:HANDLE.
      WHEN 10 THEN
        ASSIGN whRectangle = rfc10:HANDLE.
      WHEN 11 THEN
        ASSIGN whRectangle = rfc11:HANDLE.
      WHEN 12 THEN
        ASSIGN whRectangle = rfc12:HANDLE.
      WHEN 13 THEN
        ASSIGN whRectangle = rfc13:HANDLE.
      WHEN 14 THEN
        ASSIGN whRectangle = rfc14:HANDLE.
      WHEN 15 THEN
        ASSIGN whRectangle = rfc15:HANDLE.
      OTHERWISE
        MESSAGE "Unknown color number." VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    END CASE.
    IF whRectangle <> ? AND rFgHilite:HANDLE <> ? THEN DO:
      RUN BoxRectangle( whRectangle, rFgHilite:HANDLE, iipColorNumber ).
    END.
  END.
END PROCEDURE.

PROCEDURE SelectBackgroundRectangle.
  DEFINE INPUT PARAMETER iipColorNumber AS INTEGER.
  DEFINE VARIABLE whRectangle AS WIDGET-HANDLE NO-UNDO.

  DO WITH FRAME frColorEdit:
    CASE iipColorNumber:
      WHEN ? THEN
        ASSIGN whRectangle = rbcDefault:HANDLE.
      WHEN  0 THEN
        ASSIGN whRectangle = rbc0:HANDLE.
      WHEN  1 THEN
        ASSIGN whRectangle = rbc1:HANDLE.
      WHEN  2 THEN
        ASSIGN whRectangle = rbc2:HANDLE.
      WHEN  3 THEN
        ASSIGN whRectangle = rbc3:HANDLE.
      WHEN  4 THEN
        ASSIGN whRectangle = rbc4:HANDLE.
      WHEN  5 THEN
        ASSIGN whRectangle = rbc5:HANDLE.
      WHEN  6 THEN
        ASSIGN whRectangle = rbc6:HANDLE.
      WHEN  7 THEN
        ASSIGN whRectangle = rbc7:HANDLE.
      WHEN  8 THEN
        ASSIGN whRectangle = rbc8:HANDLE.
      WHEN  9 THEN
        ASSIGN whRectangle = rbc9:HANDLE.
      WHEN 10 THEN
        ASSIGN whRectangle = rbc10:HANDLE.
      WHEN 11 THEN
        ASSIGN whRectangle = rbc11:HANDLE.
      WHEN 12 THEN
        ASSIGN whRectangle = rbc12:HANDLE.
      WHEN 13 THEN
        ASSIGN whRectangle = rbc13:HANDLE.
      WHEN 14 THEN
        ASSIGN whRectangle = rbc14:HANDLE.
      WHEN 15 THEN
        ASSIGN whRectangle = rbc15:HANDLE.
      OTHERWISE
        MESSAGE "Unknown color number." VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    END CASE.
    IF whRectangle <> ? AND rBgHilite:HANDLE <> ? 
    THEN RUN BoxRectangle( whRectangle, rBgHilite:HANDLE, iipColorNumber ).
  END.
END PROCEDURE.

PROCEDURE SetForegroundRectangleColors.
  DEFINE INPUT PARAMETER iipColorStart AS INTEGER.

  IF COLOR-TABLE:NUM-ENTRIES < (iipColorStart + 16) OR
     highest-color <= (iipColorStart + 16)
  THEN COLOR-TABLE:NUM-ENTRIES = iipColorStart + 16.

  DO WITH FRAME frColorEdit:
    ASSIGN rfc0:BGCOLOR = iipColorStart
	   tfc1:SCREEN-VALUE = STRING(rfc0:BGCOLOR)
	   rfc1:BGCOLOR = rfc0:BGCOLOR + 1
	   tfc2:SCREEN-VALUE = STRING(rfc1:BGCOLOR)
	   rfc2:BGCOLOR = rfc1:BGCOLOR + 1
	   tfc3:SCREEN-VALUE = STRING(rfc2:BGCOLOR)
	   rfc3:BGCOLOR = rfc2:BGCOLOR + 1
	   tfc4:SCREEN-VALUE = STRING(rfc3:BGCOLOR)
	   rfc4:BGCOLOR = rfc3:BGCOLOR + 1
	   tfc5:SCREEN-VALUE = STRING(rfc4:BGCOLOR)
	   rfc5:BGCOLOR = rfc4:BGCOLOR + 1
	   tfc6:SCREEN-VALUE = STRING(rfc5:BGCOLOR)
	   rfc6:BGCOLOR = rfc5:BGCOLOR + 1
	   tfc7:SCREEN-VALUE = STRING(rfc6:BGCOLOR)
	   rfc7:BGCOLOR = rfc6:BGCOLOR + 1
	   tfc8:SCREEN-VALUE = STRING(rfc7:BGCOLOR)
	   rfc8:BGCOLOR = rfc7:BGCOLOR + 1
	   tfc9:SCREEN-VALUE = STRING(rfc8:BGCOLOR)
	   rfc9:BGCOLOR = rfc8:BGCOLOR + 1
	   tfc10:SCREEN-VALUE = STRING(rfc9:BGCOLOR)
	   rfc10:BGCOLOR = rfc9:BGCOLOR + 1
	   tfc11:SCREEN-VALUE = STRING(rfc10:BGCOLOR)
	   rfc11:BGCOLOR = rfc10:BGCOLOR + 1
	   tfc12:SCREEN-VALUE = STRING(rfc11:BGCOLOR)
	   rfc12:BGCOLOR = rfc11:BGCOLOR + 1
	   tfc13:SCREEN-VALUE = STRING(rfc12:BGCOLOR)
	   rfc13:BGCOLOR = rfc12:BGCOLOR + 1
	   tfc14:SCREEN-VALUE = STRING(rfc13:BGCOLOR)
	   rfc14:BGCOLOR = rfc13:BGCOLOR + 1
	   tfc15:SCREEN-VALUE = STRING(rfc14:BGCOLOR)
	   rfc15:BGCOLOR = rfc14:BGCOLOR + 1
	   tfc16:SCREEN-VALUE = STRING(rfc15:BGCOLOR).

      IF rfc0:BGCOLOR = 0 THEN
        ASSIGN ifcLeftArrow:SENSITIVE = FALSE.
      ELSE IF NOT ifcLeftArrow:SENSITIVE THEN
	ASSIGN ifcLeftArrow:SENSITIVE = TRUE.

      IF rfc15:BGCOLOR = 255 THEN
        ASSIGN ifcRightArrow:SENSITIVE = FALSE.
      ELSE IF NOT ifcRightArrow:SENSITIVE THEN
	ASSIGN ifcRightArrow:SENSITIVE = TRUE.

      IF iFgColor = ? THEN
        ASSIGN bfcEdit:SENSITIVE = FALSE.
      ELSE IF NOT bfcEdit:SENSITIVE THEN
        ASSIGN bfcEdit:SENSITIVE = TRUE.

      IF iFgColor = ? OR ( iFgColor >= rfc0:BGCOLOR AND
				iFgColor <= rfc15:BGCOLOR) THEN
        ASSIGN rFgHilite:HIDDEN = FALSE.
      ELSE
        ASSIGN rFgHilite:HIDDEN = TRUE.
  END.
END PROCEDURE.

PROCEDURE SetBackgroundRectangleColors.
  DEFINE INPUT PARAMETER iipColorStart AS INTEGER.

  IF COLOR-TABLE:NUM-ENTRIES < (iipColorStart + 16)  OR
     highest-color <= (iipColorStart + 16) 
  THEN COLOR-TABLE:NUM-ENTRIES = iipColorStart + 16.

  DO WITH FRAME frColorEdit:
    ASSIGN rbc0:BGCOLOR = iipColorStart
	   tbc1:SCREEN-VALUE = STRING(rbc0:BGCOLOR)
	   rbc1:BGCOLOR = rbc0:BGCOLOR + 1
	   tbc2:SCREEN-VALUE = STRING(rbc1:BGCOLOR)
	   rbc2:BGCOLOR = rbc1:BGCOLOR + 1
	   tbc3:SCREEN-VALUE = STRING(rbc2:BGCOLOR)
	   rbc3:BGCOLOR = rbc2:BGCOLOR + 1
	   tbc4:SCREEN-VALUE = STRING(rbc3:BGCOLOR)
	   rbc4:BGCOLOR = rbc3:BGCOLOR + 1
	   tbc5:SCREEN-VALUE = STRING(rbc4:BGCOLOR)
	   rbc5:BGCOLOR = rbc4:BGCOLOR + 1
	   tbc6:SCREEN-VALUE = STRING(rbc5:BGCOLOR)
	   rbc6:BGCOLOR = rbc5:BGCOLOR + 1
	   tbc7:SCREEN-VALUE = STRING(rbc6:BGCOLOR)
	   rbc7:BGCOLOR = rbc6:BGCOLOR + 1
	   tbc8:SCREEN-VALUE = STRING(rbc7:BGCOLOR)
	   rbc8:BGCOLOR = rbc7:BGCOLOR + 1
	   tbc9:SCREEN-VALUE = STRING(rbc8:BGCOLOR)
	   rbc9:BGCOLOR = rbc8:BGCOLOR + 1
	   tbc10:SCREEN-VALUE = STRING(rbc9:BGCOLOR)
	   rbc10:BGCOLOR = rbc9:BGCOLOR + 1
	   tbc11:SCREEN-VALUE = STRING(rbc10:BGCOLOR)
	   rbc11:BGCOLOR = rbc10:BGCOLOR + 1
	   tbc12:SCREEN-VALUE = STRING(rbc11:BGCOLOR)
	   rbc12:BGCOLOR = rbc11:BGCOLOR + 1
	   tbc13:SCREEN-VALUE = STRING(rbc12:BGCOLOR)
	   rbc13:BGCOLOR = rbc12:BGCOLOR + 1
	   tbc14:SCREEN-VALUE = STRING(rbc13:BGCOLOR)
	   rbc14:BGCOLOR = rbc13:BGCOLOR + 1
	   tbc15:SCREEN-VALUE = STRING(rbc14:BGCOLOR)
	   rbc15:BGCOLOR = rbc14:BGCOLOR + 1
	   tbc16:SCREEN-VALUE = STRING(rbc15:BGCOLOR).

      IF rbc0:BGCOLOR = 0 THEN
        ASSIGN ibcLeftArrow:SENSITIVE = FALSE.
      ELSE IF NOT ibcLeftArrow:SENSITIVE THEN
	ASSIGN ibcLeftArrow:SENSITIVE = TRUE.

      IF rbc15:BGCOLOR + 1 >= 256 THEN
        ASSIGN ibcRightArrow:SENSITIVE = FALSE.
      ELSE IF NOT ibcRightArrow:SENSITIVE THEN
	ASSIGN ibcRightArrow:SENSITIVE = TRUE.

      IF iBgColor = ? THEN
        ASSIGN bbcEdit:SENSITIVE = FALSE.
      ELSE IF NOT bbcEdit:SENSITIVE THEN
        ASSIGN bbcEdit:SENSITIVE = TRUE.

      IF iBgColor = ? OR ( iBgColor >= rbc0:BGCOLOR AND
				iBgColor <= rbc15:BGCOLOR) THEN
        ASSIGN rBgHilite:HIDDEN = FALSE.
      ELSE
        ASSIGN rBgHilite:HIDDEN = TRUE.
  END.
END PROCEDURE.
  
PROCEDURE SaveSettings:       
  DEFINE VAR ok_save AS LOGICAL NO-UNDO.
  /* Check the ability to save settings */
  RUN adeshar/_chksave.p (INPUT "uib", OUTPUT ok_save).
  IF ok_save THEN DO:
    PUT-KEY-VALUE COLOR ALL. 
    lSaved = YES.
  END.
END PROCEDURE.

/*************************** UserInterface Triggers **************************/
ON WINDOW-CLOSE OF FRAME frColorEdit APPLY "END-ERROR":U TO SELF.

ON CHOOSE OF bHelp IN FRAME frColorEdit OR HELP OF FRAME frColorEdit
  RUN adecomm/_adehelp.p ( "comm", "CONTEXT", {&Color_Dlg_Box}, ? ).

ON CHOOSE OF bfcEdit
  RUN EditColor( rSample:FGCOLOR ).

ON CHOOSE OF bbcEdit
  RUN EditColor( rSample:BGCOLOR ).

ON CHOOSE OF bSave 
  RUN SaveSettings.

ON GO OF FRAME frColorEdit DO:
  DEF VAR lOK AS LOGICAL NO-UNDO.
  /* Tell the user if they changed any colors, but did not save. */
  IF lChanged AND NOT lSaved THEN DO:
    MESSAGE "Color settings were edited, but they have" {&SKP}
            "not been saved.  These changes will be lost when" {&SKP}
            "you leave the PROGRESS session." SKIP(1)
            "Would you like to save settings?"
            VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO-CANCEL UPDATE lOK.
    IF lOK eq ? THEN RETURN NO-APPLY.
    ELSE IF lOK THEN RUN SaveSettings.
  END.
END.

ON ENDKEY OF FRAME frColorEdit DO:
  /* Tell the user that changes make to the SYSTEM-DIALOG will not
     be undone. */
  IF lChanged OR lSaved THEN DO:
    MESSAGE "Changes make editing colors or saving" {&SKP}
            "color settings cannot be undone." SKIP(1)
            "Cancelling this dialog will not undo those changes."
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
  END.
END.

ON CHOOSE OF ifcLeftArrow DO:
  DEFINE VARIABLE ivColorStart as INTEGER.

  DO WITH FRAME frColorEdit:
    ASSIGN ivColorStart = rfc0:BGCOLOR - 16.
    IF ivColorStart >= 0 THEN
      RUN SetForegroundRectangleColors( ivColorStart ).
  END.
END.

ON CHOOSE OF ifcRightArrow DO:
  DEFINE VARIABLE ivColorStart as INTEGER.

  DO WITH FRAME frColorEdit:
    ASSIGN ivColorStart = rfc15:BGCOLOR + 1.
    IF ivColorStart < 256 THEN DO:
      IF COLOR-TABLE:NUM-ENTRIES < (ivColorStart + 16) THEN
        ASSIGN COLOR-TABLE:NUM-ENTRIES = ivColorStart + 16.
      RUN SetForegroundRectangleColors( ivColorStart ).
    END.
  END.
END.

ON CHOOSE OF ibcLeftArrow DO:
  DEFINE VARIABLE ivColorStart as INTEGER.

  DO WITH FRAME frColorEdit:
    ASSIGN ivColorStart = rbc0:BGCOLOR - 16.
    IF ivColorStart >= 0 THEN
      RUN SetBackgroundRectangleColors( ivColorStart ).
  END.
END.

ON CHOOSE OF ibcRightArrow DO:
  DEFINE VARIABLE ivColorStart as INTEGER.

  DO WITH FRAME frColorEdit:
    ASSIGN ivColorStart = rbc15:BGCOLOR + 1.
    IF ivColorStart < 256 THEN DO:
      IF COLOR-TABLE:NUM-ENTRIES < (ivColorStart + 16) THEN
        ASSIGN COLOR-TABLE:NUM-ENTRIES = ivColorStart + 16.
      RUN SetBackgroundRectangleColors( ivColorStart ).
    END.
  END.
END.

ON MOUSE-SELECT-CLICK OF rbc0, rbc1, rbc2, rbc3, rbc4, rbc5, rbc6, rbc7, rbc8, 
	rbc9, rbc10, rbc11, rbc12, rbc13, rbc14, rbc15
DO:
  DO WITH FRAME frColorEdit:
    ASSIGN iBgColor = SELF:BGCOLOR.
    RUN BoxRectangle( SELF:HANDLE, rBgHilite:HANDLE, iBgColor ).
    RUN SetSampleBackground( iBgColor ).
  END.
END.

ON MOUSE-SELECT-CLICK OF rbcDefault
DO:
  DO WITH FRAME frColorEdit:
    ASSIGN iBgColor = ?.
    RUN BoxRectangle( SELF:HANDLE, rBgHilite:HANDLE, iipDfltBgColor ).
    RUN SetSampleBackground( iBgColor ).
  END.
END.

ON MOUSE-SELECT-CLICK OF rfc0, rfc1, rfc2, rfc3, rfc4, rfc5, rfc6, rfc7, rfc8, 
	rfc9, rfc10, rfc11, rfc12, rfc13, rfc14, rfc15
DO:
  DO WITH FRAME frColorEdit:
    ASSIGN iFgColor = SELF:BGCOLOR.
    RUN BoxRectangle( SELF:HANDLE, rFgHilite:HANDLE, iFgColor ).
    RUN SetSampleForeground( iFgColor ).
  END.
END.

ON MOUSE-SELECT-CLICK OF rfcDefault
DO:
  DO WITH FRAME frColorEdit:
    ASSIGN iFgColor = ?.
    RUN BoxRectangle( SELF:HANDLE, rFgHilite:HANDLE, iipDfltFgColor ).
    RUN SetSampleForeground( iFgColor ).
  END.
END.

ON MOUSE-SELECT-DBLCLICK OF rfc0, rfc1, rfc2, rfc3, rfc4, rfc5, rfc6, rfc7,
  rfc8, rfc9, rfc10, rfc11, rfc12, rfc13, rfc14, rfc15, rbc0, rbc1, rbc2,
  rbc3, rbc4, rbc5, rbc6, rbc7, rbc8, rbc9, rbc10, rbc11, rbc12, rbc13,
  rbc14, rbc15, rfcDefault, rbcDefault
DO:
  IF SELF:BGCOLOR = ? THEN
    MESSAGE "The default color is not customizable."
	VIEW-AS ALERT-BOX INFORMATION BUTTONS Ok.
  ELSE DO:
    RUN EditColor( SELF:BGCOLOR ).
  END.
END.

/****************************** Main Loop ******************************/
DO WITH FRAME frColorEdit:
  /* Run time layout for button area. */
  {adecomm/okrun.i  
     &FRAME = "FRAME frColorEdit" 
     &BOX   = "rHeavyRule"
     &OK    = "bOK" 
     &HELP  = "bHelp"}

  IF SESSION:PIXELS-PER-ROW = 21 THEN 
    ASSIGN ifcLeftArrow:HEIGHT-PIXELS = 24
           ifcLeftArrow:WIDTH-PIXELS  = 21
           ifcRightArrow:HEIGHT-PIXELS = ifcLeftArrow:HEIGHT-PIXELS
           ifcRightArrow:WIDTH-PIXELS  = ifcLeftArrow:WIDTH-PIXELS
           ibcLeftArrow:HEIGHT-PIXELS  = ifcLeftArrow:HEIGHT-PIXELS
           ibcLeftArrow:WIDTH-PIXELS   = ifcLeftArrow:WIDTH-PIXELS
           ibcRightArrow:HEIGHT-PIXELS = ifcLeftArrow:HEIGHT-PIXELS
           ibcRightArrow:WIDTH-PIXELS  = ifcLeftArrow:WIDTH-PIXELS
           ifcRightArrow:X = ifcLeftArrow:X + ifcLeftArrow:WIDTH-PIXELS
           ibcRightArrow:X = ifcRightArrow:X.
           
  ASSIGN iFgColor = iiopFgColor  /* Make local copy of input-output parameters */
         iBgColor = iiopBgColor  /* ...in case user cancels changes            */
         tMessage:SCREEN-VALUE = cipMessage
	 tBgTitle:SCREEN-VALUE = "Background Colors: "
	 tFgTitle:SCREEN-VALUE = "Foreground Colors: "
	 tSample:SCREEN-VALUE = "Sample"
         tSample:Y = rSample:Y + 5
	 tFgDefault:SCREEN-VALUE = "?"
	 tBgDefault:SCREEN-VALUE = "?"
	  
	 ifcLeftArrow:Y = (rfcDefault:Y + (rfcDefault:HEIGHT-PIXELS / 2))
				- (ifcLeftArrow:HEIGHT-PIXELS / 2)
	 ibcLeftArrow:Y = (rbcDefault:Y + (rbcDefault:HEIGHT-PIXELS / 2))
				- (ibcLeftArrow:HEIGHT-PIXELS / 2)
	 ifcRightArrow:Y = (rfc15:Y + (rfc15:HEIGHT-PIXELS / 2))
				- (ifcRightArrow:HEIGHT-PIXELS / 2)
	 ibcRightArrow:Y = (rbc15:Y + (rbc15:HEIGHT-PIXELS / 2))
				- (ibcRightArrow:HEIGHT-PIXELS / 2).

  RUN InitializeColors. 
  
  DO TRANSACTION ON ENDKEY UNDO, LEAVE:
    ENABLE ALL EXCEPT bfcEdit bbcEdit ifcRightArrow ifcLeftArrow
      ibcRightArrow ibcLeftArrow WITH FRAME frColorEdit .
    ASSIGN pressed_OK = no. 

    IF NOT RETRY THEN UPDATE bOk bCancel bHelp WITH FRAME frColorEdit.
    /* Return Changes made by user */
    ASSIGN iiopFgColor = iFgColor
           iiopBgColor = iBgColor
           pressed_OK  = yes.
  END.
  HIDE FRAME frColorEdit.
END.
