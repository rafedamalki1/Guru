&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: vpedemo.w Copyright © 1993 - 1999 IDEAL Software®. All rights reserved.

  Description: Complete demo of the VPE print engine. This is a Progress
               adaptation of the VPE VB demo. 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Tom Bergman

  Created: March 27, 1999

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */


{vpe.i}

DEF VAR HEADLINE AS INT INIT 1 NO-UNDO.
DEF VAR BODYTEXT AS INT INIT 2 NO-UNDO.
DEF VAR headline_height AS INT NO-UNDO.
DEF VAR group_title_height AS INT NO-UNDO.
DEF VAR detail_height AS INT NO-UNDO.
DEF VAR sum_amount AS DEC NO-UNDO FORMAT ">>>,>>>,>>9.99".
DEF VAR sum_prorated AS DEC NO-UNDO.
DEF VAR sum_tax AS DEC NO-UNDO.
DEF VAR lineheight AS INT NO-UNDO.
DEF VAR factor AS INT NO-UNDO INIT 49.
DEF VAR curr-dir AS CHAR NO-UNDO.
DEF VAR DATAPATH AS CHAR NO-UNDO.
DEF VAR WOOD AS CHAR NO-UNDO.
DEF VAR SAND AS CHAR NO-UNDO.
DEF VAR BAU AS CHAR NO-UNDO.
DEF VAR VPELOGO AS CHAR NO-UNDO.
DEF VAR SILENCE AS CHAR NO-UNDO.
DEF VAR MARBLE AS CHAR NO-UNDO.
DEF VAR PROD AS CHAR NO-UNDO.
DEFINE VARIABLE utskriv AS LOGICAL NO-UNDO.
ASSIGN DATAPATH = "d:\program\VPE_S32\images".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD compx C-Win 
FUNCTION compx RETURNS INTEGER
  ( INPUT w AS INT,
    INPUT numcols AS INT,
    INPUT vCOL AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD compy C-Win 
FUNCTION compy RETURNS INTEGER
  ( h as int,
    vcol as int,
    vrow as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD detailline C-Win 
FUNCTION detailline RETURNS LOGICAL
  ( input vcolor as int,
    input country as char,
    input quantity as dec )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDirectory C-Win 
FUNCTION getDirectory RETURNS CHARACTER
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD groupfooter C-Win 
FUNCTION groupfooter RETURNS LOGICAL
  ( input quantity as dec )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD groupHeader C-Win 
FUNCTION groupHeader RETURNS LOGICAL
  ( input product as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ok2open C-Win 
FUNCTION ok2open RETURNS LOGICAL
  ( input chTest as com-handle )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD printfooter C-Win 
FUNCTION printfooter RETURNS LOGICAL
  ( )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD printheader C-Win 
FUNCTION printheader RETURNS LOGICAL
  ( vtable as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD printpagefooter C-Win 
FUNCTION printpagefooter RETURNS LOGICAL
  ( v-name as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setDirectory C-Win 
FUNCTION setDirectory RETURNS LOGICAL
  ( v-dir as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sine C-Win 
FUNCTION sine RETURNS DECIMAL
  ( INPUT decinput AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_File 
       MENU-ITEM m_Exit         LABEL "Exit"          .

DEFINE SUB-MENU m_Precision__Capabilities 
       MENU-ITEM m_Preview      LABEL "Preview"       
       MENU-ITEM m_Background   LABEL "Background"    .

DEFINE SUB-MENU m_Colors 
       MENU-ITEM m_Preview2     LABEL "Preview"       .

DEFINE SUB-MENU m_Report_Example 
       MENU-ITEM m_Preview3     LABEL "Preview"       
       MENU-ITEM m_E-Mail_in_Background LABEL "e-Mail in Background"
       MENU-ITEM m_E-Mail_in_Background_withou LABEL "e-Mail in Background without &Dialog".

DEFINE SUB-MENU m_Demos 
       MENU-ITEM m_Welcome      LABEL "Welcome"       
       SUB-MENU  m_Precision__Capabilities LABEL "Precision + Capabilities"
       MENU-ITEM m_Speed__Tables LABEL "Speed + Tables"
       SUB-MENU  m_Colors       LABEL "Colors"        
       SUB-MENU  m_Report_Example LABEL "Report Example"
       MENU-ITEM m_Auto_Page_Break LABEL "Auto Page Break".

DEFINE MENU MENU-BAR-C-Win MENUBAR
       SUB-MENU  m_File         LABEL "File"          
       SUB-MENU  m_Demos        LABEL "Demos"         .


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE Auto AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chAuto AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE Cap AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCap AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE Clr AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chClr AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE Report AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chReport AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE Speed AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chSpeed AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE Welcome AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chWelcome AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 116 BY 20.19.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "VPE Demo"
         HEIGHT             = 20.19
         WIDTH              = 116
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-C-Win:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME Cap ASSIGN
       FRAME        = FRAME DEFAULT-FRAME:HANDLE
       ROW          = 1
       COLUMN       = 1
       HEIGHT       = 3.81
       WIDTH        = 20
       HIDDEN       = yes
       SENSITIVE    = yes.

CREATE CONTROL-FRAME Speed ASSIGN
       FRAME        = FRAME DEFAULT-FRAME:HANDLE
       ROW          = 1
       COLUMN       = 23
       HEIGHT       = 3.81
       WIDTH        = 20
       HIDDEN       = no
       SENSITIVE    = yes.

CREATE CONTROL-FRAME Clr ASSIGN
       FRAME        = FRAME DEFAULT-FRAME:HANDLE
       ROW          = 1
       COLUMN       = 45
       HEIGHT       = 3.81
       WIDTH        = 57
       HIDDEN       = yes
       SENSITIVE    = yes.

CREATE CONTROL-FRAME Welcome ASSIGN
       FRAME        = FRAME DEFAULT-FRAME:HANDLE
       ROW          = 6
       COLUMN       = 2
       HEIGHT       = 3.81
       WIDTH        = 19
       HIDDEN       = yes
       SENSITIVE    = yes.

CREATE CONTROL-FRAME Report ASSIGN
       FRAME        = FRAME DEFAULT-FRAME:HANDLE
       ROW          = 6
       COLUMN       = 23
       HEIGHT       = 3.81
       WIDTH        = 20
       HIDDEN       = no
       SENSITIVE    = yes.

CREATE CONTROL-FRAME Auto ASSIGN
       FRAME        = FRAME DEFAULT-FRAME:HANDLE
       ROW          = 6
       COLUMN       = 45
       HEIGHT       = 3.81
       WIDTH        = 20
       HIDDEN       = yes
       SENSITIVE    = yes.
      Cap:NAME = "Cap":U .
/* Cap OCXINFO:CREATE-CONTROL from: {399548B6-253E-11D2-BE13-000000000000} type: VPE */
      Speed:NAME = "Speed":U .
/* Speed OCXINFO:CREATE-CONTROL from: {399548B6-253E-11D2-BE13-000000000000} type: VPE */
      Clr:NAME = "Clr":U .
/* Clr OCXINFO:CREATE-CONTROL from: {399548B6-253E-11D2-BE13-000000000000} type: VPE */
      Welcome:NAME = "Welcome":U .
/* Welcome OCXINFO:CREATE-CONTROL from: {399548B6-253E-11D2-BE13-000000000000} type: VPE */
      Report:NAME = "Report":U .
/* Report OCXINFO:CREATE-CONTROL from: {399548B6-253E-11D2-BE13-000000000000} type: VPE */
      Auto:NAME = "Auto":U .
/* Auto OCXINFO:CREATE-CONTROL from: {399548B6-253E-11D2-BE13-000000000000} type: VPE */
      Speed:MOVE-AFTER(Cap).
      Clr:MOVE-AFTER(Speed).
      Welcome:MOVE-AFTER(Clr).
      Report:MOVE-AFTER(Welcome).
      Auto:MOVE-AFTER(Report).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* VPE Demo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* VPE Demo */
DO:
  /* This event will close the window and terminate the procedure.  */
  
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* VPE Demo */
DO:
  
  cap:Y = 0.
  cap:X = 0.
  FRAME {&frame-name}:HIDDEN = TRUE.
  
  FRAME {&frame-name}:VIRTUAL-WIDTH-PIXELS = {&window-name}:WIDTH-PIXELS.
  FRAME {&frame-name}:WIDTH-PIXELS = {&window-name}:WIDTH-PIXELS.
  
  cap:WIDTH-PIXELS = FRAME {&frame-name}:WIDTH-PIXELS.
  FRAME {&frame-name}:VIRTUAL-HEIGHT-PIXELS = {&window-name}:HEIGHT-PIXELS.
  FRAME {&frame-name}:HEIGHT-PIXELS = {&window-name}:HEIGHT-PIXELS.
  cap:HEIGHT-PIXELS = FRAME {&frame-name}:HEIGHT-PIXELS.
  FRAME {&frame-name}:HIDDEN = FALSE.
  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Auto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Auto C-Win
PROCEDURE Auto.VPE.AfterMail .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Result
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Result AS INTEGER NO-UNDO.

setDirectory(curr-dir).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Auto C-Win
PROCEDURE Auto.VPE.BeforeMail .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

/* Use of VPE mail choice changes the Progress current directory.
     We need to get this and then change it back in the AfterMail trigger.*/
  curr-dir = getDirectory().


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cap
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cap C-Win
PROCEDURE Cap.VPE.AfterMail .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Result
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Result AS INTEGER NO-UNDO.

setDirectory(curr-dir).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cap C-Win
PROCEDURE Cap.VPE.BeforeMail .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
  /* Use of VPE mail choice changes the Progress current directory.
     We need to get this and then change it back in the AfterMail trigger.*/
  curr-dir = getDirectory().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clr C-Win
PROCEDURE Clr.VPE.AfterMail .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Result
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Result AS INTEGER NO-UNDO.

setDirectory(curr-dir).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clr C-Win
PROCEDURE Clr.VPE.BeforeMail .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  /* Use of VPE mail choice changes the Progress current directory.
     We need to get this and then change it back in the AfterMail trigger.*/
  curr-dir = getDirectory().


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Auto_Page_Break
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Auto_Page_Break C-Win
ON CHOOSE OF MENU-ITEM m_Auto_Page_Break /* Auto Page Break */
DO:
   DEF VAR block AS CHAR NO-UNDO.
   DEF VAR s AS CHAR NO-UNDO.
   IF NOT ok2open(chAuto) THEN RETURN.

   IF SEARCH("vpedemo.w") = ? THEN DO:
      MESSAGE "Source file 'VPEDEMO.W' not found." VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.
   
   /*CM_chAuto:Enabled = False*/
   /*chAuto:Visible = True.*/
   chAuto:REFRESH.
   
   INPUT FROM VALUE(SEARCH("vpedemo.w")).
   
   REPEAT:
     IMPORT UNFORMATTED s.
     
     block = block + CHR(10) + s.
     /* Don't blow up the 32k progress variable limit */
     IF SEEK(INPUT) GT 28000 THEN LEAVE.
   END.
   INPUT CLOSE.
   
   
   MESSAGE "We loaded the file into a Progress variable using IMPORT.~n" + 
   "NOW we run VPE and create a document from the data.~n" + 
   "VPE will create the page breaks itself. This will work very fast!"
   view-as alert-box.
   
   chAuto:OpenDoc.
   chAuto:SelectFont("Courier New", 10).

   /* Set the bottom margin, so the report will fit
    * onto A4 as well as onto US-Letter paper:
    * ============================================== */
   chAuto:SetOutRect(200, 200, 1900, 2650).

   /*Rem Header will be placed outside default output rectangle:*/
   chAuto:NoPen.
   chAuto:TextUnderline = TRUE.
   chAuto:DefineHeader(100, 100, -700, -50, "Auto Text Break Demo - Page @PAGE").

   /*Rem On every intial page:
 *    Rem VLEFT   = VLEFTMARGIN
 *    Rem VTOP    = VTOPMARGIN
 *    Rem VRIGHT  = VRIGHTMARGIN
 *    Rem VBOTTOM = VTOPMARGIN !!!!!!!!!!*/
   chAuto:TextUnderline = FALSE.
   chAuto:SetPen(3, {&psSolid}, 0).
   chAuto:WriteBox({&VLEFT}, {&VBOTTOM}, {&VRIGHT}, {&VFREE}, "[N TO BC LtGray CE S 12 B]Start of Listing").
   chAuto:WriteBox({&VLEFT}, {&VBOTTOM}, {&VRIGHT}, {&VFREE}, block).
   chAuto:WriteBox({&VLEFT}, {&VBOTTOM}, {&VRIGHT}, {&VFREE}, "[N TO BC LtGray CE S 12 B]End of Listing").

   chAuto:Preview.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Background
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Background C-Win
ON CHOOSE OF MENU-ITEM m_Background /* Background */
DO:
   /*SYSTEM-DIALOG PRINTER-SETUP PORTRAIT UPDATE utskriv.*/
   
   RUN precision (1).
   
END.

  /* _UIB-CODE-BLOCK-END */
  &ANALYZE-RESUME


&Scoped-define SELF-NAME m_E-Mail_in_Background
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_E-Mail_in_Background C-Win
ON CHOOSE OF MENU-ITEM m_E-Mail_in_Background /* e-Mail in Background */
DO:
  RUN ReportTest (1).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_E-Mail_in_Background_withou
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_E-Mail_in_Background_withou C-Win
ON CHOOSE OF MENU-ITEM m_E-Mail_in_Background_withou /* e-Mail in Background without Dialog */
DO:
  RUN ReportTest (2).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Exit C-Win
ON CHOOSE OF MENU-ITEM m_Exit /* Exit */
DO:
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Preview
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Preview C-Win
ON CHOOSE OF MENU-ITEM m_Preview /* Preview */
DO:
  RUN precision (0).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Preview2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Preview2 C-Win
ON CHOOSE OF MENU-ITEM m_Preview2 /* Preview */
DO:
  DEF VAR range AS INT INIT 1400 NO-UNDO.
  DEF VAR step AS INT INIT 1 NO-UNDO.
  DEF VAR color_step AS INT INIT 2 NO-UNDO.
  DEF VAR min_color AS INT INIT 0 NO-UNDO.
  DEF VAR max_color AS INT INIT 255 NO-UNDO.

  DEF VAR X AS INT NO-UNDO.
  DEF VAR Y AS INT NO-UNDO.
  DEF VAR r AS INT NO-UNDO.
  DEF VAR g AS INT NO-UNDO.
  DEF VAR b AS INT NO-UNDO.
  DEF VAR delta_g AS INT NO-UNDO.
  DEF VAR delta_r AS INT NO-UNDO.
  DEF VAR delta_b AS INT NO-UNDO.
  DEF VAR xx AS DEC NO-UNDO.
  DEF VAR factor AS DEC NO-UNDO.
  
  IF NOT ok2open(chClr) THEN RETURN.
  
  
 /* CM_COLORS_PREVIEW.Enabled = False
 *   CM_CLOSECOLORS.Enabled = True*/
  chClr:OpenDoc.
  chClr:AutoBreakMode = {&AUTO_BREAK_NO_LIMITS}.
  chClr:NoPen.
  chClr:FontName = "Arial".
  chClr:FontSize = 30.
  SESSION:SET-WAIT-STATE("general").

  xx = -3.1415.
  factor = 2 * ABS(xx) / range * step.

  r = 192.
  delta_r = color_step.
  g = min_color + 1.
  delta_g = color_step.
  b = min_color + 1.
  delta_b = color_step.

  X = 100.
  DO WHILE X < range + 100:
   Y = sine(xx) * 500 + 500.
   chClr:TextColor = RGB-VALUE(r, g, b).
   chClr:WriteBox(X, Y, X + 800, Y + 120, "Color Test").

   IF (X MOD 10 = 0) THEN DO:
      xx = xx + factor.
      X = X + step.
      Y = sine(xx) * 500 + 500.
      chClr:TextColor = {&COLOR_BLACK}.
      chClr:WriteBox(X, Y, X + 800, Y + 120, "Color Test").
   END.

   xx = xx + factor.
   IF (r > min_color) AND (r < max_color) THEN DO:
      r = r + delta_r.
      IF (r < min_color) THEN
         r = min_color.
      
      IF (r > max_color) THEN
         r = max_color.
      
   END.
   ELSE IF (g > min_color) AND (g < max_color) THEN DO:
      g = g + delta_g.
      IF (g < min_color) THEN
         g = min_color.
      
      IF (g > max_color) THEN
         g = max_color.
      
   END.
   ELSE IF (b > min_color) AND (b < max_color) THEN DO:
      b = b + delta_b.
      IF (b < min_color) THEN
         b = min_color.
      
      IF (b > max_color) THEN
         b = max_color.
      
   END.

   IF (r >= max_color) AND (b >= max_color) THEN DO:
      delta_r = color_step * -1.
      r = max_color - 1.
   END.
   IF (r >= min_color) AND (g >= max_color) THEN DO:
      delta_g = color_step * -1.
      g = max_color - 1.
   END.
   IF (g = min_color) AND (b >= max_color) THEN DO:
      delta_b = color_step * -1.
      b = max_color - 1.
   END.

   IF (r = min_color) AND (g = min_color) AND (b = min_color) THEN
   ASSIGN
      r = min_color + 1
      g = min_color + 1
      b = min_color + 1
      delta_r = color_step
      delta_g = color_step
      delta_b = color_step.
   
   
   X = X + step.
  END. /* do while loop */
  SESSION:SET-WAIT-STATE("").
   chClr:PreviewDoc(0, 0, {&VFREE}, {&VFREE}, {&VPE_SHOW_NORMAL}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Preview3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Preview3 C-Win
ON CHOOSE OF MENU-ITEM m_Preview3 /* Preview */
DO:
  RUN ReportTest (0).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Speed__Tables
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Speed__Tables C-Win
ON CHOOSE OF MENU-ITEM m_Speed__Tables /* Speed + Tables */
DO:
  DEF VAR s AS CHAR NO-UNDO.
  DEF VAR NAME AS CHAR NO-UNDO INIT "Test Document".
  DEF VAR vyear AS CHAR NO-UNDO.
  DEF VAR vtable AS CHAR NO-UNDO.
  DEF VAR vpage AS INT NO-UNDO.
  DEF VAR footer_ok AS LOG NO-UNDO.
  DEF VAR read_ahead AS LOG NO-UNDO.
  DEF VAR v-tax AS CHAR NO-UNDO.
  
  IF NOT ok2open(chSpeed) THEN RETURN.
 
  /* If this is the first time the program is run, load the data */
  IF NOT CAN-FIND(FIRST data-table) THEN RUN data_load.
  IF RETURN-VALUE = "fail" THEN RETURN.
   chSpeed:OpenDoc.
   chSpeed:DevJobName = "My own Job Name".
   chSpeed:OpenProgressBar.
   chSpeed:Preview.
   
   /* Set the bottom margin, so the report will fit
    * onto A4 as well as onto US-Letter paper:
    * ============================================== */
   chSpeed:SetOutRect(200, 200, 1900, 2650).

   chSpeed:AutoBreakMode = {&AUTO_BREAK_NO_LIMITS}.
   chSpeed:BkgMode = {&VBKG_SOLID}.
   
   vyear = "1999".
      
   chSpeed:NoPen.
    chSpeed:SetFontAttr({&ALIGN_CENTER}, False, True, False, False).
    chSpeed:SelectFont("Arial", 16).
    chSpeed:WriteBox({&VLEFTMARGIN}, {&VTOPMARGIN}, {&VRIGHTMARGIN}, {&VFREE}, "Journal " + vyear + "~n").
    chspeed:nbottom = chspeed:nbottom + 50.
    chSpeed:PenSize = 3.
    chSpeed:SetFontAttr({&ALIGN_LEFT}, False, False, False, False).
    chSpeed:SelectFont("Arial", 11).

   If detail_height = 0 Then do:
     /*Rem determine height of one detail line only once for better performance:
 *      Rem =====================================================================*/
      chSpeed:RenderPrintBox(0, 0, "x").
     detail_height = chSpeed:nRenderHeight * -1.
   End.
   /*Rem process variable tables:
 *    Rem ========================*/
   footer_ok = False.
   read_ahead = False.
   vpage = 1.
 
 
   FOR EACH data-table NO-LOCK BREAK BY table-no BY rec-no:
     IF FIRST-OF(table-no) THEN DO:
       printheader("Table " + STRING(data-TABLE.table-no)).
       sum_amount = 0.
       sum_prorated = 0.
       sum_tax = 0.
     END.
     /* The user is browsing this report while we are generating it.
      * Call DispatchAllMessages() to hold the Preview "alive".
      * ============================================================*/
       /* The Preview or the Parent-Window was closed by the user,
        * therefore exit this function immediately:
        * ========================================================*/
     IF chSpeed:DispatchAllMessages THEN RETURN.
     
     sum_amount = sum_amount + data-table.rec-amount.
     sum_prorated = sum_prorated + data-table.prorated.
     sum_tax = sum_tax + data-table.tax.
     chSpeed:WriteBox({&VLEFTMARGIN}, {&VBOTTOM}, -200, detail_height, STRING(data-table.rec-no)).
     chSpeed:WriteBox({&VRIGHT}, {&VTOP}, -220, {&VBOTTOM}, STRING(data-table.rec-date,"99/99/9999")).
     chSpeed:TextAlignment = {&ALIGN_RIGHT}.
     chSpeed:WriteBox({&VRIGHT}, {&VTOP}, -250, {&VBOTTOM}, TRIM(STRING(data-table.rec-amount,">>,>>>,>>9.99"))).
     chSpeed:WriteBox({&VRIGHT}, {&VTOP}, -250, {&VBOTTOM}, TRIM(STRING(data-table.prorated,">>,>>>,>>9.99"))).
     chSpeed:WriteBox({&VRIGHT}, {&VTOP}, -250, {&VBOTTOM}, TRIM(STRING(data-table.tax,">>,>>>,>>9.99"))).
     chSpeed:TextAlignment = {&ALIGN_LEFT}.
     chSpeed:SelectFont("Arial", 6).
     chSpeed:WriteBox({&VRIGHT}, {&VTOP}, {&VRIGHTMARGIN}, {&VBOTTOM}, data-table.remark).
     chSpeed:SelectFont("Arial", 11).
     chSpeed:TransparentMode = FALSE.
     
     
     /*Is the room to the page bottom large enough for a new table ?
 *      To print a new table on the same page, we need the following space available:
 *      1 cm distance to the previous table, PLUS the Group-Title height, PLUS
 *      the Group-Header height PLUS the Group-Footer height PLUS the height for
 *      at least 2 detail-lines PLUS the height for another detail-line as gap to
 *      the page footer. Check, if enough space is available:
 *      We use negative signs, because we had set detail_height and group_title_height
 *      to negative values.
 *      ==============================================================================*/
     
     
     IF chSpeed:nBottom - 2 * detail_height - group_title_height > chSpeed:nBottomMargin THEN
     DO:
 
       footer_ok = FALSE.
       PrintFooter().
       PrintPageFooter (NAME).
 
       chSpeed:PageBreak.
       vpage = vpage + 1.
 
       /* If we're on the last one and we just ran out of room,
          don't print a header on the next page */
       IF NOT LAST-OF(table-no) THEN
       PrintHeader ("Table " + STRING(data-TABLE.table-no)).
 
 
       chSpeed:StatusProgress = vpage * 100 / 120.
     END.
     ELSE
     IF LAST-OF(table-no) THEN DO:
       
       
       v-tax = STRING(sum_tax,">>>,>>>,>>9.99").
       printfooter().
     
       IF chSpeed:nBottomMargin - chSpeed:nBottom < -(-100 + headline_height + 2 * group_title_height + 3 * detail_height) 
       THEN DO:
   
          PrintPageFooter(NAME).
          chSpeed:PageBreak.
          vpage = vpage + 1.
       END.
       ELSE DO:
   
          IF chSpeed:nBottom > chSpeed:nTop THEN
             chSpeed:nBottom = chSpeed:nBottom + 100.
       END.             
     END.
   END.
   chSpeed:CurrentPage = 1.
   chSpeed:PrintBox(200, 100, "[N BC LtRed B]Generated " + String(chSpeed:PageCount) + " page report from Progress database as data source!").
   
   chSpeed:TextAlignment = {&ALIGN_RIGHT}.
   chSpeed:TextItalic = True.
   DO vpage = 1 TO chSpeed:PageCount:
      chSpeed:CurrentPage = vpage.
      chSpeed:WRITE(chSpeed:nRightMargin - 500, {&VBOTTOMMARGIN}, {&VRIGHTMARGIN}, detail_height, "Journal Page " + STRING(vpage) + " of " + STRING(chSpeed:PageCount)).
   END.

   chSpeed:RefreshDoc.
   chSpeed:CloseProgressBar.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Welcome
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Welcome C-Win
ON CHOOSE OF MENU-ITEM m_Welcome /* Welcome */
DO:
   IF NOT ok2open(chWelcome) THEN RETURN.
   
   DEF VAR s AS CHAR.   
   MESSAGE "This demo shows images in true-color, which might not look that good in other color resolutions."
   VIEW-AS ALERT-BOX.
   
   chWelcome:OpenDoc.
   chWelcome:DevJobName = "Welcome".
   chWelcome:PreviewDoc(-1, 0, 0, 0, {&VPE_SHOW_MAXIMIZED}).
   chWelcome:OpenProgressBar.

   /*Set PenSize = 0 to avoid frames drawn around the images:*/
   chWelcome:NoPen.
   chWelcome:Picture(0, 0, 1900, 1350, wood, {&PIC_KEEPIMAGE}).
   chWelcome:PenSize = 3.
   chWelcome:VpePrint(50, 800, "[N 'Times New Roman' S 32 I U Rot 2700]The Story").
   chWelcome:StatusProgress = 16.

   chWelcome:Picture(100, 100, {&VFREE}, {&VFREE}, sand, {&PIC_KEEPIMAGE}).
   chWelcome:VpePrint(120, 50, "['Times New Roman' S 12]Most of the time, I walk around like that.").
   chWelcome:StatusProgress = 33.

   chWelcome:Picture(1000, 350, {&VFREE}, {&VFREE}, bau, {&PIC_KEEPIMAGE}).
   chWelcome:VpePrint(1020, 300, "My tools and components look like this.").
   chWelcome:StatusProgress = 50.

   chWelcome:Picture(250, 750, {&VFREE}, {&VFREE}, silence, {&PIC_KEEPIMAGE}).
   chWelcome:Write({&VLEFT}, {&VTOP}, {&VRIGHT}, {&VBOTTOM}, " Yes, I wish I could be there...~n Everything would be so easy.").
   chWelcome:Write(1100, 1100, -650, {&VFREE}, "[N 'Arial' S 10 C White]Well, we can't solve all of your problems, but in case of printing and on-screen presentations... (please turn the page)").
   chWelcome:StatusProgress = 66.

   chWelcome:PageBreak.
   chWelcome:NoPen.
   chWelcome:Picture(0, 0, 1900, 1750, wood, {&PIC_KEEPIMAGE}).
   chWelcome:PenSize = 3.
   chWelcome:Write(0, 100, 1900, {&VFREE}, "['Times New Roman' FontSize 36 Italic Underline Center]Welcome to a New Software:").
   chWelcome:StatusProgress = 83.
   chWelcome:Picture(350, 300, {&VFREE}, {&VFREE}, vpelogo, {&PIC_KEEPIMAGE}).
   chWelcome:StatusProgress = 100.
   chWelcome:CloseProgressBar.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Report
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Report C-Win
PROCEDURE Report.VPE.AfterMail .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Result
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Result AS INTEGER NO-UNDO.

setDirectory(curr-dir).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Report C-Win
PROCEDURE Report.VPE.BeforeMail .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  /* Use of VPE mail choice changes the Progress current directory.
     We need to get this and then change it back in the AfterMail trigger.*/
  curr-dir = getDirectory().


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Speed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Speed C-Win
PROCEDURE Speed.VPE.AfterMail .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Result
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Result AS INTEGER NO-UNDO.
setDirectory(curr-dir).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Speed C-Win
PROCEDURE Speed.VPE.BeforeMail .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  /* Use of VPE mail choice changes the Progress current directory.
     We need to get this and then change it back in the AfterMail trigger.*/
  curr-dir = getDirectory().


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Welcome
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Welcome C-Win
PROCEDURE Welcome.VPE.AfterMail .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Result
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Result AS INTEGER NO-UNDO.

  setDirectory(curr-dir).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Welcome C-Win
PROCEDURE Welcome.VPE.BeforeMail .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  /* Use of VPE mail choice changes the Progress current directory.
     We need to get this and then change it back in the AfterMail trigger.*/
  curr-dir = getDirectory().


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  
  IF chCap:isopen THEN chCap:closedoc.
  IF chWelcome:isopen THEN chWelcome:closedoc.
  IF chSpeed:isopen THEN chSpeed:closedoc.
  IF chClr:isopen THEN chClr:closeDoc.
  IF chReport:isopen THEN chReport:closedoc.
  IF chAuto:isopen THEN chAuto:closedoc.

  RUN disable_UI.
END.
/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "vpedemo.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chAuto = Auto:COM-HANDLE
    UIB_S = chAuto:LoadControls( OCXFile, "Auto":U)
    chCap = Cap:COM-HANDLE
    UIB_S = chCap:LoadControls( OCXFile, "Cap":U)
    chClr = Clr:COM-HANDLE
    UIB_S = chClr:LoadControls( OCXFile, "Clr":U)
    chReport = Report:COM-HANDLE
    UIB_S = chReport:LoadControls( OCXFile, "Report":U)
    chSpeed = Speed:COM-HANDLE
    UIB_S = chSpeed:LoadControls( OCXFile, "Speed":U)
    chWelcome = Welcome:COM-HANDLE
    UIB_S = chWelcome:LoadControls( OCXFile, "Welcome":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "vpedemo.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE data_load C-Win 
PROCEDURE data_load :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-numformat AS CHAR.
  DEF VAR v-date-format AS CHAR.
  DEF VAR i AS INT NO-UNDO.
  v-numformat = SESSION:NUMERIC-FORMAT.
  v-date-format = SESSION:DATE-FORMAT.
  
  MESSAGE 
  "Since this is the first time running this demo," SKIP
  "we need to generate random data in the database to" SKIP
  "be used by this report." SKIP
  
  VIEW-AS ALERT-BOX INFORMATION.
  SESSION:SET-WAIT-STATE("general").
    
  
  DO i = 1 TO 3500:
    
    
    CREATE data-table.
    ASSIGN
      data-table.prorated = RANDOM(10000,10000000) / 100
      data-table.rec-amount = RANDOM(10000,10000000) / 100
      data-table.rec-date = 01/01/1998 + RANDOM(1,1100)
      data-table.rec-no = i
      data-table.table-no = RANDOM(1,300)
      data-table.tax = data-table.rec-amount * (RANDOM(5,17) / 100).
  
  
  END.
  
 
 /* data-table.prorated data-table.rec-amount data-table.rec-date data-table.rec-no data-table.remark data-table.table-no data-table.tax*/
  
  MESSAGE
  "Data is fully loaded, the demo will proceed."
  VIEW-AS ALERT-BOX INFORMATION.
  SESSION:SET-WAIT-STATE("").
  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE draw3dframe C-Win 
PROCEDURE draw3dframe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER doc AS COM-HANDLE.
  DEF INPUT PARAMETER X AS INT.
  DEF INPUT PARAMETER Y AS INT.
  DEF INPUT PARAMETER x2 AS INT.
  DEF INPUT PARAMETER y2 AS INT.
  DEF INPUT PARAMETER gap AS LOG.
  DEF INPUT PARAMETER sunken AS LOG.
  DEF INPUT PARAMETER pen_size AS INT.
  DEF VAR vcolor AS INT.

   IF gap THEN DO:
      X = X - 15.
      Y = Y - 15.
      x2 = x2 + 15.
      y2 = y2 + 15.
   END.
   IF sunken THEN 
      vcolor = {&COLOR_DKGRAY}.
   ELSE
      vcolor = {&COLOR_WHITE}.
   
   doc:SetPen(pen_size, {&psSolid}, vcolor).
   doc:VpeLine(X, Y, x2, Y).
   doc:VpeLine(X, Y, X, y2).

   IF sunken THEN
      doc:PenColor = {&COLOR_WHITE}.
   ELSE
      doc:PenColor = {&COLOR_DKGRAY}.
   
   doc:VpeLine(x2, Y, x2, y2).
   doc:VpeLine(X, y2, x2, y2).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  RUN control_load.
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE external_procs C-Win 
PROCEDURE external_procs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*  external procedures defined here */
END PROCEDURE.
PROCEDURE sin EXTERNAL "MSVCRT40.DLL" CDECL:
    DEFINE INPUT  PARAMETER dblValue  AS DOUBLE NO-UNDO.
    DEFINE RETURN PARAMETER dblResult AS DOUBLE NO-UNDO.
END PROCEDURE.
PROCEDURE GetCurrentDirectoryA EXTERNAL "KERNEL32.DLL":
    DEFINE INPUT        PARAMETER intBufferSize AS LONG.
    DEFINE INPUT-OUTPUT PARAMETER ptrToString   AS MEMPTR.
    DEFINE RETURN       PARAMETER intResult     AS SHORT.
END PROCEDURE.
PROCEDURE SetCurrentDirectoryA EXTERNAL "KERNEL32.DLL":
    DEFINE INPUT  PARAMETER chrCurDir AS CHARACTER.
    DEFINE RETURN PARAMETER intResult AS LONG.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls C-Win 
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  chWelcome = chWelcome:VPE.
  chCap = chCap:VPE.
  chSpeed = chSpeed:VPE.
  chClr = chClr:VPE.
  chReport = chReport:VPE.
  chAuto = chAuto:VPE.
  
  If chCap:Edition = {&VEDITION_STANDARD} Then
  ASSIGN
    /*The Standard Edition is delivered with all example images as BMP's,
      because it is not able to read JPG, TIF, PNG, etc.*/
    WOOD = datapath + "\wood.bmp"
    SAND = datapath + "\sand.bmp"
    BAU = datapath + "\bau.bmp"
    SILENCE = datapath + "\silence.bmp"
    MARBLE = datapath + "\marble.bmp"
    PROD = datapath + "\product.bmp"
    VPELOGO = datapath + "\vpelogo.bmp".
  Else ASSIGN
    WOOD = datapath + "\wood.jpg"
    SAND = datapath + "\sand.jpg"
    BAU = datapath + "\bau.jpg"
    SILENCE = datapath + "\silence.jpg"
    MARBLE = datapath + "\marble.jpg"
    PROD = datapath + "\product.png"
    VPELOGO = datapath + "\vpelogo.png".
  
  
  
  
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE page1 C-Win 
PROCEDURE page1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR Y AS INT.
   DEF VAR oldy AS INT.
   DEF VAR node_x AS INT.
   DEF VAR node_y AS INT.
   DEF VAR vtext AS CHAR.

   
   chCap:BkgMode = {&VBKG_SOLID}.
   chCap:BkgColor = {&COLOR_LTGRAY}.
   chCap:Box(100, 100, 2000, 2700).
   chCap:BkgColor = {&COLOR_BLACK}.
   chCap:Box(106, 2700, 2006, 2706).
   chCap:Box(2000, 106, 2006, 2706).
   chCap:BkgMode = {&VBKG_TRANSPARENT}.

   chCap:Picture(300, 200, 1800, -650, marble, {&PIC_KEEPIMAGE}).
   chCap:WriteBox({&VLEFT}, {&VTOP}, {&VRIGHT}, {&VFREE}, "['Times New Roman' S 26 B U CE PS 0]Capabilities and Precision").

   vtext = "[S 18 UO IO]This demo shows, what you can do with VPE - the Virtual Print Engine." + Chr(10).
   vtext = vtext + "VPE is a hammer! It solves all your printing problems, cooperates perfectly with your development tools ".
   vtext = vtext + "and has a real big advantage in contrast to most available components:" + Chr(10).
   vtext = vtext + "It is easy, fast and robust!".
   chCap:WriteBox(chCap:nLeft + 30, {&VBOTTOM}, chCap:nRight - 30, {&VFREE}, vtext).
   chCap:textColor = {&COLOR_WHITE}.
   chCap:WriteBox(chCap:nLeft - 3, chCap:nTop - 3, chCap:nRight - 3, {&VFREE}, vtext).
   RUN Draw3DFrame(chCap, 300, 200, 1800, 850, TRUE, FALSE, 6).

   oldy = 975.
   y = chCap:Print(450, oldy, "[S 12 B U L C Blue]What is the Virtual Print Engine?").
   chCap:StoreSet (HEADLINE).
   vtext = "[S 11 BO UO C Black]You""re still watching it. ".
   vtext = vtext + "VPE is a complete new way to solve your printing and presentation problems." + Chr(10).
   vtext = vtext + "It allows you the dynamic generation of absolutely free screen and printer output ".
   vtext = vtext + "by calling functions during the runtime of your applications." + Chr(10).
   vtext = vtext + "The concept behind it makes it as easy, as if you were using ".
   vtext = vtext + "a flat DOS screen, whilst offering you the full power of Windows.".
   y = chCap:Write({&VLEFT}, y + 20, -1200, {&VFREE}, vtext).

   chCap:SetPen(3, {&psSolid}, {&COLOR_BLACK}).
   chCap:StoreSet (BODYTEXT).
   run Draw3DFrame(chCap, 450, oldy, chCap:nRight, y, True, True, 3).
   oldy = y + 200.
   chCap:SetPen(6, {&psSolid}, {&COLOR_LTRED}).
   chCap:VpeLine(1050, y + 20, 1050, y + 100).
   chCap:VpeLine(1050, y + 100, 1600, y + 100).
   chCap:VpeLine(1600, y + 100, 1600, oldy - 25).
   chCap:VpeLine(1050, y + 100, 700, y + 100).
   chCap:VpeLine(700, y + 100, 700, oldy - 25).
   node_x = 1050.
   node_y = y + 100.

   chCap:UseSet (HEADLINE).
   y = chCap:Print(1300, oldy, "Can I try it?").
   chCap:UseSet (BODYTEXT).
   vtext = "[J]Of course! This demo contains the complete VPE-SDK, including the manuals ".
   vtext = vtext + "as helpfiles, the DLL, OCX, VCL and over 600 KB of demo source code for ".
   vtext = vtext + "all common programming tools / languages.".
   y = chCap:Write({&VLEFT}, y + 20, -600, {&VFREE}, vtext).
   run Draw3DFrame(chCap, 1300, oldy, chCap:nRight, y, True, True, 3).

   chCap:UseSet (HEADLINE).
   y = chCap:Print(200, oldy, "When and why should I use VPE?").
   chCap:UseSet (BODYTEXT).
   vtext = "In all cases of printing and data presentation, like:" + Chr(10).
   vtext = vtext + "• complex dynamic documents and reports" + Chr(10).
   vtext = vtext + "• drawing plans, graphs, diagrams, etc." + Chr(10).
   vtext = vtext + "• filling-in forms exactly and printer-independent" + Chr(10).
   vtext = vtext + "• high performance printing" + Chr(10).
   vtext = vtext + "• using images and barcodes of all common types".
   y = chCap:Write({&VLEFT}, y + 20, -800, {&VFREE}, vtext).
   run Draw3DFrame(chCap, 200, oldy, chCap:nRight, y, True, True, 3).

   oldy = y + 125.
   chCap:SetPen(6, {&psSolid}, {&COLOR_LTRED}).
   chCap:VpeLine(node_x, node_y, node_x, oldy - 25).
   chCap:UseSet (HEADLINE).
   y = chCap:Print(500, oldy, "What are the key features of VPE?").
   chCap:UseSet (BODYTEXT).
   vtext = "• exact and printer independent positioning, all coordinates in 0.1 mm" + Chr(10).
   vtext = vtext + "• supports lines, polylines, polygons, ellipses, boxes, frames, vtext, true-color," + Chr(10).
   vtext = vtext + "  hatching, BMP, WMF, EMF, JPEG, PNG, TIFF, GIF, PCX, DXF and" + Chr(10).
   vtext = vtext + "  21 barcode types" + Chr(10).
   vtext = vtext + "• MEGA-FAST vector graphics, SUPER-EASY to use like a DOS screen" + Chr(10).
   vtext = vtext + "• unlimited number of simultaneously open documents and pages" + Chr(10).
   vtext = vtext + "• optional free scalable WYSIWYG preview" + Chr(10).
   vtext = vtext + "• the preview can be embedded into your own application's windows (like" + Chr(10).
   vtext = vtext + "  this one), or handled automatically by VPE (like the 'Welcome' demo)" + Chr(10).
   vtext = vtext + "• intelligent printer setup" + Chr(10).
   vtext = vtext + "• toolbar, statusbar, rulers, measuring: everything can be modified" + Chr(10).
   vtext = vtext + "• POWERFUL dynamic layout functions" + Chr(10).
   vtext = vtext + "• and many more...".
   y = chCap:Write({&VLEFT}, y + 20, -1200, {&VFREE}, vtext).
   RUN Draw3DFrame(chCap, 500, oldy, chCap:nRight, y, True, True, 3).
   chCap:StatusProgress = 20.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE page2 C-Win 
PROCEDURE page2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR Y AS INT NO-UNDO.
  DEF VAR vindex AS INT NO-UNDO.
  DEF VAR vskip AS INT NO-UNDO.
  DEF VAR vfirst AS INT NO-UNDO.
  DEF VAR xx AS INT NO-UNDO.
  DEF VAR oldy AS INT NO-UNDO.
  DEF VAR segments AS INT NO-UNDO.
  DEF VAR p AS INT NO-UNDO.
  DEF VAR xr AS DEC NO-UNDO.
  DEF VAR xstep AS DEC NO-UNDO.
  DEF VAR yr AS DEC NO-UNDO.
  DEF VAR X AS DEC NO-UNDO.
  DEF VAR s AS character NO-UNDO.
   
     
   chCap:PageBreak.
   chCap:UseSet(HEADLINE).
   chCap:VpePrint(200, 200, "['Arial' C Black BO I S 14]An example of drawing (better to turn the grid off here):").
   chCap:StoreSet(HEADLINE).
   chCap:SetPen(8, {&psSolid}, {&COLOR_BLACK}).
   chCap:Box(200, 300, 1700, 1800).
        
   xr = 6.
   xstep = xr / 750.
   yr = 18.
   segments = 1.

   /*Rem The following graphs are created with chCap:AddPolyPoint()
 *    Rem Note, that all routines contain complex handling of clipping.*/
   x = xr * -1.
   vskip = 0.
   chCap:SetPen(2, {&psSolid}, {&COLOR_BLUE}).
   p = chCap:PolyLine(1500).
   
   DO xx = 200 TO 1699:
      Y = 1050 - (EXP(X,3) - 2 * EXP(X,2) - 8 * X) / (yr / 750).
      X = X + xstep.
      
      IF Y < 300 THEN
         vskip = 1.
      ELSE IF Y > 1800 THEN
         vskip = 1.
      ELSE IF vskip = 1 THEN DO:
         chCap:AddPolyPoint(p, -1, -1).
         oldy = Y.
         vskip = 2.
      END.
      ELSE DO:
         IF vskip = 2 THEN DO:
            chCap:AddPolyPoint(p, xx - 1, oldy).
            vskip = 0.
         END.
         chCap:AddPolyPoint(p, xx, Y).
         segments = segments + 1.
      END.
   END.


   x = xr * -1.
   vskip = 0.
   chCap:SetPen(2, {&psSolid}, {&COLOR_RED}).
   p = chCap:PolyLine(1500).
   
   DO xx = 200 TO 1699:
      Y = 1050 - (3 * EXP(X,2) - 4 * X - 8) / (yr / 750).
      X = X + xstep.
      
      IF Y < 300 THEN
         vskip = 1.
      ELSE IF Y > 1800 THEN
         vskip = 1.
      ELSE IF vskip = 1 THEN DO:
         chCap:AddPolyPoint(p, -1, -1).
         oldy = Y.
         vskip = 2.
      END.
      ELSE DO:
         IF vskip = 2 THEN DO:
            chCap:AddPolyPoint(p, xx - 1, oldy).
            vskip = 0.
         END.
         chCap:AddPolyPoint(p, xx, Y).
         segments = segments + 1.
      END.
   END.   
   
   x = xr * -1.
   vskip = 0.
   chCap:SetPen(2, {&psSolid}, {&COLOR_GREEN}).
   p = chCap:PolyLine(1500).
   
   DO xx = 200 TO 1699:
      Y = 1050 - (3 * X - 4) / (yr / 750).
      X = X + xstep.
      
      IF Y < 300 THEN
         vskip = 1.
      ELSE IF Y > 1800 THEN
         vskip = 1.
      ELSE IF vskip = 1 THEN DO:
         chCap:AddPolyPoint(p, -1, -1).
         oldy = Y.
         vskip = 2.
      END.   
      ELSE DO:
         IF vskip = 2 THEN DO:
            chCap:AddPolyPoint(p, xx - 1, oldy).
            vskip = 0.
         END.
         chCap:AddPolyPoint(p, xx, Y).
         segments = segments + 1.
      END.
   END.

   chCap:SetPen(3, {&psSolid}, {&COLOR_BLACK}).
   chCap:VpeLine(200, 1050, 1700, 1050).
   chCap:VpeLine(950, 300, 950, 1800).
   chCap:SelectFont("Arial", 10).
   chCap:SetFontAttr({&ALIGN_LEFT}, 0, 0, 0, 0).
   chCap:SetPen(1, {&psDot}, {&COLOR_BLACK}).
   
   xx = 1.
   DO WHILE xx < xr:
      chCap:VpeLine(950 + xx * 750 / 6, 300, 950 + xx * 750 / 6, 1800).
      chCap:VpePrint(960 + xx * 750 / 6, 1050, STRING(xx)).
      chCap:VpeLine(950 - xx * 750 / 6, 300, 950 - xx * 750 / 6, 1800).
      chCap:VpePrint(960 - xx * 750 / 6, 1050, STRING(xx * -1)).
      xx = xx + 1.
   END.
   Y = 2.
   DO WHILE Y < yr:
      chCap:VpeLine(200, 1050 + Y * 750 / yr, 1700, 1050 + Y * 750 / yr).
      chCap:VpePrint(960, 1050 - Y * 750 / yr, STRING(Y)).
      chCap:VpeLine(200, 1050 - Y * 750 / yr, 1700, 1050 - Y * 750 / yr).
      chCap:VpePrint(960, 1050 + Y * 750 / yr, STRING(Y * -1)).
      Y = Y + 2.
   END.

   y = 1850.
   chCap:VpeWrite(200, y, 2000, -1, "[S 14]The three graphs together consist of " + String(segments - 1) + " (number determined during runtime) single lines!" + Chr(10) + Chr(10) + "VPE manages this data bulk for you FAST!").
   chCap:StatusProgress = 40.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE page3 C-Win 
PROCEDURE page3 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR X AS INT NO-UNDO.
  DEF VAR Y AS INT NO-UNDO.
  DEF VAR w AS INT NO-UNDO.
  DEF VAR h AS INT NO-UNDO.
  DEF VAR offset AS INT NO-UNDO.
  DEF VAR numcols AS INT NO-UNDO.
  DEF VAR vcol AS INT NO-UNDO.  
  DEF VAR ROW AS INT NO-UNDO.
  DEF VAR ITEM AS INT NO-UNDO.
  DEF VAR gap AS INT NO-UNDO.
  DEF VAR boxwidth AS INT NO-UNDO.
  DEF VAR boxheight AS INT NO-UNDO.

   item = 1.
   gap = 100.
   numcols = 5.
   chCap:PageBreak.
   chCap:PageOrientation = {&VORIENT_LANDSCAPE}.
   chCap:UseSet (HEADLINE).
   chCap:VpePrint(200, 150, "A Diagram on a Landscape Oriented Page in the Middle of the Document:").
   chCap:UseSet (BODYTEXT).
   chCap:AutoBreakMode = {&AUTO_BREAK_NO_LIMITS}.
   chCap:FontName = "Arial".
   chCap:VpePrint(200, chCap:nBottomMargin + 75, "Note: if this page is not printed in landscape orientation, your printer driver is not capable of switching the orientation in the middle of a print job.").
   chCap:TextBold = True.

   w = chCap:PageWidth - 500.
   h = chCap:PageHeight - 500.

   chCap:BkgMode = {&VBKG_SOLID}.
   chCap:BkgColor = RGB-VALUE(190, 255, 190).

   DO vcol = 0 TO numcols - 1:
      /*For row = 0 To 2 ^ vcol - 1*/
      DO ROW = 0 TO EXP(2,vcol) - 1:
         /*Rem draw the textbox:*/
         X = CompX(w, numcols, vcol).
         Y = CompY(h, vcol, ROW).
         IF vcol < numcols - 1 THEN
           ASSIGN
            boxwidth = 300
            boxheight = 150
            offset = 0.
         ELSE
           ASSIGN
            boxwidth = 400
            boxheight = 75
            offset = (150 - boxheight) / 2.
         

         IF vcol < numcols - 1 THEN DO:
            /*Rem Draw connecting lines to both succeeding objects:*/
            chCap:PenSize = 5.
            chCap:VpeLine(X + boxwidth, Y + boxheight / 2, X + boxwidth + gap, Y + boxheight / 2).
            chCap:VpeLine({&VRIGHT}, {&VTOP}, {&VRIGHT}, CompY(h, vcol + 1, ROW * 2) + boxheight / 2).
            chCap:VpeLine({&VRIGHT}, {&VTOP}, CompX(w, numcols, vcol + 1), {&VTOP}).
            chCap:VpeLine(X + boxwidth + gap, Y + boxheight / 2, X + boxwidth + gap, CompY(h, vcol + 1, ROW * 2 + 1) + boxheight / 2).
            chCap:VpeLine({&VRIGHT}, {&VBOTTOM}, CompX(w, numcols, vcol + 1), {&VBOTTOM}).
            chCap:PenSize = 3.
         END.
         chCap:WriteBox(X, Y + offset, boxwidth * -1, boxheight * -1, "Diagram Item #" + STRING(ITEM)).
         ITEM = ITEM + 1.
       END.
     END.
   /*Rem Restore Color Settings:*/
   chCap:UseSet (HEADLINE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE page4_5 C-Win 
PROCEDURE page4_5 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR vtext AS CHAR NO-UNDO.
  DEF VAR Y AS INT NO-UNDO.
  DEF VAR s AS CHAR NO-UNDO.
  DEF VAR X AS INT NO-UNDO.
  DEF VAR BOTTOM AS INT NO-UNDO.
  

   y = 100.
   chCap:PageBreak.
   chCap:PageOrientation = {&VORIENT_PORTRAIT}.
   IF chCap:Edition <> {&VEDITION_STANDARD} THEN DO:
        chCap:NoPen.
        chCap:SetFontAttr({&ALIGN_CENTER}, 0, 1, 0, 0).
        chCap:SelectFont("Arial", 18).
        chCap:VpeWrite(0, 150, 2100, 300, "The supported barcode-types:").
        chCap:SelectFont("Arial", 10).
        chCap:TextBold = 1.
       
        chCap:VpeWrite(200, 300, 550, 400, "2 of 5:").
        chCap:Barcode(200, 360, 550, 560, {&BCT_2OF5}, "123789", "").
    
        chCap:VpeWrite(700, 300, 1200, 400, "Interleaved 2 of 5:").
        chCap:Barcode(700, 360, 1200, 560, {&BCT_INTERLEAVED2OF5}, "12389578348", "").
    
        chCap:VpeWrite(1350, 300, 1750, 400, "Code 39 (text on top):").
        chCap:SetBarcodeParms(1, 0).
        chCap:Barcode(1350, 360, 1750, 560, {&BCT_CODE39}, "ABC123", "").
    
        chCap:VpeWrite(200, 700, 550, 800, "Code 93 (rotated):").
       chCap:Rotation = 900.
        chCap:Barcode(275, 760, -300, -200, {&BCT_CODE93}, "DEF987", "").
    
        chCap:VpeWrite(700, 700, 1200, 800, "[Rot 0]Codabar (rotated):").
       chCap:Rotation = 1800.
        chCap:Barcode(700, 760, -500, -200, {&BCT_CODABAR}, "A123456B", "").
    
        chCap:VpeWrite(1400, 700, 1700, 800, "[Rot 0]EAN-8 (rotated):").
       chCap:Rotation = 2700.
        chCap:Barcode(1450, 760, -300, -200, {&BCT_EAN8}, "40167794", "").
    
        chCap:VpeWrite(200, 1200, 500, 1400, "[Rot 0]EAN-8 + 2:").
        chCap:SetBarcodeParms(0, 1).
        chCap:Barcode(200, 1260, 500, 1460, {&BCT_EAN8_2}, "12345670", "12").
    
        chCap:VpeWrite(700, 1200, 1200, 1400, "EAN-8 + 5:").
        chCap:SetBarcodeParms(0, 0).
        chCap:Barcode(700, 1260, 1200, 1460, {&BCT_EAN8_5}, "98765430", "12345").
    
        chCap:VpeWrite(1350, 1200, 1750, 1400, "EAN-13:").
        chCap:Barcode(1350, 1260, 1750, 1460, {&BCT_EAN13}, "9781556153952", "").
    
        chCap:VpeWrite(200, 1600, 600, 1800, "EAN-13 + 2:").
        chCap:Barcode(200, 1660, 600, 1860, {&BCT_EAN13_2}, "4501645096787", "12").
    
        chCap:VpeWrite(700, 1600, 1200, 1800, "EAN-13 + 5:").
        chCap:SetBarcodeParms(0, 1).
        chCap:Barcode(700, 1660, 1200, 1860, {&BCT_EAN13_5}, "9781556153952", "12345").
    
        chCap:VpeWrite(1350, 1600, 1750, 1800, "EAN-128 A:").
        chCap:SetBarcodeParms(0, 0).
        chCap:Barcode(1350, 1660, 1750, 1860, {&BCT_EAN128A}, "EAN-128 A", "").
    
        chCap:VpeWrite(200, 2000, 600, 2200, "EAN-128 B:").
        chCap:Barcode(200, 2060, 600, 2260, {&BCT_EAN128B}, "ean-128 b", "").
    
        chCap:VpeWrite(700, 2000, 1200, 2200, "EAN-128 C:").
        chCap:Barcode(700, 2060, 1200, 2260, {&BCT_EAN128C}, "128902", "").
    
        chCap:VpeWrite(1350, 2000, 1850, 2200, "POSTNET (1.20) 5 or 9 digits:").
        chCap:Barcode(1350, 2060, 1628, 2120, {&BCT_POSTNET}, "12345", "").
        chCap:Barcode(1350, 2150, 1850, 2210, {&BCT_POSTNET}, "414649623", "").    
        vtext = "[N UO L]Barcodes are as easily generated as images. An example:" + Chr(10) + Chr(10).
        vtext = vtext + "Barcode(700, 1660, 1200, 1860, {&BCT_EAN13_5}, ""9781556153952"", ""12345"")".
        chCap:VpePrint(200, 2400, vtext).
        chCap:StatusProgress = 60.
        chCap:PageBreak.
    
        chCap:SetFontAttr({&ALIGN_CENTER}, 0, 1, 0, 0).
        chCap:SelectFont("Arial", 18).
        chCap:VpeWrite(0, 150, 2100, 300, "The supported barcode-types (continued):").
        chCap:SelectFont("Arial", 10).
        chCap:TextBold = 1.
    
        chCap:VpeWrite(200, 300, 550, 400, "UPC-A:").
        chCap:Barcode(200, 360, 550, 560, {&BCT_UPCA}, "07447079382", "").
    
        chCap:VpeWrite(700, 300, 1100, 400, "UPC-A + 2:").
        chCap:Barcode(700, 360, 1100, 560, {&BCT_UPCA_2}, "07447079382", "01").
    
        chCap:VpeWrite(1350, 300, 1800, 400, "UPC-A + 5:").
        chCap:Barcode(1350, 360, 1800, 560, {&BCT_UPCA_5}, "03126764825", "94687").
    
        chCap:VpeWrite(200, 700, 550, 900, "UPC-E:").
        chCap:Barcode(200, 760, 550, 960, {&BCT_UPCE}, "0378492", "").
    
        chCap:VpeWrite(700, 700, 1100, 900, "UPC-E + 2:").
        chCap:Barcode(700, 760, 1100, 960, {&BCT_UPCE_2}, "0378492", "14").
    
        chCap:VpeWrite(1350, 700, 1800, 900, "UPC-E + 5:").
        chCap:Barcode(1350, 760, 1800, 960, {&BCT_UPCE_5}, "0364825", "79462").
    
       chCap:SetFontAttr({&ALIGN_LEFT}, False, False, False, False).
      vtext = "VPEngine supports 21 barcode types. Barcodes can be rotated in 90 degree steps, the ".
      vtext = vtext + "text can be drawn on bottom or top of the barcode, and also independently ".
      vtext = vtext + "the add-on text. Any of these features can be combined.".
      Y = chCap:WRITE(200, 975, 1800, {&VFREE}, vtext).
   END.
   
   
   chCap:BkgMode = {&VBKG_GRD_LINE}.
   chCap:BkgGradientEndColor = {&COLOR_BLUE}.
   chCap:PenStyle = {&psSolid}.
   s = "[PS 3 S 20 C Black]How about some gradients?".
   chCap:RenderPrintBox(200, y + 100, s).
   x = 1000 - chCap:nRenderWidth / 2.
   y = chCap:PrintBox(x, y + 70, s).

   y = y + 75.
   chCap:BkgMode = {&VBKG_GRD_ELLIPSE}.
   chCap:BkgGradientStartColor = {&COLOR_DKORANGE}.
   chCap:BkgGradientEndColor = {&COLOR_LTYELLOW}.
   y = chCap:WriteBox(100, y, -600, -600, "[CE S20 B C Blue]" + Chr(10) + Chr(10) + "Virtual" + Chr(10) + "Print" + Chr(10) + "Engine").

   chCap:BkgGradientStartColor = {&COLOR_BLUE}.
   chCap:BkgGradientEndColor = {&COLOR_CYAN}.
   chCap:BkgGradientRotation = 900.
   chCap:BkgMode = {&VBKG_GRD_LINE}.
   chCap:PrintBox(800, {&VTOP}, "[C White] The printing and presentation ").

   chCap:BkgMode = {&VBKG_GRD_RECT}.
   chCap:BkgGradientStartColor = {&COLOR_LTBLUE}.
   chCap:BkgGradientEndColor = {&COLOR_YELLOW}.
   x = chCap:nWidth.
    chCap:Box(800, chCap:nBottom + 100, x * -1, y).
   bottom = chCap:nBottom.

   s = "[T C Black S48 I]Solution!".
   chCap:RenderPrint(0, 0, s).
   x = 800 + (x - chCap:nRenderWidth) / 2.
   y = chCap:nTop + (chCap:nHeight - chCap:nRenderHeight) / 2.
   chCap:VpePrint(x, y, s).

   y = bottom - 25.
   chCap:BkgMode = {&VBKG_GRD_LINE}.
   chCap:BkgGradientRotation = 0.
   chCap:BkgGradientStartColor = {&COLOR_LTBLUE}.
   chCap:BkgGradientEndColor = {&COLOR_BLACK}.
   s = "[S 20 U C White]Text and images".
   chCap:RenderPrint(750, y + 75, s).
   x = 1000 - chCap:nRenderWidth / 2.
   y = chCap:PrintBox(x, y + 75, s).
   chCap:BkgGradientRotation = 900.
   chCap:BkgGradientStartColor = {&COLOR_LTRED}.
   chCap:PrintBox({&VRIGHT}, {&VBOTTOM}, "[Rot 900] can be freely ").
   chCap:BkgGradientRotation = 1800.
   chCap:BkgGradientStartColor = {&COLOR_LTYELLOW}.
   chCap:WriteBox(x, {&VBOTTOM}, {&VLEFT}, {&VFREE}, "[Rot 1800 CE]rotated in 90").
/*
 *    Rem The WIDTH (after rotation it's the height) is the top of the last inserted text
 *    Rem minus the bottom of the first inserted object.*/
   chCap:BkgGradientRotation = 2700.
   chCap:BkgGradientStartColor = {&COLOR_GREEN}.
   chCap:RenderWriteBox(0, 0, -500, {&VFREE}, "[Rot 2700]degree steps").
   chCap:WriteBox(x - chCap:nRenderWidth, y, -(chCap:nTop - y), {&VFREE}, "[Rot 2700]degree steps").

   chCap:Rotation = 0.
   chCap:TextUnderline = False.
   chCap:BkgMode = {&VBKG_TRANSPARENT}.
   chCap:StatusProgress = 80.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE page6 C-Win 
PROCEDURE page6 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR vtext AS CHAR NO-UNDO.
   chCap:PageBreak.
   chCap:NoPen.
   IF chCap:Edition >= {&VEDITION_PROFESSIONAL} THEN DO:
      /*Rem PIC_EXACT too slow for 300 DPI image:*/
      chCap:Picture(0, 0, {&VFREE}, {&VFREE}, datapath + "\gew300g4.tif", {&PIC_KEEPIMAGE}).
      vtext = "[S 10 J I C LtRed]".
      vtext = vtext + "The bitmap form has a resolution of 300 DPI. Therefore the preview is not very nice." + CHR(10).
      vtext = vtext + "Try the Professional Edition Demo, to see the Scale-to-Gray Technology!".
      chCap:WRITE(1100, 1520, 1950, {&VFREE}, vtext).
   END.
   ELSE DO:
      IF chCap:Edition <> {&VEDITION_STANDARD} THEN
          chCap:Picture(0, 0, {&VFREE}, {&VFREE}, datapath + "\gew.png", {&PIC_KEEPIMAGE} + {&PIC_EXACT}).
      ELSE
          chCap:Picture(0, 0, {&VFREE}, {&VFREE}, datapath + "\ew.bmp", {&PIC_KEEPIMAGE} + {&PIC_EXACT}).
      vtext = "[S 10 J I C LtRed]".
      vtext = vtext + "It is very important to mention here, that the bitmap form has a resolution of 96 ".
      vtext = vtext + "DPI only. The print will not be very nice. Just try a 300 DPI bitmap on your own!".
      chCap:WRITE(1100, 1520, 1950, {&VFREE}, vtext).
   END.

   chCap:Write(250, 110, 1200, {&VFREE}, "[S 24 C LtRed L PS 0 IO]Stadt Xhausen").
   chCap:Write(250, 500, 1000, {&VFREE}, "[S 14 C Blue]Mustermann & Co." + Chr(10) + "Feinkost Im- und Export").
   chCap:Write(1200, 460, 1700, {&VFREE}, "[S 11 B]Düsseldorf").
   chCap:Write(250, 680, 750, {&VFREE}, "Schmidt").
   chCap:Write(250, 845, 750, {&VFREE}, "24.7. 1947").
   chCap:Write(500, 845, 1000, {&VFREE}, "Oberammergau").
   chCap:Write(250, 1015, 750, {&VFREE}, "Zunderstr. 93").
   chCap:Write(1000, 1010, 1750, {&VFREE}, "0 27 84 / 16 45 98").
   chCap:Write(250, 1265, 1750, {&VFREE}, "Willi-Graf-Str. 17").
   chCap:Write(1000, 1260, 1750, {&VFREE}, "0 27 84 / 23 54 90").
   chCap:Write(1220, 670, 1750, {&VFREE}, "Heinz - Willi").
   chCap:StatusProgress = 100.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE precision C-Win 
PROCEDURE precision :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER mode AS INT NO-UNDO.
    
  DEF VAR vpage AS INT NO-UNDO.
  
  IF NOT ok2open(chCap) THEN RETURN.
  
  /*CM_PREC_PREVIEW.Enabled = False
 *   CM_BACKGROUND.Enabled = False*/
  chCap:externalwindow = FALSE.
  chCap:OpenDoc.
  chCap:DevJobName = "Capabilities + Precision".
  IF mode = 0 THEN DO:
    MESSAGE "It's easy to embed a VPE window on a Progress frame's client area. You just need a few lines of code!"
    VIEW-AS ALERT-BOX.
    Cap:X = 0.
    Cap:Y = 0.
    Cap:WIDTH-PIXELS = FRAME {&frame-name}:WIDTH-PIXELS.
    Cap:HEIGHT-PIXELS = FRAME {&frame-name}:HEIGHT-PIXELS.
    Cap:VISIBLE = TRUE.
    chCap:Preview.
  END.
  chCap:OpenProgressBar.
  chCap:nRightMargin = 2000.
  chCap:SetDefOutRect(100, 100, 1900, 2730).

  RUN page1.
  RUN Page2.
  RUN Page3.
  RUN Page4_5.
  RUN page6.

  /*Insert page numbers in the style "Page n of <total>":*/
  
  chCap:UseSet (BODYTEXT).
  chCap:TextAlignment = {&ALIGN_RIGHT}.
  chCap:TextItalic = True.
  DO vpage = 2 TO chCap:PageCount:
    chCap:CurrentPage = vpage.
    chCap:WRITE(chCap:nRightMargin - 500, {&VTOPMARGIN}, chCap:nRightMargin + 100, {&VFREE}, "Page " + STRING(vpage) + " of " + STRING(chCap:PageCount)).
  END.

  chCap:RefreshDoc.
  chCap:RemoveSet (HEADLINE).
  chCap:RemoveSet (BODYTEXT).

  chCap:CloseProgressBar.
  
  IF mode <> 0 THEN DO:
    /*background printing
 *     no Setup-Dialog*/
    /*här system dialog*/
    chCap:SetupPrinter("t.prs", 2). 
    chCap:WritePrinterSetup("t.prs"). 
   /* chCap:PrintDoc (FALSE).*/
    chCap:CloseDoc.
    /*CM_PREC_PREVIEW.Enabled = True.
 *     CM_BACKGROUND.Enabled = True*/
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reporttest C-Win 
PROCEDURE reporttest :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF INPUT PARAMETER mode AS INT NO-UNDO.
  
  DEF VAR Y AS INT NO-UNDO.
  DEF VAR n AS INT NO-UNDO.
  
  DEF VAR product AS CHAR EXTENT 3 INIT ["Apples","Oranges","Bananas"] NO-UNDO.
  DEF VAR germany AS DEC EXTENT 3 INIT [2450,2830,1560] NO-UNDO.
  DEF VAR france AS DEC EXTENT 3 INIT [1700,2240,925] NO-UNDO.
  DEF VAR usa AS DEC EXTENT 3 INIT [3200,3650,2130] NO-UNDO.
  DEF VAR australia AS DEC EXTENT 3 INIT [1080,1470,520] NO-UNDO.
  
  DEF VAR vtotal AS DEC NO-UNDO.
  DEF VAR group_total AS DEC NO-UNDO.
  DEF VAR full_path AS CHAR NO-UNDO.
  
  DEF VAR vtext AS CHAR NO-UNDO.
  
  
    /*
 *   CM_REPORT_PREVIEW.Enabled = False
 *   CM_EMAIL_BKG.Enabled = False
 *   CM_EMAIL_BKG_WO_DIALOG.Enabled = False
 * */
   IF NOT ok2open(chReport) THEN RETURN.
  
   
   /* Use of VPE mail choice changes the Progress current directory.
     We need to get this and then change it back in the AfterMail trigger.*/
     curr-dir = getDirectory().
 
  
   chReport:OpenDoc.
   
   IF mode = 1 THEN DO:
     vtext = "The document will be generated and e-mailed invisibly in background." + Chr(10) + Chr(10).
     vtext = vtext + "By code, the mail subject is set to ""VPE Demo""." + Chr(10) + Chr(10).
     vtext = vtext + "By code, the following mail recipients are defined:" + Chr(10).
     vtext = vtext + "- MrX@dummyxyz.com" + Chr(10).
     vtext = vtext + "- MrY@dummyxyz.com" + Chr(10) + Chr(10).
     vtext = vtext + "By default, the chReport: document is automatiy added as attachment." + Chr(10).
     vtext = vtext + "By code, the attachment ""vpedemo.frm"" is added." + Chr(10).
     vtext = vtext + "Note: You can also remove the VPE Document attachment by code." + Chr(10) + Chr(10).
     vtext = vtext + "IMPORTANT:" + Chr(10).
     vtext = vtext + "- Netscape Messenger actually seems not to work correctly as MAPI client." + Chr(10).
      Message vtext view-as alert-box.
   END.  
   ELSE IF mode = 2 THEN DO:
     vtext = "This option does not work with some MAPI clients (dialog will be shown)." + Chr(10).
     vtext = vtext + "This also depends on the MAPI client configuration!".
     MESSAGE vtext VIEW-AS ALERT-BOX.
   END.

   chReport:RenderPrintBox(0, 0, "[S 14 C Black]x").
   LineHeight = chReport:nRenderHeight * -1.

   chReport:AutoBreakMode = {&AUTO_BREAK_NO_LIMITS}.
   chReport:SetPen(5, {&psSolid}, {&COLOR_BLACK}).
   chReport:Picture(1650, 150, {&VFREE}, {&VFREE}, datapath + "\fruits.bmp", {&PIC_KEEPIMAGE} + {&PIC_IN_FILE}).
   chReport:WriteBox(1650, chReport:nBottom, chReport:nRight, {&VFREE}, "[N S 9 CE I C Blue BC Gray TO]Better Fruits Software").
   y = chReport:nBottom + 100.
   chReport:VpePrint(150, 200, "[N S 26 U]Year End Results").
   chReport:VpePrint(150, 400, "[N S 32]Better Fruits Software").

   chReport:VpeLine(150, y, 2000, y).
   chReport:nBottom = y + 50.
   vtotal = 0.

   DO n = 1 TO 3:
      GroupHeader (product[n]).
      DetailLine({&COLOR_GRAY}, "Germany", germany[n]).
      DetailLine({&COLOR_HIBLUE}, "France", france[n]).
      DetailLine({&COLOR_GRAY}, "USA", usa[n]).
      DetailLine({&COLOR_HIBLUE}, "Australia", australia[n]).
      group_total = germany[n] + france[n] + usa[n] + australia[n].
      
      GroupFooter (group_total).
      vtotal = vtotal + group_total.
   END.

   chReport:VpePrint(150, chReport:nBottom, "[N S 20 U]Yearly Country Sales Total: $" + trim(string(vtotal * factor, ">>,>>>,>>9.99"))).

  /*Rem Draw a pie*/
  /*Rem use the nProperties to easily position this anywhere on the paper
 *   Rem =================================================================*/
  chReport:PageBreak.
  chReport:FontName = "Times New Roman".
  chReport:FontSize = 12.
   chReport:VpePrint(200, {&VBOTTOM}, "[N S 18 U]Analysis of Paradise:").
   chReport:SetPen(3, {&psSolid}, {&COLOR_BLACK}).
  chReport:BkgMode = {&VBKG_SOLID}.
  chReport:BkgColor = {&COLOR_RED}.

   chReport:Pie(200, chReport:nBottom + 100, -600, -600, 0, 300).
  chReport:StorePos.
   chReport:Box(chReport:nRight + 200, chReport:nTop + 20, -30, -30).
   chReport:VpePrint(chReport:nRight + 30, chReport:nTop - 10, "[N T]Apples").

  chReport:RestorePos.
  chReport:BkgColor = {&COLOR_BLUE}.
   chReport:Pie({&VLEFT}, {&VTOP}, {&VRIGHT}, {&VBOTTOM}, 300, 750).
  chReport:StorePos.
   chReport:Box(chReport:nRight + 200, chReport:nTop + 70, -30, -30).
   chReport:VpePrint(chReport:nRight + 30, chReport:nTop - 10, "[N T]Oranges").

  chReport:RestorePos.
  chReport:BkgColor = {&COLOR_LTYELLOW}.
   chReport:Pie({&VLEFT}, {&VTOP}, {&VRIGHT}, {&VBOTTOM}, 750, 1500).
  chReport:StorePos.
   chReport:Box(chReport:nRight + 200, chReport:nTop + 120, -30, -30).
   chReport:VpePrint(chReport:nRight + 30, chReport:nTop - 10, "[N T]Bananas").

  chReport:RestorePos.
  chReport:BkgColor = {&COLOR_GREEN}.
   chReport:Pie({&VLEFT}, {&VTOP}, {&VRIGHT}, {&VBOTTOM}, 1500, 2900).
  chReport:StorePos.
   chReport:Box(chReport:nRight + 200, chReport:nTop + 170, -30, -30).
   chReport:VpePrint(chReport:nRight + 30, chReport:nTop - 10, "[N T]Cherries").

  chReport:RestorePos.
  chReport:BkgColor = {&COLOR_CYAN}.
   chReport:Pie({&VLEFT}, {&VTOP}, {&VRIGHT}, {&VBOTTOM}, 2900, 0).
  chReport:StorePos.
   chReport:Box(chReport:nRight + 200, chReport:nTop + 220, -30, -30).
   chReport:VpePrint(chReport:nRight + 30, chReport:nTop - 10, "[N T]Coconuts").

  /*Rem E-Mail functions:
 *   Rem ================= */
     IF mode = 0 THEN
        chReport:Preview.
     ELSE DO:
       full_path = SEARCH("vpedemo.w").
       IF mode = 2 THEN
         chReport:MailWithDialog = 0.
       
       chReport:MailSubject = "VPE Demo".
       chReport:AddMailReceiver("xyz@xyz.com", {&VMAIL_TO}).
       /*chReport:AddMailReceiver("abc@xyz.com", {&VMAIL_CC}).*/
       chReport:AddMailAttachment(full_path, "").

       IF chReport:MailDoc = 1 THEN
          MESSAGE "e-mailing finished without error." VIEW-AS ALERT-BOX.
       ELSE
          MESSAGE "Document could not be e-mailed." + CHR(10) + CHR(10) + "VPE Error-Code: " + STRING(chReport:LastError).

      chReport:CloseDoc.
      setDirectory(curr-dir).

     END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION compx C-Win 
FUNCTION compx RETURNS INTEGER
  ( INPUT w AS INT,
    INPUT numcols AS INT,
    INPUT vCOL AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN INT(w / numcols * vcol + 250).   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION compy C-Win 
FUNCTION compy RETURNS INTEGER
  ( h as int,
    vcol as int,
    vrow as int ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN int(h / exp(2,vcol) * vrow + (h / exp(2,vcol)) / 2 + 200).   /* Function return value. */
  
 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION detailline C-Win 
FUNCTION detailline RETURNS LOGICAL
  ( input vcolor as int,
    input country as char,
    input quantity as dec ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   
  
   DEF VAR Y AS INTEGER.
   Y = chReport:nBottom.
   chReport:BkgMode = {&VBKG_SOLID}.
   chReport:BkgColor = vcolor.
   chReport:PenColor = vcolor.
   chReport:BOX(150, Y, 1550, LineHeight).
   chReport:BkgMode = {&VBKG_TRANSPARENT}.

   
   
   chReport:WriteBox(150, Y, -400, Lineheight, "[L S 14 C Black]" + country).
   chReport:TextAlignment = {&ALIGN_RIGHT}.
   chReport:WriteBox(650, Y, 900, LineHeight, trim(STRING(quantity, ">>>,>>9.99"))).
   chReport:WriteBox(1150, Y, 1450, LineHeight, trim(STRING(quantity * factor, ">>>,>>9.99"))).
   
  
  
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDirectory C-Win 
FUNCTION getDirectory RETURNS CHARACTER
  (  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE chrDirectoryName AS CHARACTER NO-UNDO FORMAT "X(256)".
  DEFINE VARIABLE intBufferSize    AS INTEGER   NO-UNDO INITIAL 256.
  DEFINE VARIABLE intResult        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE ptrToString      AS MEMPTR    NO-UNDO.
  
  SET-SIZE(ptrToString) = 256.
  RUN GetCurrentDirectoryA (INPUT        intBufferSize,
                          INPUT-OUTPUT ptrToString,
                          OUTPUT       intResult).

  ASSIGN chrDirectoryName = GET-STRING(ptrToString,1).


  
  
  RETURN TRIM(chrDirectoryName).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION groupfooter C-Win 
FUNCTION groupfooter RETURNS LOGICAL
  ( input quantity as dec ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    
   DEF VAR Y AS INTEGER.
   /*Rem PrintBox and WriteBox are used here, because in the Detail-Section they are used, too.
 *    Rem If we wouldn't use them, the Footer-Section would be misaligned, because of
 *    Rem the invisible surrounding rectangle of the Detail-Section.*/
   Y = chReport:nBottom + chReport:PenSize + 2.
               /* Rem  + Pensize, because the white frame would otherwise overlap the Detail-Section"s frame
 *                 Rem  +2, to avoid rounding problems we use a bit more*/
   chReport:PenColor = {&COLOR_WHITE}.
   chReport:PrintBox(150, Y, "Total").
   chReport:WriteBox(650, Y, 900, LineHeight, trim(STRING(quantity, ">>>,>>9.99"))).
   chReport:WriteBox(1150, Y, 1450, LineHeight, trim(STRING(quantity * factor, ">>>,>>9.99"))).
   chReport:nBottom = Y + 180.
  
  
  
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION groupHeader C-Win 
FUNCTION groupHeader RETURNS LOGICAL
  ( input product as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEF VAR Y AS INT.
   chReport:TextAlignment = {&ALIGN_LEFT}.
   Y = chReport:Print(150, chReport:nBottom, "[N S 18 C Blue]Product: " + product).
   Y = Y + 10.
   chReport:VpePrint(150, Y, "[S 16 C Purple]Country").
   chReport:WRITE(650, Y, 900, {&VFREE}, "[R]Quantity").
   chReport:WRITE(1150, Y, 1450, {&VFREE}, "Value (in $)").
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ok2open C-Win 
FUNCTION ok2open RETURNS LOGICAL
  ( input chTest as com-handle ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF chTest:isopen THEN chTest:closedoc.
  RETURN NOT chTest:isopen.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION printfooter C-Win 
FUNCTION printfooter RETURNS LOGICAL
  ( ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   chSpeed:SetFontAttr({&ALIGN_CENTER}, TRUE, FALSE, FALSE, FALSE).
   chSpeed:BkgColor = {&COLOR_LTGRAY}.
   chSpeed:WriteBox({&VLEFTMARGIN}, {&VBOTTOM}, -420, group_title_height, "Sum").
   chSpeed:TextAlignment = {&ALIGN_RIGHT}.
   chSpeed:WriteBox({&VRIGHT}, {&VTOP}, -250, {&VBOTTOM}, TRIM(STRING(sum_amount,">>>,>>>,>>9.99"))).
   chSpeed:WriteBox({&VRIGHT}, {&VTOP}, -250, {&VBOTTOM}, TRIM(STRING(sum_prorated,">>>,>>>,>>9.99"))).
   chSpeed:WriteBox({&VRIGHT}, {&VTOP}, -250, {&VBOTTOM}, TRIM(STRING(sum_tax,">>>,>>>,>>9.99"))).
   chSpeed:BOX({&VRIGHT}, {&VTOP}, {&VRIGHTMARGIN}, {&VBOTTOM}).
   chSpeed:SetFontAttr({&ALIGN_LEFT}, FALSE, FALSE, FALSE, FALSE).
   chSpeed:BkgColor = {&COLOR_WHITE}.
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION printheader C-Win 
FUNCTION printheader RETURNS LOGICAL
  ( vtable as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    
   chSpeed:TextAlignment = {&ALIGN_CENTER}.
   chSpeed:SelectFont("Arial", 14).
   chSpeed:BkgColor = {&COLOR_LTGRAY}.
   If headline_height = 0 Then do:
      /*Rem determine height of headline only once for better performance:*/
       chSpeed:RenderPrintBox(0, 0, "x").
      headline_height = chSpeed:nRenderHeight * -1.
   End.
   chSpeed:WriteBox({&VLEFTMARGIN}, {&VBOTTOM}, {&VRIGHTMARGIN}, headline_height, vtable).

   chSpeed:TextBold = True.
   chSpeed:SelectFont("Arial", 11).
   IF group_title_height = 0 THEN DO:
      /*Rem determine height of titles only once for better performance:*/
       chSpeed:RenderPrintBox(0, 0, "x").
      group_title_height = chSpeed:nRenderHeight * -1.
   END.
   
    chSpeed:WriteBox({&VLEFTMARGIN}, {&VBOTTOM}, -200, group_title_height, "No.").
    chSpeed:WriteBox({&VRIGHT}, {&VTOP}, -220, {&VBOTTOM}, "Date").
    chSpeed:WriteBox({&VRIGHT}, {&VTOP}, -250, {&VBOTTOM}, "Amount").
    chSpeed:WriteBox({&VRIGHT}, {&VTOP}, -250, {&VBOTTOM}, "Prorated").
    chSpeed:WriteBox({&VRIGHT}, {&VTOP}, -250, {&VBOTTOM}, "Tax").
    chSpeed:WriteBox({&VRIGHT}, {&VTOP}, {&VRIGHTMARGIN}, {&VBOTTOM}, "Remark").
    chSpeed:SetFontAttr({&ALIGN_LEFT}, False, False, False, False).
   chSpeed:BkgColor = {&COLOR_WHITE}.
  
  
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION printpagefooter C-Win 
FUNCTION printpagefooter RETURNS LOGICAL
  ( v-name as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  chSpeed:StorePos.
  chSpeed:Write({&VLEFTMARGIN}, {&VBOTTOMMARGIN}, chSpeed:nRightMargin - 400, detail_height, v-name).
  chSpeed:RestorePos.
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setDirectory C-Win 
FUNCTION setDirectory RETURNS LOGICAL
  ( v-dir as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR intresult AS INT NO-UNDO.
  RUN SetCurrentDirectoryA (INPUT v-dir, OUTPUT intResult).

  IF intresult = 1 THEN RETURN TRUE.
  ELSE
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sine C-Win 
FUNCTION sine RETURNS DECIMAL
  ( INPUT decinput AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR decresult AS DEC NO-UNDO.
  RUN sin(INPUT decinput, OUTPUT decresult).
  RETURN decresult.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


