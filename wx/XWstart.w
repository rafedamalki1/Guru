&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME WWSTART
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WWSTART 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/06/06 - 10:23 am

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
DEFINE INPUT-OUTPUT PARAMETER nyprog AS LOGICAL NO-UNDO.
/* Local Variable Definitions ---                                       */
nyprog = FALSE.
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED 



                              


DEFINE VARIABLE ReturnValue AS INTEGER.
DEFINE NEW GLOBAL SHARED VARIABLE hpApi AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE hpWinFunc AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE retvalkoll AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE tth AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE aonrrec2 AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE printer AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE printer1 AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE NEW SHARED VARIABLE regmnr AS INTEGER FORMAT "99" NO-UNDO.
DEFINE NEW SHARED VARIABLE regmannamn AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE regar AS INTEGER FORMAT "99" NO-UNDO.
DEFINE NEW SHARED VARIABLE regstart AS DECIMAL NO-UNDO. 
DEFINE NEW SHARED VARIABLE regslut AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE avdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE bilforare AS LOGICAL FORMAT "JA/NEJ" NO-UNDO.
DEFINE NEW SHARED VARIABLE enflerdygns AS LOGICAL FORMAT "ENDAGS/FLERDYGNS" NO-UNDO.
DEFINE NEW SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE NEW SHARED VARIABLE klocka AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE NEW SHARED VARIABLE valkalknr AS INTEGER NO-UNDO. 
DEFINE NEW SHARED VARIABLE kalkmtrl AS LOGICAL NO-UNDO. 
DEFINE NEW SHARED VARIABLE kalkregvar AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE RAD_FAST AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE TOG_AONR AS LOGICAL INITIAL no NO-UNDO.
DEFINE NEW SHARED VARIABLE finnskoff AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE vald_kundlev AS CHARACTER FORMAT "X(4)" NO-UNDO.
DEFINE NEW SHARED VARIABLE vald_lev AS CHARACTER FORMAT "X(4)" NO-UNDO.
DEFINE NEW SHARED VARIABLE best AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE sok AS LOGICAL NO-UNDO.  
DEFINE NEW SHARED VARIABLE sokant AS LOGICAL NO-UNDO. 
DEFINE NEW SHARED VARIABLE bestant AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE huvudlev AS CHARACTER FORMAT "X(25)" NO-UNDO.  


DEFINE NEW SHARED VARIABLE alltidmax AS LOGICAL NO-UNDO.
DEFINE VARIABLE gastsek AS LOGICAL EXTENT 20 NO-UNDO.
DEFINE VARIABLE storkollbredd AS INTEGER NO-UNDO.
DEFINE VARIABLE storkollhojd AS INTEGER NO-UNDO.
DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
DEFINE VARIABLE datornamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE ph_frame AS HANDLE NO-UNDO.
   ASSIGN
   globstorh = 682
   globstorb = 1000.
DEFINE VARIABLE raknarsek AS INTEGER NO-UNDO.
DEFINE VARIABLE anvdator AS CHARACTER NO-UNDO.  
DEFINE VARIABLE xhop AS CHARACTER NO-UNDO.            
DEFINE VARIABLE p-handle AS HANDLE NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE globanvnt AS CHARACTER NO-UNDO. 
DEFINE VARIABLE hcur AS INTEGER NO-UNDO.
DEFINE VARIABLE retval AS INTEGER NO-UNDO.

DEFINE NEW SHARED TEMP-TABLE tidut
   FIELD UT AS CHARACTER FORMAT "X(132)".




DEFINE TEMP-TABLE tesxttemp
   FIELD EGENTEXT     AS CHARACTER 
   FIELD EGENTEXTFULL AS CHARACTER
   FIELD GURUTEXT     AS CHARACTER
   FIELD PROGRAM      AS CHARACTER
   INDEX PROGRAM IS PRIMARY PROGRAM.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-B

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 EDD_FUNK SEL_UPP BTN_KOR ~
BTN_MEDD BTN_BYT BTN_BYTW BTN_UPPDAT BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS EDD_FUNK SEL_UPP FILL-IN-GURU FILL-IN-FUNK 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WWSTART AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 22 BY 2.

DEFINE BUTTON BTN_BYT 
     LABEL "Byt användare" 
     SIZE 22 BY 2.

DEFINE BUTTON BTN_BYTW 
     LABEL "Byt fönsterstorlek" 
     SIZE 22 BY 2 TOOLTIP "Anpassa Guru till din skärmupplösning.".

DEFINE BUTTON BTN_KOR 
     LABEL "Kör funktion" 
     SIZE 22 BY 2
     BGCOLOR 8 .

DEFINE BUTTON BTN_MEDD 
     LABEL "Skapa meddelande till Guruanvändare" 
     SIZE 22 BY 2.

DEFINE BUTTON BTN_UPPDAT 
     LABEL "Uppdatera program" 
     SIZE 22 BY 2 TOOLTIP "Uppdatera din Guru applikation med den senaste versionen.".

DEFINE VARIABLE EDD_FUNK AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 64.5 BY 7
     BGCOLOR 15 FGCOLOR 9 FONT 5 NO-UNDO.

DEFINE VARIABLE FILL-IN-FUNK AS CHARACTER FORMAT "X(256)":U INITIAL "Funktioner:" 
      VIEW-AS TEXT 
     SIZE 14 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-GURU AS CHARACTER FORMAT "X(256)":U INITIAL "Välkommen till" 
      VIEW-AS TEXT 
     SIZE 50.5 BY 1
     FONT 17 NO-UNDO.

DEFINE IMAGE IMAGE-3
     FILENAME "adeicon/blank":U CONVERT-3D-COLORS
     SIZE 8 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 42.5 BY 19.25.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 24.5 BY 19.25
     BGCOLOR 8 .

DEFINE VARIABLE SEL_UPP AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 38 BY 16.5
     BGCOLOR 7 FGCOLOR 15 FONT 17 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-B
     EDD_FUNK AT ROW 2.5 COL 2 NO-LABEL
     SEL_UPP AT ROW 3.25 COL 3.5 NO-LABEL
     BTN_KOR AT ROW 4.58 COL 44.88
     BTN_MEDD AT ROW 7.21 COL 44.88
     BTN_BYT AT ROW 9.83 COL 44.88
     BTN_BYTW AT ROW 12.46 COL 44.88
     BTN_UPPDAT AT ROW 15.08 COL 44.88
     BTN_AVB AT ROW 17.75 COL 44.88
     FILL-IN-GURU AT ROW 1 COL 3 NO-LABEL
     FILL-IN-FUNK AT ROW 2.5 COL 45.5 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 2 COL 1
     RECT-2 AT ROW 2 COL 43.5
     IMAGE-3 AT ROW 1 COL 56.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 67.25 BY 20.38
         BGCOLOR 8 
         DEFAULT-BUTTON BTN_KOR.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WWSTART ASSIGN
         HIDDEN             = YES
         TITLE              = "GURU"
         COLUMN             = 20.25
         ROW                = 6.63
         HEIGHT             = 20.46
         WIDTH              = 67.38
         MAX-HEIGHT         = 27.25
         MAX-WIDTH          = 100
         VIRTUAL-HEIGHT     = 27.25
         VIRTUAL-WIDTH      = 100
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = 8
         FGCOLOR            = ?
         THREE-D            = yes
         FONT               = 16
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WWSTART
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-B
                                                                        */
ASSIGN 
       BTN_UPPDAT:HIDDEN IN FRAME FRAME-B           = TRUE.

ASSIGN 
       EDD_FUNK:READ-ONLY IN FRAME FRAME-B        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-FUNK IN FRAME FRAME-B
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-FUNK:READ-ONLY IN FRAME FRAME-B        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-GURU IN FRAME FRAME-B
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR IMAGE IMAGE-3 IN FRAME FRAME-B
   NO-ENABLE                                                            */
ASSIGN 
       IMAGE-3:HIDDEN IN FRAME FRAME-B           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WWSTART)
THEN WWSTART:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-B
/* Query rebuild information for FRAME FRAME-B
     _Query            is NOT OPENED
*/  /* FRAME FRAME-B */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME WWSTART
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL WWSTART WWSTART
ON WINDOW-CLOSE OF WWSTART /* GURU */
DO:   
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WWSTART
ON CHOOSE OF BTN_AVB IN FRAME FRAME-B /* Avsluta */
DO:       
   DEFINE VARIABLE franfil  AS CHARACTER NO-UNDO.
   SESSION:PRINTER-CONTROL-HANDLE = 0.
   
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WWSTART
ON ENDKEY OF BTN_AVB IN FRAME FRAME-B /* Avsluta */
DO:  
   
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BYT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BYT WWSTART
ON CHOOSE OF BTN_BYT IN FRAME FRAME-B /* Byt användare */
DO:
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BYTW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BYTW WWSTART
ON CHOOSE OF BTN_BYTW IN FRAME FRAME-B /* Byt fönsterstorlek */
DO:
   
  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KOR WWSTART
ON CHOOSE OF BTN_KOR IN FRAME FRAME-B /* Kör funktion */
DO:
   SEL_UPP = INPUT SEL_UPP.   
   RUN vart_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_MEDD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MEDD WWSTART
ON CHOOSE OF BTN_MEDD IN FRAME FRAME-B /* Skapa meddelande till Guruanvändare */
DO:  
   
   vartpro = "MED".
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPPDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPPDAT WWSTART
ON CHOOSE OF BTN_UPPDAT IN FRAME FRAME-B /* Uppdatera program */
DO:
  
  APPLY "CHOOSE" TO BTN_AVB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_UPP WWSTART
ON MOUSE-MENU-CLICK OF SEL_UPP IN FRAME FRAME-B
DO:
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_UPP WWSTART
ON MOUSE-SELECT-DBLCLICK OF SEL_UPP IN FRAME FRAME-B
DO:
   SEL_UPP = INPUT SEL_UPP.     
   RUN vart_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_UPP WWSTART
ON VALUE-CHANGED OF SEL_UPP IN FRAME FRAME-B
DO:
   SEL_UPP = INPUT SEL_UPP.   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WWSTART 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:   
   SESSION:PRINTER-CONTROL-HANDLE = 0.
   
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN NO-APPLY.  
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:   
  SESSION:PRINTER-CONTROL-HANDLE = 0.
  
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:     
   
      {WSTART1.I}
   {windows.i}
   
   RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
   RUN SetAppstartingCursor IN Guru.Konstanter:hpApi.
   Guru.GlobalaVariabler:retvalkoll = TRUE.
   
   RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
   RUN SetAppstartingCursor IN Guru.Konstanter:hpApi.
   Guru.GlobalaVariabler:retvalkoll = TRUE.
DEF VAR vh AS WIDGET-HANDLE NO-UNDO.
   
   vh = FRAME {&FRAME-NAME}:HANDLE.
   vh:FONT = 6.
   vh = vh:FIRST-CHILD. /* field-group */
   vh = vh:FIRST-CHILD. /* first widget in field-group*/
   REPEAT:               
      IF LOOKUP(vh:TYPE,"FILL-IN") > 0 THEN DO:         
   vh:HEIGHT-CHARS = 0.85.
END.               
ELSE IF LOOKUP(vh:TYPE,"BROWSE") > 0 THEN DO:         
   vh:FONT = 24.
END.                
ELSE IF LOOKUP(vh:TYPE,"RECTANGLE") > 0 THEN DO:         
   
END.                     
ELSE IF LOOKUP(vh:TYPE,"COMBO-BOX") > 0 THEN DO:         
   
END.                    
ELSE IF LOOKUP(vh:TYPE,"TOGGLE-BOX") > 0 THEN DO:         
   
END.
ELSE IF LOOKUP(vh:TYPE,"SELECTION-LIST") > 0 THEN DO:         
   
END.
ELSE IF LOOKUP(vh:TYPE,"BUTTON") > 0 THEN DO: 
   IF vh:IMAGE NE "" THEN.
   /*
   IF vh:NAME = "BTN_KONTO" THEN.
   ELSE IF vh:NAME = "BTN_KONTO" THEN.
   ELSE IF vh:NAME = "BTN_ALLOVER" THEN DO:
      MESSAGE vh:IMAGE VIEW-AS ALERT-BOX.
   END.
   ELSE IF vh:NAME = "BTN_OVER" THEN.
   ELSE IF vh:NAME = "BTN_OVER-2" THEN.
   ELSE IF vh:NAME = "BTN_BACK" THEN.
   ELSE IF vh:NAME = "BTN_BACK-2" THEN.
   ELSE IF vh:NAME = "BTN_ALLBACK" THEN.
   */
   ELSE DO:
      IF vh:HEIGHT-CHARS > 1.5 THEN DO:
         vh:ROW = vh:ROW + (vh:HEIGHT-CHARS - 1.5) / 2.
         vh:HEIGHT-CHARS = 1.5.         
      END.
   END.
   
END.


      
      vh=vh:NEXT-SIBLING. /* volgend widget */
      IF NOT VALID-HANDLE(vh) THEN LEAVE.
   END.


   Guru.GlobalaVariabler:retvalkoll =  TRUE.    
   vartpro = "".
   
    
         
            
   
   
   IF musz = FALSE THEN RUN enable_UI.         
   RUN sek_UI.
   
   
   
   
   ASSIGN
   {&WINDOW-NAME}:MIN-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS 
   {&WINDOW-NAME}:MIN-WIDTH-PIXELS = {&WINDOW-NAME}:WIDTH-PIXELS
   {&WINDOW-NAME}:MAX-HEIGHT-PIXELS = globstorh
   {&WINDOW-NAME}:MAX-WIDTH-PIXELS = globstorb
   ph_frame = FRAME {&FRAME-NAME}:HANDLE.
   ON WINDOW-RESIZED OF {&WINDOW-NAME} 
   DO:     
      RUN FRM-SIZE.P (INPUT ph_frame,INPUT {&WINDOW-NAME}:HEIGHT-PIXELS,INPUT {&WINDOW-NAME}:WIDTH-PIXELS ).         
      RUN CenterWindow IN Guru.Konstanter:hpApi (INPUT CURRENT-WINDOW).
      
      alltidmax = NOT alltidmax.
   END.
   ON MOUSE-MENU-DBLCLICK OF CURRENT-WINDOW PERSISTENT RUN fileinfo_UI IN THIS-PROCEDURE.
   ON MOUSE-MENU-DBLCLICK OF ph_frame PERSISTENT RUN fileinfo_UI IN THIS-PROCEDURE.
   /*
   RUN SetWindowLongB IN Guru.Konstanter:hpApi (BTN_MEDD:HWND IN FRAME {&FRAME-NAME},{&GWL_STYLE},i).
   RUN SendMessageB IN Guru.Konstanter:hpApi (BTN_MEDD:HWND IN FRAME {&FRAME-NAME},{&BM_SETSTYLE},i,1).
   */
   DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
   /*
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:      
      FILE-INFO:FILE-NAME = "prox.dll".
      kommando = FILE-INFO:FULL-PATHNAME.  
      kommando = "regsvr32 /s " + '"' + kommando + '"'.
      OS-COMMAND SILENT VALUE(kommando).      
      
   END.
   */
   /*
   IF Guru.Konstanter:globforetag = "sund" THEN  DO:
      FILE-INFO:FILE-NAME = "prox.dll". 
      kommando = FILE-INFO:FULL-PATHNAME.  
      kommando = "regsvr32 /s " + '"' + kommando + '"'.
      OS-COMMAND SILENT VALUE(kommando). 
   END.
   */

   /*updateringsknapp*/ 
 BTN_UPPDAT:HIDDEN = TRUE.
   BTN_MEDD:LABEL = "Skapa meddelande~ntill Guruanvändare". 
   RUN SetWindowLongA IN Guru.Konstanter:hpApi (BTN_MEDD:HWND IN FRAME {&FRAME-NAME},-16,1409294336,OUTPUT ReturnValue).
   RUN SendMessageA IN Guru.Konstanter:hpApi (BTN_MEDD:HWND IN FRAME {&FRAME-NAME},244,1409294336,1,OUTPUT ReturnValue).
   IF musz = TRUE THEN LEAVE MAIN-BLOCK.
   IF musz = TRUE THEN RETURN.    
   
   
   IF Guru.GlobalaVariabler:retvalkoll = TRUE THEN DO:
      RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
      Guru.GlobalaVariabler:retvalkoll = FALSE.
   END.

    RUN CenterWindow IN Guru.Konstanter:hpApi (INPUT CURRENT-WINDOW).
   
    
   {AVBFRAM.I}
   {musarrow.i} 
    
   {&WINDOW-NAME}:HIDDEN = FALSE.
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WWSTART  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WWSTART)
  THEN DELETE WIDGET WWSTART.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE eddstart_UI WWSTART 
PROCEDURE eddstart_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WWSTART  _DEFAULT-ENABLE
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
  DISPLAY EDD_FUNK SEL_UPP FILL-IN-GURU FILL-IN-FUNK 
      WITH FRAME FRAME-B IN WINDOW WWSTART.
  ENABLE RECT-1 RECT-2 EDD_FUNK SEL_UPP BTN_KOR BTN_MEDD BTN_BYT BTN_BYTW 
         BTN_UPPDAT BTN_AVB 
      WITH FRAME FRAME-B IN WINDOW WWSTART.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE feldum_UI WWSTART 
PROCEDURE feldum_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fileinfo_UI WWSTART 
PROCEDURE fileinfo_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gastmedd_UI WWSTART 
PROCEDURE gastmedd_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   MESSAGE "Vill du prova flera moduler i GURU tag kontakt med Elpool 090-184540"
   VIEW-AS ALERT-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE insatt_UI WWSTART 
PROCEDURE insatt_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nextguru_UI WWSTART 
PROCEDURE nextguru_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sek_UI WWSTART 
PROCEDURE sek_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   xhop = "GURU".
   vart = "FMED".
   vart = "".
   xhop = "GURU".    
      status-ok = SEL_UPP:ADD-LAST("Faktureringsrutin") IN FRAME {&FRAME-NAME}. 
         

   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE stoppweb_UI WWSTART 
PROCEDURE stoppweb_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vart_UI WWSTART 
PROCEDURE vart_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   
     
      
   
   
   IF SEL_UPP = "Materielhantering" THEN vartpro = "DEP".
   ELSE IF SEL_UPP = "Kalkylering" THEN DO:
      ASSIGN
      vart = "KAL"         
      vartpro = "KAL".
   END.
   ELSE IF SEL_UPP = "Tidredovisning" THEN vartpro = "TID".
   ELSE IF SEL_UPP = "Flextid" THEN vartpro = "FLX".
   ELSE IF SEL_UPP = "Uppföljning" THEN vartpro = "UPP".
   ELSE IF SEL_UPP = "Personaladministration" THEN vartpro = "PER".
   ELSE IF SEL_UPP = "Sekretess" THEN vartpro = "SEK".       
   ELSE IF SEL_UPP = "Register" THEN vartpro = "REG".
   ELSE IF SEL_UPP = "Faktureringsrutin" THEN vartpro = "FAF".      
   ELSE IF SEL_UPP = "Markvärdering" THEN vartpro = "MVA".
   ELSE IF SEL_UPP = "Projekteringskalender" THEN vartpro = "END".
   ELSE IF SEL_UPP = "Avbrott/Störning" THEN vartpro = "STR".
   ELSE IF SEL_UPP = "Rapporter" THEN vartpro = "STR1".
   ELSE IF SEL_UPP = "Administration" THEN vartpro = "STR2".
   ELSE IF SEL_UPP = "Import/Export" THEN vartpro = "STR3". 
   ELSE IF SEL_UPP = "SMS-administration" THEN vartpro = "SMS".
   ELSE IF SEL_UPP = "Beredning" THEN vartpro = "BER".     
   {&WINDOW-NAME}:HIDDEN = TRUE.
 DEFAULT-WINDOW:HIDDEN = TRUE.
{&WINDOW-NAME}:ALWAYS-ON-TOP = FALSE.         

   RUN XVARTPRO.P.
   {&WINDOW-NAME}:HIDDEN = FALSE.
{&WINDOW-NAME}:MOVE-TO-TOP ().
/*{&WINDOW-NAME}:ALWAYS-ON-TOP = FALSE.*/
/*BTN_AVB:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.*/
IF Guru.GlobalaVariabler:retvalkoll = TRUE THEN DO:
   RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
   Guru.GlobalaVariabler:retvalkoll = FALSE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

