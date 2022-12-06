&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME WINDOW-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-3 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 


  Created: 10/02/96 -  2:27 pm

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
{BERTEMPTAB.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED 
{BBENAMNTEMP.I}
{KONVALTEMP.I}
DEFINE INPUT PARAMETER TABLE FOR kon_val.
DEFINE INPUT PARAMETER TABLE FOR eurberedningtemp. 
/* Local Variable Definitions ---                                       */  
&Scoped-define NEW 
&Scoped-define SHARED 
{ALLDEF.I}
{GLOBVAR2DEL1.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED 
DEFINE NEW SHARED VARIABLE kalkvar AS LOGICAL NO-UNDO.
kalkvar = TRUE.
{BERKALKTEMP.I}
DEFINE NEW SHARED VARIABLE btnbervalh AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE stolpval AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE prisvar AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE bryt AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE edmedd AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE valkonst AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE btnlistorh AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE RAD_STOLP AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE summeradberedning AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO. 
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO. 
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
{WHANDLTEMP.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-LISTOR

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS SEL_UPP BTN_SKRIVLIST BTN_VISALIST ~
BTN_EXLIST BTN_EXLISTENK BTN_IELIST BTN_PDFLIST CMB_SKRIV CMB_PRIS ~
CMB_STOLP TOG_MTRL TOG_KONSTRUKTION TOG_VAL TOG_UPP TOG_ID TOG_ANM TOG_P3 ~
TOG_TEXT TOG_SIDEXCEL TOG_KON TOG_EXCELKALK TOG_KONVAL TOG_EGENSIDA ED_MEDD ~
BTN_AVBLIST 
&Scoped-Define DISPLAYED-OBJECTS SEL_UPP CMB_SKRIV CMB_PRIS CMB_STOLP ~
TOG_MTRL TOG_KONSTRUKTION TOG_VAL TOG_UPP TOG_ID TOG_ANM TOG_JMFKONS TOG_P3 ~
TOG_TEXT TOG_SIDEXCEL TOG_KON TOG_EXCELKALK TOG_KONVAL TOG_EGENSIDA ED_MEDD 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-3 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVBLIST AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_EXLIST 
     LABEL "Visa I EXCEL" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_EXLISTENK 
     LABEL "ENKELEXCEL" 
     SIZE 14 BY 1 TOOLTIP "Snabb, men enkel Excellista".

DEFINE BUTTON BTN_IELIST 
     LABEL "Visa I ie" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_PDFLIST 
     LABEL "Visa I PDF" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKRIVLIST 
     LABEL "Skriv ut" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VISALIST 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_PRIS AS CHARACTER FORMAT "X(256)":U 
     LABEL "Pris" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Visa pris","Visa inte pris" 
     DROP-DOWN-LIST
     SIZE 16.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_SKRIV AS CHARACTER FORMAT "X(256)":U 
     LABEL "Vid utskrift" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Använda standardskrivare","Välja annan skrivare" 
     DROP-DOWN-LIST
     SIZE 31.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_STOLP AS CHARACTER FORMAT "X(256)":U 
     LABEL "Materiel" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Visa allt","Bara stolpar","Inga stolpar" 
     DROP-DOWN-LIST
     SIZE 22.88 BY 1 NO-UNDO.

DEFINE VARIABLE ED_MEDD AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 51.5 BY 4
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE SEL_UPP AS CHARACTER INITIAL ? 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 51.75 BY 12.75 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_ANM AS LOGICAL INITIAL no 
     LABEL "Anmärkning" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.5 BY .88 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_EGEN AS LOGICAL INITIAL no 
     LABEL "Egenkontroll" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .88 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_EGENSIDA AS LOGICAL INITIAL no 
     LABEL "Egenkontroll med sidbrytning" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.38 BY .79 TOOLTIP "Egenkontrollerna kommer att hamna på en egen sida." NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_EXCELKALK AS LOGICAL INITIAL no 
     LABEL "Kalkylkoder bredvid Materiel" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.38 BY .79 TOOLTIP "Placerar kalkylkoder på sidan om materielen. Nytt utseende på Egenkontroll" NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_ID AS LOGICAL INITIAL no 
     LABEL "Identitet" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.88 BY .88 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_JMFKONS AS LOGICAL INITIAL no 
     LABEL "Jämför mtrl med konstruktion" 
     VIEW-AS TOGGLE-BOX
     SIZE 31.88 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_KON AS LOGICAL INITIAL no 
     LABEL "Vill Ni vid utskrift få en konstruktion / sida?" 
     VIEW-AS TOGGLE-BOX
     SIZE 51 BY .79 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_KONSTRUKTION AS LOGICAL INITIAL no 
     LABEL "Konstruktionsgrupp" 
     VIEW-AS TOGGLE-BOX
     SIZE 21.5 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_KONVAL AS LOGICAL INITIAL no 
     LABEL "Vill Ni välja konstruktioner?" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .79 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_MTRL AS LOGICAL INITIAL no 
     LABEL "Materiel" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.13 BY .88 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_P3 AS LOGICAL INITIAL no 
     LABEL "P3-koder" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.5 BY .88 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_SIDEXCEL AS LOGICAL INITIAL no 
     LABEL "Texter i sidokolumn i Excel" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.38 BY .79 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_SPANN AS LOGICAL INITIAL no 
     LABEL "Spann/kabellängd" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .88 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_TEXT AS LOGICAL INITIAL no 
     LABEL "Mtrl text" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.5 BY .88 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_UPP AS LOGICAL INITIAL no 
     LABEL "Upplag" 
     VIEW-AS TOGGLE-BOX
     SIZE 10.25 BY .88 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_VAL AS LOGICAL INITIAL no 
     LABEL "Konstruktionsval" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .88 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-LISTOR
     SEL_UPP AT ROW 2 COL 8.38 NO-LABEL
     BTN_SKRIVLIST AT ROW 8 COL 75.5
     BTN_VISALIST AT ROW 9.08 COL 75.5
     BTN_EXLIST AT ROW 10.21 COL 75.5
     BTN_EXLISTENK AT ROW 10.21 COL 75.5 WIDGET-ID 12
     BTN_IELIST AT ROW 11.29 COL 75.63
     BTN_PDFLIST AT ROW 12.42 COL 75.5
     CMB_SKRIV AT ROW 15.29 COL 13.63 COLON-ALIGNED
     TOG_SPANN AT ROW 15.5 COL 66
     CMB_PRIS AT ROW 16.54 COL 9.63
     CMB_STOLP AT ROW 16.54 COL 47.75 COLON-ALIGNED
     TOG_MTRL AT ROW 18 COL 2
     TOG_KONSTRUKTION AT ROW 18 COL 17 WIDGET-ID 4
     TOG_VAL AT ROW 18 COL 49.75
     TOG_UPP AT ROW 19 COL 2
     TOG_ID AT ROW 19 COL 17
     TOG_ANM AT ROW 19 COL 49.75
     TOG_JMFKONS AT ROW 20 COL 2 WIDGET-ID 6
     TOG_P3 AT ROW 20 COL 49.75
     TOG_TEXT AT ROW 21 COL 2
     TOG_EGEN AT ROW 21 COL 17
     TOG_SIDEXCEL AT ROW 21 COL 49.75 WIDGET-ID 2
     TOG_KON AT ROW 22 COL 2
     TOG_EXCELKALK AT ROW 22 COL 49.75 WIDGET-ID 8
     TOG_KONVAL AT ROW 23 COL 2
     TOG_EGENSIDA AT ROW 23 COL 49.75 WIDGET-ID 10
     ED_MEDD AT ROW 24.42 COL 8.5 NO-LABEL
     BTN_AVBLIST AT ROW 27.42 COL 76
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90.25 BY 27.75.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-3 ASSIGN
         HIDDEN             = YES
         TITLE              = "Listor"
         HEIGHT             = 27.79
         WIDTH              = 90.25
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 93.75
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 93.75
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WINDOW-3
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-LISTOR
   FRAME-NAME                                                           */
/* SETTINGS FOR COMBO-BOX CMB_PRIS IN FRAME FRAME-LISTOR
   ALIGN-L                                                              */
ASSIGN 
       ED_MEDD:HIDDEN IN FRAME FRAME-LISTOR           = TRUE
       ED_MEDD:RETURN-INSERTED IN FRAME FRAME-LISTOR  = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_ANM IN FRAME FRAME-LISTOR
   SHARED                                                               */
ASSIGN 
       TOG_ANM:HIDDEN IN FRAME FRAME-LISTOR           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_EGEN IN FRAME FRAME-LISTOR
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       TOG_EGEN:HIDDEN IN FRAME FRAME-LISTOR           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_EGENSIDA IN FRAME FRAME-LISTOR
   SHARED                                                               */
ASSIGN 
       TOG_EGENSIDA:HIDDEN IN FRAME FRAME-LISTOR           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_EXCELKALK IN FRAME FRAME-LISTOR
   SHARED                                                               */
ASSIGN 
       TOG_EXCELKALK:HIDDEN IN FRAME FRAME-LISTOR           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_ID IN FRAME FRAME-LISTOR
   SHARED                                                               */
ASSIGN 
       TOG_ID:HIDDEN IN FRAME FRAME-LISTOR           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_JMFKONS IN FRAME FRAME-LISTOR
   SHARED NO-ENABLE                                                     */
/* SETTINGS FOR TOGGLE-BOX TOG_KONSTRUKTION IN FRAME FRAME-LISTOR
   SHARED                                                               */
ASSIGN 
       TOG_KONSTRUKTION:HIDDEN IN FRAME FRAME-LISTOR           = TRUE.

ASSIGN 
       TOG_KONVAL:HIDDEN IN FRAME FRAME-LISTOR           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_MTRL IN FRAME FRAME-LISTOR
   SHARED                                                               */
ASSIGN 
       TOG_MTRL:HIDDEN IN FRAME FRAME-LISTOR           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_P3 IN FRAME FRAME-LISTOR
   SHARED                                                               */
ASSIGN 
       TOG_P3:HIDDEN IN FRAME FRAME-LISTOR           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_SIDEXCEL IN FRAME FRAME-LISTOR
   SHARED                                                               */
ASSIGN 
       TOG_SIDEXCEL:HIDDEN IN FRAME FRAME-LISTOR           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_SPANN IN FRAME FRAME-LISTOR
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       TOG_SPANN:HIDDEN IN FRAME FRAME-LISTOR           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_TEXT IN FRAME FRAME-LISTOR
   SHARED                                                               */
ASSIGN 
       TOG_TEXT:HIDDEN IN FRAME FRAME-LISTOR           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_UPP IN FRAME FRAME-LISTOR
   SHARED                                                               */
ASSIGN 
       TOG_UPP:HIDDEN IN FRAME FRAME-LISTOR           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_VAL IN FRAME FRAME-LISTOR
   SHARED                                                               */
ASSIGN 
       TOG_VAL:HIDDEN IN FRAME FRAME-LISTOR           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-3)
THEN WINDOW-3:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-3 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   RUN disable_UI.
END.
   

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
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
   {WIN_M_START.I}    
   IF summeradberedning = FALSE THEN EMPTY TEMP-TABLE kon_val NO-ERROR. 
   RUN btnlistor_UI.
   {BERTITLE.I}
   ASSIGN
   TOG_MTRL   = TOG_MTRL:CHECKED
   TOG_VAL    = TOG_VAL:CHECKED
   TOG_KONSTRUKTION = TOG_KONSTRUKTION:CHECKED
   /*TOG_SPANN  = TOG_SPANN:CHECKED*/
   TOG_UPP    = TOG_UPP:CHECKED
   TOG_ID     = TOG_ID:CHECKED
   TOG_ANM    = TOG_ANM:CHECKED
   TOG_TEXT   = TOG_TEXT:CHECKED
   TOG_EGEN   = TOG_EGEN:CHECKED
   TOG_P3     = TOG_P3:CHECKED
   TOG_KON    = TOG_KON:CHECKED
   TOG_KONVAL = TOG_KONVAL:CHECKED
   SEL_UPP = SEL_UPP:SCREEN-VALUE.
   Guru.GlobalaVariabler:Egengkontroll = TOG_EGEN.
   TOG_P3:LABEL = "Kalkylkoder". 
   RUN enable_UI.     
   {FRMSIZE.I}   
   IF summeradberedning = TRUE THEN DO:
      ASSIGN
      valkonst = TRUE.
      TOG_KONVAL:HIDDEN = TRUE.
      TOG_P3 = FALSE.
      TOG_P3:HIDDEN = TRUE. 
   END.
   BTN_AVBLIST:HIDDEN = FALSE.
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_SKRIVLIST:HANDLE.           
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = BTN_VISALIST:HANDLE.           
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = BTN_EXLIST:HANDLE.           
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = BTN_PDFLIST:HANDLE.
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = BTN_IELIST:HANDLE.           
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   tthandle = TEMP-TABLE bbenamntemp:HANDLE.
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "BBENAMNING", INPUT "").
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "BBENAMNING", INPUT "").
   END. 
   APPLY "VALUE-CHANGED" TO SEL_UPP.   
   {WIN_M_SLUT.I} 
    RUN PlaceraKnapp_UI IN btnlistorh.
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avb_UI WINDOW-3 
PROCEDURE avb_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF VALID-HANDLE(btnlistorh) THEN DO:
      RUN borthand_UI IN btnlistorh.
      DELETE PROCEDURE btnlistorh NO-ERROR.      
   END.                   
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnlishelp_UI WINDOW-3 
PROCEDURE btnlishelp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN
   CMB_SKRIV  = CMB_SKRIV:SCREEN-VALUE IN FRAME FRAME-LISTOR
   CMB_PRIS   = CMB_PRIS:SCREEN-VALUE
   CMB_STOLP  = CMB_STOLP:SCREEN-VALUE     
   TOG_MTRL  = TOG_MTRL:CHECKED 
   TOG_VAL   = TOG_VAL:CHECKED
   TOG_KONSTRUKTION = TOG_KONSTRUKTION:CHECKED   
   TOG_UPP   = TOG_UPP:CHECKED
   TOG_ID    = TOG_ID:CHECKED
   TOG_ANM   = TOG_ANM:CHECKED
   TOG_TEXT  = TOG_TEXT:CHECKED
   TOG_EGEN  = TOG_EGEN:CHECKED
   TOG_P3    = TOG_P3:CHECKED
   TOG_SIDEXCEL = TOG_SIDEXCEL:CHECKED   
   TOG_KON   = TOG_KON:CHECKED
   TOG_KONVAL = TOG_KONVAL:CHECKED.
   TOG_JMFKONS = TOG_JMFKONS:CHECKED.
   Guru.GlobalaVariabler:Egengkontroll = TOG_EGEN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnlistor_UI WINDOW-3 
PROCEDURE btnlistor_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {BTNLISTOR.I}   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-3  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-3)
  THEN DELETE WIDGET WINDOW-3.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-3  _DEFAULT-ENABLE
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
  DISPLAY SEL_UPP CMB_SKRIV CMB_PRIS CMB_STOLP TOG_MTRL TOG_KONSTRUKTION TOG_VAL 
          TOG_UPP TOG_ID TOG_ANM TOG_JMFKONS TOG_P3 TOG_TEXT TOG_SIDEXCEL 
          TOG_KON TOG_EXCELKALK TOG_KONVAL TOG_EGENSIDA ED_MEDD 
      WITH FRAME FRAME-LISTOR IN WINDOW WINDOW-3.
  ENABLE SEL_UPP BTN_SKRIVLIST BTN_VISALIST BTN_EXLIST BTN_EXLISTENK BTN_IELIST 
         BTN_PDFLIST CMB_SKRIV CMB_PRIS CMB_STOLP TOG_MTRL TOG_KONSTRUKTION 
         TOG_VAL TOG_UPP TOG_ID TOG_ANM TOG_P3 TOG_TEXT TOG_SIDEXCEL TOG_KON 
         TOG_EXCELKALK TOG_KONVAL TOG_EGENSIDA ED_MEDD BTN_AVBLIST 
      WITH FRAME FRAME-LISTOR IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-LISTOR}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE frame_UI WINDOW-3 
PROCEDURE frame_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vilkenframe AS CHARACTER NO-UNDO. 
   /*SKALL VARA KVAR*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE whandle_UI WINDOW-3 
PROCEDURE whandle_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER ordnr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER ordh AS HANDLE NO-UNDO.
   ASSIGN
   whandltemp.WF[ordnr] = ordh.
   ordnr = ordnr + 1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

