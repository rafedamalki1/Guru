&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-2



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-2 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/09/15 -  2:57 pm

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
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}

{SOKDEF.I}
{REGVAR.I}

DEFINE SHARED VARIABLE listnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE franar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE tillar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.         

DEFINE VARIABLE str AS CHARACTER FORMAT "X(80)" NO-UNDO.
{TIDUTTTNEW.I}

DEFINE SHARED VARIABLE FILL-IN-PRISTYP AS CHARACTER FORMAT "X(9)":U INITIAL ? NO-UNDO.
DEFINE SHARED VARIABLE FILL-IN_ANLNR AS CHARACTER FORMAT "x(15)" NO-UNDO.
DEFINE SHARED VARIABLE FILL-IN_ARBANSVARIG AS CHARACTER FORMAT "x(5)" NO-UNDO.          
DEFINE SHARED VARIABLE FILL-IN_ARBARTKOD AS INTEGER FORMAT ">>>" INITIAL 0 NO-UNDO.
DEFINE SHARED VARIABLE FILL-IN_BEREDARE AS CHARACTER FORMAT "x(5)" NO-UNDO.
DEFINE SHARED VARIABLE FILL-IN_OMRADE AS CHARACTER FORMAT "x(6)" NO-UNDO.          
DEFINE SHARED VARIABLE FILL-IN_PKOD AS INTEGER FORMAT ">>" INITIAL 0 NO-UNDO.
DEFINE SHARED VARIABLE FILL-IN_SLUTVNR AS INTEGER FORMAT ">>>" INITIAL 0 NO-UNDO.   
DEFINE SHARED VARIABLE FILL-IN_STARTVNR AS INTEGER FORMAT ">>>" INITIAL 0 NO-UNDO.
DEFINE SHARED VARIABLE TOG_ANL AS LOGICAL INITIAL no 
     LABEL "ALLA ANLÄGGNINGAR" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY 1 NO-UNDO.
DEFINE VARIABLE varomr AS CHARACTER NO-UNDO.
DEFINE VARIABLE slutvecko AS INTEGER NO-UNDO.
DEFINE VARIABLE startvecko AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-TIDS
&Scoped-define BROWSE-NAME BRW_UT

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tidut

/* Definitions for BROWSE BRW_UT                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_UT tidut.ut 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UT 
&Scoped-define QUERY-STRING-BRW_UT FOR EACH tidut NO-LOCK
&Scoped-define OPEN-QUERY-BRW_UT OPEN QUERY BRW_UT FOR EACH tidut NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_UT tidut
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UT tidut


/* Definitions for FRAME FRAME-TIDS                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-TIDS ~
    ~{&OPEN-QUERY-BRW_UT}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_UT BTN_SKRIV BTN_AVS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKRIV 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE {&NEW} SHARED VARIABLE RAD_FAST AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga Plannr", no,
"Fasta Plannr", yes
     SIZE 32.5 BY 1
     BGCOLOR 8  NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE SEL_LIST AS CHARACTER INITIAL ? 
     VIEW-AS SELECTION-LIST SINGLE NO-DRAG SCROLLBAR-VERTICAL 
     SIZE 32.5 BY 2.5 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE SEL_OMR AS CHARACTER INITIAL ? 
     VIEW-AS SELECTION-LIST SINGLE NO-DRAG SCROLLBAR-VERTICAL 
     SIZE 21.5 BY 3 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_AVS AS LOGICAL INITIAL no 
     LABEL "Avslutade":L 
     VIEW-AS TOGGLE-BOX
     SIZE 20.5 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_BEN AS LOGICAL INITIAL no 
     LABEL "Visa benämning" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.5 BY .83 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_EJAV AS LOGICAL INITIAL no 
     LABEL "Ej avslutade":L 
     VIEW-AS TOGGLE-BOX
     SIZE 18.5 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_KONTO AS LOGICAL INITIAL no 
     LABEL "Visa konto" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .92 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_UT FOR 
      tidut SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_UT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UT WINDOW-2 _STRUCTURED
  QUERY BRW_UT NO-LOCK DISPLAY
      tidut.ut FORMAT "X(256)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 91.5 BY 26.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-TIDS
     BRW_UT AT ROW 1.5 COL 1.5
     SEL_LIST AT ROW 8 COL 5 NO-LABEL
     SEL_OMR AT ROW 8.5 COL 5.5 NO-LABEL
     TOG_EJAV AT ROW 8.75 COL 5
     RAD_FAST AT ROW 9 COL 4 NO-LABEL
     TOG_AVS AT ROW 9 COL 5
     TOG_BEN AT ROW 22.5 COL 53.5
     TOG_KONTO AT ROW 22.5 COL 72
     BTN_SKRIV AT ROW 28 COL 63.5
     BTN_AVS AT ROW 28 COL 79
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 93 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db tidut
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Plannr listor "
         HEIGHT             = 28.42
         WIDTH              = 93
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 93
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 93
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WINDOW-2
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-TIDS
                                                                        */
/* BROWSE-TAB BRW_UT 1 FRAME-TIDS */
ASSIGN 
       BRW_UT:HIDDEN  IN FRAME FRAME-TIDS                = TRUE
       BRW_UT:MAX-DATA-GUESS IN FRAME FRAME-TIDS         = 1000.

/* SETTINGS FOR RADIO-SET RAD_FAST IN FRAME FRAME-TIDS
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       RAD_FAST:HIDDEN IN FRAME FRAME-TIDS           = TRUE.

/* SETTINGS FOR SELECTION-LIST SEL_LIST IN FRAME FRAME-TIDS
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       SEL_LIST:HIDDEN IN FRAME FRAME-TIDS           = TRUE.

/* SETTINGS FOR SELECTION-LIST SEL_OMR IN FRAME FRAME-TIDS
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       SEL_OMR:HIDDEN IN FRAME FRAME-TIDS           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_AVS IN FRAME FRAME-TIDS
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       TOG_AVS:HIDDEN IN FRAME FRAME-TIDS           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_BEN IN FRAME FRAME-TIDS
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       TOG_BEN:HIDDEN IN FRAME FRAME-TIDS           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_EJAV IN FRAME FRAME-TIDS
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       TOG_EJAV:HIDDEN IN FRAME FRAME-TIDS           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_KONTO IN FRAME FRAME-TIDS
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       TOG_KONTO:HIDDEN IN FRAME FRAME-TIDS           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UT
/* Query rebuild information for BROWSE BRW_UT
     _TblList          = "temp-db.tidut"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > temp-db.tidut.ut
"tidut.ut" ? "X(256)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_UT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-TIDS
/* Query rebuild information for FRAME FRAME-TIDS
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-TIDS */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS WINDOW-2
ON CHOOSE OF BTN_AVS IN FRAME FRAME-TIDS /* Avsluta */
DO:
   {BORTBRWPROC.I}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON CHOOSE OF BTN_SKRIV IN FRAME FRAME-TIDS /* Skriv ut */
DO: 
   RUN SKRIVVAL.W (INPUT FALSE).
   IF musz = TRUE THEN musz = FALSE.
   ELSE RUN EKLOGS.P.
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON MOUSE-MENU-CLICK OF BTN_SKRIV IN FRAME FRAME-TIDS /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_BEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_BEN WINDOW-2
ON VALUE-CHANGED OF TOG_BEN IN FRAME FRAME-TIDS /* Visa benämning */
DO:
   TOG_BEN = INPUT TOG_BEN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_KONTO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_KONTO WINDOW-2
ON VALUE-CHANGED OF TOG_KONTO IN FRAME FRAME-TIDS /* Visa konto */
DO:
    TOG_KONTO = INPUT TOG_KONTO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_UT
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-2 


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
   {muswait.i}   
   {ALLSTARTDYN.I}
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   IF listnr = 1 THEN DO: 
      RUN appat_UI (INPUT 1). /*viaorg_UI.*/
   END.
   ELSE IF listnr = 2 THEN DO: 
      RUN appat_UI (INPUT 2). /*videborg_UI.*/
   END.
/*    ELSE IF listnr = 3 THEN RUN viarborg_UI. */
/*    ELSE IF listnr = 4 THEN RUN vipriorg_UI. */
/*    ELSE IF listnr = 5 THEN RUN vivnrorg_UI. */
   ELSE IF listnr = 3 THEN DO:
      RUN appat_UI (INPUT 3). /*vibesorg_UI.*/
   END.
   ELSE IF listnr = 6 THEN DO:
      RUN appat_UI (INPUT 6). /*vibesorg_UI.*/
   END.
/*    ELSE IF listnr = 7 THEN RUN viberorg_UI. */
   ELSE IF listnr = 8 THEN DO:
      RUN appat_UI (INPUT 8). /*vianvorg_UI.*/
   END.
   ELSE IF listnr = 19 THEN DO:
      RUN appat_UI (INPUT 19). /*vianvorg_UI.*/
   END.
/*    ELSE IF listnr = 10 THEN RUN vianlorg_UI. */
   IF skrivut = FALSE THEN DO: 
      BRW_UT:HIDDEN = FALSE.
   END.
   ELSE DO:
      IF musz = TRUE THEN musz = FALSE.
      ELSE RUN EKLOGS.P.
      /*status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").*/
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}.     
      LEAVE MAIN-BLOCK. 
   END.
   IF FILL-IN_OMRADE = ? THEN DO:
      ASSIGN FILL-IN_OMRADE = INPUT SEL_OMR.
   END.

   RUN enable_UI.   
   {FRMSIZE.I}     
   ASSIGN
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_AVS:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = BTN_SKRIV:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   {musarrow.i}    
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-2 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_UT:HANDLE IN FRAME {&FRAME-NAME}).
  
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE appat_UI WINDOW-2 
PROCEDURE appat_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vartvar AS INTEGER NO-UNDO.
   
   RUN ladda_UI. 
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN VGENRAPPA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT vartvar,INPUT TABLE valsoktemp,OUTPUT TABLE tidut).
   END.                                               
   ELSE DO:                                           
      RUN VGENRAPPA.P 
      (INPUT vartvar,INPUT TABLE valsoktemp,OUTPUT TABLE tidut).          
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-2  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
  THEN DELETE WIDGET WINDOW-2.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-2  _DEFAULT-ENABLE
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
  ENABLE BRW_UT BTN_SKRIV BTN_AVS 
      WITH FRAME FRAME-TIDS IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-TIDS}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ladda_UI WINDOW-2 
PROCEDURE ladda_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST valsoktemp NO-ERROR.
   IF AVAILABLE valsoktemp THEN DELETE valsoktemp.
   CREATE valsoktemp.
   ASSIGN
   valsoktemp.SOKCHAR[1] = SEL_OMR
   valsoktemp.SOKCHAR[2] = SEL_LIST
   valsoktemp.SOKLOG[1] = TOG_AVS
   valsoktemp.SOKLOG[2] = TOG_KONTO
   valsoktemp.SOKLOG[3] = TOG_BEN
   valsoktemp.SOKLOG[4] = TOG_EJAV
   valsoktemp.SOKLOG[5] = RAD_FAST
   valsoktemp.SOKCHAR[3] = FILL-IN-PRISTYP
   valsoktemp.SOKCHAR[4] = FILL-IN_OMRADE
   valsoktemp.SOKCHAR[5] = FILL-IN_ARBANSVARIG
   valsoktemp.SOKINT[1] =  tillar
   valsoktemp.SOKINT[2] =  franar
   valsoktemp.SOKINT[3] = FILL-IN_ARBARTKOD
   valsoktemp.SOKDATE[1] =  bdatum   
   valsoktemp.SOKDATE[2] =  avdatum. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

