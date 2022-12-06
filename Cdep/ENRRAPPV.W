&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
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

  Created: 10/14/96 -  1:20 pm

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
{GLOBVAR2DEL1.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{DEPATEMP.I}
{LEVTEMP.I}
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.         
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE dbenamning AS CHARACTER NO-UNDO.  
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO. 

DEFINE TEMP-TABLE lev_temp NO-UNDO
   FIELD LEVNAMN AS CHARACTER
   FIELD LEVKOD AS CHARACTER. 
   
DEFINE NEW SHARED TEMP-TABLE valdlev NO-UNDO
   FIELD LEVNAMN AS CHARACTER
   FIELD LEVKOD AS CHARACTER.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-C
&Scoped-define BROWSE-NAME BRW_LEV

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES lev_temp

/* Definitions for BROWSE BRW_LEV                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_LEV lev_temp.LEVKOD lev_temp.LEVNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_LEV 
&Scoped-define QUERY-STRING-BRW_LEV FOR EACH lev_temp NO-LOCK ~
    BY lev_temp.LEVKOD
&Scoped-define OPEN-QUERY-BRW_LEV OPEN QUERY BRW_LEV FOR EACH lev_temp NO-LOCK ~
    BY lev_temp.LEVKOD.
&Scoped-define TABLES-IN-QUERY-BRW_LEV lev_temp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_LEV lev_temp


/* Definitions for FRAME FRAME-C                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_LEV BTN_VISA BTN_skriv BTN_AVB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-3 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_skriv 
     LABEL "Skriv ut" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_LEV FOR 
      lev_temp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_LEV WINDOW-3 _STRUCTURED
  QUERY BRW_LEV NO-LOCK DISPLAY
      lev_temp.LEVKOD COLUMN-LABEL "Lev-id" FORMAT "x(4)":U
      lev_temp.LEVNAMN COLUMN-LABEL "Leverantör" FORMAT "x(25)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH MULTIPLE SIZE 34.5 BY 16.96
         TITLE "Leverantörer".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-C
     BRW_LEV AT ROW 2.42 COL 1.5
     BTN_VISA AT ROW 8 COL 39.5
     BTN_skriv AT ROW 9.1 COL 39.5
     BTN_AVB AT ROW 18.38 COL 39.5
     "Markera de leverantörer Ni vill se priser för." VIEW-AS TEXT
          SIZE 46.25 BY .75 AT ROW 1.25 COL 1.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 53.5 BY 18.75.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db lev_temp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-3 ASSIGN
         HIDDEN             = YES
         TITLE              = "Levrantörspriser"
         HEIGHT             = 18.83
         WIDTH              = 54.13
         MAX-HEIGHT         = 22
         MAX-WIDTH          = 82
         VIRTUAL-HEIGHT     = 22
         VIRTUAL-WIDTH      = 82
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
/* SETTINGS FOR FRAME FRAME-C
                                                                        */
/* BROWSE-TAB BRW_LEV TEXT-2 FRAME-C */
ASSIGN 
       BRW_LEV:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-C = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-3)
THEN WINDOW-3:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_LEV
/* Query rebuild information for BROWSE BRW_LEV
     _TblList          = "Temp-Tables.lev_temp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.lev_temp.LEVKOD|yes"
     _FldNameList[1]   > Temp-Tables.lev_temp.LEVKOD
"lev_temp.LEVKOD" "Lev-id" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.lev_temp.LEVNAMN
"lev_temp.LEVNAMN" "Leverantör" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_LEV */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_skriv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_skriv WINDOW-3
ON CHOOSE OF BTN_skriv IN FRAME FRAME-C /* Skriv ut */
DO:   
   RUN SKRIVVAL.W (INPUT FALSE).
   {muswait.i}            
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:                 
      skrivut = TRUE.     
      {AVBGOM.I}
      RUN ENRUTFV.W.
      {AVBFRAM.I}       
   END.     
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_skriv WINDOW-3
ON MOUSE-MENU-CLICK OF BTN_skriv IN FRAME FRAME-C /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VISA WINDOW-3
ON CHOOSE OF BTN_VISA IN FRAME FRAME-C /* Visa */
DO:
   {muswait.i}     
   FOR EACH valdlev.
      DELETE valdlev.
   END.    
   antal_valda = BRW_LEV:NUM-SELECTED-ROWS.
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = BRW_LEV:FETCH-SELECTED-ROW(antal_raknare).                      
      CREATE valdlev.
      ASSIGN
      valdlev.LEVKOD = lev_temp.LEVKOD
      valdlev.LEVNAMN = lev_temp.LEVNAMN
      antal_raknare = antal_raknare + 1.   
   END.  
   IF musz = TRUE THEN musz = FALSE.   
   ASSIGN    
   skrivut = FALSE.   
   {AVBGOM.I}
   RUN ENRUTFV.W.
   {AVBFRAM.I}
   {musarrow.i}             
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_LEV
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-3 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   {BORTBRWPROC.I}
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
   {muswait.i}
   {ALLSTARTDYN.I}
   FIND FIRST depatemp WHERE depatemp.DEP-NR = vald_depa.
   ASSIGN WINDOW-3:TITLE = "Lista " + Guru.Konstanter:genl + " - priser för depå - " + depatemp.BENAMNING. 
   FOR EACH levtemp WHERE levtemp.LEVKOD NE "0" 
   AND levtemp.BORTTAG = FALSE NO-LOCK:
      CREATE lev_temp.
      ASSIGN
      lev_temp.LEVKOD = levtemp.LEVKOD
      lev_temp.LEVNAMN = levtemp.LEVNAMN.
   END.   
   OPEN QUERY BRW_LEV FOR EACH lev_temp NO-LOCK BY lev_temp.LEVKOD INDEXED-REPOSITION.
  RUN enable_UI.   
   {FRMSIZE.I}   
  {musarrow.i}
   {WIN_M_SLUT.I}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-3 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/       
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_LEV:HANDLE IN FRAME {&FRAME-NAME}).      
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
  ENABLE BRW_LEV BTN_VISA BTN_skriv BTN_AVB 
      WITH FRAME FRAME-C IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-C}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

