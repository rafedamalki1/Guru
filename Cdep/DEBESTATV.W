&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-1



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 11/19/96 -  4:02 pm

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
DEFINE NEW SHARED VARIABLE levtyp AS INTEGER NO-UNDO.  
DEFINE NEW SHARED VARIABLE levtill AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE vald_best AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.

DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO. 
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 

DEFINE VARIABLE debeapph AS HANDLE NO-UNDO.
DEFINE VARIABLE best_nr_koll  AS INTEGER NO-UNDO.
DEFINE VARIABLE lev_nr_koll  AS INTEGER NO-UNDO.
DEFINE VARIABLE best_namn_koll AS CHARACTER NO-UNDO. 
&Scoped-define NEW  
&Scoped-define SHARED 
{bestnrtab.I}
{LEVNRTAB.I}    
/*DEFINE TEMP-TABLE lev_nr_tab NO-UNDO
    FIELD BESTNR AS INTEGER LABEL "BestNr"    
    FIELD BESTDATUM AS DATE LABEL "Best-Datum"
    FIELD LEVNAMN AS CHARACTER LABEL "Leverantör"
    FIELD LEVKOD AS CHARACTER
    FIELD BESTALLARE AS CHARACTER LABEL "Beställare"
    FIELD BENAMNING AS CHARACTER LABEL "Benämning".*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_BEST

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES best_nr_tab lev_nr_tab

/* Definitions for BROWSE BRW_BEST                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_BEST best_nr_tab.Bestnr ~
best_nr_tab.LEVNAMN best_nr_tab.BESTALLARE best_nr_tab.BENAMNING ~
best_nr_tab.Bestdatum 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BEST best_nr_tab.Bestnr 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_BEST best_nr_tab
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_BEST best_nr_tab
&Scoped-define QUERY-STRING-BRW_BEST FOR EACH best_nr_tab NO-LOCK ~
    BY best_nr_tab.Bestnr ~
       BY best_nr_tab.LEVNAMN INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_BEST OPEN QUERY BRW_BEST FOR EACH best_nr_tab NO-LOCK ~
    BY best_nr_tab.Bestnr ~
       BY best_nr_tab.LEVNAMN INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_BEST best_nr_tab
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BEST best_nr_tab


/* Definitions for BROWSE BRW_LEV                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_LEV lev_nr_tab.Bestnr lev_nr_tab.LEVNAMN ~
lev_nr_tab.BESTALLARE lev_nr_tab.BENAMNING lev_nr_tab.Bestdatum 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_LEV 
&Scoped-define QUERY-STRING-BRW_LEV FOR EACH lev_nr_tab NO-LOCK ~
    BY lev_nr_tab.Bestnr
&Scoped-define OPEN-QUERY-BRW_LEV OPEN QUERY BRW_LEV FOR EACH lev_nr_tab NO-LOCK ~
    BY lev_nr_tab.Bestnr.
&Scoped-define TABLES-IN-QUERY-BRW_LEV lev_nr_tab
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_LEV lev_nr_tab


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD_VAL BRW_LEV BRW_BEST FBTN_VISA BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS RAD_VAL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY DEFAULT 
     LABEL "Avsluta" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ej beställd", 1,
"Beställd", 2
     SIZE 40.38 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_BEST FOR 
      best_nr_tab SCROLLING.

DEFINE QUERY BRW_LEV FOR 
      lev_nr_tab SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_BEST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BEST WINDOW-1 _STRUCTURED
  QUERY BRW_BEST NO-LOCK DISPLAY
      best_nr_tab.Bestnr FORMAT ">>>>>9":U
      best_nr_tab.LEVNAMN COLUMN-LABEL "Leverantör" FORMAT "x(12)":U
            WIDTH 23.5
      best_nr_tab.BESTALLARE FORMAT "x(12)":U
      best_nr_tab.BENAMNING COLUMN-LABEL "Status" FORMAT "x(15)":U
      best_nr_tab.Bestdatum COLUMN-LABEL "Datum" FORMAT "99/99/99":U
  ENABLE
      best_nr_tab.Bestnr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 64 BY 24
         TITLE "Ej beställd".

DEFINE BROWSE BRW_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_LEV WINDOW-1 _STRUCTURED
  QUERY BRW_LEV NO-LOCK DISPLAY
      lev_nr_tab.Bestnr FORMAT ">>>>>9":U
      lev_nr_tab.LEVNAMN COLUMN-LABEL "Leverantör" FORMAT "x(12)":U
            WIDTH 22.5
      lev_nr_tab.BESTALLARE FORMAT "x(12)":U
      lev_nr_tab.BENAMNING COLUMN-LABEL "Status" FORMAT "x(15)":U
      lev_nr_tab.Bestdatum FORMAT "99/99/99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 64 BY 24
         TITLE "Beställd".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     RAD_VAL AT ROW 1.5 COL 1.5 NO-LABEL
     BRW_LEV AT ROW 3.17 COL 1.5
     BRW_BEST AT ROW 3.17 COL 1.5
     FBTN_VISA AT ROW 3.88 COL 68
     BTN_AVB AT ROW 27.75 COL 68
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 82.13 BY 28.42
         CANCEL-BUTTON BTN_AVB.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db best_nr_tab
      TABLE: ? T "?" NO-UNDO temp-db lev_nr_tab
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 28.42
         WIDTH              = 82.25
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 82.25
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 82.25
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
/* SETTINGS FOR WINDOW WINDOW-1
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
/* BROWSE-TAB BRW_LEV RAD_VAL FRAME-A */
/* BROWSE-TAB BRW_BEST BRW_LEV FRAME-A */
ASSIGN 
       BRW_BEST:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_BEST:MAX-DATA-GUESS IN FRAME FRAME-A         = 10000
       BRW_BEST:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

ASSIGN 
       BRW_LEV:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_LEV:MAX-DATA-GUESS IN FRAME FRAME-A         = 10000
       BRW_LEV:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BEST
/* Query rebuild information for BROWSE BRW_BEST
     _TblList          = "Temp-Tables.best_nr_tab"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Temp-Tables.best_nr_tab.Bestnr|yes,Temp-Tables.best_nr_tab.LEVNAMN|yes"
     _FldNameList[1]   > Temp-Tables.best_nr_tab.Bestnr
"best_nr_tab.Bestnr" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.best_nr_tab.LEVNAMN
"best_nr_tab.LEVNAMN" "Leverantör" "x(12)" "character" ? ? ? ? ? ? no ? no no "23.5" yes no no "U" "" ""
     _FldNameList[3]   = Temp-Tables.best_nr_tab.BESTALLARE
     _FldNameList[4]   > Temp-Tables.best_nr_tab.BENAMNING
"best_nr_tab.BENAMNING" "Status" "x(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.best_nr_tab.Bestdatum
"best_nr_tab.Bestdatum" "Datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_BEST */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_LEV
/* Query rebuild information for BROWSE BRW_LEV
     _TblList          = "Temp-Tables.lev_nr_tab"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.lev_nr_tab.Bestnr|yes"
     _FldNameList[1]   = Temp-Tables.lev_nr_tab.Bestnr
     _FldNameList[2]   > Temp-Tables.lev_nr_tab.LEVNAMN
"lev_nr_tab.LEVNAMN" "Leverantör" "x(12)" "character" ? ? ? ? ? ? no ? no no "22.5" yes no no "U" "" ""
     _FldNameList[3]   = Temp-Tables.lev_nr_tab.BESTALLARE
     _FldNameList[4]   > Temp-Tables.lev_nr_tab.BENAMNING
"lev_nr_tab.BENAMNING" "Status" "x(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   = Temp-Tables.lev_nr_tab.Bestdatum
     _Query            is NOT OPENED
*/  /* BROWSE BRW_LEV */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_BEST
&Scoped-define SELF-NAME BRW_BEST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_BEST WINDOW-1
ON VALUE-CHANGED OF BRW_BEST IN FRAME FRAME-A /* Ej beställd */
DO: 
   ASSIGN   
   vald_best = best_nr_tab.BESTNR
   vald_lev = best_nr_tab.LEVKOD.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_LEV
&Scoped-define SELF-NAME BRW_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LEV WINDOW-1
ON VALUE-CHANGED OF BRW_LEV IN FRAME FRAME-A /* Beställd */
DO:         
   ASSIGN   
   vald_best = lev_nr_tab.BESTNR.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA WINDOW-1
ON CHOOSE OF FBTN_VISA IN FRAME FRAME-A /* Visa */
DO:
   {muswait.i} 
   IF levtyp = 1 THEN DO: 
      FIND FIRST best_nr_tab NO-LOCK NO-ERROR.
      IF AVAILABLE best_nr_tab THEN DO:
         IF musz = TRUE THEN musz = FALSE.   
         ASSIGN    
         skrivut = FALSE.
         {AVBGOM.I}   
         RUN VISALEVEV.W.
         {AVBFRAM.I} 
      END.
      ELSE DO:
         MESSAGE "Det finns inte några 'ej färdiga leveranser' att visa." 
         VIEW-AS ALERT-BOX.
      END.
   END.   
   ELSE DO:
      FIND FIRST lev_nr_tab NO-LOCK NO-ERROR.
      IF AVAILABLE lev_nr_tab THEN DO:
         IF musz = TRUE THEN musz = FALSE.   
         ASSIGN    
         skrivut = FALSE.
         {AVBGOM.I}   
         RUN VISALEVEV.W.
         {AVBFRAM.I} 
      END.
      ELSE DO:
         MESSAGE "Det finns inte några 'färdiga leveranser' att visa." 
         VIEW-AS ALERT-BOX.
      END.
   END.            
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL WINDOW-1
ON VALUE-CHANGED OF RAD_VAL IN FRAME FRAME-A
DO:
   RAD_VAL = INPUT RAD_VAL.
   levtill = 1.
   IF RAD_VAL = 1 THEN DO: 
      ASSIGN
      BRW_LEV:HIDDEN = TRUE
      BRW_BEST:HIDDEN = FALSE
      levtyp = 1.
      FIND FIRST best_nr_tab NO-LOCK NO-ERROR.
      IF AVAILABLE best_nr_tab THEN   
      APPLY "VALUE-CHANGED" TO BRW_BEST IN FRAME FRAME-A.
   END.
   ELSE DO:
      ASSIGN
      BRW_LEV:HIDDEN = FALSE
      BRW_BEST:HIDDEN = TRUE
      levtyp = 2.
      FIND FIRST lev_nr_tab NO-LOCK NO-ERROR.
      IF AVAILABLE lev_nr_tab THEN 
      APPLY "VALUE-CHANGED" TO BRW_LEV IN FRAME FRAME-A.
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_BEST
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO: 
   {BORTBRWPROC.I}
   IF VALID-HANDLE(debeapph) THEN DELETE PROCEDURE debeapph.
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
   ASSIGN WINDOW-1:TITLE = "Beställningsstatus för depå - " + depatemp.BENAMNING.    
   RUN besthmt_UI IN debeapph (INPUT vald_depa,OUTPUT TABLE best_nr_tab,OUTPUT TABLE lev_nr_tab ).             
   
   RUN setcolsortvar_UI IN brwproc[1] (INPUT "").
   RUN openbdynspec_UI IN brwproc[1].      
   RUN setcolsortvar_UI IN brwproc[2] (INPUT "").
   RUN openbdynspec_UI IN brwproc[2].      
   
   ASSIGN
   RAD_VAL = 1
   levtyp = 1
   levtill = 1.
   RUN enable_UI.   
   {FRMSIZE.I}  
   FIND FIRST best_nr_tab NO-LOCK NO-ERROR.
   IF AVAILABLE best_nr_tab THEN 
   APPLY "VALUE-CHANGED" TO BRW_BEST IN FRAME FRAME-A.
   BRW_LEV:HIDDEN = TRUE.
   {musarrow.i}
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/       
   
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_BEST:HANDLE IN FRAME {&FRAME-NAME}).         
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_LEV:HANDLE IN FRAME {&FRAME-NAME}).         
   IF Guru.Konstanter:appcon THEN DO:
      RUN DEBEAPP.P PERSISTENT SET debeapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN DEBEAPP.P PERSISTENT SET debeapph.
   END.      

   /*RUN setcolindex_UI IN brwproc[1] (INPUT "best_nr_tab.Bestnr BY best_nr_tab.LEVNAMN ").
   RUN setcolindex_UI IN brwproc[2] (INPUT "best_nr_tab.Bestnr ").*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-1  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
  THEN DELETE WIDGET WINDOW-1.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-1  _DEFAULT-ENABLE
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
  DISPLAY RAD_VAL 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RAD_VAL BRW_LEV BRW_BEST FBTN_VISA BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

