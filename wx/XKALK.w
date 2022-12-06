&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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

   &SCOPED-DEFINE NEW NEW
 &SCOPED-DEFINE SHARED  SHARED
 DEFINE NEW SHARED VARIABLE tth AS HANDLE NO-UNDO.


DEFINE NEW SHARED VARIABLE retvalkoll AS LOGICAL NO-UNDO. 
DEFINE VARIABLE hcur AS INTEGER NO-UNDO.
DEFINE VARIABLE retval AS INTEGER NO-UNDO.
DEFINE VARIABLE brwproc AS HANDLE EXTENT 25 NO-UNDO.
DEFINE VARIABLE handproc AS HANDLE EXTENT 25 NO-UNDO.
DEFINE VARIABLE apphandass AS HANDLE NO-UNDO.
DEFINE VARIABLE apphandklar AS HANDLE NO-UNDO.
DEFINE VARIABLE appprogok AS LOGICAL NO-UNDO.
&GLOBAL-DEFINE ARROWS 3
DEFINE VARIABLE tthandle AS HANDLE NO-UNDO.
DEFINE VARIABLE ReturnValue AS INTEGER.

 

        
{FRAMSIZETEMP.I}
{GLOBVAR2DEL1.I}
DEFINE NEW SHARED VARIABLE appcon AS LOGICAL NO-UNDO.


   {FASTKALKTEMP.I}
{KALKTEMP3.I}
      /*{EGENBVAR.I}*/
   /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_KALK

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES efastkalktemp

/* Definitions for BROWSE BRW_KALK                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_KALK efastkalktemp.ARBKOD ~
efastkalktemp.LOPNR efastkalktemp.BENAMNING efastkalktemp.ANTAL ~
efastkalktemp.ENHET efastkalktemp.F1 efastkalktemp.F2 ~
efastkalktemp.MASKINTIMMAR efastkalktemp.UTRUST efastkalktemp.EA ~
efastkalktemp.ARBETE efastkalktemp.MATERIEL efastkalktemp.MASKINKOST ~
efastkalktemp.UTRUSTKOST efastkalktemp.OVRIGT efastkalktemp.SUMMA 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KALK efastkalktemp.LOPNR ~
efastkalktemp.BENAMNING efastkalktemp.ANTAL efastkalktemp.F1 ~
efastkalktemp.F2 efastkalktemp.MASKINTIMMAR efastkalktemp.UTRUST ~
efastkalktemp.EA efastkalktemp.ARBETE efastkalktemp.MATERIEL ~
efastkalktemp.MASKINKOST efastkalktemp.UTRUSTKOST efastkalktemp.OVRIGT 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_KALK efastkalktemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_KALK efastkalktemp
&Scoped-define QUERY-STRING-BRW_KALK FOR EACH efastkalktemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KALK OPEN QUERY BRW_KALK FOR EACH efastkalktemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KALK efastkalktemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KALK efastkalktemp


/* Definitions for FRAME FRAME-KALK                                     */

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_BORT 
     LABEL "tA BORT" 
     SIZE 15 BY 1.13.

DEFINE BUTTON BTN_NY 
     LABEL "NY" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE FILL-IN-ARBF AS DECIMAL FORMAT "->>>>>>>9.99":U INITIAL 1 
     LABEL "Faktor Arbetskostnad" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MASKF AS DECIMAL FORMAT "->>>>>>>9.99":U INITIAL 1 
     LABEL "Faktor Maskin" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MTRLF AS DECIMAL FORMAT "->>>>>>>9.99":U INITIAL 1 
     LABEL "Faktor Materiel" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-OVRF AS DECIMAL FORMAT "->>>>>>>9.99":U INITIAL 1 
     LABEL "Faktor Övrigt" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TOTSUM AS DECIMAL FORMAT "->>>>>>>9.99":U INITIAL 0 
     LABEL "Total summa" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-UTRF AS DECIMAL FORMAT "->>>>>>>9.99":U INITIAL 1 
     LABEL "Faktor Utrustning" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_KALK FOR 
      efastkalktemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_KALK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KALK C-Win _STRUCTURED
  QUERY BRW_KALK NO-LOCK DISPLAY
      efastkalktemp.ARBKOD FORMAT "X(5)":U
      efastkalktemp.LOPNR COLUMN-LABEL "Löpnr" FORMAT ">>>":U
      efastkalktemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(20)":U
            WIDTH 10
      efastkalktemp.ANTAL FORMAT "->>>>9.99":U
      efastkalktemp.ENHET FORMAT "X(3)":U
      efastkalktemp.F1 COLUMN-LABEL "F1!TEST" FORMAT ">>>9.99":U
      efastkalktemp.F2 COLUMN-LABEL "F2!TEST" FORMAT ">>>9.99":U
      efastkalktemp.MASKINTIMMAR COLUMN-LABEL "MASKIN!TIMMAR" FORMAT "->>>>9.99":U
      efastkalktemp.UTRUST COLUMN-LABEL "Utrustning!TEST" FORMAT ">>>9.99":U
      efastkalktemp.EA FORMAT ">>>9.99":U
      efastkalktemp.ARBETE COLUMN-LABEL "Arbete!TEST" FORMAT "->>>>9.99":U
      efastkalktemp.MATERIEL COLUMN-LABEL "Materiel!TEST" FORMAT "->>>>>>9.99":U
      efastkalktemp.MASKINKOST COLUMN-LABEL "Maskinkost!TEST" FORMAT "->>>>>>9.99":U
      efastkalktemp.UTRUSTKOST COLUMN-LABEL "Utrustnings!kostnad" FORMAT "->>>>>>9.99":U
      efastkalktemp.OVRIGT COLUMN-LABEL "Övrigt!TEST" FORMAT "->>>>>>9.99":U
      efastkalktemp.SUMMA FORMAT "->>>>>>>>9.99":U
  ENABLE
      efastkalktemp.LOPNR
      efastkalktemp.BENAMNING
      efastkalktemp.ANTAL
      efastkalktemp.F1
      efastkalktemp.F2
      efastkalktemp.MASKINTIMMAR
      efastkalktemp.UTRUST
      efastkalktemp.EA
      efastkalktemp.ARBETE
      efastkalktemp.MATERIEL
      efastkalktemp.MASKINKOST
      efastkalktemp.UTRUSTKOST
      efastkalktemp.OVRIGT
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 123.5 BY 16.75 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125.25 BY 26.33.

DEFINE FRAME FRAME-KALK
     FILL-IN-MASKF AT ROW 1.5 COL 23.25 COLON-ALIGNED
     FILL-IN-MTRLF AT ROW 1.5 COL 44.75
     FILL-IN-UTRF AT ROW 1.5 COL 81.25
     FILL-IN-ARBF AT ROW 3.5 COL 3.25
     FILL-IN-OVRF AT ROW 3.5 COL 59.75 COLON-ALIGNED
     BRW_KALK AT ROW 5.25 COL 1.5
     BTN_NY AT ROW 22.5 COL 44
     BTN_BORT AT ROW 22.5 COL 63.5
     FILL-IN-TOTSUM AT ROW 24.5 COL 57.5 COLON-ALIGNED
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 2.58
         SIZE 124.5 BY 26.67.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: efastkalktemp T "?" NO-UNDO temp-db efastkalktemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 26.33
         WIDTH              = 125.25
         MAX-HEIGHT         = 26.33
         MAX-WIDTH          = 125.25
         VIRTUAL-HEIGHT     = 26.33
         VIRTUAL-WIDTH      = 125.25
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-KALK:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* SETTINGS FOR FRAME FRAME-KALK
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_KALK FILL-IN-OVRF FRAME-KALK */
ASSIGN 
       FRAME FRAME-KALK:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-ARBF IN FRAME FRAME-KALK
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-MTRLF IN FRAME FRAME-KALK
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-TOTSUM:READ-ONLY IN FRAME FRAME-KALK        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-UTRF IN FRAME FRAME-KALK
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KALK
/* Query rebuild information for BROWSE BRW_KALK
     _TblList          = "Temp-Tables.efastkalktemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.efastkalktemp.ARBKOD
     _FldNameList[2]   > Temp-Tables.efastkalktemp.LOPNR
"efastkalktemp.LOPNR" "Löpnr" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.efastkalktemp.BENAMNING
"efastkalktemp.BENAMNING" "Benämning" "X(20)" "character" ? ? ? ? ? ? yes ? no no "10" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.efastkalktemp.ANTAL
"efastkalktemp.ANTAL" ? "->>>>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[5]   = Temp-Tables.efastkalktemp.ENHET
     _FldNameList[6]   > Temp-Tables.efastkalktemp.F1
"efastkalktemp.F1" "F1!TEST" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.efastkalktemp.F2
"efastkalktemp.F2" "F2!TEST" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > Temp-Tables.efastkalktemp.MASKINTIMMAR
"efastkalktemp.MASKINTIMMAR" "MASKIN!TIMMAR" "->>>>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[9]   > Temp-Tables.efastkalktemp.UTRUST
"efastkalktemp.UTRUST" "Utrustning!TEST" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[10]   > Temp-Tables.efastkalktemp.EA
"efastkalktemp.EA" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[11]   > Temp-Tables.efastkalktemp.ARBETE
"efastkalktemp.ARBETE" "Arbete!TEST" "->>>>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[12]   > Temp-Tables.efastkalktemp.MATERIEL
"efastkalktemp.MATERIEL" "Materiel!TEST" "->>>>>>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[13]   > Temp-Tables.efastkalktemp.MASKINKOST
"efastkalktemp.MASKINKOST" "Maskinkost!TEST" "->>>>>>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[14]   > Temp-Tables.efastkalktemp.UTRUSTKOST
"efastkalktemp.UTRUSTKOST" "Utrustnings!kostnad" "->>>>>>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[15]   > Temp-Tables.efastkalktemp.OVRIGT
"efastkalktemp.OVRIGT" "Övrigt!TEST" "->>>>>>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[16]   > Temp-Tables.efastkalktemp.SUMMA
"efastkalktemp.SUMMA" ? "->>>>>>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KALK */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KALK
&Scoped-define FRAME-NAME FRAME-KALK
&Scoped-define SELF-NAME BRW_KALK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KALK C-Win
ON ROW-LEAVE OF BRW_KALK IN FRAME FRAME-KALK
DO:
   RUN visa_UI.   
   ASSIGN
   efastkalktemp.ANTAL         = INPUT BROWSE BRW_KALK efastkalktemp.ANTAL       
   efastkalktemp.ARBETE        = INPUT BROWSE BRW_KALK efastkalktemp.ARBETE      
   efastkalktemp.EA            = INPUT BROWSE BRW_KALK efastkalktemp.EA          
   efastkalktemp.F1            = INPUT BROWSE BRW_KALK efastkalktemp.F1          
   efastkalktemp.F2            = INPUT BROWSE BRW_KALK efastkalktemp.F2          
   efastkalktemp.MASKINKOST    = INPUT BROWSE BRW_KALK efastkalktemp.MASKINKOST  
   efastkalktemp.MASKINTIMMAR  = INPUT BROWSE BRW_KALK efastkalktemp.MASKINTIMMAR
   efastkalktemp.MATERIEL      = INPUT BROWSE BRW_KALK efastkalktemp.MATERIEL    
   efastkalktemp.OVRIGT        = INPUT BROWSE BRW_KALK efastkalktemp.OVRIGT      
   efastkalktemp.UTRUST        = INPUT BROWSE BRW_KALK efastkalktemp.UTRUST      
   efastkalktemp.UTRUSTKOST    = INPUT BROWSE BRW_KALK efastkalktemp.UTRUSTKOST.  
   RUN visa_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT C-Win
ON CHOOSE OF BTN_BORT IN FRAME FRAME-KALK /* tA BORT */
DO:
   MESSAGE "Vill du verkligen ta bort denna post?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Kalkyl" 
   UPDATE answer AS LOGICAL.
   IF answer THEN DO TRANSACTION:
      RUN selnextprevrow_UI IN brwproc[1].
      DELETE efastkalktemp.
      RUN openbdynspec_UI IN brwproc[1].
      RUN lastselectdyn_UI IN brwproc[1].
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY C-Win
ON CHOOSE OF BTN_NY IN FRAME FRAME-KALK /* NY */
DO:
   DEBUGGER:SET-BREAK().
   DEFINE VARIABLE erow AS ROWID NO-UNDO.
   CREATE efastkalktemp.
   erow = ROWID(efastkalktemp).
   efastkalktemp.ARBKOD = "EGEN".
   efastkalktemp.lopnr = 33.
  
   RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(efastkalktemp)).
   RUN openbdynspec_UI IN brwproc[1].
   RUN lastselectdyn_UI IN brwproc[1].
   RUN visa_UI.  
   /*
   APPLY "ENTRY" TO efastkalktemp.ARBKOD IN BROWSE BRW_KALk.     
    */
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ARBF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ARBF C-Win
ON LEAVE OF FILL-IN-ARBF IN FRAME FRAME-KALK /* Faktor Arbetskostnad */
DO:
   RUN faktorer_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-MASKF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-MASKF C-Win
ON LEAVE OF FILL-IN-MASKF IN FRAME FRAME-KALK /* Faktor Maskin */
DO:
   RUN faktorer_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-MTRLF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-MTRLF C-Win
ON LEAVE OF FILL-IN-MTRLF IN FRAME FRAME-KALK /* Faktor Materiel */
DO:
   RUN faktorer_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-OVRF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-OVRF C-Win
ON LEAVE OF FILL-IN-OVRF IN FRAME FRAME-KALK /* Faktor Övrigt */
DO:
   RUN faktorer_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-UTRF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-UTRF C-Win
ON LEAVE OF FILL-IN-UTRF IN FRAME FRAME-KALK /* Faktor Utrustning */
DO:
   RUN faktorer_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {ALLSTARTDYN.I}         
   {STARTWIN.I}
  RUN enable_UI.
  {&WINDOW-NAME}:HIDDEN = FALSE.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-2 
PROCEDURE allstartbrw_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_KALK:HANDLE IN FRAME FRAME-KALK).            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY FILL-IN-MASKF FILL-IN-MTRLF FILL-IN-UTRF FILL-IN-ARBF FILL-IN-OVRF 
          FILL-IN-TOTSUM 
      WITH FRAME FRAME-KALK IN WINDOW C-Win.
  ENABLE FILL-IN-MASKF FILL-IN-MTRLF FILL-IN-UTRF FILL-IN-ARBF FILL-IN-OVRF 
         BRW_KALK BTN_NY BTN_BORT FILL-IN-TOTSUM 
      WITH FRAME FRAME-KALK IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-KALK}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

