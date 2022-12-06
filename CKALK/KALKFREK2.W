&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}

DEFINE SHARED VARIABLE dokalkproch AS HANDLE NO-UNDO. /* DOKALKAPP.P */
&SCOPED-DEFINE NEW
&SCOPED-DEFINE SHARED SHARED
{LOPTEMP.I}
&SCOPED-DEFINE SHARED
{FREKVENSTEMP.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BRW_FREK

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES frekvenstemp

/* Definitions for BROWSE BRW_FREK                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_FREK frekvenstemp.FREKOD ~
frekvenstemp.FREKNR frekvenstemp.ANTAL frekvenstemp.BENAMNING ~
frekvenstemp.ENHET 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_FREK 
&Scoped-define QUERY-STRING-BRW_FREK FOR EACH frekvenstemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_FREK OPEN QUERY BRW_FREK FOR EACH frekvenstemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_FREK frekvenstemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_FREK frekvenstemp


/* Definitions for DIALOG-BOX Dialog-Frame                              */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_FREK BTN_OK FILL-IN-ARBKOD FILL-IN-LOPNR ~
FILL-IN-BEN FILL-IN-ENHET 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-ARBKOD FILL-IN-LOPNR FILL-IN-BEN ~
FILL-IN-ENHET 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_OK 
     LABEL "OK" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-ARBKOD AS CHARACTER FORMAT "X(256)":U 
     LABEL "Arbestkod" 
      VIEW-AS TEXT 
     SIZE 24.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
      VIEW-AS TEXT 
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENHET AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enhet" 
      VIEW-AS TEXT 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LOPNR AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Löpnr" 
      VIEW-AS TEXT 
     SIZE 14 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_FREK FOR 
      frekvenstemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_FREK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_FREK Dialog-Frame _STRUCTURED
  QUERY BRW_FREK NO-LOCK DISPLAY
      frekvenstemp.FREKOD COLUMN-LABEL "Arbkod" FORMAT "X(5)":U
      frekvenstemp.FREKNR COLUMN-LABEL "Löpnr" FORMAT ">>>":U
      frekvenstemp.ANTAL COLUMN-LABEL "Antal" FORMAT "->>>>9.999<<<":U
      frekvenstemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(40)":U
      frekvenstemp.ENHET COLUMN-LABEL "Enhet" FORMAT "X(5)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 73.5 BY 20
         TITLE "Frekvenskoder".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BRW_FREK AT ROW 6.38 COL 1.5
     BTN_OK AT ROW 26.79 COL 61
     FILL-IN-ARBKOD AT ROW 1.38 COL 11.5 COLON-ALIGNED
     FILL-IN-LOPNR AT ROW 2.63 COL 11.5 COLON-ALIGNED
     FILL-IN-BEN AT ROW 3.88 COL 11.5 COLON-ALIGNED
     FILL-IN-ENHET AT ROW 5.13 COL 11.5 COLON-ALIGNED
     SPACE(48.87) SKIP(22.11)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "<insert dialog title>"
         DEFAULT-BUTTON BTN_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Temp-Tables and Buffers:
      TABLE: frekvenstemp T "?" NO-UNDO temp-db frekvenstemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_FREK 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_FREK
/* Query rebuild information for BROWSE BRW_FREK
     _TblList          = "Temp-Tables.frekvenstemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.frekvenstemp.FREKOD
"frekvenstemp.FREKOD" "Arbkod" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.frekvenstemp.FREKNR
"frekvenstemp.FREKNR" "Löpnr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.frekvenstemp.ANTAL
"frekvenstemp.ANTAL" "Antal" "->>>>9.999<<<" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.frekvenstemp.BENAMNING
"frekvenstemp.BENAMNING" "Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.frekvenstemp.ENHET
"frekvenstemp.ENHET" "Enhet" "X(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_FREK */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON END-ERROR OF FRAME Dialog-Frame /* <insert dialog title> */
DO:
   {BORTBRWPROC.I}      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON ENDKEY OF FRAME Dialog-Frame /* <insert dialog title> */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* <insert dialog title> */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK Dialog-Frame
ON CHOOSE OF BTN_OK IN FRAME Dialog-Frame /* OK */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_FREK
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {DIA_M_START.I}
   {ALLSTARTDYN.I}
   FILL-IN-LOPNR:FORMAT =  Guru.Konstanter:varforetypchar[6].
   FRAME {&FRAME-NAME}:TITLE = "Frekvens".
   FIND FIRST extraloptemp NO-LOCK NO-ERROR.
   ASSIGN
   FILL-IN-ARBKOD = extraloptemp.ARBKOD
   FILL-IN-LOPNR = extraloptemp.LOPNR
   FILL-IN-BEN = SUBSTRING(extraloptemp.BENAMNING,1,49)
   FILL-IN-ENHET = extraloptemp.ENHET.      
   RUN enable_UI. 
   /*KALKÅR*/
   RUN laddafrek_UI IN dokalkproch (INPUT extraloptemp.ARBKOD, INPUT extraloptemp.LOPNR, 
                                    INPUT extraloptemp.KATAR, OUTPUT TABLE frekvenstemp).
   RUN openbdynspec_UI IN brwproc[1].
   {FRMSIZED.I}
   {musarrow.i}
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI Dialog-Frame 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_FREK:HANDLE IN FRAME {&FRAME-NAME}).   
   RUN setcolindex_UI IN brwproc[1] (INPUT "ARBKOD BY lopnr by FREKOD by FREKNR"). 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-ARBKOD FILL-IN-LOPNR FILL-IN-BEN FILL-IN-ENHET 
      WITH FRAME Dialog-Frame.
  ENABLE BRW_FREK BTN_OK FILL-IN-ARBKOD FILL-IN-LOPNR FILL-IN-BEN FILL-IN-ENHET 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

