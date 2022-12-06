&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE mtrlbapph AS HANDLE NO-UNDO.
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{LEVTEMP.I}         
{LTRPTEMP.I}
DEFINE TEMP-TABLE bstattemp NO-UNDO
   FIELD BESTNR     AS  INTEGER FORMAT ">>>"                             
   FIELD Dep-Nr     AS  INTEGER FORMAT ">>>" LABEL "Depå-nr"                            
   FIELD LEVKOD    AS  CHARACTER                       
   FIELD OMRADE    AS  CHARACTER                            
   FIELD ANVANDARE     AS  CHARACTER LABEL "Användare"                             
   FIELD BESTALLD     AS  CHARACTER  LABEL "Beställd"
   FIELD BERDATUM     AS  DATE
   FIELD BERNR AS  INTEGER                        
   FIELD DATUM     AS  DATE LABEL "Datum"                         
   FIELD TID    AS  DECIMAL LABEL "Klockan"   
   INDEX BESTNR2 IS PRIMARY dep-nr BESTNR DATUM TID
   INDEX BERNR2 OMRADE BERNR DATUM TID.



DEFINE INPUT  PARAMETER vlev  AS CHARACTER.
DEFINE INPUT  PARAMETER depnr  AS INTEGER.
DEFINE INPUT  PARAMETER nytt_bestnr  AS INTEGER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BRW_STATUS

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES bstattemp levtemp

/* Definitions for BROWSE BRW_STATUS                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_STATUS bstattemp.DATUM bstattemp.TID ~
bstattemp.BESTALLD levtemp.LEVNAMN bstattemp.ANVANDARE bstattemp.BERDATUM 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_STATUS 
&Scoped-define QUERY-STRING-BRW_STATUS FOR EACH bstattemp NO-LOCK, ~
      EACH levtemp WHERE levtemp.LEVKOD = bstattemp.LEVKOD NO-LOCK
&Scoped-define OPEN-QUERY-BRW_STATUS OPEN QUERY BRW_STATUS FOR EACH bstattemp NO-LOCK, ~
      EACH levtemp WHERE levtemp.LEVKOD = bstattemp.LEVKOD NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_STATUS bstattemp levtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_STATUS bstattemp
&Scoped-define SECOND-TABLE-IN-QUERY-BRW_STATUS levtemp


/* Definitions for DIALOG-BOX Dialog-Frame                              */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_STATUS BTN_AVB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta" 
     SIZE 14 BY 1
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_STATUS FOR 
      bstattemp, 
      levtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_STATUS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_STATUS Dialog-Frame _STRUCTURED
  QUERY BRW_STATUS NO-LOCK DISPLAY
      bstattemp.DATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
      bstattemp.TID FORMAT "99.99":U
      bstattemp.BESTALLD FORMAT "X(18)":U
      levtemp.LEVNAMN COLUMN-LABEL "Leverantör" FORMAT "x(25)":U
      bstattemp.ANVANDARE COLUMN-LABEL "Användare" FORMAT "x(12)":U
      bstattemp.BERDATUM COLUMN-LABEL "Ber.datum" FORMAT "99/99/99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 86.5 BY 9.08
         TITLE "Beställningsstatus".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BRW_STATUS AT ROW 1.25 COL 1.5
     BTN_AVB AT ROW 10.75 COL 74
     SPACE(0.62) SKIP(0.20)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Visa status".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Temp-Tables and Buffers:
      TABLE: bstattemp T "?" NO-UNDO temp-db bstattemp
      TABLE: levtemp T "?" NO-UNDO temp-db levtemp
      TABLE: ? T "?" NO-UNDO temp-db vispers
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_STATUS 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_STATUS
/* Query rebuild information for BROWSE BRW_STATUS
     _TblList          = "Temp-Tables.bstattemp,Temp-Tables.levtemp WHERE Temp-Tables.bstattemp ..."
     _Options          = "NO-LOCK"
     _JoinCode[2]      = "Temp-Tables.levtemp.LEVKOD = Temp-Tables.bstattemp.LEVKOD"
     _FldNameList[1]   > Temp-Tables.bstattemp.DATUM
"bstattemp.DATUM" "Datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   = Temp-Tables.bstattemp.TID
     _FldNameList[3]   = Temp-Tables.bstattemp.BESTALLD
     _FldNameList[4]   > Temp-Tables.levtemp.LEVNAMN
"levtemp.LEVNAMN" "Leverantör" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.bstattemp.ANVANDARE
"bstattemp.ANVANDARE" "Användare" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.bstattemp.BERDATUM
"bstattemp.BERDATUM" "Ber.datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_STATUS */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON END-ERROR OF FRAME Dialog-Frame /* Visa status */
DO:
  IF VALID-HANDLE(mtrlbapph) THEN DELETE PROCEDURE mtrlbapph.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON ENDKEY OF FRAME Dialog-Frame /* Visa status */
DO:
  IF VALID-HANDLE(mtrlbapph) THEN DELETE PROCEDURE mtrlbapph.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Visa status */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB Dialog-Frame
ON CHOOSE OF BTN_AVB IN FRAME Dialog-Frame /* Avsluta */
DO:
  IF VALID-HANDLE(mtrlbapph) THEN DELETE PROCEDURE mtrlbapph.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_STATUS
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
   
   FIND FIRST ltrptemp WHERE ltrptemp.LEVKOD = vlev AND ltrptemp.BESTNR = nytt_bestnr 
   AND ltrptemp.DEP-NR = depnr AND ltrptemp.BERNR = 0 USE-INDEX BESTNR2 NO-LOCK NO-ERROR.   
   IF NOT AVAILABLE ltrptemp THEN RETURN.   
   RUN bstathmt_UI IN mtrlbapph (INPUT nytt_bestnr,INPUT depnr,OUTPUT TABLE bstattemp).
   RUN enable_UI.       
   IF ltrptemp.BESTNR > 0 THEN DO:       
      FRAME {&FRAME-NAME}:TITLE = "Visa status för beställning nr:" + STRING(nytt_bestnr).
      OPEN QUERY BRW_STATUS FOR EACH bstattemp WHERE bstattemp.BESTNR = nytt_bestnr 
      AND bstattemp.DEP-NR = ltrptemp.DEP-NR USE-INDEX BESTNR2 NO-LOCK,
      EACH levtemp WHERE bstattemp.LEVKOD = levtemp.LEVKOD NO-LOCK. 
   END.
   
   GET FIRST BRW_STATUS NO-LOCK.
   IF NOT AVAILABLE bstattemp THEN DO:
      MESSAGE "Beställning är inte skickad" VIEW-AS ALERT-BOX.
      LEAVE MAIN-BLOCK.
   END.  
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
   IF Guru.Konstanter:appcon THEN DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph.
   END.   
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
  ENABLE BRW_STATUS BTN_AVB 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

