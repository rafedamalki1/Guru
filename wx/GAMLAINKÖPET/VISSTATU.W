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
DEFINE INPUT PARAMETER levtrprow AS ROWID NO-UNDO.
DEFINE INPUT  PARAMETER nytt_bestnr  AS INTEGER.
DEFINE INPUT  PARAMETER kalkrow  AS ROWID.
DEFINE INPUT  PARAMETER datvar AS DATE.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
{ANVTEMPS.I}
{GLOBVAR2DEL1.I}
&Scoped-define SHARED SHARED
{BEREDNINGTEMP.I}
{LEVTRPTEMP.I}
{DEPATEMP.I}
{BESTSTATTEMP.I}
{LEVKONTAKTTEMP.I}
{DEPSEKTEMP.I}
{KUNDNRTEMP.I}
   /*
{PERSONALTEMP.I}
*/
{LEVTEMP.I}
{HUVLEVTEMP.I}
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE varrec AS RECID NO-UNDO.
DEFINE VARIABLE visstatapph AS HANDLE NO-UNDO.
DEFINE VARIABLE bernrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE omrvar AS CHARACTER NO-UNDO.

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
&Scoped-define INTERNAL-TABLES bestatlevtemp

/* Definitions for BROWSE BRW_STATUS                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_STATUS bestatlevtemp.DATUM ~
bestatlevtemp.TID bestatlevtemp.BESTALLD bestatlevtemp.LEVNAMN ~
bestatlevtemp.ANVANDARE bestatlevtemp.BERDATUM 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_STATUS 
&Scoped-define QUERY-STRING-BRW_STATUS FOR EACH bestatlevtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_STATUS OPEN QUERY BRW_STATUS FOR EACH bestatlevtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_STATUS bestatlevtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_STATUS bestatlevtemp


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
DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta" 
     SIZE 14 BY 1
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_STATUS FOR 
      bestatlevtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_STATUS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_STATUS Dialog-Frame _STRUCTURED
  QUERY BRW_STATUS NO-LOCK DISPLAY
      bestatlevtemp.DATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
      bestatlevtemp.TID FORMAT "99.99":U
      bestatlevtemp.BESTALLD FORMAT "X(18)":U
      bestatlevtemp.LEVNAMN COLUMN-LABEL "Leverantör" FORMAT "x(25)":U
      bestatlevtemp.ANVANDARE COLUMN-LABEL "Användare" FORMAT "x(12)":U
      bestatlevtemp.BERDATUM COLUMN-LABEL "Ber.datum" FORMAT "99/99/99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 84.5 BY 7
         TITLE "Beställningsstatus".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BRW_STATUS AT ROW 1.5 COL 1.5
     BTN_AVB AT ROW 9 COL 72
     SPACE(0.62) SKIP(0.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Visa status".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Temp-Tables and Buffers:
      TABLE: bestatlevtemp T "?" NO-UNDO temp-db bestatlevtemp
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

ASSIGN 
       BRW_STATUS:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_STATUS
/* Query rebuild information for BROWSE BRW_STATUS
     _TblList          = "Temp-Tables.bestatlevtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.bestatlevtemp.DATUM
"bestatlevtemp.DATUM" "Datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   = Temp-Tables.bestatlevtemp.TID
     _FldNameList[3]   = Temp-Tables.bestatlevtemp.BESTALLD
     _FldNameList[4]   > Temp-Tables.bestatlevtemp.LEVNAMN
"bestatlevtemp.LEVNAMN" "Leverantör" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.bestatlevtemp.ANVANDARE
"bestatlevtemp.ANVANDARE" "Användare" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.bestatlevtemp.BERDATUM
"bestatlevtemp.BERDATUM" "Ber.datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_STATUS */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON END-ERROR OF FRAME Dialog-Frame /* Visa status */
DO:
   {BORTBRWPROC.I}
  IF VALID-HANDLE(visstatapph) THEN DELETE PROCEDURE visstatapph.
  RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON ENDKEY OF FRAME Dialog-Frame /* Visa status */
DO:
  APPLY "END-ERROR":U TO SELF.
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
  APPLY "END-ERROR":U TO SELF.
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
   FIND FIRST levtrptemp WHERE ROWID(levtrptemp) = levtrprow NO-ERROR.
   IF NOT AVAILABLE levtrptemp THEN RETURN. 
   FIND FIRST beredningtemp NO-ERROR.
   IF NOT AVAILABLE beredningtemp THEN RETURN.
   FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_lev NO-ERROR.
   CREATE elevtrptemp.
   BUFFER-COPY levtrptemp TO elevtrptemp.
   RUN laddatemptab_UI IN visstatapph (INPUT vald_lev,INPUT nytt_bestnr,INPUT datvar,
                                    INPUT TABLE beredningtemp,
                                    INPUT TABLE elevtrptemp,OUTPUT TABLE bestatlevtemp).
   FIND FIRST elevtrptemp NO-ERROR.
   IF AVAILABLE elevtrptemp THEN DELETE elevtrptemp.
   RUN enable_UI.  
   /*
   IF levtrptemp.BESTNR > 0 THEN DO:       
      FRAME {&FRAME-NAME}:TITLE = "Visa status för beställning nr:" + STRING(nytt_bestnr).
      OPEN QUERY BRW_STATUS FOR EACH bestatlevtemp WHERE bestatlevtemp.BESTNR = nytt_bestnr 
      AND bestatlevtemp.DEP-NR = levtrptemp.DEP-NR  AND bestatlevtemp.LEVKOD = levtemp.LEVKOD USE-INDEX BESTNR2 NO-LOCK. 
   END.
   ELSE 
   */
   IF levtrptemp.BERNR > 0 THEN DO:
      FIND FIRST beredningtemp NO-ERROR.
      FRAME {&FRAME-NAME}:TITLE = "Visa status för beredning nr:" + STRING(beredningtemp.BERNR).
      IF Guru.Konstanter:globforetag = "ELPA"  OR 
       Guru.Konstanter:globforetag = "VAST" THEN DO:
         OPEN QUERY BRW_STATUS FOR EACH bestatlevtemp WHERE bestatlevtemp.BERNR = beredningtemp.BERNR 
         AND bestatlevtemp.OMRADE = bestatlevtemp.OMRADE AND bestatlevtemp.BERDATUM = datvar AND 
          bestatlevtemp.LEVKOD = levtemp.LEVKOD NO-LOCK.
      END.
      ELSE DO:
         OPEN QUERY BRW_STATUS FOR EACH bestatlevtemp WHERE bestatlevtemp.BERNR = beredningtemp.BERNR 
         AND bestatlevtemp.OMRADE = beredningtemp.OMRADE AND bestatlevtemp.LEVKOD = levtemp.LEVKOD USE-INDEX BERNR2 NO-LOCK. 
      END.   
   END.              
   GET FIRST BRW_STATUS NO-LOCK.
   IF NOT AVAILABLE bestatlevtemp THEN DO:
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
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_STATUS:HANDLE IN FRAME {&FRAME-NAME}).
   IF Guru.Konstanter:appcon THEN DO:
      RUN VISSTATAPP.P PERSISTENT SET visstatapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN VISSTATAPP.P PERSISTENT SET visstatapph.
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

