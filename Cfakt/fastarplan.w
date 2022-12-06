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
&Scoped-define NEW 
{ALLDEF.I}

{FAKTTEMP.I}
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR faktaonrtemp.
DEFINE OUTPUT PARAMETER nyprisvar AS DECIMAL NO-UNDO.
/* Local Variable Definitions ---                                       */
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BRW_VAONR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES faktaonrtemp

/* Definitions for BROWSE BRW_VAONR                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_VAONR faktaonrtemp.AONR ~
faktaonrtemp.DELNR faktaonrtemp.OPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VAONR faktaonrtemp.AONR ~
faktaonrtemp.OPRIS 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_VAONR faktaonrtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_VAONR faktaonrtemp
&Scoped-define QUERY-STRING-BRW_VAONR FOR EACH faktaonrtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VAONR OPEN QUERY BRW_VAONR FOR EACH faktaonrtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VAONR faktaonrtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VAONR faktaonrtemp


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BRW_VAONR}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_VAONR FILL-IN-ANTALP BTN_OK BTN_AVBRYT 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_FAKTNR FILL-IN-ANTALP ~
FILL-IN-TOTALT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVBRYT AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-ANTALP AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Poster/år" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TOTALT AS DECIMAL FORMAT "->>>>>>>>>9.99":U INITIAL 0 
     LABEL "Totalt detta år" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_FAKTNR AS INTEGER FORMAT ">>>>>>9" INITIAL 0 
     LABEL "Fakturaplannr." 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_VAONR FOR 
      faktaonrtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_VAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VAONR Dialog-Frame _STRUCTURED
  QUERY BRW_VAONR NO-LOCK DISPLAY
      faktaonrtemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      faktaonrtemp.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      faktaonrtemp.OPRIS COLUMN-LABEL "Offertpris" FORMAT "->>>>>>>>9.99":U
  ENABLE
      faktaonrtemp.AONR
      faktaonrtemp.OPRIS
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 32 BY 6.42
         TITLE "Valda aonr".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN_FAKTNR AT ROW 1.5 COL 15.5 COLON-ALIGNED
     BRW_VAONR AT ROW 2.75 COL 1.5
     FILL-IN-ANTALP AT ROW 9.67 COL 10.5 COLON-ALIGNED
     FILL-IN-TOTALT AT ROW 9.67 COL 31.63 COLON-ALIGNED
     BTN_OK AT ROW 12.17 COL 19.63
     BTN_AVBRYT AT ROW 12.17 COL 34.63
     SPACE(1.49) SKIP(0.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Årsplan".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Temp-Tables and Buffers:
      TABLE: faktaonrtemp T "?" NO-UNDO temp-db faktaonrtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
/* BROWSE-TAB BRW_VAONR FILL-IN_FAKTNR Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       BRW_VAONR:MAX-DATA-GUESS IN FRAME Dialog-Frame         = 10000.

/* SETTINGS FOR FILL-IN FILL-IN-TOTALT IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_FAKTNR IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VAONR
/* Query rebuild information for BROWSE BRW_VAONR
     _TblList          = "Temp-Tables.faktaonrtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.faktaonrtemp.AONR
"faktaonrtemp.AONR" "Aonr" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.faktaonrtemp.DELNR
"faktaonrtemp.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.faktaonrtemp.OPRIS
"faktaonrtemp.OPRIS" "Offertpris" "->>>>>>>>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_VAONR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Årsplan */
DO:
   {BORTBRWPROC.I} 
   APPLY "END-ERROR":U TO SELF.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VAONR
&Scoped-define SELF-NAME BRW_VAONR
&Scoped-define SELF-NAME faktaonrtemp.OPRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktaonrtemp.OPRIS BRW_VAONR _BROWSE-COLUMN Dialog-Frame
ON ENTRY OF faktaonrtemp.OPRIS IN BROWSE BRW_VAONR /* Offertpris */
DO:
   DISPLAY faktaonrtemp.OPRIS WITH BROWSE BRW_VAONR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktaonrtemp.OPRIS BRW_VAONR _BROWSE-COLUMN Dialog-Frame
ON LEAVE OF faktaonrtemp.OPRIS IN BROWSE BRW_VAONR /* Offertpris */
DO:
   IF faktaonrtemp.OPRIS NE INPUT BROWSE BRW_VAONR faktaonrtemp.OPRIS THEN DO:
      RUN prisoff_UI (INPUT INPUT BROWSE BRW_VAONR faktaonrtemp.OPRIS, INPUT FALSE).         
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVBRYT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVBRYT Dialog-Frame
ON CHOOSE OF BTN_AVBRYT IN FRAME Dialog-Frame /* Avbryt */
DO:
   musz = TRUE.
    APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK Dialog-Frame
ON CHOOSE OF BTN_OK IN FRAME Dialog-Frame /* Ok */
DO:   
   FILL-IN-ANTALP = INPUT FILL-IN-ANTALP.
   nyprisvar = FILL-IN-TOTALT.
   IF FILL-IN-ANTALP <= 0 OR FILL-IN-ANTALP > 13 THEN DO:
      MESSAGE "Antal poster per år måste vara mellan 1 och 12." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   musz = FALSE.
   {muswait.i}                 
   IF Guru.Konstanter:appcon THEN DO:
      RUN PLANARFAST.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT 1,INPUT FILL-IN-ANTALP,INPUT-OUTPUT FILL-IN-TOTALT,INPUT-OUTPUT TABLE faktaonrtemp).
   END.
   ELSE DO:
      RUN PLANARFAST.P 
      (INPUT 1,INPUT FILL-IN-ANTALP,INPUT-OUTPUT FILL-IN-TOTALT,INPUT-OUTPUT TABLE faktaonrtemp).
   END.
   {musarrow.i}                 
   {BORTBRWPROC.I}
   APPLY "GO" TO FRAME {&FRAME-NAME}.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
   {muswait.i}   
   {ALLSTARTDYN.I}
   FIND FIRST faktaonrtemp NO-ERROR.
   IF NOT AVAILABLE faktaonrtemp THEN RETURN.
   FILL-IN_FAKTNR = faktaonrtemp.FAKTNR.
   FOR EACH faktaonrtemp:
      FILL-IN-TOTALT = FILL-IN-TOTALT + faktaonrtemp.OPRIS. 
   END.
   RUN enable_UI.
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
   ASSIGN
   faktaonrtemp.AONR:READ-ONLY IN BROWSE  BRW_VAONR = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_VAONR:HANDLE IN FRAME {&FRAME-NAME}).
   
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
  DISPLAY FILL-IN_FAKTNR FILL-IN-ANTALP FILL-IN-TOTALT 
      WITH FRAME Dialog-Frame.
  ENABLE BRW_VAONR FILL-IN-ANTALP BTN_OK BTN_AVBRYT 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prisoff_UI Dialog-Frame 
PROCEDURE prisoff_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER oprisvar AS DECIMAL NO-UNDO.
   DEFINE INPUT PARAMETER nyvar AS LOGICAL NO-UNDO.
   IF nyvar = FALSE THEN FILL-IN-TOTALT = FILL-IN-TOTALT - faktaonrtemp.OPRIS.
   faktaonrtemp.OPRIS = oprisvar.     
   FILL-IN-TOTALT = FILL-IN-TOTALT + faktaonrtemp.OPRIS.
   IF FILL-IN-TOTALT < 0 THEN FILL-IN-TOTALT = 0.
   DISPLAY FILL-IN-TOTALT WITH FRAME {&FRAME-NAME}.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

