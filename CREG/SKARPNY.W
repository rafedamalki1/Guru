&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-3



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-3 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/05/04 -  1:19 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER valfaktrec AS RECID NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}

{GLOBVAR2DEL1.I}
DEFINE SHARED VARIABLE skarpproch AS HANDLE NO-UNDO. /* SKARPAPP.P */
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO. /* DYNLADDATEMP.P */

&SCOPED-DEFINE NEW NEW
&SCOPED-DEFINE SHARED SHARED 
{OMRALLT.I}
&SCOPED-DEFINE NEW 
{SKARPTEMP.I}
&SCOPED-DEFINE SHARED
{AVDELNINGTEMP.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-3
&Scoped-define BROWSE-NAME BRW_AVD

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES avdelningtemp omrallt

/* Definitions for BROWSE BRW_AVD                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_AVD avdelningtemp.AVDELNINGNR ~
avdelningtemp.AVDELNINGNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AVD 
&Scoped-define QUERY-STRING-BRW_AVD FOR EACH avdelningtemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_AVD OPEN QUERY BRW_AVD FOR EACH avdelningtemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_AVD avdelningtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AVD avdelningtemp


/* Definitions for BROWSE BRW_OMR                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_OMR omrallt.OMRADE omrallt.NAMN ~
omrallt.ORGIDNUM 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_OMR 
&Scoped-define QUERY-STRING-BRW_OMR FOR EACH omrallt NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_OMR OPEN QUERY BRW_OMR FOR EACH omrallt NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_OMR omrallt
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_OMR omrallt


/* Definitions for DIALOG-BOX DIALOG-3                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_OMR BRW_AVD FILL-IN_OMRADE ~
FILL-IN_ARKIVSTALLE FILL-IN_ARTAL FILL-IN_LOPNR BTN_OK BTN_AVBRYT 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_AVDELNINGNR FILL-IN_OMRADE ~
FILL-IN_ARKIVSTALLE FILL-IN_ARTAL FILL-IN_LOPNR 

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

DEFINE VARIABLE FILL-IN_ARKIVSTALLE AS CHARACTER FORMAT "X(8)" 
     LABEL "Arkivställe" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_ARTAL AS INTEGER FORMAT "9999" INITIAL 0 
     LABEL "Årtal" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE FILL-IN_AVDELNINGNR AS INTEGER FORMAT ">>>9" INITIAL 0 
     LABEL "Avdelning" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE FILL-IN_LOPNR AS INTEGER FORMAT "->>>>>>9" INITIAL 0 
     LABEL "Nästa lediga nr" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE FILL-IN_OMRADE AS CHARACTER FORMAT "x(6)" 
     LABEL "Område" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AVD FOR 
      avdelningtemp SCROLLING.

DEFINE QUERY BRW_OMR FOR 
      omrallt SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AVD DIALOG-3 _STRUCTURED
  QUERY BRW_AVD NO-LOCK DISPLAY
      avdelningtemp.AVDELNINGNR COLUMN-LABEL "Avdelning" FORMAT ">>>9":U
      avdelningtemp.AVDELNINGNAMN COLUMN-LABEL "Namn" FORMAT "x(16)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 30 BY 9.17.

DEFINE BROWSE BRW_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_OMR DIALOG-3 _STRUCTURED
  QUERY BRW_OMR NO-LOCK DISPLAY
      omrallt.OMRADE COLUMN-LABEL "Område" FORMAT "x(6)":U
      omrallt.NAMN COLUMN-LABEL "Namn" FORMAT "x(16)":U
      omrallt.ORGIDNUM COLUMN-LABEL "Numerisk.!org" FORMAT "X(5)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 35.88 BY 9.17.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-3
     BRW_OMR AT ROW 1.42 COL 1.5
     BRW_AVD AT ROW 1.42 COL 39
     FILL-IN_AVDELNINGNR AT ROW 11.21 COL 31.5 COLON-ALIGNED
     FILL-IN_OMRADE AT ROW 12.5 COL 31.5 COLON-ALIGNED
     FILL-IN_ARKIVSTALLE AT ROW 13.71 COL 31.5 COLON-ALIGNED
     FILL-IN_ARTAL AT ROW 15 COL 31.5 COLON-ALIGNED
     FILL-IN_LOPNR AT ROW 16.21 COL 31.5 COLON-ALIGNED
     BTN_OK AT ROW 18.17 COL 40
     BTN_AVBRYT AT ROW 18.17 COL 55
     SPACE(0.74) SKIP(0.32)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Nummerserier":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: avdelningtemp T "?" NO-UNDO temp-db avdelningtemp
      TABLE: omrallt T "?" NO-UNDO temp-db omrallt
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-3
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_OMR 1 DIALOG-3 */
/* BROWSE-TAB BRW_AVD BRW_OMR DIALOG-3 */
ASSIGN 
       FRAME DIALOG-3:SCROLLABLE       = FALSE
       FRAME DIALOG-3:HIDDEN           = TRUE.

ASSIGN 
       BRW_AVD:ALLOW-COLUMN-SEARCHING IN FRAME DIALOG-3 = TRUE.

ASSIGN 
       BRW_OMR:ALLOW-COLUMN-SEARCHING IN FRAME DIALOG-3 = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_AVDELNINGNR IN FRAME DIALOG-3
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AVD
/* Query rebuild information for BROWSE BRW_AVD
     _TblList          = "Temp-Tables.avdelningtemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.avdelningtemp.AVDELNINGNR
"avdelningtemp.AVDELNINGNR" "Avdelning" ">>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.avdelningtemp.AVDELNINGNAMN
"avdelningtemp.AVDELNINGNAMN" "Namn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AVD */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_OMR
/* Query rebuild information for BROWSE BRW_OMR
     _TblList          = "Temp-Tables.omrallt"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.omrallt.OMRADE
"omrallt.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.omrallt.NAMN
"omrallt.NAMN" "Namn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.omrallt.ORGIDNUM
"omrallt.ORGIDNUM" "Numerisk.!org" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_OMR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-3
/* Query rebuild information for DIALOG-BOX DIALOG-3
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-3 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-3 DIALOG-3
ON END-ERROR OF FRAME DIALOG-3 /* Nummerserier */
DO:
   {muswait.i}
   musz = TRUE.
   IF VALID-HANDLE(laddaproch) THEN DELETE PROCEDURE laddaproch. 
   {BORTBRWPROC.I}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-3 DIALOG-3
ON ENDKEY OF FRAME DIALOG-3 /* Nummerserier */
DO:    
   APPLY "END-ERROR" TO FRAME {&FRAME-NAME}.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AVD
&Scoped-define SELF-NAME BRW_AVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AVD DIALOG-3
ON VALUE-CHANGED OF BRW_AVD IN FRAME DIALOG-3
DO:
   IF AVAILABLE avdelningtemp THEN DO:
      FILL-IN_AVDELNINGNR = avdelningtemp.AVDELNINGNR.
      DISPLAY FILL-IN_AVDELNINGNR WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_OMR
&Scoped-define SELF-NAME BRW_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_OMR DIALOG-3
ON VALUE-CHANGED OF BRW_OMR IN FRAME DIALOG-3
DO:
   IF AVAILABLE omrallt THEN DO:
      FILL-IN_OMRADE = omrallt.OMRADE.
      DISPLAY FILL-IN_OMRADE WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVBRYT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVBRYT DIALOG-3
ON CHOOSE OF BTN_AVBRYT IN FRAME DIALOG-3 /* Avbryt */
DO:
   APPLY "END-ERROR" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-3
ON CHOOSE OF BTN_OK IN FRAME DIALOG-3 /* Ok */
DO:
   {muswait.i}
   musz = FALSE.
   ASSIGN
   FILL-IN_ARKIVSTALLE = INPUT FILL-IN_ARKIVSTALLE               
   FILL-IN_ARTAL = INPUT FILL-IN_ARTAL      
   FILL-IN_AVDELNINGNR = INPUT FILL-IN_AVDELNINGNR
   FILL-IN_LOPNR = INPUT FILL-IN_LOPNR      
   FILL-IN_OMRADE = INPUT FILL-IN_OMRADE.
   IF valfaktrec = ? THEN DO: 
      RUN skapafaktskarp_UI IN skarpproch (OUTPUT valfaktrec).
      CREATE faktskarptemp.
      faktskarptemp.FAKTREC = valfaktrec.
   END.
   ELSE DO: 
      FIND FIRST faktskarptemp WHERE faktskarptemp.FAKTREC = valfaktrec NO-LOCK.
   END.
   ASSIGN 
   faktskarptemp.ARKIVSTALLE = FILL-IN_ARKIVSTALLE 
   faktskarptemp.ARTAL = FILL-IN_ARTAL      
   faktskarptemp.AVDELNINGNR = FILL-IN_AVDELNINGNR
   faktskarptemp.LOPNR = FILL-IN_LOPNR      
   faktskarptemp.OMRADE = FILL-IN_OMRADE.
   RUN sparafaktskarp_UI IN skarpproch (INPUT valfaktrec, INPUT TABLE faktskarptemp).
   IF VALID-HANDLE(laddaproch) THEN DELETE PROCEDURE laddaproch. 
   {BORTBRWPROC.I}
   APPLY "GO" TO FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AVD
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-3 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {DIA_M_START.I}
   {muswait.i}
   {ALLSTARTDYN.I}
   RUN openbdynspec_UI IN brwproc[1].
   RUN openbdynspec_UI IN brwproc[2].
   
   IF valfaktrec = ? THEN DO: 
      ASSIGN 
      FRAME {&FRAME-NAME}:TITLE = "Ny"
      FILL-IN_ARTAL = YEAR(TODAY).
   END.
   ELSE DO:
      FIND faktskarptemp WHERE faktskarptemp.FAKTREC = valfaktrec NO-LOCK.
      ASSIGN 
      FRAME {&FRAME-NAME}:TITLE = "Ändra"
      FILL-IN_ARKIVSTALLE = faktskarptemp.ARKIVSTALLE                  
      FILL-IN_ARTAL = faktskarptemp.ARTAL
      FILL-IN_AVDELNINGNR = faktskarptemp.AVDELNINGNR
      FILL-IN_LOPNR = faktskarptemp.LOPNR
      FILL-IN_OMRADE = faktskarptemp.OMRADE.
      FIND FIRST omrallt WHERE omrallt.OMRADE = faktskarptemp.OMRADE NO-LOCK NO-ERROR.
      IF AVAILABLE omrallt THEN RUN selectbyrowid_UI IN brwproc[1] (INPUT ROWID(omrallt)).
      FIND FIRST avdelningtemp WHERE avdelningtemp.AVDELNINGNR = faktskarptemp.AVDELNINGNR NO-LOCK NO-ERROR.
      IF AVAILABLE avdelningtemp THEN RUN selectbyrowid_UI IN brwproc[2] (INPUT ROWID(avdelningtemp)).
   END.
   RUN enable_UI.
   {FRMSIZED.I} 
   avdelningtemp.AVDELNINGNR:LABEL IN BROWSE BRW_AVD= Guru.Konstanter:gavdk.
   {musarrow.i}
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI DIALOG-3 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_OMR:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[2] 
      (INPUT BRW_AVD:HANDLE IN FRAME {&FRAME-NAME}).
   tthandle = TEMP-TABLE omrallt:HANDLE.
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "OMRADETAB", INPUT "").
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "OMRADETAB", INPUT "").
   END.
   tthandle = TEMP-TABLE avdelningtemp:HANDLE.
   RUN laddatemp_UI IN laddaproch 
      (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "AVDELNING", INPUT " WHERE AVDELNING.KOSTMASK = 0 ").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-3  _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-3.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-3  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN_AVDELNINGNR FILL-IN_OMRADE FILL-IN_ARKIVSTALLE FILL-IN_ARTAL 
          FILL-IN_LOPNR 
      WITH FRAME DIALOG-3.
  ENABLE BRW_OMR BRW_AVD FILL-IN_OMRADE FILL-IN_ARKIVSTALLE FILL-IN_ARTAL 
         FILL-IN_LOPNR BTN_OK BTN_AVBRYT 
      WITH FRAME DIALOG-3.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-3}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

