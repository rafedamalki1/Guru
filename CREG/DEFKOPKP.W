&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 08/12/97 -  1:16 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER defprisid AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */
{ALLDEF.I}

DEFINE SHARED VARIABLE defprisproch AS HANDLE NO-UNDO. /* Procedure handle, DEFPRISAPP.P */
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE franprisid AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE befprislisttemp NO-UNDO 
   FIELD NAMN        AS CHARACTER FORMAT "x(20)"
   FIELD PRISID      AS CHARACTER FORMAT "X(8)"
   FIELD BEFATTNING  AS CHARACTER FORMAT "x(20)"
   FIELD PRISA       AS DECIMAL FORMAT "->>>>9.99"
   FIELD PRISRES     AS DECIMAL FORMAT "->>>>9.99"
   INDEX PRISID AS PRIMARY PRISID BEFATTNING.
&SCOPED-DEFINE NEW
&SCOPED-DEFINE SHARED SHARED
{OVERTEXTEMP.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_BEST

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES defpristemp befprislisttemp

/* Definitions for BROWSE BRW_BEST                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_BEST defpristemp.PRISNUM ~
defpristemp.PRISTEXT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BEST defpristemp.PRISNUM 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_BEST defpristemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_BEST defpristemp
&Scoped-define QUERY-STRING-BRW_BEST FOR EACH defpristemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_BEST OPEN QUERY BRW_BEST FOR EACH defpristemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_BEST defpristemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BEST defpristemp


/* Definitions for BROWSE BRW_KUND                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_KUND befprislisttemp.NAMN ~
befprislisttemp.PRISA befprislisttemp.PRISRES 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KUND befprislisttemp.NAMN 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_KUND befprislisttemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_KUND befprislisttemp
&Scoped-define QUERY-STRING-BRW_KUND FOR EACH befprislisttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KUND OPEN QUERY BRW_KUND FOR EACH befprislisttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KUND befprislisttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KUND befprislisttemp


/* Definitions for DIALOG-BOX DIALOG-1                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_BEST BTN_KOP BTN_AVSL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVSL AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KOP 
     LABEL "Kopiera":L 
     SIZE 14 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_BEST FOR 
      defpristemp SCROLLING.

DEFINE QUERY BRW_KUND FOR 
      befprislisttemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_BEST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BEST DIALOG-1 _STRUCTURED
  QUERY BRW_BEST NO-LOCK DISPLAY
      defpristemp.PRISNUM COLUMN-LABEL "Pris!lista" FORMAT ">>>>>>9":U
      defpristemp.PRISTEXT COLUMN-LABEL "Text" FORMAT "X(20)":U
  ENABLE
      defpristemp.PRISNUM
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 36.75 BY 11.

DEFINE BROWSE BRW_KUND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KUND DIALOG-1 _STRUCTURED
  QUERY BRW_KUND NO-LOCK DISPLAY
      befprislisttemp.NAMN FORMAT "x(20)":U
      befprislisttemp.PRISA COLUMN-LABEL "Tim pris" FORMAT "->>>>9.99":U
      befprislisttemp.PRISRES COLUMN-LABEL "Res pris" FORMAT "->>>>9.99":U
  ENABLE
      befprislisttemp.NAMN
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 44.5 BY 11
         TITLE "Gällande priser".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BRW_BEST AT ROW 2.92 COL 1.5
     BRW_KUND AT ROW 2.92 COL 39.88
     BTN_KOP AT ROW 4.38 COL 86.63
     BTN_AVSL AT ROW 12.92 COL 87.13
     "Välj prislista som du vill kopiera från !" VIEW-AS TEXT
          SIZE 42.25 BY 1 AT ROW 1.42 COL 1.5
          FONT 17
     SPACE(57.74) SKIP(11.95)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Kopiera prislistor".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: befprislisttemp T "?" NO-UNDO temp-db befprislisttemp
      TABLE: defpristemp T "?" NO-UNDO temp-db defpristemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_BEST TEXT-1 DIALOG-1 */
/* BROWSE-TAB BRW_KUND BRW_BEST DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE BRW_KUND IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BEST
/* Query rebuild information for BROWSE BRW_BEST
     _TblList          = "Temp-Tables.defpristemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.defpristemp.PRISNUM
"defpristemp.PRISNUM" "Pris!lista" ">>>>>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.defpristemp.PRISTEXT
"defpristemp.PRISTEXT" "Text" "X(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_BEST */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KUND
/* Query rebuild information for BROWSE BRW_KUND
     _TblList          = "Temp-Tables.befprislisttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.befprislisttemp.NAMN
"befprislisttemp.NAMN" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.befprislisttemp.PRISA
"befprislisttemp.PRISA" "Tim pris" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.befprislisttemp.PRISRES
"befprislisttemp.PRISRES" "Res pris" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KUND */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Kopiera prislistor */
DO:
   {muswait.i}
   musz = TRUE.
   {BORTBRWPROC.I}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Kopiera prislistor */
DO:
   {muswait.i}
   musz = TRUE.
   {BORTBRWPROC.I}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_BEST
&Scoped-define SELF-NAME BRW_BEST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_BEST DIALOG-1
ON VALUE-CHANGED OF BRW_BEST IN FRAME DIALOG-1
DO:         
   {muswait.i}  
   BRW_BEST:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   IF AVAILABLE defpristemp THEN DO:
      franprisid = defpristemp.PRISID.
      RUN laddabefprislisttemp_UI IN defprisproch
      (INPUT franprisid, OUTPUT TABLE befprislisttemp).
   END.
   RUN openbdynspec_Ui IN brwproc[2].
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVSL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVSL DIALOG-1
ON CHOOSE OF BTN_AVSL IN FRAME DIALOG-1 /* Avbryt */
DO:
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KOP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KOP DIALOG-1
ON CHOOSE OF BTN_KOP IN FRAME DIALOG-1 /* Kopiera */
DO:
   musz = FALSE.
   MESSAGE "Vill du kopiera priser från denna prislista ?" Skip   
   "Allt tidigare upplagt kommer att tas bort !"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Kopiera"
   UPDATE answer AS LOGICAL.
   IF answer THEN DO:
      {muswait.i}  
      RUN kopiera_UI IN defprisproch (INPUT franprisid, INPUT defprisid).
      {musarrow.i}
      {BORTBRWPROC.I}    
      APPLY "GO" TO BTN_KOP. 
   END.
   ELSE musz = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KOP DIALOG-1
ON GO OF BTN_KOP IN FRAME DIALOG-1 /* Kopiera */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-1 


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
   
   kommandoquery = "defpristemp.OVER = FALSE AND defpristemp.PRISID NE '" + defprisid + "'".
   RUN setcolsortvar_UI IN brwproc[1] (INPUT kommandoquery).
   RUN openbdynspec_UI IN brwproc[1].
   GET FIRST BRW_BEST NO-LOCK.
   franprisid = defpristemp.PRISID.
   RUN laddabefprislisttemp_UI IN defprisproch
         (INPUT franprisid, OUTPUT TABLE befprislisttemp).
   RUN openbdynspec_UI IN brwproc[2].
   RUN enable_UI.
   {FRMSIZED.I}
   {musarrow.i}
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.  
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI DIALOG-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/  
   ASSIGN
   defpristemp.PRISNUM:READ-ONLY IN BROWSE BRW_BEST = TRUE
   befprislisttemp.NAMN:READ-ONLY IN BROWSE BRW_KUND = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_BEST:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_KUND:HANDLE IN FRAME {&FRAME-NAME}).     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-1  _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-1  _DEFAULT-ENABLE
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
  ENABLE BRW_BEST BTN_KOP BTN_AVSL 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

