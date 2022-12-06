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

DEFINE SHARED TEMP-TABLE otextpristemp NO-UNDO 
   FIELD PRISID      AS CHARACTER FORMAT "X(8)"
   FIELD START       AS DECIMAL FORMAT "99.99" INITIAL 7.00
   FIELD SLUT        AS DECIMAL FORMAT "99.99" INITIAL 16.00
   FIELD BEFATTNING  AS CHARACTER FORMAT "x(20)"
   FIELD EQDAG       AS INTEGER FORMAT "9"
   FIELD PRISA       AS DECIMAL FORMAT "->>>>9.99"
   FIELD DAGTYP      AS CHARACTER FORMAT "X(8)"
   FIELD OTEXT       AS CHARACTER FORMAT "X(25)"
   FIELD OTEXTID     AS CHARACTER FORMAT "X(8)"
   INDEX PRISID AS PRIMARY PRISID BEFATTNING EQDAG START SLUT.

&SCOPED-DEFINE NEW
&SCOPED-DEFINE SHARED SHARED
{BEFTEMP.I}
{OVERTEXTEMP.I}
DEFINE SHARED TEMP-TABLE extraopltemp NO-UNDO LIKE overprislisttemp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_DEF

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES defpristemp otextpristemp

/* Definitions for BROWSE BRW_DEF                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_DEF defpristemp.PRISNUM ~
defpristemp.PRISTEXT defpristemp.STARTDATUM 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_DEF defpristemp.PRISNUM 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_DEF defpristemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_DEF defpristemp
&Scoped-define QUERY-STRING-BRW_DEF FOR EACH defpristemp NO-LOCK ~
    BY defpristemp.PRISTEXT
&Scoped-define OPEN-QUERY-BRW_DEF OPEN QUERY BRW_DEF FOR EACH defpristemp NO-LOCK ~
    BY defpristemp.PRISTEXT.
&Scoped-define TABLES-IN-QUERY-BRW_DEF defpristemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_DEF defpristemp


/* Definitions for BROWSE BRW_KUND                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_KUND otextpristemp.OTEXT ~
otextpristemp.DAGTYP otextpristemp.START otextpristemp.SLUT ~
otextpristemp.PRISA 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KUND otextpristemp.OTEXT 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_KUND otextpristemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_KUND otextpristemp
&Scoped-define QUERY-STRING-BRW_KUND FOR EACH otextpristemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KUND OPEN QUERY BRW_KUND FOR EACH otextpristemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KUND otextpristemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KUND otextpristemp


/* Definitions for DIALOG-BOX DIALOG-1                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_DEF BRW_KUND BTN_KOP BTN_AVSL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVSL 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KOP 
     LABEL "Kopiera":L 
     SIZE 14 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_DEF FOR 
      defpristemp SCROLLING.

DEFINE QUERY BRW_KUND FOR 
      otextpristemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_DEF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_DEF DIALOG-1 _STRUCTURED
  QUERY BRW_DEF NO-LOCK DISPLAY
      defpristemp.PRISNUM COLUMN-LABEL "Pris!lista" FORMAT ">>>>>>9":U
      defpristemp.PRISTEXT COLUMN-LABEL "Text" FORMAT "X(19)":U
      defpristemp.STARTDATUM COLUMN-LABEL "Stardatum" FORMAT "99/99/99":U
  ENABLE
      defpristemp.PRISNUM
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 40.5 BY 11
         TITLE "Prislista".

DEFINE BROWSE BRW_KUND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KUND DIALOG-1 _STRUCTURED
  QUERY BRW_KUND NO-LOCK DISPLAY
      otextpristemp.OTEXT COLUMN-LABEL "Övertid" FORMAT "X(24)":U
      otextpristemp.DAGTYP COLUMN-LABEL "Dagtyp" FORMAT "X(8)":U
      otextpristemp.START COLUMN-LABEL "Start! tid" FORMAT "99.99":U
      otextpristemp.SLUT COLUMN-LABEL "Slut! tid" FORMAT "99.99":U
      otextpristemp.PRISA COLUMN-LABEL "Pris" FORMAT ">>>9.99":U
  ENABLE
      otextpristemp.OTEXT
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 54.38 BY 11
         TITLE "Visning av prislistor för övertid".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BRW_DEF AT ROW 3.33 COL 1.5
     BRW_KUND AT ROW 3.33 COL 43.63
     BTN_KOP AT ROW 5 COL 100
     BTN_AVSL AT ROW 13.33 COL 100
     "Välj prislista som du vill kopiera från !" VIEW-AS TEXT
          SIZE 43.13 BY 1 AT ROW 1.71 COL 1.5
          FONT 17
     SPACE(70.24) SKIP(11.99)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Kopiera övertidspriser".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: defpristemp T "?" NO-UNDO temp-db defpristemp
      TABLE: otextpristemp T "?" NO-UNDO temp-db otextpristemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_DEF TEXT-1 DIALOG-1 */
/* BROWSE-TAB BRW_KUND BRW_DEF DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_DEF
/* Query rebuild information for BROWSE BRW_DEF
     _TblList          = "Temp-Tables.defpristemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.defpristemp.PRISTEXT|yes"
     _FldNameList[1]   > Temp-Tables.defpristemp.PRISNUM
"defpristemp.PRISNUM" "Pris!lista" ">>>>>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.defpristemp.PRISTEXT
"defpristemp.PRISTEXT" "Text" "X(19)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.defpristemp.STARTDATUM
"defpristemp.STARTDATUM" "Stardatum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_DEF */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KUND
/* Query rebuild information for BROWSE BRW_KUND
     _TblList          = "Temp-Tables.otextpristemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.otextpristemp.OTEXT
"otextpristemp.OTEXT" "Övertid" "X(24)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.otextpristemp.DAGTYP
"otextpristemp.DAGTYP" "Dagtyp" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.otextpristemp.START
"otextpristemp.START" "Start! tid" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.otextpristemp.SLUT
"otextpristemp.SLUT" "Slut! tid" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.otextpristemp.PRISA
"otextpristemp.PRISA" "Pris" ">>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KUND */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Kopiera övertidspriser */
DO:
   {muswait.i}
   musz = TRUE.
   {BORTBRWPROC.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Kopiera övertidspriser */
DO:    
   APPLY "END-ERROR" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_DEF
&Scoped-define SELF-NAME BRW_DEF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_DEF DIALOG-1
ON VALUE-CHANGED OF BRW_DEF IN FRAME DIALOG-1 /* Prislista */
DO:         
   {muswait.i}  
   BRW_DEF:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   RUN laddaotextpris_UI IN defprisproch (INPUT defpristemp.PRISID, OUTPUT TABLE otextpristemp).
   RUN openbdynspec_UI IN brwproc[2].
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVSL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVSL DIALOG-1
ON CHOOSE OF BTN_AVSL IN FRAME DIALOG-1 /* Avbryt */
DO:
   APPLY "END-ERROR" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KOP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KOP DIALOG-1
ON CHOOSE OF BTN_KOP IN FRAME DIALOG-1 /* Kopiera */
DO:
   musz = FALSE.
   MESSAGE "Vill du kopiera övertidspriser från denna prislista ?" Skip   
   "Allt tidigare upplagt kommer att tas bort !"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Kopiera"
   UPDATE answer AS LOGICAL.
   IF answer THEN DO:
      {muswait.i}  
      RUN kopoverpris_UI IN defprisproch
         (INPUT defpristemp.PRISID, INPUT defprisid).      
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
   RUN setcolsortvar_UI IN brwproc[1] 
      (INPUT "defpristemp.OVER = TRUE AND defpristemp.PRISID NE '" + defprisid + "'").
   RUN openbdynspec_UI IN brwproc[1].
   BRW_DEF:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   RUN laddaotextpris_UI IN defprisproch (INPUT defpristemp.PRISID, OUTPUT TABLE otextpristemp).
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
   defpristemp.PRISNUM:READ-ONLY IN BROWSE BRW_DEF = TRUE
   otextpristemp.OTEXT:READ-ONLY IN BROWSE BRW_KUND = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_DEF:HANDLE IN FRAME {&FRAME-NAME}).  
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
  ENABLE BRW_DEF BRW_KUND BTN_KOP BTN_AVSL 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

