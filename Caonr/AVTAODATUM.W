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
/*          This .W file was created with the Progress AppBulder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER aonrvar AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER delnrvar AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ordvar AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER avtidvar AS INTEGER NO-UNDO.

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}

{REGVAR.I}
&Scoped-define SHARED SHARED 
{DIRDEF.I}
{AONRDEF.I}
{AVTAONRTEMP.I}
{OMRTEMPW.I}

DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE brec AS RECID NO-UNDO.
DEFINE VARIABLE avtaodatapph AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BRW_HAND

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES avtaodattemp

/* Definitions for BROWSE BRW_HAND                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_HAND avtaodattemp.DATUM ~
avtaodattemp.ANMARK 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_HAND avtaodattemp.DATUM 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_HAND avtaodattemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_HAND avtaodattemp
&Scoped-define QUERY-STRING-BRW_HAND FOR EACH avtaodattemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_HAND OPEN QUERY BRW_HAND FOR EACH avtaodattemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_HAND avtaodattemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_HAND avtaodattemp


/* Definitions for DIALOG-BOX Dialog-Frame                              */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_UPDAT1 TOG_KLAR FILL-IN-STARTDAT ~
BTN_MINDAT1 BRW_HAND BTN_NY BTN_AND BTN_BORT BTN_AVB FILL-IN-TEXT 
&Scoped-Define DISPLAYED-OBJECTS TOG_KLAR FILL-IN-STARTDAT FILL-IN-TEXT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AND 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_AVB 
     LABEL "OK" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_MINDAT1 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NY 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_UPDAT1 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE VARIABLE FILL-IN-STARTDAT AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY .96 NO-UNDO.

DEFINE VARIABLE FILL-IN-TEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Datum:" 
      VIEW-AS TEXT 
     SIZE 6.5 BY .67 NO-UNDO.

DEFINE VARIABLE TOG_KLAR AS LOGICAL INITIAL no 
     LABEL "Klar" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.13 BY .88 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_HAND FOR 
      avtaodattemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_HAND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_HAND Dialog-Frame _STRUCTURED
  QUERY BRW_HAND NO-LOCK DISPLAY
      avtaodattemp.DATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
            WIDTH 10
      avtaodattemp.ANMARK COLUMN-LABEL "Händelse" FORMAT "X(40)":U
            WIDTH 50
  ENABLE
      avtaodattemp.DATUM
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 62.5 BY 8.75
         TITLE "Händelser".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BTN_UPDAT1 AT ROW 1.75 COL 29.5
     TOG_KLAR AT ROW 2 COL 3
     FILL-IN-STARTDAT AT ROW 2 COL 16 COLON-ALIGNED NO-LABEL
     BTN_MINDAT1 AT ROW 2.63 COL 29.5
     BRW_HAND AT ROW 3.79 COL 1.5
     BTN_NY AT ROW 13.17 COL 5.5
     BTN_AND AT ROW 13.17 COL 18.5
     BTN_BORT AT ROW 13.17 COL 31.5
     BTN_AVB AT ROW 13.17 COL 66
     FILL-IN-TEXT AT ROW 1.08 COL 16 COLON-ALIGNED NO-LABEL
     SPACE(56.99) SKIP(12.57)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "<insert dialog title>"
         DEFAULT-BUTTON BTN_AVB.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Temp-Tables and Buffers:
      TABLE: avtaodattemp T "?" NO-UNDO temp-db avtaodattemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_HAND BTN_MINDAT1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       FILL-IN-STARTDAT:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       FILL-IN-TEXT:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_HAND
/* Query rebuild information for BROWSE BRW_HAND
     _TblList          = "Temp-Tables.avtaodattemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.avtaodattemp.DATUM
"avtaodattemp.DATUM" "Datum" ? "date" ? ? ? ? ? ? yes ? no no "10" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.avtaodattemp.ANMARK
"avtaodattemp.ANMARK" "Händelse" "X(40)" "character" ? ? ? ? ? ? no ? no no "50" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_HAND */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON END-ERROR OF FRAME Dialog-Frame /* <insert dialog title> */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(avtaodatapph) THEN DELETE PROCEDURE avtaodatapph.   
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


&Scoped-define BROWSE-NAME BRW_HAND
&Scoped-define SELF-NAME BRW_HAND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_HAND Dialog-Frame
ON VALUE-CHANGED OF BRW_HAND IN FRAME Dialog-Frame /* Händelser */
DO:
   ASSIGN
   status-ok = BRW_HAND:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND Dialog-Frame
ON CHOOSE OF BTN_AND IN FRAME Dialog-Frame /* Ändra */
DO:
   IF INPUT TOG_KLAR = FALSE THEN DO:  
      status-ok = BRW_HAND:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.         
      IF AVAILABLE avtaodattemp THEN DO:
         {muswait.i}
         RUN NYAVTAODAT.W (INPUT avtaodattemp.AVTAODATREC, INPUT aonrvar, INPUT delnrvar, INPUT ordvar, INPUT avtidvar, OUTPUT brec).
         IF musz = FALSE THEN DO:
            FIND FIRST avtaodattemp WHERE avtaodattemp.AVTAODATREC = brec
            NO-LOCK NO-ERROR.
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(avtaodattemp)).        
            RUN openbdynspec_UI IN brwproc[1].
            RUN lastselectdyn_UI IN brwproc[1]. 
         END.
         musz = FALSE.             
         
         {musarrow.i}
      END.
      ELSE DO:
         MESSAGE "Det finns ingen händelse att ändra."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         RETURN NO-APPLY.
      END.
   END.
   ELSE DO:
      MESSAGE "Det går ej att ändra händelser då markeringen 'klar' är satt"
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB Dialog-Frame
ON CHOOSE OF BTN_AVB IN FRAME Dialog-Frame /* OK */
DO:
   FIND FIRST avtalaonrtemp WHERE avtalaonrtemp.AONR = aonrvar AND avtalaonrtemp.DELNR = delnrvar AND
   avtalaonrtemp.AVTID = avtidvar AND avtalaonrtemp.ORDN = ordvar EXCLUSIVE-LOCK NO-ERROR.
   TOG_KLAR = INPUT TOG_KLAR.
   IF TOG_KLAR = FALSE THEN DO:
       avtalaonrtemp.KLR = ?.
   END.
   ELSE DO:
       avtalaonrtemp.KLR = INPUT FILL-IN-STARTDAT.
   END.
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.
   
   RUN spardat IN avtaodatapph (INPUT aonrvar,INPUT delnrvar,INPUT avtidvar,
                                 INPUT ordvar,INPUT TOG_KLAR,INPUT FILL-IN-STARTDAT).
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT Dialog-Frame
ON CHOOSE OF BTN_BORT IN FRAME Dialog-Frame /* Ta bort */
DO:
   IF INPUT TOG_KLAR = FALSE THEN DO:         
      status-ok = BRW_HAND:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      IF AVAILABLE avtaodattemp THEN DO:
         MESSAGE "Vill du ta bort händelse: " + avtaodattemp.ANMARK + "?" VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO TITLE "Ta bort?" UPDATE svar AS LOGICAL.         
         IF svar THEN DO: 
            RUN bortavtaodat IN avtaodatapph (INPUT aonrvar,
                                              INPUT delnrvar,
                                              INPUT avtidvar).
            DELETE avtaodattemp.
            RUN selnextprevrow_UI IN brwproc[1].
            RUN openbdynspec_UI IN brwproc[1].
            RUN lastselectdyn_UI IN brwproc[1]. 
         END.
      END.
      ELSE DO:
         MESSAGE "Det finns ingen händelse att ta bort."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         RETURN NO-APPLY.
      END.
   END.
   ELSE DO:
      MESSAGE "Det går ej att ta bort händelser då markeringen 'klar' är satt."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_MINDAT1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MINDAT1 Dialog-Frame
ON CHOOSE OF BTN_MINDAT1 IN FRAME Dialog-Frame /* - */
DO: 
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT
   FILL-IN-STARTDAT = FILL-IN-STARTDAT - 1.
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY Dialog-Frame
ON CHOOSE OF BTN_NY IN FRAME Dialog-Frame /* Ny */
DO:
   {muswait.i}
   {AVBGOMD.I}
   IF INPUT TOG_KLAR = FALSE THEN DO:   
      RUN NYAVTAODAT.W (INPUT ?, INPUT aonrvar, INPUT delnrvar, INPUT ordvar, INPUT avtidvar, OUTPUT brec).
      IF musz = FALSE THEN DO:         
         FIND FIRST avtaodattemp WHERE avtaodattemp.AVTAODATREC = brec
         NO-LOCK NO-ERROR.
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(avtaodattemp)).        
         RUN openbdynspec_UI IN brwproc[1].
         RUN lastselectdyn_UI IN brwproc[1]. 
      END.
      ELSE musz = FALSE.
   END.
   ELSE DO:
      MESSAGE "Det går ej att lägga upp ny händelse då markeringen 'klar' är satt."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.
   {AVBFRAMD.I}
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPDAT1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPDAT1 Dialog-Frame
ON CHOOSE OF BTN_UPDAT1 IN FRAME Dialog-Frame /* + */
DO: 
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT + 1.
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-STARTDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT Dialog-Frame
ON LEAVE OF FILL-IN-STARTDAT IN FRAME Dialog-Frame
DO:
  FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT Dialog-Frame
ON MOUSE-MENU-CLICK OF FILL-IN-STARTDAT IN FRAME Dialog-Frame
DO:
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-STARTDAT.
   RUN AlmanBtn.w. 
   FILL-IN-STARTDAT = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_KLAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_KLAR Dialog-Frame
ON VALUE-CHANGED OF TOG_KLAR IN FRAME Dialog-Frame /* Klar */
DO:
   TOG_KLAR = INPUT TOG_KLAR.
   IF TOG_KLAR = TRUE THEN DO:
      ASSIGN
      FILL-IN-STARTDAT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_MINDAT1:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_UPDAT1:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.
   ELSE DO:
      ASSIGN
      FILL-IN-STARTDAT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_MINDAT1:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_UPDAT1:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
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
   {ALLSTARTDYN.I} 
   RUN avtaodathmt IN avtaodatapph (INPUT aonrvar,INPUT delnrvar,INPUT avtidvar,
                                    OUTPUT TABLE avtaodattemp).
   FIND FIRST avtalaonrtemp WHERE avtalaonrtemp.AONR = aonrvar AND avtalaonrtemp.DELNR = delnrvar AND
   avtalaonrtemp.AVTID = avtidvar AND avtalaonrtemp.ORDN = ordvar NO-LOCK NO-ERROR.
   ASSIGN FRAME {&FRAME-NAME}:TITLE = "Händelser kopplade till tillstånd/avtal: " +
   avtalaonrtemp.BEN.
   IF avtalaonrtemp.KLR NE ? THEN DO:
      ASSIGN
      TOG_KLAR = TRUE
      FILL-IN-STARTDAT = avtalaonrtemp.KLR.
   END.
   ELSE DO:
      ASSIGN
      TOG_KLAR = FALSE
      FILL-IN-STARTDAT = TODAY.
   END.
   RUN openbdynspec_UI IN brwproc[1].   
   RUN enable_UI.       
   {FRMSIZED.I}
   IF avtalaonrtemp.KLR = ? THEN DO:
      ASSIGN
      FILL-IN-TEXT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-STARTDAT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_MINDAT1:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_UPDAT1:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
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
   ASSIGN
   avtaodattemp.DATUM:READ-ONLY IN BROWSE BRW_HAND = TRUE.
   
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_HAND:HANDLE IN FRAME {&FRAME-NAME}).
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN AVTAODATAPP.P PERSISTENT SET avtaodatapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN AVTAODATAPP.P PERSISTENT SET avtaodatapph.
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
  DISPLAY TOG_KLAR FILL-IN-STARTDAT FILL-IN-TEXT 
      WITH FRAME Dialog-Frame.
  ENABLE BTN_UPDAT1 TOG_KLAR FILL-IN-STARTDAT BTN_MINDAT1 BRW_HAND BTN_NY 
         BTN_AND BTN_BORT BTN_AVB FILL-IN-TEXT 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

