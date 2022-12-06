&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame


/* Temp-Table and Buffer definitions                                    */



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
&Scoped-define NEW 
{GLOBVAR2DEL1.I}

{BBENAMNTEMP.I}
 
{KONVALTEMP.I}
{KONSTGRUPP.I}
{KONID.I} 

{KONVAL2TEMP.I}



DEFINE OUTPUT PARAMETER TABLE FOR kon_display.
DEFINE INPUT-OUTPUT PARAMETER blankrader AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE spec_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE VARIABLE idhj AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BRW_MTRL2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES kon_display kon_val2

/* Definitions for BROWSE BRW_MTRL2                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL2 kon_display.F1 kon_display.ID2 ~
kon_display.EXTRA1 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL2 
&Scoped-define QUERY-STRING-BRW_MTRL2 FOR EACH kon_display NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRL2 OPEN QUERY BRW_MTRL2 FOR EACH kon_display NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL2 kon_display
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL2 kon_display


/* Definitions for BROWSE BRW_VAL                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_VAL kon_val2.F1 kon_val2.ID2 ~
kon_val2.EXTRA1 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VAL 
&Scoped-define QUERY-STRING-BRW_VAL FOR EACH kon_val2 NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VAL OPEN QUERY BRW_VAL FOR EACH kon_val2 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VAL kon_val2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VAL kon_val2


/* Definitions for DIALOG-BOX Dialog-Frame                              */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CMB_VALORD TOG_TOM BRW_VAL BRW_MTRL2 ~
BTN_ALLOVER btn_over btn_back BTN_ALLBACK FBTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS CMB_VALORD TOG_TOM 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ALLBACK 
     IMAGE-UP FILE "BILDER\rewind-u":U
     LABEL "Alla konstruktioner":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla konstruktioner tas bort"
     FONT 11.

DEFINE BUTTON BTN_ALLOVER 
     IMAGE-UP FILE "BILDER\forwrd-u":U
     LABEL "Alla konstruktioneri listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla konstruktioner väljs"
     FONT 11.

DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 14 BY 1.

DEFINE BUTTON btn_back 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE BUTTON btn_over 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE BUTTON FBTN_OK AUTO-GO 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_VALORD AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 28.5 BY 1 NO-UNDO.

DEFINE VARIABLE TOG_TOM AS LOGICAL INITIAL no 
     LABEL "Skapa en tomrad mellan varje sträcka." 
     VIEW-AS TOGGLE-BOX
     SIZE 42 BY .79 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_MTRL2 FOR 
      kon_display SCROLLING.

DEFINE QUERY BRW_VAL FOR 
      kon_val2 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_MTRL2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL2 Dialog-Frame _STRUCTURED
  QUERY BRW_MTRL2 NO-LOCK DISPLAY
      kon_display.F1 COLUMN-LABEL "F1" FORMAT "X(14)":U
      kon_display.ID2 FORMAT "X(14)":U
      kon_display.EXTRA1 COLUMN-LABEL "Fri Id" FORMAT "X(14)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 47.25 BY 19.75
         TITLE "Valda byggnummer".

DEFINE BROWSE BRW_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VAL Dialog-Frame _STRUCTURED
  QUERY BRW_VAL NO-LOCK DISPLAY
      kon_val2.F1 COLUMN-LABEL "F1" FORMAT "X(10)":U WIDTH 14
      kon_val2.ID2 FORMAT "X(10)":U WIDTH 13.38
      kon_val2.EXTRA1 COLUMN-LABEL "Fri id" FORMAT "X(15)":U WIDTH 13
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 44.5 BY 19.75
         TITLE "Byggnummer".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     CMB_VALORD AT ROW 2.5 COL 10.5 NO-LABEL WIDGET-ID 12
     TOG_TOM AT ROW 4 COL 10.5 WIDGET-ID 16
     BRW_VAL AT ROW 5.54 COL 1.5 WIDGET-ID 300
     BRW_MTRL2 AT ROW 5.54 COL 50.88 WIDGET-ID 200
     BTN_ALLOVER AT ROW 9.25 COL 46.5 WIDGET-ID 2
     btn_over AT ROW 11.46 COL 46.5 WIDGET-ID 10
     btn_back AT ROW 14.63 COL 46.5 WIDGET-ID 8
     BTN_ALLBACK AT ROW 17.75 COL 46.5 WIDGET-ID 4
     FBTN_OK AT ROW 26.75 COL 65.75 WIDGET-ID 14
     BTN_AVB AT ROW 26.75 COL 80.75 WIDGET-ID 6
     SPACE(3.38) SKIP(0.24)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Konstruktioner" WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Temp-Tables and Buffers:
      TABLE: kon_display T "?" NO-UNDO temp-db kon_display
      TABLE: kon_val2 T "?" NO-UNDO temp-db kon_val2
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BRW_VAL TOG_TOM Dialog-Frame */
/* BROWSE-TAB BRW_MTRL2 BRW_VAL Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       BRW_MTRL2:HIDDEN  IN FRAME Dialog-Frame                = TRUE
       BRW_MTRL2:MAX-DATA-GUESS IN FRAME Dialog-Frame         = 10000
       BRW_MTRL2:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

ASSIGN 
       BRW_VAL:MAX-DATA-GUESS IN FRAME Dialog-Frame         = 300
       BRW_VAL:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_VALORD IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL2
/* Query rebuild information for BROWSE BRW_MTRL2
     _TblList          = "Temp-Tables.kon_display"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kon_display.F1
"kon_display.F1" "F1" "X(14)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.kon_display.ID2
"kon_display.ID2" ? "X(14)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.kon_display.EXTRA1
"kon_display.EXTRA1" "Fri Id" "X(14)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRL2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VAL
/* Query rebuild information for BROWSE BRW_VAL
     _TblList          = "Temp-Tables.kon_val2"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kon_val2.F1
"kon_val2.F1" "F1" ? "character" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.kon_val2.ID2
"kon_val2.ID2" ? "X(10)" "character" ? ? ? ? ? ? no ? no no "13.38" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.kon_val2.EXTRA1
"kon_val2.EXTRA1" "Fri id" "X(15)" "character" ? ? ? ? ? ? no ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VAL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Konstruktioner */
DO:
  {BORTBRWPROC.I}
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_MTRL2
&Scoped-define SELF-NAME BRW_MTRL2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL2 Dialog-Frame
ON VALUE-CHANGED OF BRW_MTRL2 IN FRAME Dialog-Frame /* Valda byggnummer */
DO:
   /*
   {muswait.i}
   */
   status-ok = BRW_MTRL2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
   RUN vcbrwval_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VAL
&Scoped-define SELF-NAME BRW_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VAL Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF BRW_VAL IN FRAME Dialog-Frame /* Byggnummer */
DO:
    APPLY "CHOOSE" TO BTN_OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VAL Dialog-Frame
ON VALUE-CHANGED OF BRW_VAL IN FRAME Dialog-Frame /* Byggnummer */
DO:
   /*
   {muswait.i}
   */
   status-ok = BRW_VAL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
   RUN vcbrwval2_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ALLBACK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ALLBACK Dialog-Frame
ON CHOOSE OF BTN_ALLBACK IN FRAME Dialog-Frame /* Alla konstruktioner */
DO:
   APPLY "CTRL-A" TO BRW_MTRL2.
   APPLY "CHOOSE" TO btn_BACK.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ALLOVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ALLOVER Dialog-Frame
ON CHOOSE OF BTN_ALLOVER IN FRAME Dialog-Frame /* Alla konstruktioneri listan */
DO:
   APPLY "CTRL-A" TO BRW_VAL.
   APPLY "CHOOSE" TO btn_over.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB Dialog-Frame
ON CHOOSE OF BTN_AVB IN FRAME Dialog-Frame /* Avbryt */
DO:    
   musz = TRUE.
   RETURN.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_back
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_back Dialog-Frame
ON CHOOSE OF btn_back IN FRAME Dialog-Frame
DO:   
   antal_valda = BRW_MTRL2:NUM-SELECTED-ROWS.
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = BRW_MTRL2:FETCH-SELECTED-ROW(antal_raknare).
      IF AVAILABLE kon_display THEN DELETE kon_display.
      antal_raknare = antal_raknare + 1.   
   END.
   RUN setcolindex_UI IN brwproc[1] (INPUT "INTEGER(ID2)").
   RUN setcolindex_UI IN brwproc[2] (INPUT "INTEGER(ID2)").
   RUN refreshbrw_UI IN brwproc[2].

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_over
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_over Dialog-Frame
ON CHOOSE OF btn_over IN FRAME Dialog-Frame
DO:      
   antal_valda = BRW_VAL:NUM-SELECTED-ROWS.         
   antal_raknare = 1.
   IF antal_valda > 0 THEN DO:
      DO WHILE antal_raknare LE antal_valda:                                   
         status-ok = BRW_VAL:FETCH-SELECTED-ROW(antal_raknare).
         IF AVAILABLE  kon_val2 THEN DO:
            FIND FIRST kon_display WHERE kon_display.BERAONR = kon_val2.BERAONR AND 
            kon_display.OMRADE = kon_val2.OMRADE AND kon_display.NUM = kon_val2.NUM
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE kon_display THEN DO:
               CREATE kon_display. 
               BUFFER-COPY kon_val2 TO kon_display.
            END.
         END.
         antal_raknare = antal_raknare + 1.
      END.
      RUN setcolindex_UI IN brwproc[1] (INPUT "INTEGER(ID2)").
      RUN setcolindex_UI IN brwproc[2] (INPUT "INTEGER(ID2)").
      RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].                  
      antal_raknare = 0.
      
      FIND FIRST kon_display WHERE ROWID(kon_display) = spec_rowid NO-LOCK NO-ERROR.
      IF AVAILABLE kon_display THEN DO:
         RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(kon_display)).
         RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].
      END.
      RUN vcbrwval_UI.
      status-ok = BRW_VAL:DESELECT-ROWS().
      APPLY "HOME" TO BRW_VAL IN FRAME {&FRAME-NAME}.
   END.                     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_VALORD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_VALORD Dialog-Frame
ON VALUE-CHANGED OF CMB_VALORD IN FRAME Dialog-Frame
DO:
   RUN cmbval_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_OK Dialog-Frame
ON CHOOSE OF FBTN_OK IN FRAME Dialog-Frame /* Ok */
DO:
   DEFINE VARIABLE varextra AS CHARACTER NO-UNDO.
   DEFINE VARIABLE varbernr AS CHARACTER NO-UNDO.
   DEFINE VARIABLE varomrber AS CHARACTER NO-UNDO.
   DEFINE VARIABLE varnum AS INTEGER NO-UNDO.
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_TOM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TOM Dialog-Frame
ON VALUE-CHANGED OF TOG_TOM IN FRAME Dialog-Frame /* Skapa en tomrad mellan varje sträcka. */
DO:
   blankrader = input TOG_TOM.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_MTRL2
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
   IF Guru.Konstanter:globforetag = "FORS" THEN DO:
      TOG_TOM:LABEL = "Skapa en tomrad mellan varje Byggnr.".
   END.
   
     
   IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
      ASSIGN
      BRW_MTRL2:TITLE = "Valda byggnummer" 
      BRW_VAL:TITLE = "Byggnummer".
   END.
   ELSE IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT"  THEN DO:
      ASSIGN
      BRW_MTRL2:TITLE = "Valda objekt" 
      BRW_VAL:TITLE = "Objekt".
   END.
   ELSE DO:
      ASSIGN
      BRW_MTRL2:TITLE = "Valda konstruktioner" 
      BRW_VAL:TITLE = "Konstruktioner".
      
   END.
  
   FOR EACH kon_val WHERE kon_val.KSKAP = FALSE AND kon_val.ID2 NE "":
      idhj = 0.
      idhj = INTEGER(kon_val.ID2) NO-ERROR.
      IF idhj > 0 THEN DO:
         CREATE kon_val2.
         BUFFER-COPY kon_val TO kon_val2.
      END.   
   END.
   FIND FIRST kon_val2 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE kon_val2 THEN DO:
      MESSAGE "Du har inga identifierade konstruktioner" VIEW-AS ALERT-BOX.
      IF Guru.GlobalaVariabler:retvalkoll = TRUE THEN DO:
         RUN SetDefaultCursors IN Guru.Konstanter:hpApi.  
         Guru.GlobalaVariabler:retvalkoll = FALSE.
      END.
      musz = TRUE.
      RETURN.
   END.
   CMB_VALORD:LIST-ITEMS = "".
   status-ok = CMB_VALORD:ADD-LAST("Alla").
   CMB_VALORD:SCREEN-VALUE = "Alla".
   FOR EACH konstgrptemp  USE-INDEX ORD NO-LOCK:
      FIND FIRST kon_id WHERE kon_id.GRUPP = konstgrptemp.KONSKOD 
      AND kon_id.ENDKOMB = FALSE NO-LOCK NO-ERROR.
      IF AVAILABLE kon_id THEN DO:
         ASSIGN
         status-ok = CMB_VALORD:ADD-LAST(konstgrptemp.BENAMNING). 
           
      END.
   END.   
      
   
   {BERTITLE.I}
   IF CMB_VALORD:SCREEN-VALUE = ? THEN DO:
      MESSAGE "Du har inga valda konstruktioner som kan användas i denna funktion!"
      VIEW-AS ALERT-BOX.
      LEAVE MAIN-BLOCK.
   END.
   RUN setcolindex_UI IN brwproc[1] (INPUT "INTEGER(ID2)").
   RUN setcolindex_UI IN brwproc[2] (INPUT "INTEGER(ID2)").
   RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].
   RUN openbdynspec_UI IN brwproc[{&LEFT-BROWSE}].
   
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Objektnr".
      kon_display.ID2:LABEL IN BROWSE BRW_MTRL2 = "Objektnr".
   END.
   ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
      kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Byggnr".
      kon_display.ID2:LABEL IN BROWSE BRW_MTRL2 = "Byggnr".
   END.
   ELSE DO:
      kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Id".
      kon_display.ID2:LABEL IN BROWSE BRW_MTRL2 = "Id".
   END.
   IF kon_display.F1:LABEL IN BROWSE BRW_MTRL2 = "F1" THEN DO:
      kon_display.F1:LABEL IN BROWSE BRW_MTRL2 = kon_val2.F1:LABEL IN BROWSE BRW_VAL.
   END.
   RUN enable_UI.   
   {FRMSIZED.I}  
   APPLY "VALUE-CHANGED" TO CMB_VALORD.
   APPLY "VALUE-CHANGED" TO BRW_VAL IN FRAME {&FRAME-NAME}.
   APPLY "VALUE-CHANGED" TO BRW_MTRL2 IN FRAME {&FRAME-NAME}.
   
   {musarrow.i}
   {DIA_M_SLUT.I}.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI Dialog-Frame 
PROCEDURE allstartbrw_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN DYNBRW.P PERSISTENT SET brwproc[{&LEFT-BROWSE}] 
      (INPUT BRW_VAL:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[{&RIGHT-BROWSE}] 
      (INPUT BRW_MTRL2:HANDLE IN FRAME {&FRAME-NAME}).
   RUN brwsetupstop_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT 1).
   RUN brwsetupstop_UI IN brwproc[{&LEFT-BROWSE}] (INPUT 1).
   RUN setcolindex_UI IN brwproc[1] (INPUT "INTEGER(ID2)").   
   RUN setcolindex_UI IN brwproc[2] (INPUT "INTEGER(ID2)").
   tthandle = TEMP-TABLE bbenamntemp:HANDLE.
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "BBENAMNING", INPUT "").
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "BBENAMNING", INPUT "").
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN IMPORTSCHSTRAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT valaonr,INPUT valomrade,OUTPUT TABLE konstgrptemp,OUTPUT TABLE kon_val,OUTPUT TABLE kon_id).
   END.
   ELSE DO:
      RUN IMPORTSCHSTRAPP.P 
         (INPUT valaonr,INPUT valomrade,OUTPUT TABLE konstgrptemp,OUTPUT TABLE kon_val,OUTPUT TABLE kon_id).
   END.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cmbval_UI Dialog-Frame 
PROCEDURE cmbval_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND FIRST konstgrptemp WHERE konstgrptemp.BENAMNING = CMB_VALORD:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.    
  
   IF AVAILABLE konstgrptemp THEN DO:
      FIND FIRST bbenamntemp WHERE bbenamntemp.KONSKOD = konstgrptemp.KONSKOD NO-LOCK NO-ERROR.
      RUN setcolsortvar_UI IN brwproc[1] (INPUT " WHERE kon_val2.GRUPP = '" + STRING(konstgrptemp.KONSKOD) + "'").
   END.
   ELSE RUN setcolsortvar_UI IN brwproc[1] (INPUT " ").   
   RUN setcolindex_UI IN brwproc[1] (INPUT "INTEGER(ID2)").
   RUN setcolindex_UI IN brwproc[2] (INPUT "INTEGER(ID2)").
   RUN openbdynspec_UI IN brwproc[1].
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
  DISPLAY CMB_VALORD TOG_TOM 
      WITH FRAME Dialog-Frame.
  ENABLE CMB_VALORD TOG_TOM BRW_VAL BRW_MTRL2 BTN_ALLOVER btn_over btn_back 
         BTN_ALLBACK FBTN_OK BTN_AVB 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vcbrwval2_UI Dialog-Frame 
PROCEDURE vcbrwval2_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF AVAILABLE kon_val2 THEN DO:
      FIND FIRST bbenamntemp WHERE bbenamntemp.KONSKOD = kon_val2.GRUPP
      USE-INDEX KOD NO-LOCK NO-ERROR.
      IF AVAILABLE bbenamntemp THEN DO:
         IF INDEX(kon_val2.F1:LABEL IN BROWSE BRW_VAL," ^") = 0 AND INDEX(kon_val2.F1:LABEL," v") = 0 THEN kon_val2.F1:LABEL IN BROWSE BRW_VAL = SUBSTRING(bbenamntemp.B1,1,20).      
         ELSE IF INDEX(kon_val2.F1:LABEL," ^") NE 0 THEN kon_val2.F1:LABEL IN BROWSE BRW_VAL = SUBSTRING(bbenamntemp.B1,1,20) + " ^".      
         ELSE IF INDEX(kon_val2.F1:LABEL," v") NE 0 THEN kon_val2.F1:LABEL IN BROWSE BRW_VAL = SUBSTRING(bbenamntemp.B1,1,20) + " v".      
      END.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
         IF INDEX(kon_val2.ID2:LABEL IN BROWSE BRW_VAL," ^") = 0 AND INDEX(kon_val2.ID2:LABEL," v") = 0 
            THEN kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Objektnr".
         ELSE IF INDEX(kon_val2.ID2:LABEL," ^") NE 0 THEN kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Objektnr" + " ^".      
         ELSE IF INDEX(kon_val2.ID2:LABEL," v") NE 0 THEN kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Objektnr" + " v".      
         
      END.
      ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
         IF INDEX(kon_val2.ID2:LABEL IN BROWSE BRW_VAL," ^") = 0 AND INDEX(kon_val2.ID2:LABEL," v") = 0 
            THEN kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Byggnr".
         ELSE IF INDEX(kon_val2.ID2:LABEL," ^") NE 0 THEN kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Byggnr" + " ^".      
         ELSE IF INDEX(kon_val2.ID2:LABEL," v") NE 0 THEN kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Byggnr" + " v".      
      END.
      ELSE DO:
         IF INDEX(kon_val2.ID2:LABEL IN BROWSE BRW_VAL," ^") = 0 AND INDEX(kon_val2.ID2:LABEL," v") = 0 
            THEN kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Id".
         ELSE IF INDEX(kon_val2.ID2:LABEL," ^") NE 0 THEN kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Id" + " ^".      
         ELSE IF INDEX(kon_val2.ID2:LABEL," v") NE 0 THEN kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Id" + " v".      
      END.
      
      
      {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}            
   END.
   ELSE DO:
            {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
      RETURN.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vcbrwval_UI Dialog-Frame 
PROCEDURE vcbrwval_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF AVAILABLE kon_display THEN DO:
      /*
      RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(kon_val)).
      RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].
      */
      FIND FIRST bbenamntemp WHERE bbenamntemp.KONSKOD = kon_display.GRUPP
      USE-INDEX KOD NO-LOCK NO-ERROR.      
      IF AVAILABLE bbenamntemp THEN DO:
         IF INDEX(kon_display.F1:LABEL IN BROWSE BRW_MTRL2," ^") = 0 AND INDEX(kon_display.F1:LABEL," v") = 0 THEN kon_display.F1:LABEL IN BROWSE BRW_MTRL2 = SUBSTRING(bbenamntemp.B1,1,20).      
         ELSE IF INDEX(kon_display.F1:LABEL," ^") NE 0 THEN kon_display.F1:LABEL IN BROWSE BRW_MTRL2 = SUBSTRING(bbenamntemp.B1,1,20) + " ^".      
         ELSE IF INDEX(kon_display.F1:LABEL," v") NE 0 THEN kon_display.F1:LABEL IN BROWSE BRW_MTRL2 = SUBSTRING(bbenamntemp.B1,1,20) + " v".               
      END.
      IF Guru.Konstanter:globforetag = "SUND" THEN DO:
         IF INDEX(kon_display.ID2:LABEL IN BROWSE BRW_MTRL2," ^") = 0 AND INDEX(kon_display.ID2:LABEL," v") = 0 
            THEN kon_display.ID2:LABEL IN BROWSE BRW_MTRL2 = "Objektnr".
         ELSE IF INDEX(kon_display.ID2:LABEL," ^") NE 0 THEN kon_display.ID2:LABEL IN BROWSE BRW_MTRL2 = "Objektnr" + " ^".      
         ELSE IF INDEX(kon_display.ID2:LABEL," v") NE 0 THEN kon_display.ID2:LABEL IN BROWSE BRW_MTRL2 = "Objektnr" + " v".      
         
      END.
      ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
         IF INDEX(kon_display.ID2:LABEL IN BROWSE BRW_MTRL2," ^") = 0 AND INDEX(kon_display.ID2:LABEL," v") = 0 
            THEN kon_display.ID2:LABEL IN BROWSE BRW_MTRL2 = "Byggnr".
         ELSE IF INDEX(kon_display.ID2:LABEL," ^") NE 0 THEN kon_display.ID2:LABEL IN BROWSE BRW_MTRL2 = "Byggnr" + " ^".      
         ELSE IF INDEX(kon_display.ID2:LABEL," v") NE 0 THEN kon_display.ID2:LABEL IN BROWSE BRW_MTRL2 = "Byggnr" + " v".      
      END.
      ELSE DO:
         IF INDEX(kon_display.ID2:LABEL IN BROWSE BRW_MTRL2," ^") = 0 AND INDEX(kon_display.ID2:LABEL," v") = 0 
            THEN kon_display.ID2:LABEL IN BROWSE BRW_MTRL2 = "Id".
         ELSE IF INDEX(kon_display.ID2:LABEL," ^") NE 0 THEN kon_display.ID2:LABEL IN BROWSE BRW_MTRL2 = "Id" + " ^".      
         ELSE IF INDEX(kon_display.ID2:LABEL," v") NE 0 THEN kon_display.ID2:LABEL IN BROWSE BRW_MTRL2 = "Id" + " v".      
      END.
      
      {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}         
   END.
   ELSE DO:
      
      {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
      RETURN.
   END.             
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

