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

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
DEFINE INPUT  PARAMETER tillxml AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER avnr AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER storar AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE franar AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE prec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE andra AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE forvar AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE natvar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE uppar AS INTEGER NO-UNDO. 
DEFINE VARIABLE slutar AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.

&SCOPED-DEFINE NEW
&SCOPED-DEFINE SHARED SHARED 
{STORTEMP.I}
{AVDELNINGTEMP.I}

DEFINE SHARED TEMP-TABLE val_temp
   FIELD AVDELNINGNR AS INTEGER
   FIELD DISTRIKTID AS INTEGER
   FIELD VIDISTRIKT AS CHARACTER
   FIELD ARTAL AS INTEGER
   FIELD NAMN AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BRW_P1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES stordistemp

/* Definitions for BROWSE BRW_P1                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_P1 stordistemp.VIDISTRIKT ~
stordistemp.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_P1 stordistemp.VIDISTRIKT 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_P1 stordistemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_P1 stordistemp
&Scoped-define QUERY-STRING-BRW_P1 FOR EACH stordistemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_P1 OPEN QUERY BRW_P1 FOR EACH stordistemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_P1 stordistemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_P1 stordistemp


/* Definitions for DIALOG-BOX Dialog-Frame                              */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CMB_FOR CMB_ARTAL BRW_P1 BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS CMB_FOR CMB_ARTAL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE CMB_ARTAL AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Årtal" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 12.25 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_FOR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Företag" 
     VIEW-AS COMBO-BOX INNER-LINES 8
     DROP-DOWN-LIST
     SIZE 28.75 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_P1 FOR 
      stordistemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_P1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_P1 Dialog-Frame _STRUCTURED
  QUERY BRW_P1 NO-LOCK DISPLAY
      stordistemp.VIDISTRIKT COLUMN-LABEL "Distrikt" FORMAT "X(8)":U
      stordistemp.NAMN COLUMN-LABEL "Namn" FORMAT "X(30)":U
  ENABLE
      stordistemp.VIDISTRIKT
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 42 BY 11.17
         TITLE "Distrikt".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     CMB_FOR AT ROW 1.54 COL 9.13 COLON-ALIGNED
     CMB_ARTAL AT ROW 2.88 COL 20.5
     BRW_P1 AT ROW 4.5 COL 1.5
     BTN_OK AT ROW 15.96 COL 29.5
     BTN_AVB AT ROW 15.96 COL 44.5
     SPACE(0.62) SKIP(0.11)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Export av avbrott/störningsuppgifter"
         DEFAULT-BUTTON BTN_OK CANCEL-BUTTON BTN_AVB.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Temp-Tables and Buffers:
      TABLE: stordistemp T "?" NO-UNDO TEMP-DB stordistemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_P1 CMB_ARTAL Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       BRW_P1:HIDDEN  IN FRAME Dialog-Frame                = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_ARTAL IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_P1
/* Query rebuild information for BROWSE BRW_P1
     _TblList          = "Temp-Tables.stordistemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.stordistemp.VIDISTRIKT
"stordistemp.VIDISTRIKT" "Distrikt" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.stordistemp.NAMN
"stordistemp.NAMN" "Namn" "X(30)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_P1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON END-ERROR OF FRAME Dialog-Frame /* Export av avbrott/störningsuppgifter */
DO:
   {muswait.i}
   musz = TRUE.   
   {BORTBRWPROC.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON ENDKEY OF FRAME Dialog-Frame /* Export av avbrott/störningsuppgifter */
DO:
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Export av avbrott/störningsuppgifter */
DO:
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB Dialog-Frame
ON CHOOSE OF BTN_AVB IN FRAME Dialog-Frame /* Avbryt */
DO:
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK Dialog-Frame
ON CHOOSE OF BTN_OK IN FRAME Dialog-Frame /* Ok */
DO:
   EMPTY TEMP-TABLE val_temp NO-ERROR.    
   antal_valda = BRW_P1:NUM-SELECTED-ROWS.
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = BRW_P1:FETCH-SELECTED-ROW(antal_raknare).
      CREATE val_temp.
      ASSIGN
      val_temp.AVDELNINGNR = stordistemp.AVDELNINGNR
      val_temp.DISTRIKTID = stordistemp.DISTRIKTID
      val_temp.VIDISTRIKT = stordistemp.VIDISTRIKT
      val_temp.ARTAL = stordistemp.ARTAL
      val_temp.NAMN = stordistemp.NAMN.
      antal_raknare = antal_raknare + 1.   
   END.
   {BORTBRWPROC.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_ARTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_ARTAL Dialog-Frame
ON VALUE-CHANGED OF CMB_ARTAL IN FRAME Dialog-Frame /* Årtal */
DO:                           
   ASSIGN
   CMB_ARTAL = INPUT CMB_ARTAL
   franar = CMB_ARTAL.
   storar = franar.     
   RUN changequery_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_FOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_FOR Dialog-Frame
ON VALUE-CHANGED OF CMB_FOR IN FRAME Dialog-Frame /* Företag */
DO:
   CMB_FOR = INPUT CMB_FOR.   
   FIND FIRST avdelningtemp WHERE avdelningtemp.AVDELNINGNAMN = CMB_FOR NO-LOCK NO-ERROR.
   forvar = avdelningtemp.AVDELNINGNR.
   avnr = forvar.
   RUN changequery_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_P1
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
   CMB_FOR:DELIMITER = "$".
   OPEN QUERY aq FOR EACH avdelningtemp  NO-LOCK BY avdelningtemp.AVDELNINGNAMN.
   GET FIRST aq NO-LOCK.
   DO WHILE AVAILABLE(avdelningtemp):     
      IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
         IF avdelningtemp.AVDELNINGNR >= 64 THEN DO:
            status-ok = CMB_FOR:ADD-LAST(avdelningtemp.AVDELNINGNAMN).
         END.
      END.
      ELSE DO:
         status-ok = CMB_FOR:ADD-LAST(avdelningtemp.AVDELNINGNAMN).
      END.     
      GET NEXT aq NO-LOCK.
   END.
   CLOSE QUERY aq.
   IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
      FIND FIRST avdelningtemp WHERE avdelningtemp.AVDELNINGNR >= 64 
      USE-INDEX AVDELNINGNR NO-LOCK NO-ERROR.
   END.
   ELSE DO:   
      FIND FIRST avdelningtemp USE-INDEX AVDELNINGNR NO-LOCK NO-ERROR.
   END.
   ASSIGN
   forvar = avdelningtemp.AVDELNINGNR
   avnr = forvar
   CMB_FOR:SCREEN-VALUE = avdelningtemp.AVDELNINGNAMN.   
   andra = FALSE.
   FIND FIRST stordistemp 
   USE-INDEX ARTAL NO-LOCK NO-ERROR.
   IF AVAILABLE stordistemp THEN DO:      
      ASSIGN  
      uppar = stordistemp.ARTAL.
      FIND LAST stordistemp 
      USE-INDEX ARTAL NO-LOCK NO-ERROR.
      ASSIGN
      slutar = stordistemp.ARTAL + 1.
   END.   
   ELSE DO:
      ASSIGN
      uppar = YEAR(TODAY)
      slutar = YEAR(TODAY) + 1.
   END.
   /*LADDAR ÅR I CMB_ARTAL*/ 
   ASSIGN
   status-ok = CMB_ARTAL:DELETE("0")  
   status-ok = CMB_ARTAL:ADD-LAST(STRING(uppar,"9999")).             
   DO WHILE uppar < slutar: 
       uppar = uppar + 1.             
       status-ok = CMB_ARTAL:ADD-LAST(STRING(uppar,"9999")).    
   END.
   ASSIGN
   CMB_ARTAL:SCREEN-VALUE = STRING(slutar - 1,"9999")
   franar = slutar - 1
   CMB_ARTAL = INPUT CMB_ARTAL.
   storar = franar.
   
   RUN enable_UI.       
   {FRMSIZED.I}
   RUN changequery_UI.
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
   stordistemp.VIDISTRIKT:READ-ONLY IN BROWSE BRW_P1 = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_P1:HANDLE IN FRAME {&FRAME-NAME}).    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changequery_UI Dialog-Frame 
PROCEDURE changequery_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/                                
   DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.
   kommandoquery = "stordistemp.AVDELNINGNR = " + STRING(forvar) + 
      " AND stordistemp.ARTAL = " + STRING(franar).
   RUN setcolsortvar_UI IN brwproc[1] (INPUT kommandoquery).
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
  DISPLAY CMB_FOR CMB_ARTAL 
      WITH FRAME Dialog-Frame.
  ENABLE CMB_FOR CMB_ARTAL BRW_P1 BTN_OK BTN_AVB 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

