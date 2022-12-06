&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME WINDOW-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-3 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 03/30/97 -  8:41 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
DEFINE INPUT  PARAMETER stormemyh AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE striproch AS HANDLE NO-UNDO. /*Handle till STRINUTAPP.P*/
DEFINE NEW SHARED VARIABLE filnamn AS CHARACTER NO-UNDO.
 
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE OKvald AS LOGICAL INITIAL TRUE.
DEFINE VARIABLE locwtidvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE ledfel AS LOGICAL NO-UNDO.
DEFINE VARIABLE dirnamn AS CHARACTER NO-UNDO.
{SESSIONTEMPDIR.I}
&SCOPED-DEFINE NEW
&SCOPED-DEFINE SHARED SHARED 
{AVDELNINGTEMP.I}

DEFINE TEMP-TABLE temp_text
   FIELD B1 AS INTEGER /*företag*/
   FIELD B2 AS CHARACTER /*distrikt*/
   FIELD PROGNAMN AS CHARACTER FORMAT "X(100)".

DEFINE TEMP-TABLE dumpa_temp
   FIELD AVDELNINGNR AS INTEGER
   FIELD VIDISTRIKT AS CHARACTER
   FIELD ARTAL AS INTEGER
   FIELD SPANID AS CHARACTER FORMAT "X(2)"
   FIELD ANTALLUFT AS INTEGER
   FIELD ANTALBLAND AS INTEGER
   FIELD ANTALKABEL AS INTEGER
   FIELD ANTALKUNDER AS INTEGER
   INDEX ORD AVDELNINGNR VIDISTRIKT SPANID ASCENDING.

DEFINE TEMP-TABLE dumpa_temp2
   FIELD FORETAG            AS INTEGER 
   FIELD DISTRIKT           AS CHARACTER
   FIELD BDAT               AS DATE FORMAT "9999/99/99"
   FIELD BTID               AS INTEGER FORMAT "9999"
   FIELD C1                 AS INTEGER   
   FIELD D1                 AS CHARACTER
   FIELD D2                 AS CHARACTER 
   FIELD E1                 AS INTEGER
   FIELD E2                 AS CHARACTER
   FIELD F1                 AS INTEGER
   FIELD F2                     AS CHARACTER
   FIELD G1                 AS INTEGER
   FIELD G2                 AS INTEGER
   FIELD G3                 AS INTEGER
   FIELD H                  AS INTEGER 
   FIELD J1DAT              AS DATE FORMAT "9999/99/99"
   FIELD J1KLOCK            AS INTEGER FORMAT "9999"
   FIELD J2DAT              AS DATE FORMAT "9999/99/99"
   FIELD J2KLOCK            AS INTEGER FORMAT "9999"
   FIELD J3                 AS DECIMAL
   FIELD K1                 AS INTEGER
   FIELD K2                 AS INTEGER
   FIELD L1                 AS INTEGER
   FIELD L2                 AS INTEGER
   FIELD M1                 AS INTEGER
   FIELD M2                 AS INTEGER
   FIELD N1                 AS INTEGER
   FIELD N2                 AS CHARACTER
   FIELD P1                 AS INTEGER
   FIELD P2                 AS INTEGER
   FIELD Q                  AS INTEGER
   FIELD R                  AS INTEGER
   FIELD S                  AS INTEGER
   FIELD T1                 AS INTEGER
   FIELD T2                 AS INTEGER
   FIELD T3                 AS INTEGER
   FIELD FEL                AS CHARACTER
   FIELD FEL2               AS LOGICAL INITIAL FALSE   
   INDEX ORD FORETAG DISTRIKT BDAT BTID ASCENDING.

DEFINE TEMP-TABLE dumpa_temp3
   FIELD AVDELNINGNR AS INTEGER
   FIELD VIDISTRIKT AS CHARACTER
   FIELD ARTAL AS INTEGER
   FIELD SPANID AS CHARACTER FORMAT "X(2)"
   FIELD LANGDLUFT AS DECIMAL
   FIELD LANGDBLAND AS DECIMAL
   FIELD LANGDKABEL AS DECIMAL
   FIELD LANGD AS DECIMAL
   FIELD TEXT1 AS CHARACTER
   FIELD TEXT2 AS CHARACTER
   INDEX ORD AVDELNINGNR VIDISTRIKT SPANID ASCENDING.

DEFINE NEW SHARED TEMP-TABLE val_temp
   FIELD AVDELNINGNR AS INTEGER
   FIELD DISTRIKTID AS INTEGER
   FIELD VIDISTRIKT AS CHARACTER
   FIELD ARTAL AS INTEGER
   FIELD NAMN AS CHARACTER.
{DARINTIDIN.I}

/* /*ANVÄNDS VI XXDARIN5*/                                     */
/* DEFINE TEMP-TABLE tidin                                     */
/*    FIELD FORETAG            AS INTEGER                      */
/*    FIELD DISTRIKT           AS CHARACTER                    */
/*    FIELD BDAT               AS DATE FORMAT "9999/99/99"     */
/*    FIELD BTID               AS INTEGER FORMAT "9999"        */
/*    FIELD C1                 AS INTEGER                      */
/*    FIELD D1                 AS CHARACTER                    */
/*    FIELD D2                 AS CHARACTER                    */
/*    FIELD E1                 AS INTEGER                      */
/*    FIELD E2                 AS CHARACTER                    */
/*    FIELD F1                 AS INTEGER                      */
/*    FIELD F2                 AS CHARACTER                    */
/*    FIELD G1                 AS INTEGER                      */
/*    FIELD G2                 AS INTEGER                      */
/*    FIELD G3                 AS INTEGER                      */
/*    FIELD H                  AS INTEGER                      */
/*    FIELD J1                 AS INTEGER                      */
/*    FIELD J1KLOCK            AS INTEGER FORMAT "9999"        */
/*    FIELD J2                 AS INTEGER                      */
/*    FIELD J2KLOCK            AS INTEGER FORMAT "9999"        */
/*    FIELD J3                 AS CHARACTER                    */
/*    FIELD K1                 AS INTEGER                      */
/*    FIELD K2                 AS INTEGER                      */
/*    FIELD L1                 AS INTEGER                      */
/*    FIELD L2                 AS INTEGER                      */
/*    FIELD M1                 AS INTEGER                      */
/*    FIELD M2                 AS INTEGER                      */
/*    FIELD N1                 AS INTEGER                      */
/*    FIELD N2                 AS CHARACTER                    */
/*    FIELD P1                 AS INTEGER                      */
/*    FIELD P2                 AS INTEGER                      */
/*    FIELD Q                  AS INTEGER                      */
/*    FIELD R                  AS INTEGER                      */
/*    FIELD S                  AS INTEGER                      */
/*    FIELD T1                 AS INTEGER                      */
/*    FIELD T2                 AS INTEGER                      */
/*    FIELD T3                 AS INTEGER                      */
/*    FIELD FEL                AS CHARACTER                    */
/*    FIELD FEL2               AS LOGICAL INITIAL FALSE        */
/*    FIELD OKVAR              AS LOGICAL INITIAL FALSE        */
/*    INDEX ORD FORETAG DISTRIKT BDAT BTID FEL FEL2 ASCENDING. */

DEFINE TEMP-TABLE finns_temp
   FIELD AVDELNINGNR AS INTEGER /*företag*/
   FIELD DISTRIKT AS CHARACTER /*distrikt*/
   FIELD ARTAL AS INTEGER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-C

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-3 SEL_UPP BTN_KOR BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS SEL_UPP 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-3 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY DEFAULT 
     LABEL "Avsluta" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_KOR 
     LABEL "Kör funktion" 
     SIZE 14 BY 1.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 71 BY 20.25
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 18 BY 20.25
     BGCOLOR 8 .

DEFINE VARIABLE SEL_UPP AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 62.38 BY 17.46
     BGCOLOR 7 FGCOLOR 15 FONT 17 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-C
     SEL_UPP AT ROW 1.5 COL 5.38 NO-LABEL
     BTN_KOR AT ROW 3 COL 73.63
     BTN_AVB AT ROW 17.33 COL 73.63
     RECT-2 AT ROW 1 COL 1
     RECT-3 AT ROW 1 COL 72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.13 BY 20.38.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-3 ASSIGN
         HIDDEN             = YES
         TITLE              = "Export/Import"
         HEIGHT             = 20.54
         WIDTH              = 89.63
         MAX-HEIGHT         = 24.88
         MAX-WIDTH          = 91.75
         VIRTUAL-HEIGHT     = 24.88
         VIRTUAL-WIDTH      = 91.75
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = 8
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WINDOW-3
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-C
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-3)
THEN WINDOW-3:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-3
ON CHOOSE OF BTN_AVB IN FRAME FRAME-C /* Avsluta */
DO:
   /*
   IF VALID-HANDLE(StorningHelpStartH) THEN DELETE PROCEDURE StorningHelpStartH NO-ERROR.
   StorningHelpStartH= ?.
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KOR WINDOW-3
ON CHOOSE OF BTN_KOR IN FRAME FRAME-C /* Kör funktion */
DO:
   SEL_UPP = INPUT SEL_UPP.   
   RUN vart_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_UPP WINDOW-3
ON MOUSE-SELECT-DBLCLICK OF SEL_UPP IN FRAME FRAME-C
DO:
   SEL_UPP = INPUT SEL_UPP.     
   RUN vart_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_UPP WINDOW-3
ON VALUE-CHANGED OF SEL_UPP IN FRAME FRAME-C
DO:
   SEL_UPP = INPUT SEL_UPP.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-3 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   IF VALID-HANDLE(striproch) THEN DELETE PROCEDURE striproch.
   {BORTBRWPROC.I}
   RUN disable_UI.
END.

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:  
   {WIN_M_START.I}
   
   ASSIGN
   RECT-2:BGCOLOR = ?
   RECT-3:BGCOLOR = ?  
   FRAME {&FRAME-NAME}:BGCOLOR = ? 
   SEL_UPP:FONT = Guru.Konstanter:varforetypval[20] 
   SEL_UPP:BGCOLOR = 15
   SEL_UPP:FGCOLOR = ?.
{muswait.i}
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN STRINUTAPP.P PERSISTENT SET striproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
   END.
   ELSE DO:
      RUN STRINUTAPP.P PERSISTENT SET striproch.
   END.
   ASSIGN
   status-ok = SEL_UPP:ADD-LAST("Exportera distrikt")
   status-ok = SEL_UPP:ADD-LAST("Exportera kunddata")
   status-ok = SEL_UPP:ADD-LAST("Exportera ledningsdata")
   status-ok = SEL_UPP:ADD-LAST("Exportera störningar")
   status-ok = SEL_UPP:ADD-LAST("Importera distrikt")
   status-ok = SEL_UPP:ADD-LAST("Importera kunddata")
   status-ok = SEL_UPP:ADD-LAST("Importera ledningsdata")
   status-ok = SEL_UPP:ADD-LAST("Importera störningar").
   status-ok = SEL_UPP:ADD-LAST("Importera störningar XML/JSON/xlsx format"). 
   status-ok = SEL_UPP:ADD-LAST("Exportera störningar XML format").
   status-ok = SEL_UPP:ADD-LAST("Exportera störningar till Darwin+").
   RUN laddadir_UI IN striproch (INPUT Guru.Konstanter:globanv, OUTPUT dirnamn).   
   RUN enable_UI. 
    
   {FRMSIZE.I}    
   {musarrow.i}
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN   
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-3  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-3)
  THEN DELETE WIDGET WINDOW-3.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-3  _DEFAULT-ENABLE
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
  DISPLAY SEL_UPP 
      WITH FRAME FRAME-C IN WINDOW WINDOW-3.
  ENABLE RECT-2 RECT-3 SEL_UPP BTN_KOR BTN_AVB 
      WITH FRAME FRAME-C IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-C}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE minne_UI WINDOW-3 
PROCEDURE minne_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   RUN DIRNAMN.P (INPUT filnamn, OUTPUT dirnamn).
   RUN sparadir_UI IN striproch (INPUT Guru.Konstanter:globanv, INPUT dirnamn).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Storning_UI WINDOW-3 
PROCEDURE Storning_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {AVBGOM.I}
   IF Guru.GlobalaVariabler:retvalkoll = TRUE THEN DO:
      RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
      Guru.GlobalaVariabler:retvalkoll = FALSE.
   END. 
   RUN Storning_UI IN stormemyh.
   {AVBFRAM.I}
   DEBUGGER:SET-BREAK().
 /*
   RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
   Guru.GlobalaVariabler:retvalkoll = FALSE.
   */
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vart_UI WINDOW-3 
PROCEDURE vart_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE StorningImportDSh AS HANDLE NO-UNDO.  
DEFINE VARIABLE avnr AS INTEGER NO-UNDO.
DEFINE VARIABLE storar AS INTEGER NO-UNDO.
DEFINE VARIABLE cTargetType     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFile           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFormatted      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cEncoding       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSchemaLocation AS CHARACTER NO-UNDO.
DEFINE VARIABLE lWriteSchema    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lMinSchema      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lWriteBeforeImage   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE dummy AS LOGICAL NO-UNDO.
  
   EMPTY TEMP-TABLE temp_text NO-ERROR. 
   EMPTY TEMP-TABLE val_temp NO-ERROR. 
   EMPTY TEMP-TABLE dumpa_temp NO-ERROR. 
   EMPTY TEMP-TABLE dumpa_temp2 NO-ERROR. 
   musz = FALSE.
   IF SEL_UPP = "Exportera distrikt" THEN DO:       
      RUN VALDIST.W (INPUT  "", OUTPUT avnr, OUTPUT storar).      
      IF musz = TRUE THEN RETURN.
      filnamn = "Distrikt".
      SYSTEM-DIALOG GET-FILE filnamn
      TITLE                  "Spara distrikt"
      FILTERS                "Text filer (*.txt)"  "*.txt"
      INITIAL-DIR            dirnamn
      ASK-OVERWRITE
      DEFAULT-EXTENSION      ".txt"      
      SAVE-AS
      USE-FILENAME
      UPDATE                 OKvald.
      IF OKvald = TRUE THEN DO:                    
         RUN minne_UI.
         OUTPUT TO VALUE(filnamn)
         CONVERT TARGET "iso8859-1" SOURCE "iso8859-1"
         NO-ECHO.         
         FOR EACH val_temp:
            PUT UNFORMATTED
            val_temp.AVDELNINGNR ";"
            val_temp.VIDISTRIKT ";"
            val_temp.ARTAL ";"
            val_temp.NAMN
            SKIP.
         END.
         OUTPUT CLOSE.
         MESSAGE "Export av distrikt till filen:" +
         filnamn + " är nu klart." VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.

   END.
   ELSE IF SEL_UPP = "Importera distrikt" THEN DO:                                                      
      SYSTEM-DIALOG GET-FILE filnamn
      TITLE          "Välj den fil som motsvarar distrikt"
      FILTERS        "All Files (*.*)"  "*.*"
      INITIAL-DIR    dirnamn
      MUST-EXIST         
      USE-FILENAME
      UPDATE OKvald.
      IF OKvald = TRUE THEN DO: 
         RUN minne_UI.   
         
         EMPTY TEMP-TABLE tidin3 NO-ERROR.         
         INPUT FROM VALUE(filnamn) NO-ECHO.
         REPEAT:
            DO TRANSACTION:
               CREATE tidin3.
               ASSIGN.
               IMPORT DELIMITER ";" tidin3   NO-ERROR.
            END.
         END.
             FOR EACH tidin3 WHERE tidin3.B1 = 0 OR tidin3.B3 = ? OR tidin3.B4 = "":
            DELETE tidin3.
         END.
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN IMPDIST.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT TABLE tidin3, OUTPUT TABLE temp_text).
         END.
         ELSE DO:
            RUN IMPDIST.P 
            (INPUT TABLE tidin3, OUTPUT TABLE temp_text).
         END.
         ASSIGN
         locwtidvar = SESSION:TEMP-DIRECTORY.
         IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN locwtidvar = webclienttempdir.
         locwtidvar = locwtidvar + "distrikt.txt".
         FIND FIRST temp_text NO-LOCK NO-ERROR.
         IF AVAILABLE temp_text THEN DO:
            OUTPUT TO VALUE(locwtidvar) 
            CONVERT TARGET "iso8859-1" SOURCE "iso8859-1" 
            NO-ECHO.
            PUT UNFORMATTED "FÖRETAG;DISTRIKT;FEL" SKIP.
            FOR EACH temp_text:
               PUT UNFORMATTED temp_text.B1 ";" temp_text.B2 ";" temp_text.PROGNAMN SKIP.
            END.
            OUTPUT CLOSE.
            MESSAGE "Inläsningen misslyckades Ni kan se varför i filen:" +
            locwtidvar VIEW-AS ALERT-BOX TITLE "Meddelande".
         END.
         ELSE DO:
            MESSAGE "Import av distrikt klart." VIEW-AS ALERT-BOX TITLE "Meddelande".
         END.
      END.                     
   END.
   IF SEL_UPP = "Exportera kunddata" THEN DO:       
      RUN VALDIST.W (INPUT  "", OUTPUT avnr, OUTPUT storar).      
      IF musz = TRUE THEN RETURN.
      filnamn = "Kunddata".
      SYSTEM-DIALOG GET-FILE filnamn
      TITLE                  "Spara kunddata"
      FILTERS                "Text filer (*.txt)"  "*.txt"
      INITIAL-DIR            dirnamn
      ASK-OVERWRITE
      DEFAULT-EXTENSION      ".txt"      
      SAVE-AS
      USE-FILENAME
      UPDATE                 OKvald.
      IF OKvald = TRUE THEN DO: 
         RUN minne_UI.
         IF Guru.Konstanter:appcon THEN DO:
            RUN EXPKUND.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
            (INPUT TABLE val_temp, OUTPUT TABLE dumpa_temp).
         END.
         ELSE DO:
            RUN EXPKUND.P
            (INPUT TABLE val_temp, OUTPUT TABLE dumpa_temp).
         END.                 
         OUTPUT TO VALUE(filnamn)
         CONVERT TARGET "iso8859-1" SOURCE "iso8859-1"
         NO-ECHO.         
         FOR EACH dumpa_temp:
            PUT UNFORMATTED
            dumpa_temp.AVDELNINGNR ";"
            dumpa_temp.VIDISTRIKT ";"
            dumpa_temp.ARTAL ";"
            dumpa_temp.SPANID ";"
            dumpa_temp.ANTALLUFT ";"
            dumpa_temp.ANTALBLAND ";"
            dumpa_temp.ANTALKABEL ";"
            dumpa_temp.ANTALKUNDER ";"
            SKIP.
         END.
         OUTPUT CLOSE.
         MESSAGE "Export av kunddata till filen:" +
         filnamn + " är nu klart." VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.

   END.
   ELSE IF SEL_UPP = "Importera kunddata" THEN DO:                                                      
      SYSTEM-DIALOG GET-FILE filnamn
      TITLE          "Välj den fil som motsvarar kunddata"
      FILTERS        "All Files (*.*)"  "*.*"
      INITIAL-DIR    dirnamn
      MUST-EXIST         
      USE-FILENAME
      UPDATE OKvald.
      IF OKvald = TRUE THEN DO:
         RUN minne_UI.
         EMPTY TEMP-TABLE tidin3 NO-ERROR. 
         INPUT FROM VALUE(filnamn) NO-ECHO.
         REPEAT:
            DO TRANSACTION: 
               CREATE tidin3.
               ASSIGN.
               IMPORT DELIMITER ";" tidin3   NO-ERROR.
            END.               
         END.
         FOR EACH tidin3 WHERE tidin3.B1 = 0:
            DELETE tidin3.
         END.
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN DARIN3.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT TABLE tidin3, OUTPUT TABLE temp_text).
         END.
         ELSE DO:
            RUN DARIN3.P 
            (INPUT TABLE tidin3, OUTPUT TABLE temp_text).
         END.
         /*
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN DARIN3.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT filnamn, OUTPUT TABLE temp_text).
         END.
         ELSE DO:
            RUN DARIN3.P 
            (INPUT filnamn, OUTPUT TABLE temp_text).
         END.
         */
         ASSIGN
         locwtidvar = SESSION:TEMP-DIRECTORY.
         IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN locwtidvar = webclienttempdir.
         locwtidvar = locwtidvar + "kunddata.txt".
         FIND FIRST temp_text NO-LOCK NO-ERROR.
         IF AVAILABLE temp_text THEN DO:
            OUTPUT TO VALUE(locwtidvar) 
            CONVERT TARGET "iso8859-1" SOURCE "iso8859-1" 
            NO-ECHO.
            PUT UNFORMATTED "FÖRETAG;DISTRIKT;FEL" SKIP.
            FOR EACH temp_text:
               PUT UNFORMATTED temp_text.B1 ";" temp_text.B2 ";" temp_text.PROGNAMN SKIP.
            END.
            OUTPUT CLOSE.
            MESSAGE "Inläsningen misslyckades Ni kan se varför i filen:" +
            locwtidvar VIEW-AS ALERT-BOX TITLE "Meddelande".
         END.
         ELSE DO:
            MESSAGE "Import av kunddata klart." VIEW-AS ALERT-BOX TITLE "Meddelande".
         END.
      END.                     
   END. 
   IF SEL_UPP = "Exportera ledningsdata" THEN DO:       
      RUN VALDIST.W (INPUT  "", OUTPUT avnr, OUTPUT storar).      
      IF musz = TRUE THEN RETURN.
      filnamn = "Ledningsdata".
      SYSTEM-DIALOG GET-FILE filnamn
      TITLE                  "Spara ledningsdata"
      FILTERS                "Text filer (*.txt)"  "*.txt"
      INITIAL-DIR            dirnamn
      ASK-OVERWRITE
      DEFAULT-EXTENSION      ".txt"      
      SAVE-AS
      USE-FILENAME
      UPDATE                 OKvald.
      IF OKvald = TRUE THEN DO: 
         RUN minne_UI.
         IF Guru.Konstanter:appcon THEN DO:
            RUN EXPLEDN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
            (INPUT TABLE val_temp, OUTPUT TABLE dumpa_temp3).
         END.
         ELSE DO:
            RUN EXPLEDN.P
            (INPUT TABLE val_temp, OUTPUT TABLE dumpa_temp3).
         END.                 
         OUTPUT TO VALUE(filnamn)
         CONVERT TARGET "iso8859-1" SOURCE "iso8859-1"
         NO-ECHO.         
         FOR EACH dumpa_temp3:
            PUT UNFORMATTED
            dumpa_temp3.AVDELNINGNR ";"
            dumpa_temp3.VIDISTRIKT ";"
            dumpa_temp3.ARTAL ";"
            dumpa_temp3.SPANID ";"
            dumpa_temp3.TEXT1 ";"
            dumpa_temp3.TEXT2 ";"
            dumpa_temp3.LANGDLUFT ";"
            dumpa_temp3.LANGDBLAND ";"
            dumpa_temp3.LANGDKABEL ";"
            dumpa_temp3.LANGD ";"
            SKIP.
         END.
         OUTPUT CLOSE.
         MESSAGE "Export av ledningsdata till filen:" +
         filnamn + " är nu klart." VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.

   END.
   ELSE IF SEL_UPP = "Importera ledningsdata" THEN DO:                                                      
      ledfel = FALSE.
      SYSTEM-DIALOG GET-FILE filnamn
      TITLE          "Välj den fil som motsvarar ledningsdata"
      FILTERS        "All Files (*.*)"  "*.*"
      INITIAL-DIR    dirnamn
      MUST-EXIST         
      USE-FILENAME
      UPDATE OKvald.
      IF OKvald = TRUE THEN DO: 
         RUN minne_UI.
         EMPTY TEMP-TABLE tidin4 NO-ERROR. 
         INPUT FROM VALUE(filnamn) NO-ECHO.
         REPEAT:
            DO TRANSACTION: 
               CREATE tidin4.
               ASSIGN.
               IMPORT DELIMITER ";" tidin4   NO-ERROR.
            END.               
         END.
         FOR EACH tidin4 WHERE tidin4.B1 = 0:
            DELETE tidin4.
         END.
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN DARIN4.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
            (INPUT TABLE tidin4, OUTPUT TABLE temp_text, INPUT-OUTPUT ledfel).
         END.
         ELSE DO:
            RUN DARIN4.P 
            (INPUT TABLE tidin4, OUTPUT TABLE temp_text, INPUT-OUTPUT ledfel).
         END.
         ASSIGN
         locwtidvar = SESSION:TEMP-DIRECTORY.
         IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN locwtidvar = webclienttempdir.
         locwtidvar = locwtidvar + "leddata.txt".
         FIND FIRST temp_text NO-LOCK NO-ERROR.
         IF AVAILABLE temp_text THEN DO:
            OUTPUT TO VALUE(locwtidvar) 
            CONVERT TARGET "iso8859-1" SOURCE "iso8859-1" 
            NO-ECHO.
            PUT UNFORMATTED "FÖRETAG;DISTRIKT;FEL" SKIP.
            FOR EACH temp_text:
               PUT UNFORMATTED temp_text.B1 ";" temp_text.B2 ";" temp_text.PROGNAMN SKIP.
            END.
            OUTPUT CLOSE.
            IF ledfel = FALSE THEN DO:
               MESSAGE "Inläsningen av ledningsdata misslyckades. Ni kan se varför i filen:" +
               locwtidvar VIEW-AS ALERT-BOX TITLE "Meddelande".
            END.
            ELSE DO:
               MESSAGE "Inläsningen av vissa ledningsdata misslyckades. Ni kan se varför i filen:" +
               locwtidvar VIEW-AS ALERT-BOX TITLE "Meddelande".
            END.
         END.
         ELSE DO:
            MESSAGE "Import av ledningsdata klart." VIEW-AS ALERT-BOX TITLE "Meddelande".
         END.
      END.                     
   END.
   IF SEL_UPP = "Exportera störningar" THEN DO:       
      RUN VALDIST.W (INPUT  "", OUTPUT avnr, OUTPUT storar).      
      IF musz = TRUE THEN RETURN.
      filnamn = "Störningar".
      SYSTEM-DIALOG GET-FILE filnamn
      TITLE                  "Spara störningsdata"
      FILTERS                "Text filer (*.txt)"  "*.txt"
      INITIAL-DIR            dirnamn
      ASK-OVERWRITE
      DEFAULT-EXTENSION      ".txt"      
      SAVE-AS
      USE-FILENAME                       
      UPDATE                 OKvald.
      IF OKvald = TRUE THEN DO: 
         RUN minne_UI.
         IF Guru.Konstanter:appcon THEN DO:
            RUN EXPSTOR.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
            (INPUT TABLE val_temp, OUTPUT TABLE dumpa_temp2).
         END.
         ELSE DO:
            RUN EXPSTOR.P
            (INPUT TABLE val_temp, OUTPUT TABLE dumpa_temp2).
         END.                 
         OUTPUT TO VALUE(filnamn)
         CONVERT TARGET "iso8859-1" SOURCE "iso8859-1"
         NO-ECHO.         
         FOR EACH dumpa_temp2 USE-INDEX ORD:
            PUT UNFORMATTED
           dumpa_temp2.FORETAG            ';' 
           dumpa_temp2.DISTRIKT           ';'
           dumpa_temp2.BDAT FORMAT "9999/99/99" ';'
           dumpa_temp2.BTID FORMAT "9999" ';' 
           dumpa_temp2.C1                 ';'   
           dumpa_temp2.D1                 ';'
           dumpa_temp2.D2                 ';' 
           dumpa_temp2.E1                 ';'
       '"' dumpa_temp2.E2 '"'         ';'
           dumpa_temp2.F1                 ';'
       '"' dumpa_temp2.F2 '"'         ';'
           dumpa_temp2.G1                 ';'
           dumpa_temp2.G2                 ';'
           dumpa_temp2.G3                 ';'
           dumpa_temp2.H                  ';'
           dumpa_temp2.J1DAT FORMAT "9999/99/99"       ';'
           dumpa_temp2.J1KLOCK FORMAT "9999"           ';' 
           dumpa_temp2.J2DAT FORMAT "9999/99/99"       ';'
           dumpa_temp2.J2KLOCK FORMAT "9999"           ';' 
            dumpa_temp2.J3 FORMAT ">>>>>>9.99"          ';'
            dumpa_temp2.K1                 ';'
            dumpa_temp2.K2                 ';'
            dumpa_temp2.L1                 ';'
            dumpa_temp2.L2                 ';'
            dumpa_temp2.M1                 ';'
            dumpa_temp2.M2                 ';'
            dumpa_temp2.N1                 ';'
            '"' dumpa_temp2.N2 '"'         ';'
            dumpa_temp2.P1                 ';'
            dumpa_temp2.P2                 ';'
            dumpa_temp2.Q                  ';'
            dumpa_temp2.R                  ';'
            dumpa_temp2.S                  ';'
            dumpa_temp2.T1                 ';'
            dumpa_temp2.T2                 ';'
            dumpa_temp2.T3                 
            SKIP.
         END.
         OUTPUT CLOSE.
         MESSAGE "Export av störningar till filen:" +
         filnamn + " är nu klart." VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.

   END.
   ELSE IF SEL_UPP = "Importera störningar" THEN DO:                                                      
      SYSTEM-DIALOG GET-FILE filnamn
      TITLE          "Välj den fil som motsvarar störningsdata"
      FILTERS        "All Files (*.*)"  "*.*"
      INITIAL-DIR    dirnamn
      MUST-EXIST         
      USE-FILENAME
      UPDATE OKvald.
      IF OKvald = TRUE THEN DO: 
         RUN minne_UI.
         
         EMPTY TEMP-TABLE tidin5  NO-ERROR. 
         INPUT FROM VALUE(filnamn) NO-ECHO.
         REPEAT:
            DO TRANSACTION:         
               CREATE tidin5.
               ASSIGN.
               IMPORT DELIMITER ";" tidin5  NO-ERROR.         
            END.               
         END.   
         
         FOR EACH tidin5 WHERE tidin5.FORETAG = 0:
            DELETE tidin5.
         END.
         DEFINE VARIABLE istor AS INTEGER NO-UNDO.
         FOR EACH tidin5 WHERE :
            istor = istor + 1.
         END.
         MESSAGE istor " poster kommer att läsas in"
         VIEW-AS ALERT-BOX.
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN DARIN5.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT-OUTPUT TABLE temp_text, INPUT-OUTPUT TABLE tidin5,
            INPUT 1, INPUT-OUTPUT TABLE finns_temp).
         END.
         ELSE DO:
            RUN DARIN5.P 
            (INPUT-OUTPUT TABLE temp_text, INPUT-OUTPUT TABLE tidin5,
            INPUT 1, INPUT-OUTPUT TABLE finns_temp).
         END.
         FOR EACH finns_temp NO-LOCK:
            FIND FIRST avdelningtemp WHERE avdelningtemp.AVDELNINGNR = finns_temp.AVDELNINGNR
            NO-LOCK NO-ERROR.
            MESSAGE "För företag " + avdelningtemp.AVDELNINGNAMN + " distrikt " + 
            finns_temp.DISTRIKT + " finns det redan störningar inlästa. Vill Ni fortsätta att läsa in störningar för distriktet? Befintliga störningar kommer då att tas bort."
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Befintliga störningar" UPDATE svar AS LOGICAL.
            IF NOT svar THEN DO:
               FOR EACH tidin5 WHERE tidin5.FORETAG = finns_temp.AVDELNINGNR AND 
               tidin5.DISTRIKT = finns_temp.DISTRIKT AND YEAR(tidin5.BDAT) = finns_temp.ARTAL:
                  tidin5.OKVAR = TRUE.
               END.
            END.
            ELSE DO:
               IF Guru.Konstanter:appcon THEN DO:
                  RUN DELIMP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
                  (INPUT finns_temp.AVDELNINGNR, INPUT finns_temp.DISTRIKT, INPUT finns_temp.ARTAL).
               END.
               ELSE DO:
                  RUN DELIMP.P
                  (INPUT finns_temp.AVDELNINGNR, INPUT finns_temp.DISTRIKT, INPUT finns_temp.ARTAL).
               END.
            END.
         END.
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN DARIN5.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT-OUTPUT TABLE temp_text, INPUT-OUTPUT TABLE tidin5,
            INPUT 2, INPUT-OUTPUT TABLE finns_temp).
         END.
         ELSE DO:
            RUN DARIN5.P 
            (INPUT-OUTPUT TABLE temp_text, INPUT-OUTPUT TABLE tidin5,
            INPUT 2, INPUT-OUTPUT TABLE finns_temp).
         END.
         ASSIGN
         locwtidvar = SESSION:TEMP-DIRECTORY.
         IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN locwtidvar = webclienttempdir.
         locwtidvar = locwtidvar + "storning.txt".
         FIND FIRST temp_text NO-LOCK NO-ERROR.
         IF AVAILABLE temp_text THEN DO:
            OUTPUT TO VALUE(locwtidvar) 
            CONVERT TARGET "iso8859-1" SOURCE "iso8859-1" 
            NO-ECHO.
            PUT UNFORMATTED "FÖRETAG;DISTRIKT;FEL" SKIP.
            FOR EACH temp_text:
               PUT UNFORMATTED temp_text.B1 ";" temp_text.B2 ";" temp_text.PROGNAMN SKIP.
            END.
            OUTPUT CLOSE.
            MESSAGE "Inläsningen av störningsdata misslyckades. Ni kan se varför i filen:" +
            locwtidvar VIEW-AS ALERT-BOX TITLE "Meddelande".
         END.
         ELSE DO:            
            FIND FIRST tidin5 WHERE tidin5.FEL NE "" NO-LOCK NO-ERROR.
            IF AVAILABLE tidin5 THEN DO:          
               ASSIGN
               locwtidvar = SESSION:TEMP-DIRECTORY.
               IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN locwtidvar = webclienttempdir.
               locwtidvar = locwtidvar + "felovrigt.txt".
               OUTPUT TO VALUE(locwtidvar) 
               CONVERT TARGET "iso8859-1" SOURCE "iso8859-1" 
               NO-ECHO.
               FOR EACH tidin5 WHERE tidin5.FEL NE " " AND tidin5.FEL2 = FALSE:
                  PUT UNFORMATTED tidin5.FORETAG ";" tidin5.DISTRIKT
                  ";" tidin5.BDAT ";" tidin5.BTID ";" tidin5.C1 ";" tidin5.D1 ";" tidin5.D2
                  ";" tidin5.E1 ";" tidin5.E2 ";" tidin5.F1 ";" tidin5.F2 ";" tidin5.G1 ";" tidin5.G2 ";" tidin5.G3
                  ";" tidin5.H ";" tidin5.J1DAT ";" tidin5.J1KLOCK ";" tidin5.J2DAT ";" tidin5.J2KLOCK ";" tidin5.J3 ";" tidin5.K1 ";" tidin5.K2
                  ";" tidin5.L1 ";" tidin5.L2 ";" tidin5.M1 ";" tidin5.M2 ";" tidin5.N1 ";" tidin5.N2
                  ";" tidin5.P1 ";" tidin5.P2 ";" tidin5.Q ";" tidin5.R ";" tidin5.S ";" tidin5.T1
                  ";" tidin5.T2 ";" tidin5.T3 ";" tidin5.FEL SKIP.

/*                   PUT UNFORMATTED tidin5.FORETAG ";" tidin5.DISTRIKT                                                                 */
/*                   ";" tidin5.BDAT ";" tidin5.BTID ";" tidin5.C1 ";" tidin5.D1 ";" tidin5.D2                                             */
/*                   ";" tidin5.E1 ";" tidin5.E2 ";" tidin5.F1 ";" tidin5.F2 ";" tidin5.G1 ";" tidin5.G2 ";" tidin5.G3                       */
/*                   ";" tidin5.H ";" tidin5.J1 ";" tidin5.J1KLOCK ";" tidin5.J2 ";" tidin5.J2KLOCK ";" tidin5.J3 ";" tidin5.K1 ";" tidin5.K2 */
/*                   ";" tidin5.L1 ";" tidin5.L2 ";" tidin5.M1 ";" tidin5.M2 ";" tidin5.N1 ";" tidin5.N2                                    */
/*                   ";" tidin5.P1 ";" tidin5.P2 ";" tidin5.Q ";" tidin5.R ";" tidin5.S ";" tidin5.T1                                       */
/*                   ";" tidin5.T2 ";" tidin5.T3 ";" tidin5.FEL SKIP.                                                                    */
               END.
               OUTPUT CLOSE. 
               ASSIGN
               locwtidvar = SESSION:TEMP-DIRECTORY.
               IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN locwtidvar = webclienttempdir.
               locwtidvar = locwtidvar + "feltid.txt".
               OUTPUT TO VALUE(locwtidvar) 
               CONVERT TARGET "iso8859-1" SOURCE "iso8859-1" 
               NO-ECHO.
               FOR EACH tidin5 WHERE tidin5.FEL2 = TRUE:
                  PUT UNFORMATTED tidin5.FORETAG ";" tidin5.DISTRIKT
                  ";" tidin5.BDAT ";" tidin5.BTID ";" tidin5.C1 ";" tidin5.D1 ";" tidin5.D2
                  ";" tidin5.E1 ";" tidin5.E2 ";" tidin5.F1 ";" tidin5.F2 ";" tidin5.G1 ";" tidin5.G2 ";" tidin5.G3
                  ";" tidin5.H ";" tidin5.J1DAT ";" tidin5.J1KLOCK ";" tidin5.J2DAT ";" tidin5.J2KLOCK ";" tidin5.J3 ";" tidin5.K1 ";" tidin5.K2
                  ";" tidin5.L1 ";" tidin5.L2 ";" tidin5.M1 ";" tidin5.M2 ";" tidin5.N1 ";" tidin5.N2
                  ";" tidin5.P1 ";" tidin5.P2 ";" tidin5.Q ";" tidin5.R ";" tidin5.S ";" tidin5.T1
                  ";" tidin5.T2 ";" tidin5.T3 ";" tidin5.FEL SKIP.
/*                   PUT UNFORMATTED tidin5.FORETAG ";" tidin5.DISTRIKT                                                                 */
/*                   ";" tidin5.BDAT ";" tidin5.BTID ";" tidin5.C1 ";" tidin5.D1 ";" tidin5.D2                                             */
/*                   ";" tidin5.E1 ";" tidin5.E2 ";" tidin5.F1 ";" tidin5.F2 ";" tidin5.G1 ";" tidin5.G2 ";" tidin5.G3                       */
/*                   ";" tidin5.H ";" tidin5.J1 ";" tidin5.J1KLOCK ";" tidin5.J2 ";" tidin5.J2KLOCK ";" tidin5.J3 ";" tidin5.K1 ";" tidin5.K2 */
/*                   ";" tidin5.L1 ";" tidin5.L2 ";" tidin5.M1 ";" tidin5.M2 ";" tidin5.N1 ";" tidin5.N2                                    */
/*                   ";" tidin5.P1 ";" tidin5.P2 ";" tidin5.Q ";" tidin5.R ";" tidin5.S ";" tidin5.T1                                       */
/*                   ";" tidin5.T2 ";" tidin5.T3 ";" tidin5.FEL SKIP.                                                                    */
               END.  
               OUTPUT CLOSE.
               locwtidvar = SESSION:TEMP-DIRECTORY.
               IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN locwtidvar = webclienttempdir.
               MESSAGE "Viss inläsningen av störningsdata misslyckades. Ni kan se varför i filerna:" +
               locwtidvar + "feltid.txt som visar fel som har med klockslag och datum att göra och " +
               locwtidvar + "felovrigt.txt som visar övriga fel." VIEW-AS ALERT-BOX TITLE "Meddelande".
            END.
            ELSE DO:
               MESSAGE "Import av störningsdata klart." VIEW-AS ALERT-BOX TITLE "Meddelande".
            END.
         END.
      END.                     
   END.
   ELSE IF SEL_UPP = "Importera störningar XML/JSON/xlsx format" THEN DO:
      RUN  Storning_UI.
     /*
      MESSAGE "slut i rutan"
      VIEW-AS ALERT-BOX.
      */
   END.
   ELSE IF SEL_UPP = "Exportera störningar till Darwin+" THEN DO:
      MESSAGE "Vill Du skicka störningar till Darwin+?"
      VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE nht AS LOGICAL.
      CASE nht:
         WHEN TRUE THEN DO:
            status-mus2 = SESSION:SET-WAIT-STATE("GENERAL").
            IF Guru.Konstanter:appcon THEN DO:
              
               RUN  DARWINPLUSEXPORT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT FALSE).
            END.
            ELSE DO:
               RUN DARWINPLUSEXPORT.P (INPUT TRUE).
            END. 
            status-mus = SESSION:SET-WAIT-STATE("").
            MESSAGE "Klart med överläsning till till Darwin+?"
            VIEW-AS ALERT-BOX.
            
         END.
         WHEN FALSE THEN DO:
         END.
         
      END CASE.   
     
   END.
   ELSE IF SEL_UPP = "Exportera störningar XML format" THEN DO:
      RUN VALDIST.W (INPUT  "XML", OUTPUT avnr, OUTPUT storar).
         
      IF musz = TRUE THEN RETURN.
      IF Guru.Konstanter:appcon THEN DO:
         RUN StorDSIExp.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT avnr, INPUT storar,OUTPUT DATASET-HANDLE StorningImportDSh ).
      END.
      ELSE DO:
         RUN StorDSIExp.P
         (INPUT avnr, INPUT storar,OUTPUT DATASET-HANDLE StorningImportDSh ).
      END.  
      FIND FIRST avdelningtemp WHERE avdelningtemp.AVDELNINGNR = avnr NO-LOCK NO-ERROR.
      filnamn = SESSION:TEMP-DIRECTORY.
      {SESSIONTEMPDIR.I}.
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN filnamn = webclienttempdir.
      filnamn = filnamn  + avdelningtemp.AVDELNINGNAMN  + STRING(storar) + ".xml".
      
      ASSIGN
      cTargetType       = "FILE"
      lFormatted        = NO
      cEncoding         = ?
      cSchemaLocation   = ?
      lWriteSchema      = no
      lMinSchema        = FALSE
      lWriteBeforeImage = FALSE.
      StorningImportDSh:WRITE-XML (cTargetType, filnamn, lFormatted, cEncoding, cSchemaLocation, lWriteSchema, lMinSchema, lWriteBeforeImage). 
      
                     
              MESSAGE "Export av störningar till filen:" +
      filnamn + " är nu klart." VIEW-AS ALERT-BOX TITLE "Meddelande".
  

      
   END.   
   
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

