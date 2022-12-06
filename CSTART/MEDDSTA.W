&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-2


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-2 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 03/25/96 -  6:28 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
&Scoped-define NEW
{GLOBVAR2DEL1.I}
DEFINE SHARED VARIABLE knappval AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
{TIDUTTTNEW.I}
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE meddrec AS RECID NO-UNDO.
DEFINE VARIABLE vismeddh AS HANDLE NO-UNDO.
DEFINE TEMP-TABLE flermeddtemp
  FIELD EDATUM AS DATE 
  FIELD EMOTAGET AS LOGICAL 
  FIELD MEDD AS CHARACTER 
  FIELD MOTTAGARE AS CHARACTER 
  FIELD SANDARE AS CHARACTER 
  FIELD SDATUM AS DATE
  FIELD FORETAG AS CHARACTER
  FIELD MEDREC AS RECID
  INDEX MEDD IS PRIMARY FORETAG SANDARE SDATUM MOTTAGARE EMOTAGET 
  INDEX MEDD2 FORETAG SANDARE MOTTAGARE  
  INDEX MOTTAGARE FORETAG MOTTAGARE EMOTAGET SANDARE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-2
&Scoped-define BROWSE-NAME BRW_MED

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES flermeddtemp

/* Definitions for BROWSE BRW_MED                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_MED flermeddtemp.SDATUM ~
flermeddtemp.MOTTAGARE flermeddtemp.EMOTAGET flermeddtemp.MEDD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MED flermeddtemp.MOTTAGARE 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_MED flermeddtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_MED flermeddtemp
&Scoped-define QUERY-STRING-BRW_MED FOR EACH flermeddtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MED OPEN QUERY BRW_MED FOR EACH flermeddtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MED flermeddtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MED flermeddtemp


/* Definitions for DIALOG-BOX DIALOG-2                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_MED FBTN_VISA FBTN_SKR BTN_TALLA BTN_TAM ~
BTN_AVB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-BRW_MED 
       MENU-ITEM m_Visa_meddelande LABEL "Visa meddelande".


/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_TALLA 
     LABEL "Ta bort alla" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_TAM 
     LABEL "Ta bort markerade" 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_SKR 
     LABEL "Skriv ut" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VISA 
     LABEL "VISA" 
     SIZE 14 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_MED FOR 
      flermeddtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_MED
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MED DIALOG-2 _STRUCTURED
  QUERY BRW_MED NO-LOCK DISPLAY
      flermeddtemp.SDATUM COLUMN-LABEL "Sänt datum" FORMAT "99/99/99":U
      flermeddtemp.MOTTAGARE COLUMN-LABEL "Mottagare" FORMAT "x(12)":U
      flermeddtemp.EMOTAGET COLUMN-LABEL "Mottaget" FORMAT "Ja/Nej":U
      flermeddtemp.MEDD COLUMN-LABEL "Meddelande" FORMAT "X(256)":U
            WIDTH 43
  ENABLE
      flermeddtemp.MOTTAGARE
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 79.5 BY 13
         TITLE "Ej lästa meddelanden" TOOLTIP "Dubbel klicka om du vill läsa meddelandet".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-2
     BRW_MED AT ROW 1.5 COL 1.5
     FBTN_VISA AT ROW 4.13 COL 83.5
     FBTN_SKR AT ROW 5.21 COL 83.5
     BTN_TALLA AT ROW 15 COL 16.38
     BTN_TAM AT ROW 15 COL 45.88
     BTN_AVB AT ROW 15 COL 83.5
     SPACE(1.12) SKIP(0.37)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ej lästa meddelanden".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: flermeddtemp T "?" NO-UNDO temp-db flermeddtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-2
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_MED 1 DIALOG-2 */
ASSIGN 
       FRAME DIALOG-2:SCROLLABLE       = FALSE
       FRAME DIALOG-2:HIDDEN           = TRUE.

ASSIGN 
       BRW_MED:POPUP-MENU IN FRAME DIALOG-2             = MENU POPUP-MENU-BRW_MED:HANDLE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MED
/* Query rebuild information for BROWSE BRW_MED
     _TblList          = "Temp-Tables.flermeddtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.flermeddtemp.SDATUM
"flermeddtemp.SDATUM" "Sänt datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.flermeddtemp.MOTTAGARE
"flermeddtemp.MOTTAGARE" "Mottagare" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.flermeddtemp.EMOTAGET
"flermeddtemp.EMOTAGET" "Mottaget" ? "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.flermeddtemp.MEDD
"flermeddtemp.MEDD" "Meddelande" "X(256)" "character" ? ? ? ? ? ? no ? no no "43" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MED */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_MED
&Scoped-define SELF-NAME BRW_MED
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MED DIALOG-2
ON MOUSE-SELECT-DBLCLICK OF BRW_MED IN FRAME DIALOG-2 /* Ej lästa meddelanden */
DO:
   ASSIGN
   vart = "MST"
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   Guru.GlobalaVariabler:plusrec = flermeddtemp.MEDREC.
   RUN VISMEDDU.W.
   musz = FALSE.
   vart = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-2
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-2 /* Avsluta */
DO:
   APPLY "GO" TO BTN_AVB IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-2
ON GO OF BTN_AVB IN FRAME DIALOG-2 /* Avsluta */
DO:
   IF VALID-HANDLE(vismeddh) THEN DELETE PROCEDURE vismeddh NO-ERROR.
   vismeddh = ?.
   {BORTBRWPROC.I}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_TALLA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_TALLA DIALOG-2
ON CHOOSE OF BTN_TALLA IN FRAME DIALOG-2 /* Ta bort alla */
DO:
   {muswait.i}         
   GET FIRST {&BROWSE-NAME}.
   DO WHILE AVAILABLE(flermeddtemp):
      flermeddtemp.FORETAG = "tabort".
      GET NEXT {&BROWSE-NAME}.    
   END.
   RUN medbort_UI IN vismeddh (INPUT TABLE flermeddtemp).  
   EMPTY TEMP-TABLE flermeddtemp NO-ERROR. 
   
   {musarrow.i}
   APPLY "GO" TO BTN_AVB IN FRAME {&FRAME-NAME}.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_TAM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_TAM DIALOG-2
ON CHOOSE OF BTN_TAM IN FRAME DIALOG-2 /* Ta bort markerade */
DO:
   antal_valda = {&BROWSE-NAME}:NUM-SELECTED-ROWS.
   IF antal_valda = 0 THEN DO:
      MESSAGE "Du måste välja någon." VIEW-AS ALERT-BOX.
   END.
   ELSE DO:
      {muswait.i}                
      antal_raknare = 1.      
      DO WHILE antal_raknare LE antal_valda :
         status-ok = {&BROWSE-NAME}:FETCH-SELECTED-ROW(antal_raknare).            
         meddrec = RECID(flermeddtemp).
         flermeddtemp.FORETAG = "tabort".
         antal_raknare = antal_raknare + 1.         
      END.          
      RUN medbort_UI IN vismeddh (INPUT TABLE flermeddtemp).  
      FOR EACH flermeddtemp WHERE flermeddtemp.FORETAG = "tabort":
         DELETE flermeddtemp.
      END.      
   END.
   RUN refreshbrw_UI IN brwproc[1].
   {musarrow.i}
   GET FIRST {&BROWSE-NAME} NO-LOCK.
   IF NOT AVAILABLE flermeddtemp THEN DO:   
      APPLY "GO" TO BTN_AVB IN FRAME {&FRAME-NAME}.     
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKR DIALOG-2
ON CHOOSE OF FBTN_SKR IN FRAME DIALOG-2 /* Skriv ut */
DO:
   RUN SKRIVVAL.W (INPUT FALSE).         
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:    
      RUN ut_UI.      
   END.
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKR DIALOG-2
ON MOUSE-MENU-CLICK OF FBTN_SKR IN FRAME DIALOG-2 /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA DIALOG-2
ON CHOOSE OF FBTN_VISA IN FRAME DIALOG-2 /* VISA */
DO:
   APPLY "MOUSE-SELECT-DBLCLICK" TO BRW_MED.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA DIALOG-2
ON MOUSE-MENU-CLICK OF FBTN_VISA IN FRAME DIALOG-2 /* VISA */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Visa_meddelande
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Visa_meddelande DIALOG-2
ON CHOOSE OF MENU-ITEM m_Visa_meddelande /* Visa meddelande */
DO:
   ASSIGN
   vart = "MST"
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   Guru.GlobalaVariabler:plusrec = flermeddtemp.MEDREC.
   RUN VISMEDDU.W.
   musz = FALSE.
   vart = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-2 


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
   {ALLSTARTDYN.I}
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN APAMEDDU.P PERSISTENT SET vismeddh ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN APAMEDDU.P PERSISTENT SET vismeddh.
   END.   
   IF vad = 1 THEN DO:
      RUN meddstatus_UI IN vismeddh (INPUT Guru.Konstanter:globanv,OUTPUT TABLE flermeddtemp).
   END.
   ELSE DO:
      RUN meddssparade_UI IN vismeddh (INPUT Guru.Konstanter:globanv,OUTPUT TABLE flermeddtemp).
   END.
  
   RUN enable_UI.       
   {FRMSIZED.I}  
   IF vad = 1 THEN DO:
      BRW_MED:TITLE = "Ej lästa meddelanden". 
      RUN openbdyn_UI IN brwproc[1] (INPUT " WHERE flermeddtemp.SANDARE = """ + Guru.Konstanter:globanv + '"').
   END.
   ELSE DO:
      FRAME DIALOG-2:TITLE = "Sparade meddelanden".
      BRW_MED:TITLE = "Sparade meddelanden". 
      RUN openbdyn_UI IN brwproc[1] (INPUT " WHERE flermeddtemp.MOTTAGARE = """ + Guru.Konstanter:globanv + '"').
   END.
   GET FIRST {&BROWSE-NAME} NO-LOCK.
   IF NOT AVAILABLE flermeddtemp THEN DO:
      IF vad = 1 THEN DO: 
         MESSAGE "Alla dina meddelande är redan lästa." VIEW-AS ALERT-BOX.
      END.
      ELSE DO:
         MESSAGE "Du har inga sparade meddelanden." VIEW-AS ALERT-BOX.
      END.

      IF VALID-HANDLE(vismeddh) THEN DELETE PROCEDURE vismeddh NO-ERROR.
      vismeddh = ?.
      {BORTBRWPROC.I}
      DEFINE VARIABLE status-musdia2 AS LOGICAL NO-UNDO. 
      status-musdia2 = SESSION:SET-WAIT-STATE("").
      IF Guru.GlobalaVariabler:retvalkoll = TRUE THEN DO:
         RUN SetDefaultCursors IN Guru.Konstanter:hpApi.  
         Guru.GlobalaVariabler:retvalkoll = FALSE.
      END.
      LEAVE MAIN-BLOCK.
   END.    
  
  {DIA_M_SLUT.I}
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI DIALOG-2 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/ 
   flermeddtemp.MOTTAGARE:READ-ONLY IN BROWSE BRW_MED = TRUE.    
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_MED:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN setdescvarcol_UI IN brwproc[1] (INPUT TRUE ).
   /*RUN setdefaultcolbyname_UI IN brwproc[1] (INPUT "SDATUM" ).*/
   /*RUN setcolindex_UI IN brwproc[1] (INPUT "SDATUM" ).      */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-2  _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-2.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-2  _DEFAULT-ENABLE
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
  ENABLE BRW_MED FBTN_VISA FBTN_SKR BTN_TALLA BTN_TAM BTN_AVB 
      WITH FRAME DIALOG-2.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-2}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI DIALOG-2 
PROCEDURE ut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   skrivut = FALSE.                         
   FIND LAST tidut NO-LOCK NO-ERROR.
   CREATE tidut.
   SUBSTRING(tidut.UT,30) = STRING(TODAY).    
   CREATE tidut.  
   ASSIGN                                    
   SUBSTRING(tidut.UT,1) = "EJ LÄSTA MEDDELANDEN FRÅN"
   SUBSTRING(tidut.UT,27) = Guru.Konstanter:globanv.
   CREATE tidut. 
   ASSIGN            
   SUBSTRING(tidut.UT,1) = "SÄNT DATUM" 
   SUBSTRING(tidut.UT,12) = "MOTAGARE" 
   SUBSTRING(tidut.UT,25) = "EMOTAGET" 
   SUBSTRING(tidut.UT,35) = "MEDDELANDE" .
   CREATE tidut.
   SUBSTRING(tidut.UT,1) =    
   "==========.============.=========.=========================================================".        
   GET FIRST {&BROWSE-NAME} NO-LOCK.
   DO WHILE AVAILABLE(flermeddtemp):
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = STRING(flermeddtemp.SDATUM)     
      SUBSTRING(tidut.UT,12) = flermeddtemp.MOTTAGARE.
      IF flermeddtemp.EMOTAGET = FALSE THEN 
      ASSIGN SUBSTRING(tidut.UT,25) = "NEJ". 
      ELSE  
      ASSIGN SUBSTRING(tidut.UT,25) = "JA". 
      ASSIGN
      SUBSTRING(tidut.UT,35) = STRING(flermeddtemp.MEDD,"X(40)").
      GET NEXT {&BROWSE-NAME} NO-LOCK.
   END.        
   RUN EKLOGS.P.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

