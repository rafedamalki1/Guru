&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
DEFINE VARIABLE nettoh AS HANDLE NO-UNDO.
DEFINE TEMP-TABLE visamtrl NO-UNDO
      FIELD ENR AS CHARACTER FORMAT "x(11)" LABEL "Enr"    
      FIELD BENAMNING AS CHARACTER FORMAT "x(40)" LABEL "Benämning"   
      FIELD ENHET AS CHARACTER FORMAT "x(5)" LABEL "Enhet" 
      FIELD ANTAL AS INTEGER FORMAT ">>>>>9" LABEL "Antal"
      FIELD LEVKOD AS CHARACTER
      INDEX ENR IS PRIMARY ENR ASCENDING.
DEFINE TEMP-TABLE  evisamtrl NO-UNDO LIKE visamtrl.  /*kopia för att märka nettopris */
DEFINE INPUT PARAMETER ktypkodvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER vismtrl AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER visval AS INTEGER NO-UNDO.

DEFINE INPUT PARAMETER TABLE FOR visamtrl.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
&Scoped-define SHARED SHARED
{LEVTEMP.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BRW_MTRL

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES visamtrl

/* Definitions for BROWSE BRW_MTRL                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL visamtrl.ENR visamtrl.BENAMNING ~
visamtrl.ENHET visamtrl.ANTAL visamtrl.LEVKOD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL visamtrl.ENR 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_MTRL visamtrl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_MTRL visamtrl
&Scoped-define QUERY-STRING-BRW_MTRL FOR EACH visamtrl NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRL OPEN QUERY BRW_MTRL FOR EACH visamtrl NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL visamtrl
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL visamtrl


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BRW_MTRL}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_MTRL BTN_AVB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta" 
     SIZE 14 BY 1
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_MTRL FOR 
      visamtrl SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL Dialog-Frame _STRUCTURED
  QUERY BRW_MTRL NO-LOCK DISPLAY
      visamtrl.ENR FORMAT "X(11)":U
      visamtrl.BENAMNING FORMAT "x(256)":U WIDTH 50
      visamtrl.ENHET FORMAT "x(5)":U
      visamtrl.ANTAL FORMAT ">>>,>>9":U WIDTH 13.5
      visamtrl.LEVKOD COLUMN-LABEL "Lev-id" FORMAT "X(8)":U
  ENABLE
      visamtrl.ENR
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SEPARATORS SIZE 82 BY 12.25 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BRW_MTRL AT ROW 1.75 COL 1.5
     BTN_AVB AT ROW 14.5 COL 69.5
     SPACE(1.62) SKIP(0.32)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "<insert dialog title>"
         CANCEL-BUTTON BTN_AVB.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Temp-Tables and Buffers:
      TABLE: visamtrl T "?" NO-UNDO temp-db visamtrl
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_MTRL 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       BRW_MTRL:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE
       BRW_MTRL:COLUMN-RESIZABLE IN FRAME Dialog-Frame       = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL
/* Query rebuild information for BROWSE BRW_MTRL
     _TblList          = "Temp-Tables.visamtrl"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.visamtrl.ENR
"visamtrl.ENR" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.visamtrl.BENAMNING
"visamtrl.BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "50" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.visamtrl.ENHET
     _FldNameList[4]   > Temp-Tables.visamtrl.ANTAL
"visamtrl.ANTAL" ? ? "integer" ? ? ? ? ? ? no ? no no "13.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.visamtrl.LEVKOD
"visamtrl.LEVKOD" "Lev-id" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_MTRL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* <insert dialog title> */
DO:
   IF VALID-HANDLE(nettoh) THEN DELETE PROCEDURE nettoh. 
   {BORTBRWPROC.I}
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_MTRL
&Scoped-define SELF-NAME BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL Dialog-Frame
ON ROW-LEAVE OF BRW_MTRL IN FRAME Dialog-Frame
DO:   
   IF AVAILABLE visamtrl THEN DO:
      DISPLAY visamtrl.ENR WITH BROWSE BRW_MTRL NO-ERROR.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL Dialog-Frame
ON VALUE-CHANGED OF BRW_MTRL IN FRAME Dialog-Frame
DO:
   BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME visamtrl.ENR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL visamtrl.ENR BRW_MTRL _BROWSE-COLUMN Dialog-Frame
ON ENTRY OF visamtrl.ENR IN BROWSE BRW_MTRL /* Enr */
DO:   
   BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL visamtrl.ENR BRW_MTRL _BROWSE-COLUMN Dialog-Frame
ON LEAVE OF visamtrl.ENR IN BROWSE BRW_MTRL /* Enr */
DO:   
   IF AVAILABLE visamtrl THEN DO:         
      DISPLAY visamtrl.ENR WITH BROWSE BRW_MTRL NO-ERROR.
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL visamtrl.ENR BRW_MTRL _BROWSE-COLUMN Dialog-Frame
ON MOUSE-SELECT-CLICK OF visamtrl.ENR IN BROWSE BRW_MTRL /* Enr */
DO:
    APPLY "ENTRY" TO  visamtrl.ENR IN BROWSE BRW_MTRL.
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
   IF visval = 1 THEN DO:
      FRAME {&FRAME-NAME}:TITLE = "Materiel kopplat till konstruktionsval " + ktypkodvar.
   END.   
   ELSE DO:
      FRAME {&FRAME-NAME}:TITLE = "Materiel kopplat till konstruktionsval "  + ktypkodvar + " : " + vismtrl.
   END.   
   IF Guru.Konstanter:varforetypval[29] = 1  THEN DO:                 
      /*nettopris beredning inköp*/               
      EMPTY TEMP-TABLE evisamtrl NO-ERROR.       
      RUN visnettomark_UI IN nettoh (INPUT TABLE visamtrl, OUTPUT TABLE evisamtrl).            
   END.   
   OPEN QUERY BRW_MTRL FOR EACH visamtrl NO-LOCK.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI Dialog-Frame 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/ 
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_MTRL:HANDLE IN FRAME {&FRAME-NAME}).
   RUN dynprogextra IN brwproc[1] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN StartSokEnrLev_UI IN brwproc[1] (INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[1] (INPUT TRUE).   
   IF Guru.Konstanter:appcon THEN DO:
      RUN NETTOMARK.P PERSISTENT SET nettoh ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NETTOMARK.P PERSISTENT SET nettoh.
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
  ENABLE BRW_MTRL BTN_AVB 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE infoES_UI Dialog-Frame 
PROCEDURE infoES_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/ 
   DEFINE INPUT  PARAMETER valenr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER brwname AS CHARACTER NO-UNDO.
   DEFINE VARIABLE levnamnvar AS CHARACTER NO-UNDO.
   /*GÅR TILL ELEKTROSKANDIAS HEMSIDA*/      
   BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF NOT AVAILABLE visamtrl THEN RETURN.
   valenr = visamtrl.ENR.
   FIND FIRST levtemp WHERE levtemp.LEVKOD = visamtrl.LEVKOD NO-LOCK NO-ERROR.    
   levnamnvar = levtemp.LEVNAMN.
   {LEVLANK.I}   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdispextra_UI Dialog-Frame 
PROCEDURE rowdispextra_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER TABLE FOR coltemp.
   DEFINE INPUT PARAMETER brwh AS HANDLE NO-UNDO.   
   IF brwh:NAME = "BRW_MTRL" THEN RUN rowdispmtrl_UI (INPUT 1).    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdispmtrl_UI Dialog-Frame 
PROCEDURE rowdispmtrl_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.                 
   IF vad = 1 THEN DO:       
      IF AVAILABLE visamtrl THEN DO:         
         IF Guru.Konstanter:varforetypval[29] = 1  THEN DO:                 
            /*nettopris beredning inköp*/         
            FIND FIRST evisamtrl WHERE evisamtrl.LEVKOD =  visamtrl.LEVKOD AND evisamtrl.ENR =  visamtrl.ENR  NO-LOCK NO-ERROR.
            IF AVAILABLE evisamtrl THEN DO:
               /*STOLPAR turkosa fortum  gula övriga*/
               visamtrl.ENR:BGCOLOR IN BROWSE BRW_MTRL = Guru.Konstanter:varforetypval[28].               
            END.
         END.
      END.      
   END.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

