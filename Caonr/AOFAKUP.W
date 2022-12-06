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

  Created: 95/05/10 -  1:37 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER aonrvar AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER delnrvar AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}

{FAKTTYPDEF.I}
{ANVPERS.I}
&Scoped-define SHARED SHARED 
{DIRDEF.I}
{AONRDEF.I}

{AVTAONRTEMP.I}
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE aofakapph AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_ANV

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES anvandartemp

/* Definitions for BROWSE BRW_ANV                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_ANV anvandartemp.ANVANDARE ~
anvandartemp.AV-NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ANV anvandartemp.ANVANDARE 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_ANV anvandartemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_ANV anvandartemp
&Scoped-define QUERY-STRING-BRW_ANV FOR EACH anvandartemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ANV OPEN QUERY BRW_ANV FOR EACH anvandartemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ANV anvandartemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ANV anvandartemp


/* Definitions for DIALOG-BOX DIALOG-1                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DIALOG-1 ~
    ~{&OPEN-QUERY-BRW_ANV}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_ANV FILL-IN_ANV FILL-IN_INTERNT ~
FILL-IN_EXTERNT BTN_OK 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_ANV CMB_FAK FILL-IN_INTERNT ~
FILL-IN_EXTERNT FILL-IN-ET 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_FAK AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fakturakategori" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     DROP-DOWN-LIST
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ET AS CHARACTER FORMAT "X(256)":U INITIAL "Planerat externt offertpris" 
      VIEW-AS TEXT 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_ANV AS CHARACTER FORMAT "X(12)":U 
     LABEL "Ansvarig" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_EXTERNT AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     LABEL "faktureras ej i GURU" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE FILL-IN_FAKTNR AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Ingår i fakturaplannr" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE FILL-IN_INTERNT AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     LABEL "Planerat internt offertpris" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     FGCOLOR 9 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_ANV FOR 
      anvandartemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_ANV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ANV DIALOG-1 _STRUCTURED
  QUERY BRW_ANV NO-LOCK DISPLAY
      anvandartemp.ANVANDARE COLUMN-LABEL "Ansvarig" FORMAT "x(12)":U
      anvandartemp.AV-NAMN COLUMN-LABEL "Namn" FORMAT "x(40)":U
  ENABLE
      anvandartemp.ANVANDARE
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 40.5 BY 7.42.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BRW_ANV AT ROW 1.5 COL 72
     FILL-IN_FAKTNR AT ROW 3.25 COL 29 COLON-ALIGNED
     FILL-IN_ANV AT ROW 4.63 COL 29 COLON-ALIGNED
     CMB_FAK AT ROW 6 COL 29 COLON-ALIGNED
     FILL-IN_INTERNT AT ROW 7.21 COL 29 COLON-ALIGNED
     FILL-IN_EXTERNT AT ROW 10.04 COL 29 COLON-ALIGNED
     BTN_OK AT ROW 10.04 COL 98.5
     FILL-IN-ET AT ROW 8.75 COL 2 NO-LABEL
     "Ange faktura ansvarig mm.:" VIEW-AS TEXT
          SIZE 30.25 BY 1.46 AT ROW 1.5 COL 3
          FONT 17
     SPACE(79.74) SKIP(8.36)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Faktureringsuppgifter":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: anvandartemp T "?" NO-UNDO temp-db anvandartemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_ANV TEXT-1 DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_FAK IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-ET IN FRAME DIALOG-1
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN_FAKTNR IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_FAKTNR:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ANV
/* Query rebuild information for BROWSE BRW_ANV
     _TblList          = "Temp-Tables.anvandartemp"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.anvandartemp.ANVANDARE
"anvandartemp.ANVANDARE" "Ansvarig" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.anvandartemp.AV-NAMN
"anvandartemp.AV-NAMN" "Namn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_ANV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Faktureringsuppgifter */
DO:                                     
   CMB_FAK = INPUT CMB_FAK.
   FIND FIRST anvandartemp WHERE anvandartemp.AV-LEVEL NE 0 AND anvandartemp.ANVANDARE = uppaonrtemp.ANVANDARE
   USE-INDEX ANV NO-LOCK NO-ERROR. 
   IF NOT AVAILABLE anvandartemp THEN DO:
      MESSAGE "Användare finns ej." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_ANV.
   END.
   ELSE DO:
      FIND FIRST faktyptemp WHERE faktyptemp.VIFAKTTYP = CMB_FAK NO-ERROR.
      uppaonrtemp.FAKTTYP = faktyptemp.FAKTTYP.
      {muswait.i}    
      FRAME DIALOG-1:HIDDEN = TRUE.   
      IF VALID-HANDLE(aofakapph) THEN DELETE PROCEDURE aofakapph.   
      {BORTBRWPROC.I}
      RETURN.
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Faktureringsuppgifter */
DO:
   APPLY "END-ERROR" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_ANV
&Scoped-define SELF-NAME BRW_ANV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_ANV DIALOG-1
ON VALUE-CHANGED OF BRW_ANV IN FRAME DIALOG-1
DO:
   status-ok = BRW_ANV:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   ASSIGN FILL-IN_ANV = anvandartemp.ANVANDARE.
   DISPLAY FILL-IN_ANV WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:   
   ASSIGN
   CMB_FAK = INPUT CMB_FAK
   FILL-IN_ANV = INPUT FILL-IN_ANV
   FILL-IN_INTERNT = INPUT FILL-IN_INTERNT
   FILL-IN_EXTERNT = INPUT FILL-IN_EXTERNT.
   IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
      FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = FILL-IN_ANV
      USE-INDEX ANV NO-LOCK NO-ERROR. 
   END.
   ELSE DO:
      FIND FIRST anvandartemp WHERE anvandartemp.AV-LEVEL NE 0 AND anvandartemp.ANVANDARE = FILL-IN_ANV
      USE-INDEX ANV NO-LOCK NO-ERROR. 
   END.
   IF NOT AVAILABLE anvandartemp THEN DO:
      MESSAGE "Användare finns ej." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_ANV.
   END.
   ELSE DO:
      ASSIGN 
      uppaonrtemp.BETNR = FILL-IN_INTERNT /*+ FILL-IN_EXTERNT*/
      uppaonrtemp.STARTVNR = FILL-IN_EXTERNT
      uppaonrtemp.ANVANDARE = FILL-IN_ANV.
      FIND FIRST faktyptemp WHERE faktyptemp.VIFAKTTYP = CMB_FAK NO-ERROR.
      uppaonrtemp.FAKTTYP = faktyptemp.FAKTTYP.     
      {muswait.i}    
      FRAME DIALOG-1:HIDDEN = TRUE.   
      APPLY "GO" TO BTN_OK IN FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON GO OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:
   IF VALID-HANDLE(aofakapph) THEN DELETE PROCEDURE aofakapph. 
   {BORTBRWPROC.I}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_ANV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ANV DIALOG-1
ON LEAVE OF FILL-IN_ANV IN FRAME DIALOG-1 /* Ansvarig */
DO:
  FILL-IN_ANV = INPUT FILL-IN_ANV.
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
   {musarrow.i}
   {ALLSTARTDYN.I} 
   FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND 
   uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.
   RUN laddaanv IN aofakapph (INPUT aonrvar,INPUT delnrvar,
                              OUTPUT TABLE anvandartemp).
    
   FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = uppaonrtemp.ANVANDARE NO-LOCK NO-ERROR.
   IF uppaonrtemp.ANVANDARE = "" THEN ASSIGN uppaonrtemp.ANVANDARE = Guru.Konstanter:globanv. 
   IF uppaonrtemp.FAKTTYP = "" THEN DO:
      IF Guru.Konstanter:globforetag = "cELPA" {GLOBVES.I} THEN DO:
         FIND FIRST faktyptemp WHERE faktyptemp.FAKTTYP = "Löpande räkning" NO-ERROR.
         uppaonrtemp.FAKTTYP = faktyptemp.FAKTTYP.        
      END.
      ELSE DO:      
         FIND FIRST faktyptemp WHERE faktyptemp.FAKTTYP = "Faktureras ej" NO-ERROR.
         uppaonrtemp.FAKTTYP = faktyptemp.FAKTTYP.        
      END.
   END.
   RUN enable_UI.       
   {FRMSIZED.I}
   IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
      FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = uppaonrtemp.ANVANDARE
      USE-INDEX ANV NO-LOCK NO-ERROR.      
   END.
   ELSE DO:
      FIND FIRST anvandartemp WHERE anvandartemp.AV-LEVEL NE 0 AND anvandartemp.ANVANDARE = uppaonrtemp.ANVANDARE
      USE-INDEX ANV NO-LOCK NO-ERROR.       
   END.
               
   IF AVAILABLE anvandartemp  THEN DO:
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() NO-ERROR. 
      RUN repo_UI (INPUT RECID(anvandartemp)).
      ASSIGN FILL-IN_ANV = anvandartemp.ANVANDARE.
      DISPLAY FILL-IN_ANV WITH FRAME {&FRAME-NAME}.
   END.                                               
   ELSE DO:
      APPLY "HOME" TO {&BROWSE-NAME}.
   END.
  
   FILL-IN_FAKTNR = uppaonrtemp.FAKTNR.
   IF uppaonrtemp.FAKTNR = 0 THEN ENABLE CMB_FAK WITH FRAME {&FRAME-NAME}.       
   IF uppaonrtemp.FAKTNR NE 0 THEN DISPLAY FILL-IN_FAKTNR WITH FRAME {&FRAME-NAME}.
   
   ASSIGN 
   FILL-IN_INTERNT = uppaonrtemp.BETNR /*- uppaonrtemp.STARTVNR*/     
   FILL-IN_EXTERNT = uppaonrtemp.STARTVNR.      

   IF Guru.Konstanter:varforetypval[3] >= 1 AND Guru.Konstanter:varforetypval[3] < 4 THEN DO:
   END.
   ELSE DO:
      FILL-IN_FAKTNR:HIDDEN = TRUE. 
   END.
   IF Guru.Konstanter:varforetypval[10] = 1 THEN DO:
      DISPLAY FILL-IN-ET FILL-IN_INTERNT FILL-IN_EXTERNT WITH FRAME {&FRAME-NAME}.
   END.
   ELSE DO:
      ASSIGN
      FILL-IN-ET:HIDDEN = TRUE
      FILL-IN_EXTERNT:HIDDEN = TRUE                       
      FILL-IN_INTERNT:LABEL = "Planerat offertpris".
      DISPLAY FILL-IN_INTERNT WITH FRAME {&FRAME-NAME}.
   END.
   FOR EACH faktyptemp WHERE faktyptemp.ORDNING >= 0:
      status-ok = CMB_FAK:ADD-LAST(faktyptemp.VIFAKTTYP).      
   END.   
   FIND FIRST faktyptemp WHERE faktyptemp.FAKTTYP = uppaonrtemp.FAKTTYP NO-ERROR.
   IF Guru.Konstanter:globforetag = "ELPA" {GLOBVES.I} THEN DO:
      IF NOT AVAILABLE faktyptemp THEN DO:        
         FIND FIRST faktyptemp WHERE faktyptemp.FAKTTYP = "Löpande räkning" NO-ERROR.        
      END.
   END.
   CMB_FAK = faktyptemp.VIFAKTTYP.
   ASSIGN CMB_FAK:SCREEN-VALUE = faktyptemp.VIFAKTTYP.
   DISPLAY CMB_FAK WITH FRAME {&FRAME-NAME}. 
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
   anvandartemp.ANVANDARE:READ-ONLY IN BROWSE BRW_ANV = TRUE.
   
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_ANV:HANDLE IN FRAME {&FRAME-NAME}).
      
   IF Guru.Konstanter:appcon THEN DO:
      RUN AOFAKAPP.P PERSISTENT SET aofakapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN AOFAKAPP.P PERSISTENT SET aofakapph.
   END.
  
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
  DISPLAY FILL-IN_ANV CMB_FAK FILL-IN_INTERNT FILL-IN_EXTERNT FILL-IN-ET 
      WITH FRAME DIALOG-1.
  ENABLE BRW_ANV FILL-IN_ANV FILL-IN_INTERNT FILL-IN_EXTERNT BTN_OK 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo_UI DIALOG-1 
PROCEDURE repo_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER browrec AS RECID NO-UNDO.
   {&BROWSE-NAME}:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
   REPOSITION {&BROWSE-NAME} TO RECID browrec.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

