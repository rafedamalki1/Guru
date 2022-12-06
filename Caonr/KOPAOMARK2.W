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

  Created: 95/07/05 - 10:41 am

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

&Scoped-define SHARED SHARED 
{DIRDEF.I}
{AONRDEF.I}
{AVTAONRTEMP.I}
{OMRTEMPW.I}
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vardrec AS RECID NO-UNDO.
DEFINE VARIABLE vardrec2 AS RECID NO-UNDO.
DEFINE VARIABLE omr AS CHARACTER NO-UNDO.
DEFINE VARIABLE ortben AS CHARACTER NO-UNDO.
DEFINE VARIABLE ortssok AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE valford AS CHARACTER NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE kopaomapph AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_VARDAONR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES varderingtemp

/* Definitions for BROWSE BRW_VARDAONR                                  */
&Scoped-define FIELDS-IN-QUERY-BRW_VARDAONR varderingtemp.OMRADE ~
varderingtemp.VARDNR varderingtemp.BENAMNING varderingtemp.AONR ~
varderingtemp.DELNR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VARDAONR varderingtemp.VARDNR 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_VARDAONR varderingtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_VARDAONR varderingtemp
&Scoped-define QUERY-STRING-BRW_VARDAONR FOR EACH varderingtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VARDAONR OPEN QUERY BRW_VARDAONR FOR EACH varderingtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VARDAONR varderingtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VARDAONR varderingtemp


/* Definitions for DIALOG-BOX DIALOG-1                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-22 CMB_OMR FILL-IN_VARDNR ~
FILL-IN_KALKYL FILL-IN_KALKB BTN_REG BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_AONR FILL-IN_DELNR FILL-IN_ORT ~
CMB_OMR FILL-IN_VARDNR FILL-IN_KALKYL FILL-IN_KALKB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_REG 
     LABEL "Koppla":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_OMR AS CHARACTER FORMAT "X(16)":U 
     LABEL "Utförande område" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_AONR AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_DELNR AS INTEGER FORMAT ">99" INITIAL 0 
     LABEL "Delnr" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_KALKB AS CHARACTER FORMAT "x(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_KALKYL AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     LABEL "Värdering nr" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_ORT AS CHARACTER FORMAT "x(40)" 
     LABEL "Ort/Benämning" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1.

DEFINE VARIABLE FILL-IN_VARDNR AS INTEGER FORMAT ">>>>>>9" INITIAL 0 
     LABEL "Värdering nr" 
     VIEW-AS FILL-IN 
     SIZE 9.5 BY 1.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 47.5 BY 2.83
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_VARDAONR FOR 
      varderingtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_VARDAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VARDAONR DIALOG-1 _STRUCTURED
  QUERY BRW_VARDAONR NO-LOCK DISPLAY
      varderingtemp.OMRADE COLUMN-LABEL "Område" FORMAT "x(6)":U
      varderingtemp.VARDNR COLUMN-LABEL "Värdering nr" FORMAT "->>>>>>9":U
      varderingtemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(8)":U
      varderingtemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      varderingtemp.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
  ENABLE
      varderingtemp.VARDNR
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 47.5 BY 7.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN_AONR AT ROW 2 COL 18 COLON-ALIGNED
     FILL-IN_DELNR AT ROW 2 COL 33.63 COLON-ALIGNED
     FILL-IN_ORT AT ROW 3.5 COL 18 COLON-ALIGNED
     CMB_OMR AT ROW 5 COL 18 COLON-ALIGNED
     FILL-IN_VARDNR AT ROW 6.5 COL 18 COLON-ALIGNED
     BRW_VARDAONR AT ROW 6.5 COL 30
     FILL-IN_KALKYL AT ROW 14 COL 54.5 COLON-ALIGNED
     FILL-IN_KALKB AT ROW 15.17 COL 54.5 COLON-ALIGNED
     BTN_REG AT ROW 17.54 COL 48.5
     BTN_AVS AT ROW 17.54 COL 63.5
     "Sök på:" VIEW-AS TEXT
          SIZE 7.5 BY .67 AT ROW 13.92 COL 31.88
     RECT-22 AT ROW 13.67 COL 30
     SPACE(0.74) SKIP(2.12)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Koppla aonr till kalkyl":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: varderingtemp T "?" NO-UNDO temp-db varderingtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_VARDAONR FILL-IN_VARDNR DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE BRW_VARDAONR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BRW_VARDAONR:HIDDEN  IN FRAME DIALOG-1                = TRUE
       BRW_VARDAONR:MAX-DATA-GUESS IN FRAME DIALOG-1         = 1000.

/* SETTINGS FOR FILL-IN FILL-IN_AONR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_DELNR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_ORT IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VARDAONR
/* Query rebuild information for BROWSE BRW_VARDAONR
     _TblList          = "Temp-Tables.varderingtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.varderingtemp.OMRADE
"varderingtemp.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.varderingtemp.VARDNR
"varderingtemp.VARDNR" "Värdering nr" "->>>>>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.varderingtemp.BENAMNING
"varderingtemp.BENAMNING" "Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.varderingtemp.AONR
"varderingtemp.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.varderingtemp.DELNR
"varderingtemp.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VARDAONR */
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
ON END-ERROR OF FRAME DIALOG-1 /* Koppla aonr till kalkyl */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(kopaomapph) THEN DELETE PROCEDURE kopaomapph.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Koppla aonr till kalkyl */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(kopaomapph) THEN DELETE PROCEDURE kopaomapph.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON WINDOW-CLOSE OF FRAME DIALOG-1 /* Koppla aonr till kalkyl */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VARDAONR
&Scoped-define SELF-NAME BRW_VARDAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VARDAONR DIALOG-1
ON VALUE-CHANGED OF BRW_VARDAONR IN FRAME DIALOG-1
DO:
   ASSIGN FILL-IN_VARDNR = varderingtemp.VARDNR.
   DISPLAY FILL-IN_VARDNR WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS DIALOG-1
ON CHOOSE OF BTN_AVS IN FRAME DIALOG-1 /* Avbryt */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_REG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON CHOOSE OF BTN_REG IN FRAME DIALOG-1 /* Koppla */
DO:
   {muswait.i}  
   FILL-IN_VARDNR = INPUT FILL-IN_VARDNR.
   FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND 
   uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.
   RUN kopplakoll IN kopaomapph (INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR,INPUT omr,
                             INPUT FILL-IN_VARDNR,OUTPUT TABLE felmeddtemp). 
   FIND FIRST felmeddtemp WHERE felmeddtemp.VAL = 1 NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
      DELETE felmeddtemp.
      APPLY "ENTRY" TO FILL-IN_VARDNR IN FRAME {&FRAME-NAME}.
   END.
   FIND FIRST felmeddtemp WHERE felmeddtemp.VAL = 2 NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD 
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val1 AS LOGICAL.
      CASE val1:
         WHEN TRUE THEN DO:
            RUN bortkopp IN kopaomapph (INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR,INPUT omr,
                             INPUT FILL-IN_VARDNR). 
         END.
         WHEN FALSE THEN DO:                                        
             status-mus2 = SESSION:SET-WAIT-STATE("").
             APPLY "ENTRY" TO FILL-IN_VARDNR IN FRAME {&FRAME-NAME}.
         END.
      END CASE.
      DELETE felmeddtemp.
   END.
   RUN koppla IN kopaomapph (INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR,INPUT omr,
                             INPUT FILL-IN_VARDNR,INPUT-OUTPUT TABLE varderingtemp,
                             INPUT-OUTPUT TABLE aovardtemp). 

   {musarrow.i}   
   APPLY "GO" TO BTN_REG IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON GO OF BTN_REG IN FRAME DIALOG-1 /* Koppla */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(kopaomapph) THEN DELETE PROCEDURE kopaomapph.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OMR DIALOG-1
ON LEAVE OF CMB_OMR IN FRAME DIALOG-1 /* Utförande område */
DO:
   CMB_OMR = INPUT CMB_OMR.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OMR DIALOG-1
ON VALUE-CHANGED OF CMB_OMR IN FRAME DIALOG-1 /* Utförande område */
DO:
   CMB_OMR = INPUT CMB_OMR.
   FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR NO-ERROR.
   IF AVAILABLE omrtemp THEN DO:
      ASSIGN
      omr = omrtemp.OMRADE.      
      RUN vardomrhmt IN kopaomapph (INPUT omr,OUTPUT TABLE varderingtemp).      
      RUN hmtvard_UI.
   END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_KALKYL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_KALKYL DIALOG-1
ON LEAVE OF FILL-IN_KALKYL IN FRAME DIALOG-1 /* Värdering nr */
DO:
   FILL-IN_KALKYL = INPUT FILL-IN_KALKYL.
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
   {ALLSTARTDYN.I} 
   &Scoped-define FORMATNAMN FILL-IN_AONR
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN_DELNR   
   {DELNRFORMAT.I}
   FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND 
   uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.
   IF AVAILABLE uppaonrtemp THEN DO:
      ASSIGN
      omr = uppaonrtemp.OMRADE
      ortben = uppaonrtemp.ORT.
      RUN vardhmt IN kopaomapph (INPUT aonrvar,INPUT delnrvar,INPUT omr,
                                 OUTPUT TABLE varderingtemp,OUTPUT TABLE aovardtemp).
   END.   
   
   {OMRHMT.I}   
   FOR EACH omrtemp:
      status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).
   END.   
   
   FILL-IN_ORT:LABEL = Guru.Konstanter:gaonamnk.     
   RUN grundtid_UI. 
   RUN enable_UI.       
   
   ENABLE BRW_VARDAONR WITH FRAME {&FRAME-NAME}.
   
   RUN hmtvard_UI.   
   ASSIGN
   varderingtemp.AONR:LABEL IN BROWSE BRW_VARDAONR = Guru.Konstanter:gaok
   FILL-IN_AONR:LABEL = Guru.Konstanter:gaok.
   ASSIGN FRAME {&FRAME-NAME}:TITLE = "Koppla " + LC(Guru.Konstanter:gaol) + " till markvärdering".       
   {musarrow.i}  
   my1hand = FILL-IN_VARDNR:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = BRW_VARDAONR:MOVE-AFTER-TAB-ITEM(my1hand).
   {FRMSIZED.I}
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
   varderingtemp.VARDNR:READ-ONLY IN BROWSE BRW_VARDAONR = TRUE.
    RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_VARDAONR:HANDLE IN FRAME {&FRAME-NAME}).         
   RUN addfillin_UI IN brwproc[1] (INPUT FILL-IN_KALKB:HANDLE, INPUT "BENAMNING").
   RUN addfillin_UI IN brwproc[1] (INPUT FILL-IN_KALKYL:HANDLE, INPUT "VARDNR").
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_VARDAONR:HANDLE IN FRAME {&FRAME-NAME}).
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN KOPAOMAPP.P PERSISTENT SET kopaomapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN KOPAOMAPP.P PERSISTENT SET kopaomapph.
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
  DISPLAY FILL-IN_AONR FILL-IN_DELNR FILL-IN_ORT CMB_OMR FILL-IN_VARDNR 
          FILL-IN_KALKYL FILL-IN_KALKB 
      WITH FRAME DIALOG-1.
  ENABLE RECT-22 CMB_OMR FILL-IN_VARDNR FILL-IN_KALKYL FILL-IN_KALKB BTN_REG 
         BTN_AVS 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE grundtid_UI DIALOG-1 
PROCEDURE grundtid_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  -------------------------------------------------------------*/
  FIND FIRST omrtemp WHERE omrtemp.NAMN = omr
   USE-INDEX OMR NO-LOCK NO-ERROR.
   IF AVAILABLE omrtemp THEN DO:
      ASSIGN CMB_OMR:SCREEN-VALUE IN FRAME {&FRAME-NAME} = omrtemp.NAMN.   
      APPLY "VALUE-CHANGED" TO CMB_OMR IN FRAME {&FRAME-NAME}.                       
   END.
   IF NOT AVAILABLE omrtemp THEN DO:
      FIND FIRST omrtemp USE-INDEX OMR NO-LOCK NO-ERROR.
      IF AVAILABLE omrtemp THEN DO:
         ASSIGN CMB_OMR:SCREEN-VALUE IN FRAME {&FRAME-NAME} = omrtemp.NAMN.   
         APPLY "VALUE-CHANGED" TO CMB_OMR IN FRAME {&FRAME-NAME}.                       
      END.
   END.
  FIND FIRST aovardtemp WHERE aovardtemp.AONR = aonrvar AND 
  aovardtemp.DELNR = delnrvar USE-INDEX AONR NO-LOCK NO-ERROR.
  IF AVAILABLE aovardtemp THEN DO:
     ASSIGN
     FILL-IN_VARDNR = aovardtemp.VARDNR.
  END.
  ELSE DO: 
     RUN skapaovard IN kopaomapph (INPUT aonrvar,INPUT delnrvar).
     CREATE aovardtemp.
     ASSIGN 
     aovardtemp.AONR = aonrvar
     aovardtemp.DELNR = delnrvar.
     FILL-IN_VARDNR = ?.
  END.   
  ASSIGN
  FILL-IN_AONR = aonrvar
  FILL-IN_DELNR = delnrvar
  FILL-IN_ORT = ortben.          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hmtvard_UI DIALOG-1 
PROCEDURE hmtvard_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND FIRST varderingtemp WHERE varderingtemp.OMRADE = omr AND 
   varderingtemp.AONR = uppaonrtemp.AONR AND varderingtemp.DELNR = uppaonrtemp.DELNR
   USE-INDEX VARDNR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE varderingtemp THEN DO:
      FIND FIRST varderingtemp WHERE varderingtemp.OMRADE = omr AND 
      varderingtemp.AKTIV = TRUE 
      USE-INDEX VARDNR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE varderingtemp THEN DO:
         MESSAGE "Det finns inga markvärderingar på detta " + LC(Guru.Konstanter:gomrk) + "." VIEW-AS ALERT-BOX.
         RETURN.
      END.
      ELSE DO:
         OPEN QUERY BRW_VARDAONR FOR EACH varderingtemp WHERE varderingtemp.AKTIV = TRUE  AND
         varderingtemp.OMRADE = omr  AND varderingtemp.AONR = ?
         USE-INDEX VARDNR NO-LOCK.
         APPLY "HOME" TO BRW_VARDAONR IN FRAME {&FRAME-NAME}.
         status-ok = BRW_VARDAONR:SELECT-FOCUSED-ROW() NO-ERROR.         
         IF NOT AVAILABLE varderingtemp THEN DO:
            MESSAGE "Alla markvärderingar är kopplade." VIEW-AS ALERT-BOX.
            RETURN.
         END.
      END.  
   END.
   ELSE DO: 
      vardrec = varderingtemp.VARDERINGREC.
      IF varderingtemp.AKTIV = FALSE THEN DO:
         MESSAGE "Markvärderingen till detta " + LC(Guru.Konstanter:gaok) + " är inaktiv! Aktivera den först!"
         VIEW-AS ALERT-BOX.
         RETURN.               
      END.
      ELSE DO:
         OPEN QUERY BRW_VARDAONR FOR EACH varderingtemp WHERE varderingtemp.AKTIV = TRUE AND
         varderingtemp.OMRADE = omr AND varderingtemp.AONR = ?    
         USE-INDEX VARDNR NO-LOCK.                 
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(varderingtemp)).
         RUN lastselectdyn_UI IN brwproc[1].          
      END.
   END.   
   
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
   REPOSITION {&BROWSE-NAME} TO RECID browrec NO-ERROR.  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

