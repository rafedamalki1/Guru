&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER mtrl_rowid AS ROWID.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}

&Scoped-define SHARED SHARED 
{PARSTOLP.I} 
{LISTMTRL.I}
{KONID.I}

DEFINE SHARED VARIABLE skapapar AS LOGICAL NO-UNDO.
DEFINE VARIABLE valparapph AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE temp_mtrl 
   FIELD NUM AS INTEGER 
   FIELD KTYPKOD AS CHARACTER
   FIELD ENR AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD ENHET AS CHARACTER
   FIELD ANTAL AS INTEGER
   FIELD PRIS AS DECIMAL
   FIELD LEVKOD AS CHARACTER 
   FIELD LINKAB AS LOGICAL
   FIELD MODUL AS INTEGER
   FIELD SKAPNUM AS INTEGER
   FIELD TYPBER AS LOGICAL
   FIELD SKAPMTRL AS LOGICAL
   FIELD SKAPMODUL AS LOGICAL
   FIELD DIAMETER AS INTEGER
   FIELD MTRLTEXT AS CHARACTER
   FIELD SATS AS LOGICAL
   FIELD PAR AS INTEGER
   FIELD PAR2 AS CHARACTER
   INDEX ENR IS PRIMARY ENR ASCENDING
   INDEX NUM NUM ENR ASCENDING
   INDEX NUM2 NUM SKAPNUM ASCENDING.

/* DEFINE TEMP-TABLE temp_mtrl                */
/*    FIELD NUM LIKE BERVAL.NUM               */
/*    FIELD KTYPKOD LIKE KONSTRUKTION.KTYPKOD */
/*    FIELD ENR LIKE MTRLBER.ENR              */
/*    FIELD BENAMNING LIKE MTRLBER.BENAMNING  */
/*    FIELD ENHET LIKE MTRLBER.ENHET          */
/*    FIELD ANTAL LIKE MTRLBER.ANTAL          */
/*    FIELD PRIS LIKE MTRLBER.PRIS            */
/*    FIELD LEVKOD LIKE MTRLBER.LEVKOD        */
/*    FIELD LINKAB LIKE MTRLBER.LINKAB        */
/*    FIELD MODUL LIKE MTRLBER.MODUL          */
/*    FIELD SKAPNUM LIKE BERVAL.SKAPNUM       */
/*    FIELD TYPBER LIKE MTRLBER.TYPBER        */
/*    FIELD SKAPMTRL LIKE BERMTRL.SKAPMTRL    */
/*    FIELD SKAPMODUL LIKE BERMTRL.SKAPMODUL  */
/*    FIELD DIAMETER LIKE BERMTRL.DIAMETER    */
/*    FIELD MTRLTEXT LIKE BERMTRL.MTRLTEXT    */
/*    FIELD SATS LIKE BERMTRL.SATS            */
/*    FIELD PAR LIKE BERMTRL.PAR              */
/*    FIELD PAR2 LIKE PARSTOLP.A              */
/*    INDEX ENR IS PRIMARY ENR ASCENDING      */
/*    INDEX NUM NUM ENR ASCENDING             */
/*    INDEX NUM2 NUM SKAPNUM ASCENDING.       */
DEFINE BUFFER listbuff FOR list_mtrl.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_UP FILL-IN-ANTAL BTN_MIN BTN_UPA BTN_UPB ~
BTN_UPC FILL-IN-ANTALA FILL-IN-ANTALB FILL-IN-ANTALC BTN_MINA BTN_MINB ~
BTN_MINC BTN_OK BTN_AVB FILL-IN-ENR FILL-IN-BEN FILL-IN-ENHET FILL-IN-1 ~
FILL-IN-2 FILL-IN-3 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-ANTAL FILL-IN-ANTALA ~
FILL-IN-ANTALB FILL-IN-ANTALC FILL-IN-ENR FILL-IN-BEN FILL-IN-ENHET ~
FILL-IN-1 FILL-IN-2 FILL-IN-3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_MIN 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_MINA 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_MINB 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_MINC 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_UP 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_UPA 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_UPB 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_UPC 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(1)":U 
      VIEW-AS TEXT 
     SIZE 3.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(1)":U 
      VIEW-AS TEXT 
     SIZE 3.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(1)":U 
      VIEW-AS TEXT 
     SIZE 3.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ANTAL AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ANTALA AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ANTALB AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ANTALC AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
      VIEW-AS TEXT 
     SIZE 33 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENHET AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enhet" 
      VIEW-AS TEXT 
     SIZE 16 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enr" 
      VIEW-AS TEXT 
     SIZE 16 BY .75 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BTN_UP AT ROW 6.08 COL 32.63
     FILL-IN-ANTAL AT ROW 6.54 COL 21.88 COLON-ALIGNED NO-LABEL
     BTN_MIN AT ROW 7.21 COL 32.63
     BTN_UPA AT ROW 11.88 COL 16.75
     BTN_UPB AT ROW 11.88 COL 32.63
     BTN_UPC AT ROW 11.88 COL 48.5
     FILL-IN-ANTALA AT ROW 12.21 COL 6 COLON-ALIGNED NO-LABEL
     FILL-IN-ANTALB AT ROW 12.21 COL 21.88 COLON-ALIGNED NO-LABEL
     FILL-IN-ANTALC AT ROW 12.21 COL 37.75 COLON-ALIGNED NO-LABEL
     BTN_MINA AT ROW 12.88 COL 16.75
     BTN_MINB AT ROW 12.88 COL 32.63
     BTN_MINC AT ROW 12.88 COL 48.5
     BTN_OK AT ROW 14.63 COL 22.13
     BTN_AVB AT ROW 14.63 COL 37.13
     FILL-IN-ENR AT ROW 1.75 COL 10.5 COLON-ALIGNED
     FILL-IN-BEN AT ROW 3.25 COL 10.5 COLON-ALIGNED
     FILL-IN-ENHET AT ROW 4.88 COL 10.5 COLON-ALIGNED
     FILL-IN-1 AT ROW 10.46 COL 7 COLON-ALIGNED NO-LABEL
     FILL-IN-2 AT ROW 10.46 COL 23 COLON-ALIGNED NO-LABEL
     FILL-IN-3 AT ROW 10.46 COL 39 COLON-ALIGNED NO-LABEL
     SPACE(7.74) SKIP(4.45)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Fördela antal på parstolpar".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       FILL-IN-2:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       FILL-IN-3:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON END-ERROR OF FRAME Dialog-Frame /* Fördela antal på parstolpar */
DO:
  IF VALID-HANDLE(valparapph) THEN DELETE PROCEDURE valparapph NO-ERROR.
  RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON ENDKEY OF FRAME Dialog-Frame /* Fördela antal på parstolpar */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Fördela antal på parstolpar */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB Dialog-Frame
ON CHOOSE OF BTN_AVB IN FRAME Dialog-Frame /* Avbryt */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_MIN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MIN Dialog-Frame
ON CHOOSE OF BTN_MIN IN FRAME Dialog-Frame /* - */
DO: 
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL.
   IF FILL-IN-ANTAL >= 1 THEN DO:
      FILL-IN-ANTAL = FILL-IN-ANTAL - 1.
   END.   
   ELSE DO:
      MESSAGE "Antal kan inte vara mindre än 0!" VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.         
   DISPLAY FILL-IN-ANTAL WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_MINA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MINA Dialog-Frame
ON CHOOSE OF BTN_MINA IN FRAME Dialog-Frame /* - */
DO: 
   FILL-IN-ANTALA = INPUT FILL-IN-ANTALA.
   IF FILL-IN-ANTALA >= 1 THEN DO:
      FILL-IN-ANTALA = FILL-IN-ANTALA - 1.
   END.   
   ELSE DO:
      MESSAGE "Antal kan inte vara mindre än 0!" VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.         
   DISPLAY FILL-IN-ANTALA WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_MINB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MINB Dialog-Frame
ON CHOOSE OF BTN_MINB IN FRAME Dialog-Frame /* - */
DO: 
   FILL-IN-ANTALB = INPUT FILL-IN-ANTALB.
   IF FILL-IN-ANTALB >= 1 THEN DO:
      FILL-IN-ANTALB = FILL-IN-ANTALB - 1.
   END.   
   ELSE DO:
      MESSAGE "Antal kan inte vara mindre än 0!" VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.         
   DISPLAY FILL-IN-ANTALB WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_MINC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MINC Dialog-Frame
ON CHOOSE OF BTN_MINC IN FRAME Dialog-Frame /* - */
DO: 
   FILL-IN-ANTALC = INPUT FILL-IN-ANTALC.
   IF FILL-IN-ANTALC >= 1 THEN DO:
      FILL-IN-ANTALC = FILL-IN-ANTALC - 1.
   END.   
   ELSE DO:
      MESSAGE "Antal kan inte vara mindre än 0!" VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.         
   DISPLAY FILL-IN-ANTALC WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK Dialog-Frame
ON CHOOSE OF BTN_OK IN FRAME Dialog-Frame /* Ok */
DO:
   ASSIGN   
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL
   FILL-IN-ANTALA = INPUT FILL-IN-ANTALA
   FILL-IN-ANTALB = INPUT FILL-IN-ANTALB
   FILL-IN-ANTALC = INPUT FILL-IN-ANTALC.
   IF kon_id.A = TRUE THEN DO:
      FIND FIRST listbuff WHERE listbuff.NUM = temp_mtrl.NUM AND 
      listbuff.ENR = temp_mtrl.ENR AND listbuff.LEVKOD = temp_mtrl.LEVKOD AND
      listbuff.PAR = 1 NO-LOCK NO-ERROR.
      IF FILL-IN-ANTALA > 0 THEN DO:
         IF NOT AVAILABLE listbuff THEN DO:
            RUN skapa_UI.
         END.
         ASSIGN
         listbuff.ANTAL = FILL-IN-ANTALA
         listbuff.PAR2 = parstolptemp.A
         listbuff.PAR = 1.        
      END.
      ELSE DO:
         IF AVAILABLE listbuff THEN DELETE listbuff.
      END.
   END.  
   IF kon_id.B = TRUE THEN DO:
      FIND FIRST listbuff WHERE listbuff.NUM = temp_mtrl.NUM AND 
      listbuff.ENR = temp_mtrl.ENR AND listbuff.LEVKOD = temp_mtrl.LEVKOD AND
      listbuff.PAR = 2 NO-LOCK NO-ERROR.
      IF FILL-IN-ANTALB > 0 THEN DO:
         IF NOT AVAILABLE listbuff THEN DO:
            RUN skapa_UI.
         END.
         ASSIGN
         listbuff.ANTAL = FILL-IN-ANTALB
         listbuff.PAR2 = parstolptemp.B
         listbuff.PAR = 2.        
      END.
      ELSE DO:
         IF AVAILABLE listbuff THEN DELETE listbuff.
      END.
   END.
   IF kon_id.C = TRUE THEN DO:
      FIND FIRST listbuff WHERE listbuff.NUM = temp_mtrl.NUM AND 
      listbuff.ENR = temp_mtrl.ENR AND listbuff.LEVKOD = temp_mtrl.LEVKOD AND
      listbuff.PAR = 3 NO-LOCK NO-ERROR.
      IF FILL-IN-ANTALC > 0 THEN DO:
         IF NOT AVAILABLE listbuff THEN DO:
            RUN skapa_UI.
         END.
         ASSIGN
         listbuff.ANTAL = FILL-IN-ANTALC
         listbuff.PAR2 = parstolptemp.C
         listbuff.PAR = 3.        
      END.
      ELSE DO:
         IF AVAILABLE listbuff THEN DELETE listbuff.
      END.
   END.
   FIND FIRST listbuff WHERE listbuff.NUM = temp_mtrl.NUM AND 
   listbuff.ENR = temp_mtrl.ENR AND listbuff.LEVKOD = temp_mtrl.LEVKOD AND
   listbuff.PAR = 0 NO-LOCK NO-ERROR.
   IF FILL-IN-ANTAL > 0 THEN DO:
      IF NOT AVAILABLE listbuff THEN DO:
         RUN skapa_UI.
      END.
      ASSIGN
      listbuff.ANTAL = FILL-IN-ANTAL
      listbuff.PAR2 = ""
      listbuff.PAR = 0.        
   END.
   ELSE DO:
      IF AVAILABLE listbuff THEN DELETE listbuff.
   END. 
   APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UP Dialog-Frame
ON CHOOSE OF BTN_UP IN FRAME Dialog-Frame /* + */
DO: 
   ASSIGN
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL
   FILL-IN-ANTAL = FILL-IN-ANTAL + 1.
   DISPLAY FILL-IN-ANTAL WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPA Dialog-Frame
ON CHOOSE OF BTN_UPA IN FRAME Dialog-Frame /* + */
DO: 
   ASSIGN
   FILL-IN-ANTALA = INPUT FILL-IN-ANTALA
   FILL-IN-ANTALA = FILL-IN-ANTALA + 1.
   DISPLAY FILL-IN-ANTALA WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPB Dialog-Frame
ON CHOOSE OF BTN_UPB IN FRAME Dialog-Frame /* + */
DO: 
   ASSIGN
   FILL-IN-ANTALB = INPUT FILL-IN-ANTALB
   FILL-IN-ANTALB = FILL-IN-ANTALB + 1.
   DISPLAY FILL-IN-ANTALB WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPC Dialog-Frame
ON CHOOSE OF BTN_UPC IN FRAME Dialog-Frame /* + */
DO: 
   ASSIGN
   FILL-IN-ANTALC = INPUT FILL-IN-ANTALC
   FILL-IN-ANTALC = FILL-IN-ANTALC + 1.
   DISPLAY FILL-IN-ANTALC WITH FRAME {&FRAME-NAME}. 
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
   EMPTY TEMP-TABLE temp_mtrl NO-ERROR.    
   FIND list_mtrl WHERE ROWID(list_mtrl) = mtrl_rowid NO-ERROR.
   ASSIGN
   FILL-IN-BEN = list_mtrl.BENAMNING
   FILL-IN-ENR = list_mtrl.ENR         
   FILL-IN-ENHET = list_mtrl.ENHET.
   
   EMPTY TEMP-TABLE parstolptemp NO-ERROR. 
   RUN ladda_UI IN valparapph (OUTPUT TABLE parstolptemp).

   FIND FIRST parstolptemp NO-LOCK NO-ERROR.
   IF AVAILABLE parstolptemp THEN DO:
      ASSIGN      
      FILL-IN-1 = parstolptemp.A
      FILL-IN-2 = parstolptemp.B
      FILL-IN-3 = parstolptemp.C.
   END. 
   RUN temp_UI.  
   RUN enable_UI.       
   {FRMSIZED.I}   
   FIND FIRST listbuff WHERE listbuff.NUM = temp_mtrl.NUM AND 
   listbuff.ENR = temp_mtrl.ENR AND listbuff.LEVKOD = temp_mtrl.LEVKOD AND
   listbuff.PAR = 0 NO-LOCK NO-ERROR.
   IF AVAILABLE listbuff THEN FILL-IN-ANTAL = listbuff.ANTAL.
   ELSE FILL-IN-ANTAL = 0.
   DISPLAY FILL-IN-ANTAL WITH FRAME {&FRAME-NAME}.
   FIND FIRST kon_id WHERE kon_id.NUM = temp_mtrl.NUM USE-INDEX NUM
   NO-LOCK NO-ERROR.
   IF kon_id.A = FALSE THEN DO:
      ASSIGN
      FILL-IN-1:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-ANTALA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_UPA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_MINA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   ELSE DO:
      FIND FIRST listbuff WHERE listbuff.NUM = temp_mtrl.NUM AND 
      listbuff.ENR = temp_mtrl.ENR AND listbuff.LEVKOD = temp_mtrl.LEVKOD AND
      listbuff.PAR = 1 NO-LOCK NO-ERROR.
      IF AVAILABLE listbuff THEN FILL-IN-ANTALA = listbuff.ANTAL.
      ELSE FILL-IN-ANTALA = 0.
      DISPLAY FILL-IN-ANTALA WITH FRAME {&FRAME-NAME}.
   END.
   IF kon_id.B = FALSE THEN DO:
      ASSIGN
      FILL-IN-2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-ANTALB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_UPB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_MINB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   ELSE DO:
      FIND FIRST listbuff WHERE listbuff.NUM = temp_mtrl.NUM AND 
      listbuff.ENR = temp_mtrl.ENR AND listbuff.LEVKOD = temp_mtrl.LEVKOD AND
      listbuff.PAR = 2 NO-LOCK NO-ERROR.
      IF AVAILABLE listbuff THEN FILL-IN-ANTALB = listbuff.ANTAL.
      ELSE FILL-IN-ANTALB = 0.
      DISPLAY FILL-IN-ANTALB WITH FRAME {&FRAME-NAME}.
   END.
   IF kon_id.C = FALSE THEN DO:
      ASSIGN
      FILL-IN-3:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-ANTALC:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_UPC:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_MINC:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   ELSE DO:
      FIND FIRST listbuff WHERE listbuff.NUM = temp_mtrl.NUM AND 
      listbuff.ENR = temp_mtrl.ENR AND listbuff.LEVKOD = temp_mtrl.LEVKOD AND
      listbuff.PAR = 3 NO-LOCK NO-ERROR.
      IF AVAILABLE listbuff THEN FILL-IN-ANTALC = listbuff.ANTAL.
      ELSE FILL-IN-ANTALC = 0.
      DISPLAY FILL-IN-ANTALC WITH FRAME {&FRAME-NAME}.
   END.
   FILL-IN-ENR:LABEL = Guru.Konstanter:genk.
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
   IF Guru.Konstanter:appcon THEN DO:   
      RUN VALPARAPP.P PERSISTENT SET valparapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN VALPARAPP.P PERSISTENT SET valparapph.
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
  DISPLAY FILL-IN-ANTAL FILL-IN-ANTALA FILL-IN-ANTALB FILL-IN-ANTALC FILL-IN-ENR 
          FILL-IN-BEN FILL-IN-ENHET FILL-IN-1 FILL-IN-2 FILL-IN-3 
      WITH FRAME Dialog-Frame.
  ENABLE BTN_UP FILL-IN-ANTAL BTN_MIN BTN_UPA BTN_UPB BTN_UPC FILL-IN-ANTALA 
         FILL-IN-ANTALB FILL-IN-ANTALC BTN_MINA BTN_MINB BTN_MINC BTN_OK 
         BTN_AVB FILL-IN-ENR FILL-IN-BEN FILL-IN-ENHET FILL-IN-1 FILL-IN-2 
         FILL-IN-3 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skapa_UI Dialog-Frame 
PROCEDURE skapa_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   CREATE listbuff. 
   ASSIGN  
   listbuff.NUM = temp_mtrl.NUM  
   listbuff.SKAPNUM = temp_mtrl.SKAPNUM        
   listbuff.ENR = temp_mtrl.ENR
   listbuff.BENAMNING = temp_mtrl.BENAMNING
   listbuff.PRIS = temp_mtrl.PRIS           
   listbuff.LEVKOD = temp_mtrl.LEVKOD            
   listbuff.ENHET = temp_mtrl.ENHET
   listbuff.LINKAB = temp_mtrl.LINKAB
   listbuff.MODUL = temp_mtrl.MODUL
   listbuff.TYPBER = temp_mtrl.TYPBER
   listbuff.DIAMETER = temp_mtrl.DIAMETER
   listbuff.SATS = temp_mtrl.SATS
   listbuff.MTRLTEXT = temp_mtrl.MTRLTEXT
   skapapar = TRUE.                
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE temp_UI Dialog-Frame 
PROCEDURE temp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   CREATE temp_mtrl. 
   ASSIGN  
   temp_mtrl.NUM = list_mtrl.NUM  
   temp_mtrl.SKAPNUM = list_mtrl.SKAPNUM        
   temp_mtrl.ENR = list_mtrl.ENR
   temp_mtrl.BENAMNING = list_mtrl.BENAMNING
   temp_mtrl.PRIS = list_mtrl.PRIS           
   temp_mtrl.LEVKOD = list_mtrl.LEVKOD            
   temp_mtrl.ENHET = list_mtrl.ENHET
   temp_mtrl.LINKAB = list_mtrl.LINKAB
   temp_mtrl.MODUL = list_mtrl.MODUL
   temp_mtrl.TYPBER = list_mtrl.TYPBER
   temp_mtrl.DIAMETER = list_mtrl.DIAMETER
   temp_mtrl.SATS = list_mtrl.SATS.             
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

