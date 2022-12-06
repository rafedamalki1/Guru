&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
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

  Created: 95/05/03 -  9:26 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER aonrvar AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER aonrrec AS RECID NO-UNDO.
/* Local Variable Definitions ---                                       */
{AVDTEMP.I}
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{OMRTEMPW.I}

DEFINE TEMP-TABLE hmtomr NO-UNDO
  FIELD OMRADE AS CHARACTER
  FIELD NRAONR AS CHARACTER
  INDEX OMR OMRADE
  INDEX NRAONR NRAONR.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE VARIABLE orgaonr AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE aoomradenr AS LOGICAL NO-UNDO.
DEFINE VARIABLE nrserierec AS RECID NO-UNDO.
DEFINE VARIABLE nraoapph AS HANDLE NO-UNDO.
DEFINE VARIABLE org AS CHARACTER NO-UNDO.
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.
DEFINE VARIABLE nyttaoapph AS HANDLE NO-UNDO.                     /* NYTTAOAPP.P */
DEFINE VARIABLE jp AS CHARACTER NO-UNDO.
DEFINE VARIABLE aook AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-2

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN_AONR BTN_OK BTN_AVB RAD_FAST CMB_OMR 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_AONR FILL-IN_DELNR RAD_FAST ~
CMB_OMR 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Utförande omr" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_AONR AS CHARACTER FORMAT "X(7)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE FILL-IN_DELNR AS INTEGER FORMAT ">99" INITIAL 0 
     LABEL "Delnr" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE {&NEW} SHARED VARIABLE RAD_FAST AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga aonr", no,
"Fasta aonr", yes
     SIZE 37.5 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-2
     FILL-IN_AONR AT ROW 3.92 COL 14.5 COLON-ALIGNED
     FILL-IN_DELNR AT ROW 3.92 COL 33 COLON-ALIGNED
     BTN_OK AT ROW 5.5 COL 23.5
     BTN_AVB AT ROW 5.5 COL 38.5
     RAD_FAST AT ROW 1.5 COL 1.5 NO-LABEL
     CMB_OMR AT ROW 2.63 COL 14.5 COLON-ALIGNED
     SPACE(14.12) SKIP(3.44)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ny arbetsorder":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-2
   NOT-VISIBLE FRAME-NAME Custom                                        */
ASSIGN 
       FRAME DIALOG-2:SCROLLABLE       = FALSE
       FRAME DIALOG-2:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_DELNR IN FRAME DIALOG-2
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET RAD_FAST IN FRAME DIALOG-2
   SHARED                                                               */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-2
/* Query rebuild information for DIALOG-BOX DIALOG-2
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-2 DIALOG-2
ON END-ERROR OF FRAME DIALOG-2 /* Ny arbetsorder */
DO:
   musz = TRUE.
   IF VALID-HANDLE(nraoapph) THEN DO:
      FIND FIRST hmtomr WHERE USE-INDEX NRAONR NO-LOCK NO-ERROR.
      IF AVAILABLE hmtomr THEN RUN sistproj_UI IN nraoapph (INPUT hmtomr.NRAONR).
      ELSE RUN sistproj_UI IN nraoapph (INPUT "").
      DELETE PROCEDURE nraoapph.
   END.
   IF VALID-HANDLE(nyttaoapph) THEN DO:
      RUN borthandle_UI IN nyttaoapph.
      DELETE PROCEDURE nyttaoapph NO-ERROR.
      nyttaoapph = ?.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-2 DIALOG-2
ON ENDKEY OF FRAME DIALOG-2 /* Ny arbetsorder */
DO:
   musz = TRUE.
    IF VALID-HANDLE(nraoapph) THEN DO:
      FIND FIRST hmtomr WHERE USE-INDEX NRAONR NO-LOCK NO-ERROR.
      IF AVAILABLE hmtomr THEN RUN sistproj_UI IN nraoapph (INPUT hmtomr.NRAONR).
      ELSE RUN sistproj_UI IN nraoapph (INPUT "").
      DELETE PROCEDURE nraoapph.
   END.
   IF VALID-HANDLE(nyttaoapph) THEN DO:
      RUN borthandle_UI IN nyttaoapph.
      DELETE PROCEDURE nyttaoapph NO-ERROR.
      nyttaoapph = ?.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-2
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-2 /* Avbryt */
DO:   
   APPLY "END-ERROR":U TO SELF.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-2
ON CHOOSE OF BTN_OK IN FRAME DIALOG-2 /* Ok */
DO:
   
   ASSIGN   
   CMB_OMR = INPUT CMB_OMR     
   FILL-IN_AONR = INPUT FILL-IN_AONR    
   FILL-IN_DELNR = INPUT FILL-IN_DELNR. 
   orgaonr = FILL-IN_AONR.
   IF FILL-IN_AONR = ? THEN DO:
      MESSAGE Guru.Konstanter:gaok " kan inte vara okänt!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.   
   IF FILL-IN_AONR = "" THEN DO:
      MESSAGE Guru.Konstanter:gaol " kan inte vara blankt!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY. 
   END.
  IF Guru.Konstanter:globforetag = "LULE" THEN DO:     
     RUN jpkontroll IN nraoapph (INPUT org,OUTPUT jp).
     IF jp = "03" THEN DO:
        IF LENGTH(FILL-IN_AONR) < 6 THEN DO:
           MESSAGE Guru.Konstanter:gaol + " måste innehålla 6 tecken." VIEW-AS ALERT-BOX.
           RETURN NO-APPLY.      
        END.
     END.
     ELSE IF jp = "01" THEN DO:
        IF LENGTH(FILL-IN_AONR) NE 3 THEN DO:
           MESSAGE Guru.Konstanter:gaol + " måste innehålla 3 tecken." VIEW-AS ALERT-BOX.
           RETURN NO-APPLY.      
        END.
     END.
      
   END.
   FILL-IN_AONR = TRIM(FILL-IN_AONR).
   DISPLAY FILL-IN_AONR WITH FRAME {&FRAME-NAME}.
   RUN AONRNAMNKOLL.P (INPUT 1, INPUT FILL-IN_AONR,OUTPUT musz).
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.  
   RUN AONRNAMNKOLL.P (INPUT 2, INPUT FILL-IN_AONR,OUTPUT musz).
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.
   IF Guru.Konstanter:varforetypval[8] NE 0 THEN DO:   
      IF Guru.Konstanter:globforetag = "BORL" THEN DO:      
         IF LENGTH(FILL-IN_AONR) NE Guru.Konstanter:varforetypval[8] THEN DO:
            MESSAGE "Skall innehålla " Guru.Konstanter:varforetypval[8] " tecken!" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
         END.      
         org = SUBSTRING(FILL-IN_AONR,2,2).
         FIND FIRST omrtemp WHERE omrtemp.OMRADE = org USE-INDEX OMR NO-LOCK NO-ERROR.  
         IF NOT AVAILABLE omrtemp THEN DO:
            MESSAGE Guru.Konstanter:gomrl + " " + org + " finns ej!" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
         END.
         orgaonr = FILL-IN_AONR.
      END.
      ELSE DO:
         IF LENGTH(FILL-IN_AONR) > Guru.Konstanter:varforetypval[8] THEN DO:
            MESSAGE "Skall innehålla max " Guru.Konstanter:varforetypval[8] " tecken!" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
         END.      
      END.    
      RUN kontroll IN nraoapph (INPUT FILL-IN_AONR,
                                INPUT FILL-IN_DELNR,OUTPUT musz).
      IF musz = TRUE THEN DO:      
         MESSAGE Guru.Konstanter:gaol + " finns redan upplagt." VIEW-AS ALERT-BOX.
         FILL-IN_AONR = orgaonr.
         musz = FALSE.
         RETURN NO-APPLY.      
      END.
      
      IF Guru.Konstanter:globforetag = "krin" THEN DO:
         IF FILL-IN_DELNR NE 1 THEN DO:
            RUN kontrolldelnrkrin IN nraoapph (INPUT FILL-IN_AONR,
                                        INPUT FILL-IN_DELNR,OUTPUT musz).
            IF musz = TRUE THEN DO:      
               MESSAGE "Du lägger upp ett nytt "  Guru.Konstanter:gaol " med delnr " FILL-IN_DELNR  " Då måste delnr 01 finnas!"
               VIEW-AS ALERT-BOX.
               FILL-IN_AONR = orgaonr.
               musz = FALSE.
               RETURN NO-APPLY.      
            END.
         END.   
      END.
      ELSE IF FILL-IN_DELNR NE 0 THEN DO:
         RUN kontrolldelnr IN nraoapph (INPUT FILL-IN_AONR,
                                     INPUT FILL-IN_DELNR,OUTPUT musz).
         IF musz = TRUE THEN DO:      
            MESSAGE "Du lägger upp ett nytt "  Guru.Konstanter:gaol " med delnr " FILL-IN_DELNR  " Då måste delnr 00 finnas!"
            VIEW-AS ALERT-BOX.
            FILL-IN_AONR = orgaonr.
            musz = FALSE.
            RETURN NO-APPLY.      
         END.
      END.
      &Scoped-define FORMATNAMN orgaonr
      {AOFORMAT2.I} 
      
   END.
   ELSE DO:
      IF LENGTH(FILL-IN_AONR) > 6 THEN DO:
         MESSAGE "Skall innehålla max 6 tecken!" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
      RUN kontroll IN nraoapph (INPUT FILL-IN_AONR,
                                INPUT FILL-IN_DELNR,OUTPUT musz).
      IF musz = TRUE THEN DO:      
         MESSAGE Guru.Konstanter:gaol + " finns redan upplagt." VIEW-AS ALERT-BOX.
         musz = FALSE.
         RETURN NO-APPLY.      
      END.
      IF Guru.Konstanter:globforetag = "krin" THEN DO:
         IF FILL-IN_DELNR NE 1 THEN DO:
            RUN kontrolldelnrkrin IN nraoapph (INPUT FILL-IN_AONR,
                                        INPUT FILL-IN_DELNR,OUTPUT musz).
            IF musz = TRUE THEN DO:      
               MESSAGE "Du lägger upp ett nytt "  Guru.Konstanter:gaol " med delnr " FILL-IN_DELNR  " Då måste delnr 01 finnas!"
               VIEW-AS ALERT-BOX.
               FILL-IN_AONR = orgaonr.
               musz = FALSE.
               RETURN NO-APPLY.      
            END.
         END.   
      END.
      ELSE IF FILL-IN_DELNR NE 0 THEN DO:
         RUN kontrolldelnr IN nraoapph (INPUT FILL-IN_AONR,
                                     INPUT FILL-IN_DELNR,OUTPUT musz).
         IF musz = TRUE THEN DO:      
            MESSAGE "Du lägger upp ett nytt "  Guru.Konstanter:gaol " med delnr " FILL-IN_DELNR  " Då måste delnr 00 finnas!"
            VIEW-AS ALERT-BOX.
            FILL-IN_AONR = orgaonr.
            musz = FALSE.
            RETURN NO-APPLY.      
         END.
      END.
      &Scoped-define FORMATNAMN fill-in_aonr
      {AOFORMAT2E.I}
     
   END.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      FILL-IN_AONR = orgaonr.
      RETURN NO-APPLY.              
   END. 
   RUN kontroll IN nraoapph (INPUT FILL-IN_AONR,
                             INPUT FILL-IN_DELNR,OUTPUT musz).
   IF musz = TRUE THEN DO:      
      MESSAGE Guru.Konstanter:gaol + " finns redan upplagt." 
      VIEW-AS ALERT-BOX.
      musz = FALSE.
      RETURN NO-APPLY.
   END.
   FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR NO-LOCK NO-ERROR.
   RUN aonrsekkoll_UI IN nraoapph (INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:varforetypchar[4],INPUT omrtemp.OMRADE,OUTPUT musz).
   IF musz = TRUE THEN DO:      
      MESSAGE "Du är inte behörig att lägga upp " Guru.Konstanter:gaok  " på detta " Guru.Konstanter:gomrl  
      VIEW-AS ALERT-BOX.
      musz = FALSE.
      RETURN NO-APPLY.
   END.
   /*mumma*/
   RUN btnok IN nraoapph (INPUT Guru.Konstanter:globforetag,INPUT FILL-IN_AONR,
                          INPUT FILL-IN_DELNR,INPUT RAD_FAST,
                          INPUT org,INPUT nrserierec,INPUT aoomradenr,
                          OUTPUT aonrrec).

   
   FRAME DIALOG-2:HIDDEN = TRUE.
   IF VALID-HANDLE(nraoapph) THEN DELETE PROCEDURE nraoapph.
   IF VALID-HANDLE(nyttaoapph) THEN DO:
      RUN borthandle_UI IN nyttaoapph.
      DELETE PROCEDURE nyttaoapph NO-ERROR.
      nyttaoapph = ?.
   END.
   RETURN. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-2
ON ENTRY OF BTN_OK IN FRAME DIALOG-2 /* Ok */
DO:
    FILL-IN_AONR = INPUT FILL-IN_AONR. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OMR DIALOG-2
ON VALUE-CHANGED OF CMB_OMR IN FRAME DIALOG-2 /* Utförande omr */
DO:
   CMB_OMR = INPUT CMB_OMR.
   
   FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR NO-LOCK NO-ERROR.
   IF AVAILABLE omrtemp THEN DO:
      ASSIGN org = omrtemp.OMRADE.
   END.
   FIND FIRST hmtomr WHERE hmtomr.OMRADE = org NO-LOCK NO-ERROR.
   IF AVAILABLE hmtomr THEN DO:
      FILL-IN_AONR = hmtomr.NRAONR.
   END.
   ELSE DO:
      RUN nummer_UI.
      CREATE hmtomr.
      hmtomr.OMRADE = org.
      hmtomr.NRAONR = FILL-IN_AONR.
   END.      
   DISPLAY FILL-IN_AONR FILL-IN_DELNR  WITH FRAME {&FRAME-NAME}.
   IF Guru.Konstanter:globforetag = "GKAL" THEN DO:   
      RUN kalmanv_UI IN nyttaoapph (INPUT Guru.Konstanter:globanv,OUTPUT aook).
      IF aook = TRUE THEN.
      ELSE IF FILL-IN_AONR = ""  THEN /*ENABLE FILL-IN_AONR FILL-IN_DELNR WITH FRAME {&FRAME-NAME}*/.
      ELSE DISABLE FILL-IN_AONR FILL-IN_DELNR WITH FRAME {&FRAME-NAME}.
   END.
   ELSE IF Guru.Konstanter:globforetag = "PENO" THEN DO:
      ENABLE FILL-IN_AONR WITH FRAME {&FRAME-NAME}.
   END.
   ELSE DO:
      IF FILL-IN_AONR = ""  THEN ENABLE FILL-IN_AONR FILL-IN_DELNR WITH FRAME {&FRAME-NAME}.
      ELSE DISABLE FILL-IN_AONR FILL-IN_DELNR WITH FRAME {&FRAME-NAME}.
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AONR DIALOG-2
ON ENTRY OF FILL-IN_AONR IN FRAME DIALOG-2 /* Aonr */
DO:
   {muswait.i}
   FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR NO-LOCK NO-ERROR.
   IF AVAILABLE omrtemp THEN DO:
      ASSIGN org = omrtemp.OMRADE.
   END.
   FIND FIRST hmtomr WHERE hmtomr.OMRADE = org NO-LOCK NO-ERROR.
   IF AVAILABLE hmtomr THEN DO:
      FILL-IN_AONR = hmtomr.NRAONR.
   END.
   ELSE DO:
      RUN nummer_UI.
      CREATE hmtomr.
      hmtomr.OMRADE = org.
      hmtomr.NRAONR = FILL-IN_AONR.
   END. 
   DISPLAY FILL-IN_AONR FILL-IN_DELNR  WITH FRAME {&FRAME-NAME}.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AONR DIALOG-2
ON LEAVE OF FILL-IN_AONR IN FRAME DIALOG-2 /* Aonr */
DO:
   ASSIGN FILL-IN_AONR = INPUT FILL-IN_AONR. 
   FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR NO-LOCK NO-ERROR.
   IF AVAILABLE omrtemp THEN DO:
      ASSIGN org = omrtemp.OMRADE.
   END.
   
   FIND FIRST omrtemp WHERE omrtemp.OMRADE = org USE-INDEX OMR NO-LOCK NO-ERROR.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_FAST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_FAST DIALOG-2
ON VALUE-CHANGED OF RAD_FAST IN FRAME DIALOG-2
DO:
   RAD_FAST = INPUT RAD_FAST.
   IF RAD_FAST = TRUE THEN DO:   
      IF Guru.Konstanter:globforetag = "LULE" THEN ENABLE FILL-IN_AONR FILL-IN_DELNR WITH FRAME {&FRAME-NAME}.
      IF Guru.Konstanter:globforetag = "elpa" THEN ENABLE FILL-IN_AONR FILL-IN_DELNR WITH FRAME {&FRAME-NAME}.
   END.
   ELSE DO:   
      IF Guru.Konstanter:globforetag = "LULE" THEN DISABLE FILL-IN_AONR FILL-IN_DELNR WITH FRAME {&FRAME-NAME}.
      IF Guru.Konstanter:globforetag = "elpa" THEN DISABLE FILL-IN_AONR FILL-IN_DELNR WITH FRAME {&FRAME-NAME}.
   END.
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
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK TRANSACTION:   
   {DIA_M_START.I}
   {muswait.i}  
   {ALLSTARTDYN.I}
   {TILLFAST.I}
   RUN anvavdhmt IN nyttaoapph (INPUT Guru.Konstanter:globanv,INPUT "",INPUT "",OUTPUT jid, OUTPUT TABLE eavdtemp ).                       
   FOR EACH eavdtemp,         
   EACH omrtemp WHERE omrtemp.AVDELNINGNR = eavdtemp.AVDELNINGNR.
      status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).                        
   END.            
   ASSIGN
   CMB_OMR:LABEL = Guru.Konstanter:gutfk + " " + LC(Guru.Konstanter:gomrk) 
   FILL-IN_AONR:LABEL = Guru.Konstanter:gaok     
   FRAME {&FRAME-NAME}:TITLE = "Nytt " + LC(Guru.Konstanter:gaok).   
   &Scoped-define FORMATNAMN FILL-IN_AONR   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN_DELNR   
   {DELNRFORMAT.I}
   FIND FIRST omrtemp WHERE omrtemp.OMRADE = Guru.Konstanter:globomr 
   USE-INDEX OMR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE omrtemp THEN DO:
      FIND FIRST omrtemp 
      USE-INDEX OMR NO-LOCK NO-ERROR.
   END.
   ASSIGN  CMB_OMR:SCREEN-VALUE = omrtemp.NAMN.
   org = omrtemp.OMRADE.
   RAD_FAST = FALSE.
   APPLY "VALUE-CHANGED" TO CMB_OMR IN FRAME {&FRAME-NAME}.
   RUN enable_UI.       
   
    /* VISA RAD_FAST ELLER EJ  mumma*/
   IF Guru.Konstanter:varforetypval[33] = 0 THEN DO:
      RAD_FAST = FALSE.
      RAD_FAST:HIDDEN = TRUE.
   END.


   /*{FRMSIZED.I}              */
   ASSIGN
   CMB_OMR = INPUT CMB_OMR. 
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      IF felmeddtemp.VAL = 1 THEN DO:
         DISABLE CMB_OMR WITH FRAME {&FRAME-NAME}.         
      END.
      DELETE felmeddtemp.
   END.   
   IF Guru.Konstanter:globforetag = "BORL" THEN CMB_OMR:HIDDEN = TRUE.
   {musarrow.i}
   {DIA_M_SLUT.I}
 
   /*
   iF Guru.GlobalaVariabler:fonstercol > 0 THEN CURRENT-WINDOW:X = Guru.GlobalaVariabler:fonstercol.
IF Guru.GlobalaVariabler:fonsterrow > 0 THEN CURRENT-WINDOW:Y = Guru.GlobalaVariabler:fonsterrow.
   
   */
   
   
   APPLY "ENTRY" TO FILL-IN_AONR.   
   ENABLE FILL-IN_DELNR WITH FRAME {&FRAME-NAME}.
   IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
      RUN kalmanv_UI IN nyttaoapph (INPUT Guru.Konstanter:globanv,OUTPUT aook).
      IF aook = TRUE THEN.
      ELSE IF FILL-IN_AONR = ""  THEN.
      ELSE DISABLE FILL-IN_AONR FILL-IN_DELNR WITH FRAME {&FRAME-NAME}.
   END.
   IF Guru.Konstanter:globforetag = "KRIN" THEN FILL-IN_DELNR = 01.
   IF Guru.Konstanter:globforetag = "LULE" THEN DISABLE FILL-IN_AONR FILL-IN_DELNR WITH FRAME {&FRAME-NAME}.   
   APPLY "VALUE-CHANGED" TO CMB_OMR.
   APPLY "ENTRY" TO BTN_OK.
   IF INPUT FILL-IN_AONR = "" THEN  APPLY "ENTRY" TO FILL-IN_AONR.     
   
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
   IF Guru.Konstanter:appcon THEN DO:
      RUN NRAONRAPP.P PERSISTENT SET nraoapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NRAONRAPP.P PERSISTENT SET nraoapph.
   END.
   IF Guru.Konstanter:appcon THEN DO:      
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph.
   END.   
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
  DISPLAY FILL-IN_AONR FILL-IN_DELNR RAD_FAST CMB_OMR 
      WITH FRAME DIALOG-2.
  ENABLE FILL-IN_AONR BTN_OK BTN_AVB RAD_FAST CMB_OMR 
      WITH FRAME DIALOG-2.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-2}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nummer_UI DIALOG-2 
PROCEDURE nummer_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*AONR FRÅN NUMMERSERIER*/
   DEFINE VARIABLE globforetag AS CHARACTER NO-UNDO.
   globforetag = Guru.Konstanter:globforetag.
   RUN nummer IN nraoapph (INPUT 1,INPUT-OUTPUT aonrvar,INPUT-OUTPUT RAD_FAST,INPUT-OUTPUT CMB_OMR,
                           INPUT-OUTPUT globforetag,INPUT-OUTPUT org,
                           INPUT-OUTPUT FILL-IN_AONR,INPUT-OUTPUT FILL-IN_DELNR,
                           INPUT-OUTPUT nrserierec,INPUT-OUTPUT aoomradenr,OUTPUT musz).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valomr_UI DIALOG-2 
PROCEDURE valomr_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF SUBSTRING(Guru.Konstanter:globomr,1,1) = "4" THEN DO:
      IF SUBSTRING(omrtemp.OMRADE,1,1) = "4" THEN DO:  
         status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN) IN FRAME {&FRAME-NAME}.
      END.
   END.
   ELSE status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

