&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
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

franvart = varifran = 1 NY/ELLER ÄNDRAD DIREKT FRÅN BEREDNINGSMENYN import av beredning
franvart = varifran = 3 NY BERE FRÅN AONRHUVUD
franvart = varifran = 2 ÄNDRAD BERE FRÅN AONRHUVUD 
vart = NYA eller AND DVS NY BEREDNING ELLER ÄNDRA HUVUDET PÅ BEFINTLIG
lnamnex ÄR LEVERANTÖR VALD FRÅM IMPORT
FRÅN 
EXPIMPBERDS.W
   FRÅN BERMENY.P
   IMPORT FRÅN ANNAN BEREDNING
   vart = "NYA".
   lnamnex = LEVERANTÖRENS NAMN I KLARTEXT 
   RUN NYBEREU.W (INPUT 1,INPUT lnamnex ).
   IMPORT FRÅN AVCAD TYPDATOR = 3 = FRÅN AVCAD
   RUN NYBEREU.W (INPUT 1,INPUT "").
BERMENY.P
  RUN NYBEREU.W (INPUT franvart,INPUT "").
     andra_UI
        vart = "AND".
        RUN NYBEREU.W (INPUT franvart,INPUT "").
     btnkopi_UI
        vart = "NYA".  
        RUN NYBEREU.W (INPUT franvart,INPUT "").
     ny_UI      
        vart = "NYA".  
        RUN NYBEREU.W (INPUT franvart,INPUT "").  
  AONRM.P
      NYA
      franvart = 3
      RUN menyberval_UI IN Guru.SharedVariable:btnberh (INPUT bervart,INPUT 3, INPUT aonrvar, INPUT delnrvar, INPUT TABLE berintemp,INPUT-OUTPUT inkopvart). 
          ny_UI      
          vart = "NYA".  
          RUN NYBEREU.W (INPUT franvart,INPUT "").
      ÄNDRA    
          franvart = 2    
          RUN menyberval_UI IN Guru.SharedVariable:btnberh (INPUT bervart,INPUT 2, INPUT aonrvar, INPUT delnrvar, INPUT TABLE berintemp,INPUT-OUTPUT inkopvart).
          andra_UI   
          vart = "AND".
          andra_UI  
          RUN NYBEREU.W (INPUT franvart,INPUT "").
      IMPORT
      vart = "NYA". 
      VIA BERMENY.P  EXPIMPBERDS.W   
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER varifran AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER lnamnex AS CHARACTER  NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{HOPALLA.I}
{OMRTEMPW.I}
{ANVTEMPS.I}
DEFINE SHARED TEMP-TABLE berkalanvandartemp NO-UNDO LIKE anvandartemp.
&SCOPED-DEFINE SHARED SHARED
{BERTEMPTAB.I}
DEFINE TEMP-TABLE extravalbertemp NO-UNDO LIKE valberedningtemp.
&Scoped-define NEW NEW  
{BEREDNINGTEMP.I}
{LEVTEMP.I}
{HUVLEVTEMP.I}
DEFINE SHARED VARIABLE avbryt AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE regvnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE kalkrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valbernr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE katvar AS INTEGER NO-UNDO.
DEFINE VARIABLE ejkalk AS LOGICAL NO-UNDO.
DEFINE VARIABLE nyberapph AS HANDLE NO-UNDO.
DEFINE VARIABLE varkalknr AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE VARIABLE omrnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE klknr AS INTEGER NO-UNDO.
DEFINE VARIABLE klktext AS CHARACTER NO-UNDO.
DEFINE VARIABLE anv AS CHARACTER NO-UNDO.
DEFINE VARIABLE togklk AS LOGICAL NO-UNDO. 
DEFINE VARIABLE cmbomr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vald_kundlev AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE batemp NO-UNDO   
   FIELD ANVANDARE AS CHARACTER 
   FIELD ANVNAMN AS CHARACTER.
DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
DEFINE VARIABLE AppServerHandle AS HANDLE NO-UNDO.   
DEFINE VARIABLE UtbytlistorDS AS HANDLE NO-UNDO.
DEFINE VARIABLE UtbytNamnbuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE qh AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CMB_OMR FILL-IN_KALKTEXT CMB_UTF CMB_ERS ~
CMB_LEV TOG_KALK BTN_REG BTN_AVS FILL-IN-R1 
&Scoped-Define DISPLAYED-OBJECTS CMB_OMR FILL-IN_KALKNR FILL-IN_KALKTEXT ~
CMB_UTF FILL-IN_ANVANDARE CMB_ERS CMB_LEV TOG_KALK FILL-IN-R1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_REG 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_ERS AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18.25 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_LEV AS CHARACTER FORMAT "X(40)":U 
     LABEL "Välj Leverantör" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18.25 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR AS CHARACTER FORMAT "X(16)":U 
     LABEL "Utförande omr" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_UTF AS CHARACTER FORMAT "X(256)":U 
     LABEL "Välj utfärdare" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-R1 AS CHARACTER FORMAT "X(256)":U INITIAL "Välj Ersättningslista för Kund och Konstruktioner" 
      VIEW-AS TEXT 
     SIZE 29.5 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN_ANVANDARE AS CHARACTER FORMAT "X(256)" 
     LABEL "Utfärdare" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1.

DEFINE VARIABLE FILL-IN_KALKNR AS INTEGER FORMAT "->>>>>>9" INITIAL 0 
     LABEL "Beredningsnr" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_KALKTEXT AS CHARACTER FORMAT "x(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1.

DEFINE VARIABLE TOG_KALK AS LOGICAL INITIAL no 
     LABEL "P3-kalkyl" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY .88 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     CMB_OMR AT ROW 1.75 COL 32.5 COLON-ALIGNED
     FILL-IN_KALKNR AT ROW 3.17 COL 32.5 COLON-ALIGNED
     FILL-IN_KALKTEXT AT ROW 4.63 COL 32.5 COLON-ALIGNED
     CMB_UTF AT ROW 6 COL 18.5
     FILL-IN_ANVANDARE AT ROW 7.5 COL 32.5 COLON-ALIGNED
     CMB_ERS AT ROW 9 COL 32.5 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     CMB_LEV AT ROW 10.42 COL 32.5 COLON-ALIGNED
     TOG_KALK AT ROW 11.75 COL 34.5
     BTN_REG AT ROW 13.25 COL 27.38
     BTN_AVS AT ROW 13.25 COL 42.38
     FILL-IN-R1 AT ROW 9.25 COL 2 NO-LABEL WIDGET-ID 4
     SPACE(48.12) SKIP(4.65)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ny beredning":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

ASSIGN 
       CMB_ERS:HIDDEN IN FRAME DIALOG-1           = TRUE.

ASSIGN 
       CMB_LEV:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_UTF IN FRAME DIALOG-1
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-R1 IN FRAME DIALOG-1
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-R1:READ-ONLY IN FRAME DIALOG-1        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_ANVANDARE IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_KALKNR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Ny beredning */
DO:

   IF VALID-HANDLE(nyberapph) THEN DELETE PROCEDURE nyberapph NO-ERROR.
   IF VALID-HANDLE(laddaproch) THEN DELETE PROCEDURE laddaproch NO-ERROR.
   avbryt = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Ny beredning */
DO:
   APPLY "END-ERROR":U TO SELF.
   IF VALID-HANDLE(AppServerHandle) THEN DO:
      RUN AvsUtb_UI IN AppServerHandle.
      DELETE PROCEDURE AppServerHandle.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS DIALOG-1
ON CHOOSE OF BTN_AVS IN FRAME DIALOG-1 /* Avbryt */
DO:
   musz = TRUE.
   FOR EACH eomrtemp WHERE NO-LOCK:
      FIND FIRST omrtemp WHERE omrtemp.OMRADE = eomrtemp.OMRADE NO-LOCK NO-ERROR.
      IF AVAILABLE omrtemp THEN DELETE omrtemp.
   END.
   IF VALID-HANDLE(AppServerHandle) THEN DO:
      RUN AvsUtb_UI IN AppServerHandle.
      DELETE PROCEDURE AppServerHandle.
   END.
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_REG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON CHOOSE OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:
   FILL-IN_KALKTEXT = INPUT FILL-IN_KALKTEXT.
   CMB_ERS = INPUT CMB_ERS.
   
   IF FILL-IN_KALKTEXT = "" THEN DO:
      MESSAGE "Benämningen får ej vara blank!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF Guru.Konstanter:globforetag = "gran" AND CMB_ERS = "Ej vald!" THEN DO:
      IF lnamnex = "" THEN DO:
         MESSAGE "Du måste välja ersättningslista!" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.   
   END.      
   RUN reg_UI.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON GO OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:
  IF VALID-HANDLE(AppServerHandle) THEN DO:
      RUN AvsUtb_UI IN AppServerHandle.
      DELETE PROCEDURE AppServerHandle.
   END.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_ERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_ERS DIALOG-1
ON VALUE-CHANGED OF CMB_ERS IN FRAME DIALOG-1
DO:                                 
   ASSIGN           
   CMB_ERS = INPUT CMB_ERS.
                         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEV DIALOG-1
ON VALUE-CHANGED OF CMB_LEV IN FRAME DIALOG-1 /* Välj Leverantör */
DO:                                 
   ASSIGN           
   CMB_LEV = INPUT CMB_LEV.  
   FIND FIRST levtemp WHERE levtemp.LEVNAMN = CMB_LEV 
   USE-INDEX LEV NO-LOCK NO-ERROR.
   vald_lev = levtemp.LEVKOD.                         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OMR DIALOG-1
ON LEAVE OF CMB_OMR IN FRAME DIALOG-1 /* Utförande omr */
DO:
   CMB_OMR = INPUT CMB_OMR.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OMR DIALOG-1
ON VALUE-CHANGED OF CMB_OMR IN FRAME DIALOG-1 /* Utförande omr */
DO:
   CMB_OMR = INPUT CMB_OMR.  
   RUN omr_UI IN nyberapph (INPUT CMB_OMR,OUTPUT varkalknr).
   FILL-IN_KALKNR = varkalknr.
   DISPLAY FILL-IN_KALKNR WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_UTF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_UTF DIALOG-1
ON VALUE-CHANGED OF CMB_UTF IN FRAME DIALOG-1 /* Välj utfärdare */
DO:
  CMB_UTF = INPUT CMB_UTF.
  FILL-IN_ANVANDARE = CMB_UTF.
  DISPLAY FILL-IN_ANVANDARE WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_KALKTEXT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_KALKTEXT DIALOG-1
ON LEAVE OF FILL-IN_KALKTEXT IN FRAME DIALOG-1 /* Benämning */
DO:
   FILL-IN_KALKTEXT = INPUT FILL-IN_KALKTEXT.
   IF FILL-IN_KALKTEXT = "" THEN DO:
      MESSAGE "Får ej vara blank!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   FILL-IN_KALKTEXT = TRIM(FILL-IN_KALKTEXT).
   DISPLAY FILL-IN_KALKTEXT WITH FRAME {&FRAME-NAME}.
   RUN AONRNAMNKOLL.P (INPUT 1, INPUT FILL-IN_KALKTEXT,OUTPUT musz).
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_KALK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_KALK DIALOG-1
ON VALUE-CHANGED OF TOG_KALK IN FRAME DIALOG-1 /* P3-kalkyl */
DO:
   TOG_KALK = INPUT TOG_KALK.
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
   
   
   
   IF vart = "NYA" THEN DO:
      FIND FIRST omrtemp WHERE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE omrtemp THEN DO:
         IF varifran = 1 THEN DO:
            IF valaonr = ? THEN Guru.Konstanter:OmradeTTh:FIND-FIRST("WHERE  OMRADE = " + QUOTER(Guru.Konstanter:globomr) ,NO-LOCK) NO-ERROR.
            ELSE Guru.Konstanter:OmradeTTh:FIND-FIRST("WHERE  OMRADE = " + QUOTER(valomrade) ,NO-LOCK) NO-ERROR.
         END.
         ELSE DO.
            Guru.Konstanter:OmradeTTh:FIND-FIRST("WHERE  OMRADE = " + QUOTER(valomrade) ,NO-LOCK) NO-ERROR.
         END.
         IF Guru.Konstanter:OmradeTTh:AVAILABLE THEN DO:
            CREATE omrtemp.
            BUFFER omrtemp:HANDLE:BUFFER-COPY(Guru.Konstanter:OmradeTTh).
            CREATE eomrtemp.
            BUFFER eomrtemp:HANDLE:BUFFER-COPY(Guru.Konstanter:OmradeTTh).
         END.
      END.           
   END.
       
   TOG_KALK:LABEL = "Automtisk hämting av Kalkylkoder per konstruktion/schakt.".
   IF vart NE "NYA" THEN DO:
      FIND FIRST beredningtemp WHERE beredningtemp.OMRADE = valomrade AND
      beredningtemp.BERNR = valbernr NO-LOCK NO-ERROR. 
   END.
   OPEN QUERY oq FOR EACH omrtemp USE-INDEX OMR NO-LOCK.
   GET FIRST oq NO-LOCK.
   DO WHILE AVAILABLE(omrtemp): 
      status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).
      GET NEXT oq NO-LOCK.
   END.      
   IF vart = "NYA" THEN DO:
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ny beredning".
   END.
   ELSE DO:
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ändring av beredning".
   END. 
   CMB_OMR:LABEL = Guru.Konstanter:gutfk + " " + LC(Guru.Konstanter:gomrk). 
   RUN UtbytStart_UI.
   RUN grundtid_UI.   
   IF vart = "NYA" THEN DO:
      IF lnamnex = "" THEN DO:   
         FIND FIRST huvlevtemp WHERE huvlevtemp.DEP-NR = 999 NO-LOCK NO-ERROR.
         IF AVAILABLE huvlevtemp THEN DO:
            ASSIGN
            vald_kundlev = huvlevtemp.LEVKOD
            vald_lev = huvlevtemp.LEVKOD.
         END.
         ELSE DO:
            FIND FIRST levtemp WHERE levtemp.LEVKOD NE "0" AND levtemp.LEVKOD NE "99"
            AND levtemp.BORTTAG = FALSE NO-LOCK NO-ERROR.
            ASSIGN
            vald_kundlev = levtemp.LEVKOD         
            vald_lev = levtemp.LEVKOD.
         END.
      END.
      ELSE DO:
         FIND FIRST huvlevtemp WHERE huvlevtemp.DEP-NR = 999 NO-LOCK NO-ERROR.
         IF AVAILABLE huvlevtemp THEN DO:
            ASSIGN
            vald_kundlev = huvlevtemp.LEVKOD.
         END.   
         FIND FIRST levtemp WHERE levtemp.LEVNAMN = lnamnex NO-LOCK NO-ERROR.
         IF AVAILABLE levtemp THEN DO:
            /*Exporterat företags leverantör*/
            ASSIGN                     
            vald_lev = levtemp.LEVKOD.
         END.
         ELSE DO:   
            FIND FIRST huvlevtemp WHERE huvlevtemp.DEP-NR = 999 NO-LOCK NO-ERROR.
            IF AVAILABLE huvlevtemp THEN DO:
               ASSIGN               
               vald_lev = huvlevtemp.LEVKOD.
            END.
         END.
         
      END.      
      status-ok = CMB_LEV:DELETE("0").
      
      FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_lev
      USE-INDEX LEV NO-LOCK NO-ERROR.   
      ASSIGN
      status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}
      CMB_LEV:SCREEN-VALUE = levtemp.LEVNAMN.   
      FOR EACH levtemp WHERE levtemp.LEVKOD NE vald_lev AND
      levtemp.LEVKOD NE "0" AND levtemp.BORTTAG = FALSE USE-INDEX LEV NO-LOCK:
         /*spec_mtrl*/
         IF levtemp.LEVKOD = "99" THEN DO:
            musz = musz.
         END.
         ELSE DO:
            status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
         END.      
      END.
      RUN Utbytladda_UI.         
      ASSIGN
      CMB_LEV = INPUT CMB_LEV.   
   END.
   ELSE DO:
      RUN hamtlev_UI IN nyberapph (INPUT valomrade,INPUT valbernr,OUTPUT vald_lev).
      status-ok = CMB_LEV:DELETE("0").
      IF vald_lev = "" THEN DO:
         FIND FIRST huvlevtemp WHERE huvlevtemp.DEP-NR = 999 NO-LOCK NO-ERROR.
         IF AVAILABLE huvlevtemp THEN DO:
            ASSIGN
            vald_kundlev = huvlevtemp.LEVKOD
            vald_lev = huvlevtemp.LEVKOD.
         END.
         ELSE DO:
            FIND FIRST levtemp WHERE levtemp.LEVKOD NE "0" AND levtemp.LEVKOD NE "99"
            AND levtemp.BORTTAG = FALSE NO-LOCK NO-ERROR.
            ASSIGN
            vald_kundlev = levtemp.LEVKOD         
            vald_lev = levtemp.LEVKOD.
         END.         
         FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_lev
         USE-INDEX LEV NO-LOCK NO-ERROR.   
         ASSIGN
         status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}
         CMB_LEV:SCREEN-VALUE = levtemp.LEVNAMN.   
      END.
      ELSE DO:
         FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_lev
         USE-INDEX LEV NO-LOCK NO-ERROR.   
         ASSIGN
         status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}
         CMB_LEV:SCREEN-VALUE = levtemp.LEVNAMN.   
      END.
      ASSIGN
      CMB_LEV = INPUT CMB_LEV. 
      RUN Utbytladda_UI.  
   END.
   IF Guru.Konstanter:hoppsekvar[3] = FALSE THEN DO:
      TOG_KALK = FALSE.
   END.
   ELSE TOG_KALK = TRUE.
   CMB_ERS:LABEL = Guru.Konstanter:gutbytk.
   RUN enable_UI.       
   {FRMSIZED.I} 
   CMB_ERS:HIDDEN = FALSE.
   TOG_KALK = INPUT TOG_KALK.  
   TOG_KALK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.   
   IF vart = "AND" THEN DO:
      DISABLE CMB_OMR CMB_LEV CMB_ERS WITH FRAME {&FRAME-NAME}.
   END.
   IF Guru.Konstanter:globforetag = "gran" THEN DO:
       DISABLE CMB_LEV  WITH FRAME {&FRAME-NAME}.
   END.
   IF varifran = 1 THEN musz = musz.
   ELSE IF vart = "NYA" THEN DO:
      /*om projektets område är borttaget måste det gå att välja område lena 20120508*/
      FIND FIRST omrtemp WHERE omrtemp.NAMN = omrnamn NO-ERROR.                  
      IF omrtemp.OMRADE NE valomrade THEN .
      ELSE DISABLE CMB_OMR WITH FRAME {&FRAME-NAME}.
   END.
   {musarrow.i}
   CMB_OMR:LABEL = "Uförande " + LC(Guru.Konstanter:gomrk).         
   IF Guru.Konstanter:kalk2sekvar[1] = FALSE THEN DO: 
      TOG_KALK:HIDDEN = TRUE.
   END.
   IF vart = "NYA" AND lnamnex NE "" THEN DO:
      DISABLE CMB_ERS CMB_LEV  
      WITH FRAME DIALOG-1. 
   END.         
   Guru.GlobalaVariabler:StartKolumnForKnappar = CMB_ERS:COLUMN.  
   Guru.Konstanter:PlaceraKnapparVagrattFranHoger(FILL-IN-R1:HANDLE).
   FILL-IN-R1:ROW = CMB_ERS:ROW.
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
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   IF Guru.Konstanter:appcon THEN DO:
      RUN NYBERAPP.P PERSISTENT SET nyberapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:varforetypchar[48]). 
   END.
   ELSE DO:
      RUN NYBERAPP.P PERSISTENT SET nyberapph (INPUT Guru.Konstanter:varforetypchar[48]).
   END. 
   EMPTY TEMP-TABLE beredningtemp NO-ERROR. 
   tthandle = TEMP-TABLE beredningtemp:HANDLE.
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "BEREDNING", INPUT " WHERE OMRADE = '" + valomrade + "'").
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "BEREDNING", INPUT " WHERE OMRADE = '" + valomrade + "'").

   END.
   tthandle = TEMP-TABLE levtemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "LEVERANTOR", INPUT " WHERE LEVKOD NE '" + STRING(0) + "' AND BORTTAG = FALSE ").
   tthandle = TEMP-TABLE huvlevtemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "HUVUDLEV", INPUT "").
   
   

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
  DISPLAY CMB_OMR FILL-IN_KALKNR FILL-IN_KALKTEXT CMB_UTF FILL-IN_ANVANDARE 
          CMB_ERS CMB_LEV TOG_KALK FILL-IN-R1 
      WITH FRAME DIALOG-1.
  ENABLE CMB_OMR FILL-IN_KALKTEXT CMB_UTF CMB_ERS CMB_LEV TOG_KALK BTN_REG 
         BTN_AVS FILL-IN-R1 
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
   IF vart = "NYA" THEN DO:           
      IF valaonr = ? THEN DO:
         RUN ejvalaonr_UI IN nyberapph (INPUT valomrade,
                                        INPUT Guru.Konstanter:globomr,OUTPUT omrnamn,OUTPUT klknr).
         IF omrnamn = "" THEN DO:
            FIND FIRST omrtemp WHERE NO-LOCK NO-ERROR.
            ASSIGN CMB_OMR:SCREEN-VALUE IN FRAME {&FRAME-NAME} = omrtemp.NAMN. 
         END.                                  
         ELSE ASSIGN CMB_OMR:SCREEN-VALUE IN FRAME {&FRAME-NAME} = omrnamn. 
         FILL-IN_KALKTEXT = valort.
      END.
      ELSE DO:
         RUN valaonr_UI IN nyberapph (INPUT valaonr,INPUT valdelnr,
                                      OUTPUT omrnamn,OUTPUT klktext,
                                      OUTPUT klknr).
         IF omrnamn = "" THEN DO:
            FIND FIRST omrtemp WHERE NO-LOCK NO-ERROR.
            ASSIGN CMB_OMR:SCREEN-VALUE IN FRAME {&FRAME-NAME} = omrtemp.NAMN.  
         END.                               
         ELSE DO:
            ASSIGN CMB_OMR:SCREEN-VALUE IN FRAME {&FRAME-NAME} = omrnamn.
            /*om projektets område är borttaget måste det gå att välja område lena 20120508*/
            FIND FIRST omrtemp WHERE omrtemp.NAMN = omrnamn NO-ERROR.                  
            IF omrtemp.OMRADE NE valomrade THEN .              
            ELSE DISABLE CMB_OMR WITH FRAME {&FRAME-NAME}.
         END.   
         FILL-IN_KALKTEXT = klktext.            
      END.   
      ASSIGN 
      FILL-IN_KALKNR = klknr
      FILL-IN_ANVANDARE = Guru.Konstanter:globanv
      TOG_KALK = TRUE.
      anv = FILL-IN_ANVANDARE.
   END.   
   ELSE DO:
      RUN andraladd_UI IN nyberapph (INPUT valomrade,INPUT valbernr,
                                     OUTPUT omrnamn,OUTPUT anv,
                                     OUTPUT klknr,OUTPUT klktext,
                                     OUTPUT togklk).
      FIND FIRST omrtemp WHERE omrtemp.OMRADE = valomrade NO-LOCK NO-ERROR.
      IF NOT AVAILABLE omrtemp THEN DO:
        IF Guru.Konstanter:appcon THEN DO:
            RUN NAMNBORTAGETOMR.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT valomrade, OUTPUT omrnamn). 
         END.
         ELSE DO:
            RUN NAMNBORTAGETOMR.P (INPUT valomrade, OUTPUT omrnamn).
         END.
          status-ok = CMB_OMR:ADD-LAST(omrnamn). 
      END.                               
      ASSIGN CMB_OMR:SCREEN-VALUE IN FRAME {&FRAME-NAME} = omrnamn.
      ASSIGN
      FILL-IN_ANVANDARE = anv          
      FILL-IN_KALKNR = klknr
      FILL-IN_KALKTEXT = klktext
      TOG_KALK = togklk.           
   END.  
   EMPTY TEMP-TABLE batemp NO-ERROR. 
   FIND FIRST berkalanvandartemp WHERE berkalanvandartemp.ANVANDARE = anv NO-LOCK NO-ERROR.
   IF NOT AVAILABLE berkalanvandartemp THEN DO:
      FIND FIRST berkalanvandartemp WHERE berkalanvandartemp.ANVANDARE = Guru.Konstanter:globanv NO-LOCK NO-ERROR.
      anv = Guru.Konstanter:globanv.
   END.
   IF NOT AVAILABLE berkalanvandartemp THEN DO:
      FIND FIRST berkalanvandartemp NO-LOCK NO-ERROR.
      anv = berkalanvandartemp.ANVANDARE.
      MESSAGE "OBS! Utfärdare är ändrad!"  VIEW-AS ALERT-BOX.
   END.
   IF AVAILABLE berkalanvandartemp THEN DO:
      FILL-IN_ANVANDARE = FILL-IN_ANVANDARE + " -- " + berkalanvandartemp.AV-NAMN.
      status-ok = CMB_UTF:ADD-LAST(anv + " -- " + berkalanvandartemp.AV-NAMN).
   END.   
   FOR EACH berkalanvandartemp:
      IF berkalanvandartemp.ANVANDARE = anv THEN.
      ELSE DO: 
         status-ok = CMB_UTF:ADD-LAST(berkalanvandartemp.ANVANDARE + " -- " + berkalanvandartemp.AV-NAMN).         
      END.
      CREATE batemp.
      ASSIGN batemp.ANVANDARE = berkalanvandartemp.ANVANDARE
      batemp.ANVNAMN = berkalanvandartemp.ANVANDARE + " -- " + berkalanvandartemp.AV-NAMN.         
   END.
   FIND FIRST berkalanvandartemp WHERE berkalanvandartemp.ANVANDARE = anv NO-LOCK NO-ERROR.
   ASSIGN CMB_UTF:SCREEN-VALUE = anv + " -- " + berkalanvandartemp.AV-NAMN.
   CMB_UTF = CMB_UTF:SCREEN-VALUE. 
   FILL-IN_ANVANDARE = CMB_UTF.   
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reg_UI DIALOG-1 
PROCEDURE reg_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  -------------------------------------------------------------*/
   DEFINE VARIABLE lnr AS INTEGER NO-UNDO.
   {muswait.i} 
   EMPTY TEMP-TABLE felmeddtemp NO-ERROR. 
   ASSIGN
   FILL-IN_KALKTEXT = INPUT FRAME {&FRAME-NAME} FILL-IN_KALKTEXT
   CMB_OMR = INPUT CMB_OMR
   CMB_UTF = INPUT CMB_UTF.
   CMB_ERS = INPUT CMB_ERS.
   TOG_KALK = INPUT TOG_KALK.      
   cmbomr = CMB_OMR.   
   FIND FIRST batemp WHERE batemp.ANVNAMN = CMB_UTF NO-ERROR.
   anv = batemp.ANVANDARE.   
   IF vart = "NYA" THEN DO:
      RUN ny_UI IN nyberapph (INPUT cmbomr,INPUT vart,OUTPUT klknr).
   END.   
   FILL-IN_KALKNR = klknr.
   DISPLAY FILL-IN_KALKNR WITH FRAME {&FRAME-NAME}.       
   ASSIGN
   musz = FALSE
   klktext = FILL-IN_KALKTEXT.        
   togklk = TOG_KALK.   
   IF valaonr = ? THEN DO:
      FIND FIRST valberedningtemp WHERE valberedningtemp.BERNR = klknr AND 
      valberedningtemp.OMRADE = valomrade NO-LOCK NO-ERROR.
      IF NOT AVAILABLE valberedningtemp THEN DO:
         CREATE valberedningtemp.
         ASSIGN 
         valberedningtemp.BERNR = klknr
         valberedningtemp.OMRADE = valomrade.
      END.      
   END.
   ELSE DO:
      FIND FIRST valberedningtemp WHERE valberedningtemp.AONR = valaonr AND 
      valberedningtemp.DELNR = valdelnr NO-LOCK NO-ERROR.
   END.
   EMPTY TEMP-TABLE extravalbertemp NO-ERROR. 
   CREATE extravalbertemp.
   BUFFER-COPY valberedningtemp TO extravalbertemp.
   RUN reg_UI IN nyberapph (INPUT CMB_OMR,INPUT vart,INPUT-OUTPUT valaonr,
                            INPUT-OUTPUT valdelnr,INPUT-OUTPUT valomrade,INPUT-OUTPUT valbernr,
                            INPUT anv,INPUT klknr,
                            INPUT klktext,INPUT togklk,
                            INPUT-OUTPUT TABLE extravalbertemp,
                            OUTPUT valort,OUTPUT katvar,OUTPUT TABLE felmeddtemp).
  /* andlev_UI körs från reg_UI och skapar betfria för huvudlev och ska den ändras så görs det nedan*/ 
   IF vart = "NYA" THEN DO:
      UtbytNamnbuffh:FIND-FIRST("WHERE UNAMN =  " + QUOTER(CMB_ERS),NO-LOCK) NO-ERROR.
      IF UtbytNamnbuffh:AVAILABLE THEN DO:
         lnr = UtbytNamnbuffh:BUFFER-FIELD("UID"):BUFFER-VALUE.
         RUN SparBerVald_UI IN AppServerHandle (INPUT valbernr,INPUT valomrade, INPUT lnr).
      END.
      IF vald_lev NE vald_kundlev THEN DO:
         RUN andlev_UI IN nyberapph (INPUT valomrade,INPUT klknr,INPUT vald_lev).
      END.
   END.
   FIND FIRST extravalbertemp NO-LOCK NO-ERROR.
   IF AVAILABLE extravalbertemp THEN DO:
      BUFFER-COPY extravalbertemp TO valberedningtemp.
      DELETE extravalbertemp.      
   END.
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      IF felmeddtemp.VAL = 1 THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX TITLE "Meddelande". 
         RETURN.
      END.
      ELSE IF felmeddtemp.VAL = 2 THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX 
         QUESTION BUTTONS YES-NO TITLE "Borttag P3-kalkyl?" UPDATE svar AS LOGICAL.         
         IF svar THEN DO:
            RUN delberklk_UI IN nyberapph (INPUT valaonr,INPUT valomrade).                                       
         END.
         ELSE DO:
            RUN klktrue_ui IN nyberapph (INPUT valomrade,INPUT valbernr).         
            ASSIGN
            valberedningtemp.KALKYL = TRUE.   
         END.              
      END.
   END.
   {musarrow.i}   
   FOR EACH eomrtemp WHERE NO-LOCK:
      FIND FIRST omrtemp WHERE omrtemp.OMRADE = eomrtemp.OMRADE NO-LOCK NO-ERROR.
      IF AVAILABLE omrtemp THEN DELETE omrtemp.
   END.
   APPLY "GO" TO BTN_REG IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Utbytladda_UI DIALOG-1 
PROCEDURE Utbytladda_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE lnr AS INTEGER NO-UNDO.
   status-ok = CMB_ERS:DELETE("0") IN FRAME {&FRAME-NAME} .
   status-ok = CMB_ERS:DELETE("o").
   status-ok = CMB_ERS:ADD-LAST("Ej vald!") IN FRAME {&FRAME-NAME}.
   Guru.GlobalaVariabler:CustomQueryDyn:QUERY-OPEN().
   Guru.GlobalaVariabler:CustomQueryDyn:GET-FIRST().
   DO WHILE Guru.GlobalaVariabler:CustomQueryDyn:QUERY-OFF-END = FALSE:
      IF UtbytNamnbuffh:BUFFER-FIELD("UID"):BUFFER-VALUE = 0 THEN.
      ELSE status-ok = CMB_ERS:ADD-LAST(UtbytNamnbuffh:BUFFER-FIELD("UNAMN"):BUFFER-VALUE).
      Guru.GlobalaVariabler:CustomQueryDyn:GET-NEXT().   
   END.
   CMB_ERS:SCREEN-VALUE = "Ej vald!".
   IF vart = "NYA" THEN.
   ELSE DO:
      RUN HamtBerVald_UI IN AppServerHandle (INPUT valbernr,INPUT valomrade, OUTPUT lnr).
      UtbytNamnbuffh:FIND-FIRST("WHERE UID =  " + STRING(lnr),NO-LOCK) NO-ERROR.
      IF UtbytNamnbuffh:AVAILABLE THEN DO:
         IF UtbytNamnbuffh:BUFFER-FIELD("UID"):BUFFER-VALUE = 0 THEN.
         ELSE CMB_ERS:SCREEN-VALUE = UtbytNamnbuffh:BUFFER-FIELD("UNAMN"):BUFFER-VALUE.
      END.
   END.       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UtbytStart_UI DIALOG-1 
PROCEDURE UtbytStart_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF Guru.Konstanter:appcon THEN DO:
      RUN UTBYTESLISTADS.P PERSISTENT SET AppServerHandle ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN UTBYTESLISTADS.P PERSISTENT SET AppServerHandle.
   END. 
   IF VALID-HANDLE(UtbytNamnbuffh) THEN UtbytNamnbuffh:EMPTY-TEMP-TABLE().
   RUN HamtalistaUtb_UI IN AppServerHandle (OUTPUT DATASET-HANDLE UtbytlistorDS APPEND BIND). 
   IF VALID-HANDLE(UtbytlistorDS) THEN DO:
      UtbytNamnbuffh = UtbytlistorDS:GET-BUFFER-HANDLE(1).               
   END.   
   kommandoquery =  "FOR EACH " + UtbytNamnbuffh:TABLE.
   Guru.Konstanter:CreateCustomQuery(UtbytNamnbuffh,kommandoquery).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

