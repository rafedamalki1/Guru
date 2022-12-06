&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1


/* Temp-Table and Buffer definitions                                    */




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/04/24 -  9:36 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER depinnan AS INTEGER NO-UNDO.    
/* Local Variable Definitions ---                                       */   
{ALLDEF.I}
{GLOBVAR2DEL1.I}
&Scoped-define NEW   
&Scoped-define SHARED
{ANVTEMPS.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{LEVTEMP.I}
{DEPATEMP.I}
DEFINE SHARED VARIABLE vald_kundlev AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE depaapph AS HANDLE NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO. 
DEFINE VARIABLE leverant AS CHARACTER NO-UNDO.
DEFINE VARIABLE depaspar AS INTEGER NO-UNDO.  
DEFINE VARIABLE depefter AS INTEGER NO-UNDO.  
DEFINE VARIABLE nydep AS INTEGER NO-UNDO.  
DEFINE VARIABLE vald_levkod AS CHARACTER NO-UNDO.
DEFINE VARIABLE nylevkund AS CHARACTER NO-UNDO.
DEFINE VARIABLE kundnummer AS INTEGER NO-UNDO.  
DEFINE VARIABLE nyakund AS INTEGER NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
{EXTRADATA.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_ANV

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES anvandartemp

/* Definitions for BROWSE BRW_ANV                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_ANV anvandartemp.ANVANDARE ~
anvandartemp.AV-NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ANV 
&Scoped-define QUERY-STRING-BRW_ANV FOR EACH anvandartemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ANV OPEN QUERY BRW_ANV FOR EACH anvandartemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ANV anvandartemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ANV anvandartemp


/* Definitions for DIALOG-BOX DIALOG-1                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS depatemp.Dep-Nr depatemp.Benamning ~
depatemp.Firma depatemp.Adress depatemp.Pnr depatemp.Ort depatemp.Telefon ~
depatemp.FaxNr depatemp.LevAdress depatemp.LevPnr depatemp.LevOrt ~
depatemp.FakAdress depatemp.FakPnr depatemp.FakOrt 
&Scoped-define ENABLED-TABLES depatemp
&Scoped-define FIRST-ENABLED-TABLE depatemp
&Scoped-Define ENABLED-OBJECTS FILL-IN-XKORDCH FILL-IN-YKORDCH CMB_LEV ~
FILL-IN_ANVANDARE BRW_ANV CMB_LEVKUND FILL-IN-KUNDNR BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-FIELDS depatemp.Dep-Nr depatemp.Benamning ~
depatemp.Firma depatemp.Adress depatemp.Pnr depatemp.Ort depatemp.Telefon ~
depatemp.FaxNr depatemp.LevAdress depatemp.LevPnr depatemp.LevOrt ~
depatemp.FakAdress depatemp.FakPnr depatemp.FakOrt 
&Scoped-define DISPLAYED-TABLES depatemp
&Scoped-define FIRST-DISPLAYED-TABLE depatemp
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-XKORDCH FILL-IN-YKORDCH CMB_LEV ~
FILL-IN_ANVANDARE CMB_LEVKUND FILL-IN-KUNDNR 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_LEV AS CHARACTER FORMAT "X(256)":U 
     LABEL "Huvudlev" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18 BY .96 NO-UNDO.

DEFINE VARIABLE CMB_LEVKUND AS CHARACTER FORMAT "X(256)":U 
     LABEL "Leverantörer" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18 BY .96 NO-UNDO.

DEFINE VARIABLE FILL-IN-KUNDNR AS INTEGER FORMAT ">>>>>>>>>>":U INITIAL 0 
     LABEL "Kundnummer" 
     VIEW-AS FILL-IN 
     SIZE 15.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-XKORDCH AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "X" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-YKORDCH AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Y" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_ANVANDARE AS CHARACTER FORMAT "x(255)" 
     LABEL "Depå Ansvarig" 
     VIEW-AS FILL-IN 
     SIZE 20.13 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_ANV FOR 
      anvandartemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_ANV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ANV DIALOG-1 _STRUCTURED
  QUERY BRW_ANV NO-LOCK DISPLAY
      anvandartemp.ANVANDARE COLUMN-LABEL "Användare" FORMAT "x(255)":U
            WIDTH 12
      anvandartemp.AV-NAMN COLUMN-LABEL "Namn" FORMAT "x(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 60.5 BY 4.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     depatemp.Dep-Nr AT ROW 1.75 COL 13.75 COLON-ALIGNED
          LABEL "Depå-Nr"
          VIEW-AS FILL-IN 
          SIZE 4.75 BY 1
     depatemp.Benamning AT ROW 1.75 COL 37.13 COLON-ALIGNED
          LABEL "Benämning"
          VIEW-AS FILL-IN 
          SIZE 31.13 BY 1
     depatemp.Firma AT ROW 3.75 COL 13.75 COLON-ALIGNED
          LABEL "FirmaNamn"
          VIEW-AS FILL-IN 
          SIZE 54.5 BY 1
     depatemp.Adress AT ROW 5 COL 13.75 COLON-ALIGNED
          LABEL "Adress"
          VIEW-AS FILL-IN 
          SIZE 54.5 BY 1
     depatemp.Pnr AT ROW 6.25 COL 13.75 COLON-ALIGNED
          LABEL "PostNr"
          VIEW-AS FILL-IN 
          SIZE 11.13 BY 1
     depatemp.Ort AT ROW 6.25 COL 37.13 COLON-ALIGNED
          LABEL "Ort"
          VIEW-AS FILL-IN 
          SIZE 31 BY 1
     depatemp.Telefon AT ROW 7.5 COL 13.75 COLON-ALIGNED
          LABEL "Telefon"
          VIEW-AS FILL-IN 
          SIZE 16.13 BY 1
     depatemp.FaxNr AT ROW 7.5 COL 37.13 COLON-ALIGNED
          LABEL "FaxNr"
          VIEW-AS FILL-IN 
          SIZE 16.13 BY 1
     depatemp.LevAdress AT ROW 10.25 COL 13.75 COLON-ALIGNED
          LABEL "Adress"
          VIEW-AS FILL-IN 
          SIZE 54.5 BY 1
     depatemp.LevPnr AT ROW 11.5 COL 13.75 COLON-ALIGNED
          LABEL "PostNr"
          VIEW-AS FILL-IN 
          SIZE 11.13 BY 1
     depatemp.LevOrt AT ROW 11.5 COL 37.13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 31.25 BY 1
     FILL-IN-XKORDCH AT ROW 12.75 COL 13.75 COLON-ALIGNED WIDGET-ID 18
     FILL-IN-YKORDCH AT ROW 12.75 COL 45.25 COLON-ALIGNED WIDGET-ID 20
     depatemp.FakAdress AT ROW 15.96 COL 13.25 COLON-ALIGNED
          LABEL "Adress"
          VIEW-AS FILL-IN 
          SIZE 54.5 BY 1
     depatemp.FakPnr AT ROW 17.21 COL 13.25 COLON-ALIGNED
          LABEL "PostNr"
          VIEW-AS FILL-IN 
          SIZE 11.13 BY 1
     depatemp.FakOrt AT ROW 17.21 COL 36.63 COLON-ALIGNED
          LABEL "Ort"
          VIEW-AS FILL-IN 
          SIZE 31.13 BY 1
     CMB_LEV AT ROW 18.96 COL 13.25 COLON-ALIGNED
     FILL-IN_ANVANDARE AT ROW 18.96 COL 42.63 COLON-ALIGNED
     BRW_ANV AT ROW 20.13 COL 3.38
     CMB_LEVKUND AT ROW 25.29 COL 13.75 COLON-ALIGNED
     FILL-IN-KUNDNR AT ROW 25.29 COL 43.75 COLON-ALIGNED
     BTN_OK AT ROW 27.46 COL 35.63
     BTN_AVB AT ROW 27.46 COL 50.63
     "Kordinater" VIEW-AS TEXT
          SIZE 10.88 BY 1 AT ROW 12.83 COL 1.38 WIDGET-ID 8
     "Leverans-" VIEW-AS TEXT
          SIZE 10.88 BY .75 AT ROW 9.5 COL 3.88
     "Faktura-" VIEW-AS TEXT
          SIZE 10.88 BY .75 AT ROW 15.21 COL 4.38
     SPACE(62.48) SKIP(13.32)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Depå-information":L
         DEFAULT-BUTTON BTN_OK.


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
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BRW_ANV FILL-IN_ANVANDARE DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN depatemp.Adress IN FRAME DIALOG-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN depatemp.Benamning IN FRAME DIALOG-1
   EXP-LABEL                                                            */
ASSIGN 
       BRW_ANV:ALLOW-COLUMN-SEARCHING IN FRAME DIALOG-1 = TRUE.

/* SETTINGS FOR FILL-IN depatemp.Dep-Nr IN FRAME DIALOG-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN depatemp.FakAdress IN FRAME DIALOG-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN depatemp.FakOrt IN FRAME DIALOG-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN depatemp.FakPnr IN FRAME DIALOG-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN depatemp.FaxNr IN FRAME DIALOG-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN depatemp.Firma IN FRAME DIALOG-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN depatemp.LevAdress IN FRAME DIALOG-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN depatemp.LevPnr IN FRAME DIALOG-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN depatemp.Ort IN FRAME DIALOG-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN depatemp.Pnr IN FRAME DIALOG-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN depatemp.Telefon IN FRAME DIALOG-1
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ANV
/* Query rebuild information for BROWSE BRW_ANV
     _TblList          = "Temp-Tables.anvandartemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.anvandartemp.ANVANDARE
"anvandartemp.ANVANDARE" "Användare" "x(255)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.anvandartemp.AV-NAMN
"anvandartemp.AV-NAMN" "Namn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
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
ON END-ERROR OF FRAME DIALOG-1 /* Depå-information */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.
   IF VALID-HANDLE(depaapph) THEN DELETE PROCEDURE depaapph.
   
   APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Depå-information */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_ANV
&Scoped-define SELF-NAME BRW_ANV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_ANV DIALOG-1
ON VALUE-CHANGED OF BRW_ANV IN FRAME DIALOG-1
DO:
   status-ok = BRW_ANV:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   FILL-IN_ANVANDARE = anvandartemp.ANVANDARE.   
   DISPLAY FILL-IN_ANVANDARE WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
   IF depinnan = ? THEN DO:
      RUN depavb_UI IN depaapph (INPUT depinnan, INPUT TABLE depatemp ).
      FIND FIRST depatemp WHERE depatemp.DEP-NR = 0 NO-ERROR. 
      DELETE depatemp.
   END.   
   
   musz = TRUE.
   IF VALID-HANDLE(depaapph) THEN DELETE PROCEDURE depaapph.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:
  IF INPUT depatemp.DEP-NR = "" THEN DO:
     MESSAGE "DepåNr får inte vara blankt." VIEW-AS ALERT-BOX TITLE "Meddelande".
     UNDO,RETRY.
  END.     
  IF INPUT depatemp.DEP-NR = 0 THEN DO:
     MESSAGE "DepåNr får inte vara 0." VIEW-AS ALERT-BOX TITLE "Meddelande".
     UNDO,RETRY.
  END.                 
  IF INPUT depatemp.DEP-NR = 999 THEN DO:
     MESSAGE "DepåNr 999 är upptaget av systemtekniska skäl." VIEW-AS ALERT-BOX TITLE "Meddelande".
     UNDO,RETRY.
  END. 
  IF INPUT FILL-IN_ANVANDARE = "" THEN DO:
     MESSAGE "Användare får inte vara blankt." VIEW-AS ALERT-BOX TITLE "Meddelande".
     UNDO,RETRY. 
  END.          
           
  DO TRANSACTION:
     FIND CURRENT depatemp EXCLUSIVE-LOCK NO-ERROR.
     /*ASSIGN {&FIELDS-IN-QUERY-{&FRAME-NAME}}.      */
     ASSIGN
     depatemp.Dep-Nr = INPUT depatemp.Dep-Nr   
     depatemp.Benamning = INPUT depatemp.Benamning
     depatemp.Adress = INPUT depatemp.Adress   
     depatemp.FakAdress = INPUT depatemp.FakAdress
     depatemp.FakOrt  = INPUT depatemp.FakOrt  
     depatemp.FakPnr = INPUT depatemp.FakPnr 
     depatemp.FaxNr= INPUT depatemp.FaxNr     
     depatemp.Firma= INPUT depatemp.Firma    
     depatemp.LevAdress = INPUT depatemp.LevAdress
     depatemp.LevOrt = INPUT depatemp.LevOrt   
     depatemp.LevPnr = INPUT depatemp.LevPnr   
     depatemp.Ort  = INPUT depatemp.Ort   
     depatemp.Pnr   = INPUT depatemp.Pnr  
     depatemp.Telefon  = INPUT depatemp.Telefon 
     depatemp.ANVANDARE = INPUT FILL-IN_ANVANDARE.
     

  END.  
  FILL-IN-XKORDCH = INPUT FILL-IN-XKORDCH.
  FILL-IN-YKORDCH = INPUT FILL-IN-YKORDCH.
  EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
  CREATE inextradatatemp.          
  ASSIGN
  inextradatatemp.PROGRAM = "DEPAKORD"                   
  inextradatatemp.HUVUDINT = depatemp.Dep-Nr
  inextradatatemp.SOKCHAR[1] = FILL-IN-XKORDCH
  inextradatatemp.SOKCHAR[2] = FILL-IN-YKORDCH.
  
  RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).
  EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
  ASSIGN
  depefter = depatemp.DEP-NR.
  RUN depspara_UI IN depaapph (INPUT depinnan,INPUT depefter, INPUT vald_lev, INPUT vald_levkod, INPUT FILL-IN-KUNDNR,INPUT TABLE depatemp,OUTPUT TABLE felmeddtemp).                   
   FIND FIRST felmeddtemp NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
      DELETE felmeddtemp.
      RETURN NO-APPLY.
   END.
   ASSIGN depinnan = depatemp.DEP-NR.
   {BORTBRWPROC.I}
   IF VALID-HANDLE(depaapph) THEN DELETE PROCEDURE depaapph.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.
   APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEV DIALOG-1
ON VALUE-CHANGED OF CMB_LEV IN FRAME DIALOG-1 /* Huvudlev */
DO:                                        
   leverant = INPUT CMB_LEV.                
   FIND FIRST levtemp WHERE levtemp.LEVNAMN = leverant NO-LOCK NO-ERROR.
   vald_lev = levtemp.LEVKOD.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEVKUND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEVKUND DIALOG-1
ON VALUE-CHANGED OF CMB_LEVKUND IN FRAME DIALOG-1 /* Leverantörer */
DO:        
   ASSIGN
   nydep = INPUT depatemp.DEP-NR
   nylevkund = INPUT CMB_LEVKUND
   nyakund = INPUT FILL-IN-KUNDNR.
   RUN depkund_UI IN depaapph (INPUT nydep, INPUT-OUTPUT vald_levkod,INPUT nylevkund,INPUT-OUTPUT nyakund).
   ASSIGN
   FILL-IN-KUNDNR = nyakund.  
   DISPLAY FILL-IN-KUNDNR WITH FRAME {&FRAME-NAME}.       
          
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-KUNDNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KUNDNR DIALOG-1
ON LEAVE OF FILL-IN-KUNDNR IN FRAME DIALOG-1 /* Kundnummer */
DO:
  FILL-IN-KUNDNR = INPUT FILL-IN-KUNDNR.
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
  
   IF depinnan = ? THEN DO:
      CREATE depatemp.            
   END.
   ELSE DO:   
      FIND FIRST depatemp WHERE depatemp.DEP-NR = depinnan NO-LOCK NO-ERROR. 
   END.
   RUN hamtalev_UI IN depaapph (INPUT depinnan, OUTPUT vald_kundlev,OUTPUT kundnummer,OUTPUT TABLE anvandartemp).                      
  
   IF depinnan = ? THEN DO:
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ny".     
      depatemp.ANVANDARE = Guru.Konstanter:globanv.
   END.
   ELSE DO:
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ändra-" + depatemp.BENAMNING.
      IF depatemp.ANVANDARE = "" THEN depatemp.ANVANDARE = Guru.Konstanter:globanv.         
   END.    
   status-ok = CMB_LEV:DELETE("0"). 
   FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_kundlev
   USE-INDEX LEV NO-LOCK NO-ERROR.  
   ASSIGN
   status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}
   CMB_LEV:SCREEN-VALUE = levtemp.LEVNAMN.    
   FOR EACH levtemp WHERE levtemp.LEVKOD NE vald_kundlev AND 
   levtemp.LEVKOD NE "0" AND levtemp.LEVKOD NE "99" AND levtemp.BORTTAG = FALSE USE-INDEX LEV NO-LOCK:      
      status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.              
   END.           
   ASSIGN     
   CMB_LEV = INPUT CMB_LEV.  
   status-ok = CMB_LEVKUND:DELETE("0"). 
   FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_kundlev
   USE-INDEX LEV NO-LOCK NO-ERROR.  
   ASSIGN
   status-ok = CMB_LEVKUND:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}
   CMB_LEVKUND:SCREEN-VALUE = levtemp.LEVNAMN.    
   FOR EACH levtemp WHERE levtemp.LEVKOD NE vald_kundlev AND 
   levtemp.LEVKOD NE "0" AND levtemp.LEVKOD NE "99" AND levtemp.BORTTAG = FALSE USE-INDEX LEV NO-LOCK:      
      status-ok = CMB_LEVKUND:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.              
   END.  
   /*         
   OPEN QUERY BRW_ANV FOR EACH anvandartemp WHERE anvandartemp.AV-LEVEL <> 0 NO-LOCK INDEXED-REPOSITION.
   */
   ASSIGN vald_levkod = vald_kundlev.
   ASSIGN FILL-IN-KUNDNR = kundnummer.
   FILL-IN_ANVANDARE = depatemp.ANVANDARE.   
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "DEPAKORD"                   
   inextradatatemp.HUVUDINT = depatemp.DEP-NR.                    
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.
   IF AVAILABLE extradatatemp THEN DO:      
      ASSIGN
      FILL-IN-XKORDCH = extradatatemp.SOKCHAR[1]
      FILL-IN-YKORDCH = extradatatemp.SOKCHAR[2].      
   END.
   
   RUN enable_UI.       
   {FRMSIZED.I}
   
    
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = depatemp.ANVANDARE AND anvandartemp.AV-LEVEL NE 0
   NO-LOCK NO-ERROR.
   IF AVAILABLE anvandartemp THEN DO:      
      RUN repo_UI (INPUT RECID(anvandartemp)).
   END.
   APPLY "VALUE-CHANGED" TO CMB_LEV.
   APPLY "VALUE-CHANGED" TO CMB_LEVKUND.
  
   {musarrow.i}
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
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_ANV:HANDLE IN FRAME {&FRAME-NAME}).         
     IF Guru.Konstanter:appcon THEN DO:
      RUN DEPAAPP.P PERSISTENT SET depaapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN DEPAAPP.P PERSISTENT SET depaapph.
   END.    
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
   END.   
  
   
     
END PROCEDURE.


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
  DISPLAY FILL-IN-XKORDCH FILL-IN-YKORDCH CMB_LEV FILL-IN_ANVANDARE CMB_LEVKUND 
          FILL-IN-KUNDNR 
      WITH FRAME DIALOG-1.
  IF AVAILABLE depatemp THEN 
    DISPLAY depatemp.Dep-Nr depatemp.Benamning depatemp.Firma depatemp.Adress 
          depatemp.Pnr depatemp.Ort depatemp.Telefon depatemp.FaxNr 
          depatemp.LevAdress depatemp.LevPnr depatemp.LevOrt depatemp.FakAdress 
          depatemp.FakPnr depatemp.FakOrt 
      WITH FRAME DIALOG-1.
  ENABLE depatemp.Dep-Nr depatemp.Benamning depatemp.Firma depatemp.Adress 
         depatemp.Pnr depatemp.Ort depatemp.Telefon depatemp.FaxNr 
         depatemp.LevAdress depatemp.LevPnr depatemp.LevOrt FILL-IN-XKORDCH 
         FILL-IN-YKORDCH depatemp.FakAdress depatemp.FakPnr depatemp.FakOrt 
         CMB_LEV FILL-IN_ANVANDARE BRW_ANV CMB_LEVKUND FILL-IN-KUNDNR BTN_OK 
         BTN_AVB 
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
   REPOSITION {&BROWSE-NAME} TO RECID browrec NO-ERROR.  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

