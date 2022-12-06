&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME WINDOW-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-2 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 08/22/96 - 10:33 am

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

&Scoped-define NEW 
&Scoped-define SHARED SHARED
{GLOBVAR2DEL1.I}
{DEPATEMP.I}

DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE foljesedel AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vald_kundlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.   
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.       
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.   
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.

DEFINE VARIABLE mtrlbapph AS HANDLE NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE leverant AS CHARACTER NO-UNDO.   
DEFINE VARIABLE leve AS CHARACTER NO-UNDO.   
DEFINE VARIABLE omra AS CHARACTER NO-UNDO.   
DEFINE VARIABLE valomra AS CHARACTER NO-UNDO.  
DEFINE VARIABLE val AS LOGICAL NO-UNDO.
DEFINE VARIABLE kinamn AS CHARACTER NO-UNDO.  
DEFINE VARIABLE ktele AS CHARACTER NO-UNDO.  
DEFINE VARIABLE bnamn AS CHARACTER NO-UNDO.  
DEFINE VARIABLE bkontakt AS CHARACTER NO-UNDO.  
DEFINE VARIABLE btel AS CHARACTER NO-UNDO.  
DEFINE VARIABLE badr AS CHARACTER NO-UNDO.  
DEFINE VARIABLE bpnr AS CHARACTER NO-UNDO.  
DEFINE VARIABLE bort AS CHARACTER NO-UNDO.  



DEFINE SHARED TEMP-TABLE gam_mtrl NO-UNDO
   FIELD DATUM AS DATE
   FIELD DATUM2 AS DATE
   FIELD KOM AS CHARACTER.  

DEFINE NEW SHARED TEMP-TABLE skapa_mtrl NO-UNDO                
   FIELD FORE AS CHARACTER
   FIELD KADR AS CHARACTER  
   FIELD KPNR AS CHARACTER
   FIELD KORT AS CHARACTER
   FIELD BOX AS CHARACTER FORMAT "X(5)"
   FIELD FAX AS CHARACTER
   FIELD KIKONTAKT AS CHARACTER                            
   FIELD KITELE AS CHARACTER        
   FIELD BESTNAMN AS CHARACTER    
   FIELD TELE AS CHARACTER
   FIELD ADR AS CHARACTER
   FIELD PNR AS CHARACTER
   FIELD ORT AS CHARACTER
   FIELD KONTAKT AS CHARACTER 
   FIELD FAXK AS CHARACTER
   FIELD FAKADR AS CHARACTER
   FIELD FAKPNR AS CHARACTER
   FIELD FAKORT AS CHARACTER
   FIELD LEVADR AS CHARACTER
   FIELD LEVPNR AS CHARACTER
   FIELD LEVORT AS CHARACTER   
   FIELD DATUM AS DATE
   FIELD DATUM2 AS DATE
   FIELD MARK AS CHARACTER FORMAT "X(35)"
   FIELD KOM AS CHARACTER FORMAT "X(40)".




/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-B

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-FOR FILL-IN-KADR FILL-IN-BOX ~
FILL-IN-KPNR FILL-IN-KORT FILL-IN-FAX FBTN_VISA FILL-IN-KIKONTAKT ~
FILL-IN-KITELE FILL-IN-FOR2 FILL-IN-KADR2 FILL-IN-KPNR2 FILL-IN-KORT2 ~
FILL-IN-LKONTAKT FILL-IN-TEL FILL-IN-DATUM FILL-IN-DATUM2 FILL-IN-KOM ~
BTN_AVB FILL-IN-DEPA FILL-IN-AONR FILL-IN-DELNR FILL-IN-ORT 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-FOR FILL-IN-KADR FILL-IN-BOX ~
FILL-IN-KPNR FILL-IN-KORT FILL-IN-FAX FILL-IN-KIKONTAKT FILL-IN-KITELE ~
FILL-IN-FOR2 FILL-IN-KADR2 FILL-IN-KPNR2 FILL-IN-KORT2 FILL-IN-LKONTAKT ~
FILL-IN-TEL FILL-IN-DATUM FILL-IN-DATUM2 FILL-IN-KOM FILL-IN-DEPA ~
FILL-IN-AONR FILL-IN-DELNR FILL-IN-ORT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE VARIABLE FILL-IN-AONR AS CHARACTER FORMAT "X(16)":U 
     LABEL "Aonr" 
      VIEW-AS TEXT 
     SIZE 14.25 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-BOX AS CHARACTER FORMAT "X(4)":U 
     LABEL "Box" 
     VIEW-AS FILL-IN 
     SIZE 7.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DATUM AS DATE FORMAT "99/99/99":U 
     LABEL "Uttagsdatum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DATUM2 AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DELNR AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "Delnr" 
      VIEW-AS TEXT 
     SIZE 4.63 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-DEPA AS CHARACTER FORMAT "X(25)":U 
     LABEL "Information för depå" 
      VIEW-AS TEXT 
     SIZE 26.38 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-FAX AS CHARACTER FORMAT "X(15)":U 
     LABEL "Fax" 
     VIEW-AS FILL-IN 
     SIZE 13.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FOR AS CHARACTER FORMAT "X(25)":U 
     LABEL "Företag" 
     VIEW-AS FILL-IN 
     SIZE 26.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FOR2 AS CHARACTER FORMAT "X(25)":U 
     LABEL "Företag" 
     VIEW-AS FILL-IN 
     SIZE 26.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KADR AS CHARACTER FORMAT "X(20)":U 
     LABEL "Adress" 
     VIEW-AS FILL-IN 
     SIZE 21.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KADR2 AS CHARACTER FORMAT "X(20)":U 
     LABEL "Adress" 
     VIEW-AS FILL-IN 
     SIZE 21.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KIKONTAKT AS CHARACTER FORMAT "X(25)":U 
     LABEL "Kontaktperson" 
     VIEW-AS FILL-IN 
     SIZE 26.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KITELE AS CHARACTER FORMAT "X(15)":U 
     LABEL "Tel" 
     VIEW-AS FILL-IN 
     SIZE 13.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOM AS CHARACTER FORMAT "X(40)":U 
     LABEL "Kommentarer" 
     VIEW-AS FILL-IN 
     SIZE 45.88 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KORT AS CHARACTER FORMAT "X(25)":U 
     LABEL "Ort" 
     VIEW-AS FILL-IN 
     SIZE 16.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KORT2 AS CHARACTER FORMAT "X(25)":U 
     LABEL "Ort" 
     VIEW-AS FILL-IN 
     SIZE 31.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KPNR AS CHARACTER FORMAT "999 99":U 
     LABEL "Postnr" 
     VIEW-AS FILL-IN 
     SIZE 7.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KPNR2 AS CHARACTER FORMAT "999 99":U 
     LABEL "Postnr" 
     VIEW-AS FILL-IN 
     SIZE 7.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LKONTAKT AS CHARACTER FORMAT "X(25)":U 
     LABEL "Kontaktperson" 
     VIEW-AS FILL-IN 
     SIZE 26.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ORT AS CHARACTER FORMAT "X(40)":U 
     LABEL "Ort/Benämning" 
      VIEW-AS TEXT 
     SIZE 45.88 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-TEL AS CHARACTER FORMAT "X(11)":U 
     LABEL "Tel" 
     VIEW-AS FILL-IN 
     SIZE 13.25 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-B
     FILL-IN-FOR AT ROW 3.29 COL 21.5 COLON-ALIGNED
     FILL-IN-KADR AT ROW 4.58 COL 21.5 COLON-ALIGNED
     FILL-IN-BOX AT ROW 4.58 COL 49.5 COLON-ALIGNED
     FILL-IN-KPNR AT ROW 5.88 COL 21.5 COLON-ALIGNED
     FILL-IN-KORT AT ROW 5.88 COL 36.25 COLON-ALIGNED
     FILL-IN-FAX AT ROW 7.17 COL 21.5 COLON-ALIGNED
     FBTN_VISA AT ROW 8 COL 71.38
     FILL-IN-KIKONTAKT AT ROW 8.38 COL 21.5 COLON-ALIGNED
     FILL-IN-KITELE AT ROW 8.38 COL 54.25 COLON-ALIGNED
     FILL-IN-FOR2 AT ROW 13.42 COL 21.63 COLON-ALIGNED
     FILL-IN-KADR2 AT ROW 14.83 COL 21.63 COLON-ALIGNED
     FILL-IN-KPNR2 AT ROW 16.25 COL 21.63 COLON-ALIGNED
     FILL-IN-KORT2 AT ROW 16.25 COL 36.38 COLON-ALIGNED
     FILL-IN-LKONTAKT AT ROW 17.67 COL 21.63 COLON-ALIGNED
     FILL-IN-TEL AT ROW 17.67 COL 54.25 COLON-ALIGNED
     FILL-IN-DATUM AT ROW 19.08 COL 21.63 COLON-ALIGNED
     FILL-IN-DATUM2 AT ROW 19.08 COL 38.63 COLON-ALIGNED NO-LABEL
     FILL-IN-KOM AT ROW 20.58 COL 21.63 COLON-ALIGNED
     BTN_AVB AT ROW 20.58 COL 71.38
     FILL-IN-DEPA AT ROW 2 COL 21.5 COLON-ALIGNED
     FILL-IN-AONR AT ROW 10.58 COL 21.75 COLON-ALIGNED
     FILL-IN-DELNR AT ROW 10.58 COL 44.38 COLON-ALIGNED
     FILL-IN-ORT AT ROW 12 COL 21.63 COLON-ALIGNED
     "till" VIEW-AS TEXT
          SIZE 4.88 BY .96 AT ROW 19.17 COL 34.63
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 85.5 BY 21.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Leverans och transport"
         HEIGHT             = 21.08
         WIDTH              = 85.75
         MAX-HEIGHT         = 24.42
         MAX-WIDTH          = 96.88
         VIRTUAL-HEIGHT     = 24.42
         VIRTUAL-WIDTH      = 96.88
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = ?
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
/* SETTINGS FOR WINDOW WINDOW-2
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-2
ON CHOOSE OF BTN_AVB IN FRAME FRAME-B /* Avsluta */
DO:
   APPLY "LEAVE" TO FILL-IN-FOR.
   RUN skapa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA WINDOW-2
ON CHOOSE OF FBTN_VISA IN FRAME FRAME-B /* Visa */
DO:           
   {muswait.i}  
   IF musz = TRUE THEN musz = FALSE.   
   ASSIGN    
   skrivut = FALSE.
   APPLY "LEAVE" TO FILL-IN-FOR.
   RUN skapa_UI.                 
   {AVBGOM.I}
   IF foljesedel = TRUE THEN RUN VISAFOLJV.W.
   ELSE RUN VISAUTV.W.
   {AVBFRAM.I}
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DATUM WINDOW-2
ON MOUSE-MENU-CLICK OF FILL-IN-DATUM IN FRAME FRAME-B /* Uttagsdatum */
DO:
   ASSIGN  
   Guru.GlobalaVariabler:regdatum = TODAY.
   RUN AlmanBtn.w.
   FILL-IN-DATUM = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-DATUM WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DATUM2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DATUM2 WINDOW-2
ON MOUSE-MENU-CLICK OF FILL-IN-DATUM2 IN FRAME FRAME-B
DO:
   ASSIGN  
   Guru.GlobalaVariabler:regdatum = TODAY.
   RUN AlmanBtn.w.
   FILL-IN-DATUM2 = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-DATUM2 WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FOR WINDOW-2
ON LEAVE OF FILL-IN-FOR IN FRAME FRAME-B /* Företag */
DO:
   ASSIGN
   FILL-IN-FOR = INPUT FILL-IN-FOR 
   FILL-IN-KADR = INPUT FILL-IN-KADR
   FILL-IN-KPNR = INPUT FILL-IN-KPNR
   FILL-IN-KORT = INPUT FILL-IN-KORT
   FILL-IN-BOX = INPUT FILL-IN-BOX
   FILL-IN-FAX = INPUT FILL-IN-FAX
   FILL-IN-KIKONTAKT = INPUT FILL-IN-KIKONTAKT                        
   FILL-IN-KITELE = INPUT FILL-IN-KITELE            
   FILL-IN-LKONTAKT = INPUT FILL-IN-LKONTAKT
   FILL-IN-TEL = INPUT FILL-IN-TEL            
   FILL-IN-KOM = INPUT FILL-IN-KOM
   FILL-IN-FOR2 = INPUT FILL-IN-FOR2
   FILL-IN-KADR2 = INPUT FILL-IN-KADR2
   FILL-IN-KPNR2 = INPUT FILL-IN-KPNR2
   FILL-IN-KORT2 = INPUT FILL-IN-KORT2
   FILL-IN-DATUM = INPUT FILL-IN-DATUM
   FILL-IN-DATUM2 = INPUT FILL-IN-DATUM2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-2 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   IF VALID-HANDLE(mtrlbapph) THEN DELETE PROCEDURE mtrlbapph.
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
   {muswait.i}
   {ALLSTARTDYN.I}
   FIND FIRST depatemp WHERE depatemp.DEP-NR = vald_depa NO-LOCK NO-ERROR.
   FIND FIRST skapa_mtrl NO-LOCK NO-ERROR.
   IF NOT AVAILABLE skapa_mtrl THEN DO:
      ASSIGN  
      FILL-IN-DEPA = depatemp.BENAMNING
      FILL-IN-FOR = depatemp.FIRMA
      FILL-IN-KADR = depatemp.ADRESS
      FILL-IN-KPNR = depatemp.PNR
      FILL-IN-KORT = depatemp.ORT   
      FILL-IN-FAX = depatemp.FAXNR
      FILL-IN-DATUM = TODAY
      FILL-IN-DATUM2 = TODAY.
      FIND FIRST gam_mtrl NO-LOCK NO-ERROR.
      IF AVAILABLE gam_mtrl THEN DO:
         ASSIGN
         FILL-IN-DATUM = gam_mtrl.DATUM
         FILL-IN-DATUM2 = gam_mtrl.DATUM2
         FILL-IN-KOM = gam_mtrl.KOM.   
         DELETE gam_mtrl.
      END.   
      RUN uppghmt_UI IN mtrlbapph (INPUT Guru.Konstanter:globanv,INPUT valaonr,INPUT valdelnr,OUTPUT kinamn, OUTPUT ktele,OUTPUT bnamn,
                                   OUTPUT bkontakt,OUTPUT btel,OUTPUT badr,OUTPUT bpnr,OUTPUT bort).             
      ASSIGN
      FILL-IN-KIKONTAKT = kinamn 
      FILL-IN-KITELE = ktele                           
      FILL-IN-AONR = valaonr
      FILL-IN-DELNR = valdelnr 
      FILL-IN-ORT = valort
      FILL-IN-FOR2 = bnamn
      FILL-IN-LKONTAKT = bkontakt 
      FILL-IN-TEL = btel
      FILL-IN-KADR2 = badr
      FILL-IN-KPNR2 = bpnr
      FILL-IN-KORT2 = bort.                     
   END.   
   ELSE DO:
      ASSIGN
      FILL-IN-FOR = skapa_mtrl.FORE
      FILL-IN-KADR = skapa_mtrl.KADR
      FILL-IN-KPNR = skapa_mtrl.KPNR
      FILL-IN-KORT = skapa_mtrl.KORT
      FILL-IN-BOX = skapa_mtrl.BOX
      FILL-IN-FAX = skapa_mtrl.FAX
      FILL-IN-KIKONTAKT = skapa_mtrl.KIKONTAKT                        
      FILL-IN-KITELE = skapa_mtrl.KITELE
      FILL-IN-FOR2 = skapa_mtrl.BESTNAMN
      FILL-IN-KADR2 = skapa_mtrl.ADR
      FILL-IN-KPNR2 = skapa_mtrl.PNR
      FILL-IN-KORT2 = skapa_mtrl.ORT      
      FILL-IN-LKONTAKT = skapa_mtrl.KONTAKT
      FILL-IN-TEL = skapa_mtrl.TELE            
      FILL-IN-KOM = skapa_mtrl.KOM
      FILL-IN-DATUM = skapa_mtrl.DATUM
      FILL-IN-DATUM2 = skapa_mtrl.DATUM2
      FILL-IN-AONR = valaonr
      FILL-IN-DELNR = valdelnr 
      FILL-IN-ORT = valort.                      
   END.      
   FILL-IN-AONR:LABEL = Guru.Konstanter:gaok.
   FILL-IN-ORT:LABEL = Guru.Konstanter:gaonamnk.
   &Scoped-define FORMATNAMN FILL-IN-AONR   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN-DELNR   
   {DELNRFORMAT.I}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-2 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/       
   IF Guru.Konstanter:appcon THEN DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph.
   END.   
END PROCEDURE.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-2  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
  THEN DELETE WIDGET WINDOW-2.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-2  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-FOR FILL-IN-KADR FILL-IN-BOX FILL-IN-KPNR FILL-IN-KORT 
          FILL-IN-FAX FILL-IN-KIKONTAKT FILL-IN-KITELE FILL-IN-FOR2 
          FILL-IN-KADR2 FILL-IN-KPNR2 FILL-IN-KORT2 FILL-IN-LKONTAKT FILL-IN-TEL 
          FILL-IN-DATUM FILL-IN-DATUM2 FILL-IN-KOM FILL-IN-DEPA FILL-IN-AONR 
          FILL-IN-DELNR FILL-IN-ORT 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  ENABLE FILL-IN-FOR FILL-IN-KADR FILL-IN-BOX FILL-IN-KPNR FILL-IN-KORT 
         FILL-IN-FAX FBTN_VISA FILL-IN-KIKONTAKT FILL-IN-KITELE FILL-IN-FOR2 
         FILL-IN-KADR2 FILL-IN-KPNR2 FILL-IN-KORT2 FILL-IN-LKONTAKT FILL-IN-TEL 
         FILL-IN-DATUM FILL-IN-DATUM2 FILL-IN-KOM BTN_AVB FILL-IN-DEPA 
         FILL-IN-AONR FILL-IN-DELNR FILL-IN-ORT 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skapa_UI WINDOW-2 
PROCEDURE skapa_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   EMPTY TEMP-TABLE skapa_mtrl NO-ERROR.    
   CREATE skapa_mtrl.
   ASSIGN      
   skapa_mtrl.FORE = FILL-IN-FOR
   skapa_mtrl.KADR = FILL-IN-KADR
   skapa_mtrl.KPNR = FILL-IN-KPNR
   skapa_mtrl.KORT = FILL-IN-KORT
   skapa_mtrl.BOX = FILL-IN-BOX
   skapa_mtrl.FAX = FILL-IN-FAX
   skapa_mtrl.KIKONTAKT = FILL-IN-KIKONTAKT                        
   skapa_mtrl.KITELE = FILL-IN-KITELE            
   skapa_mtrl.KONTAKT = FILL-IN-LKONTAKT
   skapa_mtrl.TELE = FILL-IN-TEL            
   skapa_mtrl.KOM = FILL-IN-KOM
   skapa_mtrl.BESTNAMN = FILL-IN-FOR2
   skapa_mtrl.ADR = FILL-IN-KADR2 
   skapa_mtrl.PNR = FILL-IN-KPNR2
   skapa_mtrl.ORT = FILL-IN-KORT2
   skapa_mtrl.DATUM = FILL-IN-DATUM
   skapa_mtrl.DATUM2 = FILL-IN-DATUM2.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

