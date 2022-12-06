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
{GLOBVAR2DEL1.I}
DEFINE NEW SHARED VARIABLE detvar AS LOGICAL NO-UNDO.       
DEFINE NEW SHARED VARIABLE alla AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE mailvar AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE edivar AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE andra AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kontaktrec AS RECID NO-UNDO. 
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE huvudlev AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE vald_kundlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.   
DEFINE SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.

DEFINE VARIABLE mtrlbapph AS HANDLE NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE mtrl_recid AS RECID NO-UNDO.
DEFINE VARIABLE leverant AS CHARACTER NO-UNDO.   
DEFINE VARIABLE leve AS CHARACTER NO-UNDO.   
DEFINE VARIABLE omra AS CHARACTER NO-UNDO.   
DEFINE VARIABLE valomra AS CHARACTER NO-UNDO.  
DEFINE VARIABLE val AS LOGICAL NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE levtrprec AS RECID NO-UNDO.
DEFINE VARIABLE bestoff AS CHARACTER NO-UNDO.
DEFINE VARIABLE nyttnr AS INTEGER NO-UNDO.
DEFINE VARIABLE ediben AS CHARACTER NO-UNDO.
DEFINE VARIABLE forsta AS LOGICAL NO-UNDO.
DEFINE VARIABLE knr AS INTEGER NO-UNDO.
DEFINE VARIABLE namn AS CHARACTER NO-UNDO.
DEFINE VARIABLE telf AS CHARACTER NO-UNDO.
DEFINE VARIABLE mtelf AS CHARACTER NO-UNDO.


&Scoped-define NEW NEW 
&Scoped-define SHARED SHARED
{SKAPAMTRL.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{LEVTEMP.I}      
{DEPATEMP.I}      

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-B

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CMB_DEPA FILL-IN-KUNDNR FBTN_OMAIL ~
FILL-IN-FOR FILL-IN-KADR FILL-IN-BOX FILL-IN-KPNR FILL-IN-KORT FILL-IN-FAX ~
FILL-IN-KIKONTAKT FILL-IN-KITELE FILL-IN-KIMOBIL BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS CMB_DEPA FILL-IN-KUNDNR FILL-IN-FOR ~
FILL-IN-KADR FILL-IN-BOX FILL-IN-KPNR FILL-IN-KORT FILL-IN-FAX ~
FILL-IN-KIKONTAKT FILL-IN-KITELE FILL-IN-KIMOBIL 

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

DEFINE BUTTON FBTN_OMAIL 
     LABEL "Offert via Epost" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE VARIABLE CMB_DEPA AS CHARACTER FORMAT "X(40)":U 
     LABEL "Depåadresser" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 21.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BOX AS CHARACTER FORMAT "X(4)":U 
     LABEL "Box" 
     VIEW-AS FILL-IN 
     SIZE 13.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FAX AS CHARACTER FORMAT "X(12)":U 
     LABEL "Fax" 
     VIEW-AS FILL-IN 
     SIZE 13.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FOR AS CHARACTER FORMAT "X(35)":U 
     LABEL "Företag" 
     VIEW-AS FILL-IN 
     SIZE 35.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KADR AS CHARACTER FORMAT "X(30)":U 
     LABEL "Adress" 
     VIEW-AS FILL-IN 
     SIZE 32.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KIKONTAKT AS CHARACTER FORMAT "X(25)":U 
     LABEL "Kontaktperson inköp" 
     VIEW-AS FILL-IN 
     SIZE 26.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KIMOBIL AS CHARACTER FORMAT "X(15)":U 
     LABEL "Mobiltel" 
     VIEW-AS FILL-IN 
     SIZE 13.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KITELE AS CHARACTER FORMAT "X(12)":U 
     LABEL "Tel" 
     VIEW-AS FILL-IN 
     SIZE 13.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KORT AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ort" 
     VIEW-AS FILL-IN 
     SIZE 16.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KPNR AS CHARACTER FORMAT "999 99":U 
     LABEL "Postnr" 
     VIEW-AS FILL-IN 
     SIZE 7.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KUNDNR AS INTEGER FORMAT "99999":U INITIAL 0 
     LABEL "Kundnummer" 
     VIEW-AS FILL-IN 
     SIZE 15.25 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-B
     CMB_DEPA AT ROW 1.5 COL 22.5 COLON-ALIGNED
     FILL-IN-KUNDNR AT ROW 2.75 COL 22.5 COLON-ALIGNED
     FBTN_OMAIL AT ROW 3.5 COL 77.13
     FILL-IN-FOR AT ROW 4.04 COL 22.5 COLON-ALIGNED
     FILL-IN-KADR AT ROW 5.33 COL 22.5 COLON-ALIGNED
     FILL-IN-BOX AT ROW 5.33 COL 60.38 COLON-ALIGNED
     FILL-IN-KPNR AT ROW 6.63 COL 22.5 COLON-ALIGNED
     FILL-IN-KORT AT ROW 6.63 COL 37.25 COLON-ALIGNED
     FILL-IN-FAX AT ROW 6.63 COL 60.38 COLON-ALIGNED
     FILL-IN-KIKONTAKT AT ROW 7.92 COL 22.5 COLON-ALIGNED
     FILL-IN-KITELE AT ROW 7.92 COL 60.38 COLON-ALIGNED
     FILL-IN-KIMOBIL AT ROW 9.25 COL 22.5 COLON-ALIGNED
     BTN_AVB AT ROW 10.5 COL 77.13
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 91.38 BY 10.96.


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
         HEIGHT             = 11
         WIDTH              = 91.5
         MAX-HEIGHT         = 25.75
         MAX-WIDTH          = 99.38
         VIRTUAL-HEIGHT     = 25.75
         VIRTUAL-WIDTH      = 99.38
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
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_DEPA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_DEPA WINDOW-2
ON VALUE-CHANGED OF CMB_DEPA IN FRAME FRAME-B /* Depåadresser */
DO:
   ASSIGN                                               
   CMB_DEPA = INPUT CMB_DEPA.                
   FIND FIRST depatemp WHERE depatemp.BENAMNING = CMB_DEPA NO-LOCK NO-ERROR. 
   IF AVAILABLE depatemp THEN DO:
      ASSIGN      
      FILL-IN-KADR:SCREEN-VALUE = depatemp.ADRESS  
      FILL-IN-KPNR:SCREEN-VALUE = depatemp.PNR
      FILL-IN-KORT:SCREEN-VALUE = depatemp.ORT
      FILL-IN-FAX:SCREEN-VALUE = depatemp.FAXNR.
      RUN kundhmt2_UI IN mtrlbapph (INPUT depatemp.DEP-NR ,INPUT levtemp.LEVKOD ,INPUT Guru.Konstanter:globanv ,OUTPUT knr , OUTPUT namn , OUTPUT telf, OUTPUT mtelf ).     
      ASSIGN FILL-IN-KUNDNR = knr.   
      /*FIND FIRST KUNDNR WHERE KUNDNR.DEP-NR = depatemp.DEP-NR AND KUNDNR.LEVKOD = levtemp.LEVKOD NO-LOCK NO-ERROR.
      IF AVAILABLE KUNDNR THEN ASSIGN FILL-IN-KUNDNR = KUNDNR.KUNDNR.
      ELSE ASSIGN FILL-IN-KUNDNR = 0.                                */
      DISPLAY FILL-IN-KUNDNR WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_OMAIL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_OMAIL WINDOW-2
ON CHOOSE OF FBTN_OMAIL IN FRAME FRAME-B /* Offert via Epost */
DO:           
   {muswait.i}  
   IF musz = TRUE THEN musz = FALSE.   
   ASSIGN    
   skrivut = FALSE    
   detvar = TRUE.                 
   IF (LENGTH(INPUT FILL-IN-KUNDNR)> 5 OR INPUT FILL-IN-KUNDNR = "") THEN DO:
      MESSAGE "Felaktigt kundnummer."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-KUNDNR IN FRAME {&FRAME-NAME}.  
      RETURN. 
   END.   
   APPLY "LEAVE" TO FILL-IN-FOR IN FRAME {&FRAME-NAME}.
   RUN skapamtrl_UI.                
   ASSIGN 
   bestoff = "Offert"
   mailvar = TRUE
   edivar = FALSE.        
   {AVBGOM.I}
   RUN ELEFOFF2V.W (INPUT bestoff).
   {AVBFRAM.I}
   mailvar = FALSE.     
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FOR WINDOW-2
ON LEAVE OF FILL-IN-FOR IN FRAME FRAME-B /* Företag */
DO:
   ASSIGN
   FILL-IN-KUNDNR = INPUT FILL-IN-KUNDNR 
   FILL-IN-FOR = INPUT FILL-IN-FOR 
   FILL-IN-KADR = INPUT FILL-IN-KADR 
   FILL-IN-KPNR = INPUT FILL-IN-KPNR 
   FILL-IN-BOX = INPUT FILL-IN-BOX 
   FILL-IN-KORT = INPUT FILL-IN-KORT 
   FILL-IN-FAX = INPUT FILL-IN-FAX 
   FILL-IN-KIKONTAKT = INPUT FILL-IN-KIKONTAKT 
   FILL-IN-KITELE = INPUT FILL-IN-KITELE 
   FILL-IN-KIMOBIL = INPUT FILL-IN-KIMOBIL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-KUNDNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KUNDNR WINDOW-2
ON LEAVE OF FILL-IN-KUNDNR IN FRAME FRAME-B /* Kundnummer */
DO:
  FILL-IN-KUNDNR = INPUT FILL-IN-KUNDNR.
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
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

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
   ASSIGN
   mailvar = FALSE
   edivar = FALSE
   forsta = TRUE. 
   FIND FIRST levtemp WHERE levtemp.LEVNAMN = "Onninen" NO-LOCK NO-ERROR.
   IF Guru.Konstanter:globforetag = "ELPA" THEN DO:     
      ediben = "EP.".
   END.
   ELSE IF Guru.Konstanter:globforetag = "VSYD" THEN DO:
      ediben = "SP.".
   END.
   ELSE IF Guru.Konstanter:globforetag = "VAST" THEN DO:
      ediben = "VP.".
   END.
   ELSE IF Guru.Konstanter:globforetag = "VORD" THEN DO:
      ediben = "NP.".
   END.   
   {&WINDOW-NAME}:TITLE = "Offert till Onninen".
                            
   status-ok = CMB_DEPA:DELETE("0"). 
   FOR EACH depatemp BY depatemp.BENAMNING:      
      ASSIGN
      status-ok = CMB_DEPA:ADD-LAST(depatemp.BENAMNING)IN FRAME {&FRAME-NAME}.                     
   END.       
   FIND FIRST depatemp NO-LOCK NO-ERROR.
   ASSIGN CMB_DEPA:SCREEN-VALUE = depatemp.BENAMNING.             
   IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
      FILL-IN-FOR = "Elpool i Umeå AB".
   END.                  
   IF Guru.Konstanter:globforetag = "SOLE"   THEN DO:
      FILL-IN-FOR = "Graninge Nät AB".
   END.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      FILL-IN-FOR = "Sundsvall Energi Elnät AB".
   END.
   IF Guru.Konstanter:globforetag = "VORD"  OR
   Guru.Konstanter:globforetag = "VAST"  THEN DO:
      FILL-IN-FOR = "Vattenfall Elnätservice AB".   
   END.   
   
   RUN kundhmt2_UI IN mtrlbapph (INPUT depatemp.DEP-NR ,INPUT levtemp.LEVKOD ,INPUT Guru.Konstanter:globanv ,OUTPUT knr , OUTPUT namn , OUTPUT telf, OUTPUT mtelf ).     
   ASSIGN FILL-IN-KUNDNR = knr   
   FILL-IN-KIKONTAKT = namn
   FILL-IN-KITELE = telf
   FILL-IN-KIMOBIL = mtelf.   

   /*FIND FIRST KUNDNR WHERE KUNDNR.DEP-NR = depatemp.DEP-NR AND KUNDNR.LEVKOD = levtemp.LEVKOD NO-LOCK NO-ERROR.
   IF AVAILABLE KUNDNR THEN ASSIGN FILL-IN-KUNDNR = KUNDNR.KUNDNR.
   ELSE ASSIGN FILL-IN-KUNDNR = 0.  
   FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = Guru.Konstanter:globanv NO-LOCK NO-ERROR.
   FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = ANVANDARE.PERSONALKOD NO-LOCK NO-ERROR.
   IF AVAILABLE PERSONALTAB THEN DO:
      ASSIGN
      FILL-IN-KIKONTAKT = PERSONALTAB.FORNAMN + " " + PERSONALTAB.EFTERNAMN
      FILL-IN-KITELE = PERSONALTAB.TELEFON      
      FILL-IN-KIMOBIL = PERSONALTAB.MOBILTEL.                          
    END.      */
   RUN enable_UI.   
   {FRMSIZE.I}       
   {musarrow.i}
    APPLY "VALUE-CHANGED" TO CMB_DEPA. 
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY CMB_DEPA FILL-IN-KUNDNR FILL-IN-FOR FILL-IN-KADR FILL-IN-BOX 
          FILL-IN-KPNR FILL-IN-KORT FILL-IN-FAX FILL-IN-KIKONTAKT FILL-IN-KITELE 
          FILL-IN-KIMOBIL 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  ENABLE CMB_DEPA FILL-IN-KUNDNR FBTN_OMAIL FILL-IN-FOR FILL-IN-KADR 
         FILL-IN-BOX FILL-IN-KPNR FILL-IN-KORT FILL-IN-FAX FILL-IN-KIKONTAKT 
         FILL-IN-KITELE FILL-IN-KIMOBIL BTN_AVB 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skapamtrl_UI WINDOW-2 
PROCEDURE skapamtrl_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
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
   skapa_mtrl.KIMOBIL = FILL-IN-KIMOBIL
   skapa_mtrl.KUNDNR = FILL-IN-KUNDNR.     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

