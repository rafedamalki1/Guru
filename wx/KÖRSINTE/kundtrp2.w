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
   
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{BESTKUNDALLT.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{OMRTEMPW.I}
{GLOBVAR2DEL1.I}
{SOKMTRL.I}
DEFINE NEW SHARED VARIABLE vald_kund AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE kundoffproch AS HANDLE NO-UNDO. /* KUNDOFFAPP.P */
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.   
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.

DEFINE VARIABLE val AS LOGICAL NO-UNDO.  



/*{EGENBEN.I}*/
DEFINE VARIABLE persproch AS HANDLE NO-UNDO.     /* PERSONALAPP.P */
   {JURPERST.I}
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-B

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-KUNDNAMN FILL-IN-LKONTAKT ~
FILL-IN-LTELE FILL-IN-LADR FILL-IN-LPNR FILL-IN-LORT FBTN_VISA CMB_LEV ~
CMB_OMR FBTN_SKRIV FILL-IN-FOR FILL-IN-KADR FILL-IN-BOX FILL-IN-KPNR ~
FILL-IN-KORT FILL-IN-FAX FILL-IN-KIKONTAKT FILL-IN-KITELE ED_ANM BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-KUNDNAMN FILL-IN-LKONTAKT ~
FILL-IN-LTELE FILL-IN-LADR FILL-IN-LPNR FILL-IN-LORT CMB_LEV CMB_OMR ~
FILL-IN-FOR FILL-IN-KADR FILL-IN-BOX FILL-IN-KPNR FILL-IN-KORT FILL-IN-FAX ~
FILL-IN-KIKONTAKT FILL-IN-KITELE ED_ANM 

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

DEFINE BUTTON FBTN_SKRIV 
     LABEL "Skriv ut" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE VARIABLE CMB_LEV AS CHARACTER FORMAT "X(256)":U 
     LABEL "Beställare" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Områden" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE ED_ANM AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 250 SCROLLBAR-VERTICAL
     SIZE 53 BY 4.75
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-BOX AS CHARACTER FORMAT "X(256)":U 
     LABEL "Box" 
     VIEW-AS FILL-IN 
     SIZE 7.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FAX AS CHARACTER FORMAT "X(12)":U 
     LABEL "Fax" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FOR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Företag" 
     VIEW-AS FILL-IN 
     SIZE 26.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KADR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Adress" 
     VIEW-AS FILL-IN 
     SIZE 21.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KIKONTAKT AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kontaktperson" 
     VIEW-AS FILL-IN 
     SIZE 26.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KITELE AS CHARACTER FORMAT "X(12)":U 
     LABEL "Tel" 
     VIEW-AS FILL-IN 
     SIZE 15.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KORT AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ort" 
     VIEW-AS FILL-IN 
     SIZE 16.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KPNR AS CHARACTER FORMAT "999 99":U 
     LABEL "Postnr" 
     VIEW-AS FILL-IN 
     SIZE 7.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KUNDNAMN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kund" 
     VIEW-AS FILL-IN 
     SIZE 26.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LADR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Adress" 
     VIEW-AS FILL-IN 
     SIZE 21.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LKONTAKT AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kontaktperson" 
     VIEW-AS FILL-IN 
     SIZE 26.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LORT AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ort" 
     VIEW-AS FILL-IN 
     SIZE 16.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LPNR AS CHARACTER FORMAT "999 99":U 
     LABEL "Postnr" 
     VIEW-AS FILL-IN 
     SIZE 7.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LTELE AS CHARACTER FORMAT "X(14)":U 
     LABEL "Tel" 
     VIEW-AS FILL-IN 
     SIZE 15.5 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-B
     FILL-IN-KUNDNAMN AT ROW 1.5 COL 15 COLON-ALIGNED
     FILL-IN-LKONTAKT AT ROW 3 COL 15 COLON-ALIGNED
     FILL-IN-LTELE AT ROW 3 COL 46.25 COLON-ALIGNED
     FILL-IN-LADR AT ROW 4.5 COL 15 COLON-ALIGNED
     FILL-IN-LPNR AT ROW 6 COL 15 COLON-ALIGNED
     FILL-IN-LORT AT ROW 6 COL 46.25 COLON-ALIGNED
     FBTN_VISA AT ROW 8 COL 71
     CMB_LEV AT ROW 8.04 COL 15 COLON-ALIGNED
     CMB_OMR AT ROW 8.04 COL 46.25 COLON-ALIGNED
     FBTN_SKRIV AT ROW 9.08 COL 71
     FILL-IN-FOR AT ROW 10.08 COL 15 COLON-ALIGNED
     FILL-IN-KADR AT ROW 11.58 COL 15 COLON-ALIGNED
     FILL-IN-BOX AT ROW 11.58 COL 46.25 COLON-ALIGNED
     FILL-IN-KPNR AT ROW 13.08 COL 15 COLON-ALIGNED
     FILL-IN-KORT AT ROW 13.08 COL 46.25 COLON-ALIGNED
     FILL-IN-FAX AT ROW 14.58 COL 15 COLON-ALIGNED
     FILL-IN-KIKONTAKT AT ROW 16.08 COL 15 COLON-ALIGNED
     FILL-IN-KITELE AT ROW 16.08 COL 46.25 COLON-ALIGNED
     ED_ANM AT ROW 18.67 COL 17.13 NO-LABEL
     BTN_AVB AT ROW 22.42 COL 71
     "Kommentarer:" VIEW-AS TEXT
          SIZE 15.88 BY .67 AT ROW 17.83 COL 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 85.5 BY 22.83.


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
         TITLE              = "Kundoffert"
         HEIGHT             = 22.88
         WIDTH              = 85.75
         MAX-HEIGHT         = 24.88
         MAX-WIDTH          = 96.88
         VIRTUAL-HEIGHT     = 24.88
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
   IF VALID-HANDLE(persproch) THEN DELETE PROCEDURE persproch NO-ERROR.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEV WINDOW-2
ON VALUE-CHANGED OF CMB_LEV IN FRAME FRAME-B /* Beställare */
DO:                                        
   CMB_LEV = INPUT CMB_LEV.                
   FIND FIRST bestkundallt WHERE bestkundallt.BESTNAMN = CMB_LEV NO-LOCK NO-ERROR.
   vald_kund = bestkundallt.BESTID.
   ASSIGN
   FILL-IN-KUNDNAMN:SCREEN-VALUE IN FRAME {&FRAME-NAME} = bestkundallt.BESTNAMN
   FILL-IN-LKONTAKT:SCREEN-VALUE = bestkundallt.KONTAKT
   FILL-IN-LTELE:SCREEN-VALUE = bestkundallt.TEL
   FILL-IN-LADR:SCREEN-VALUE = bestkundallt.ADRESS
   FILL-IN-LPNR:SCREEN-VALUE = bestkundallt.PNR
   FILL-IN-LORT:SCREEN-VALUE = bestkundallt.ORT.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OMR WINDOW-2
ON VALUE-CHANGED OF CMB_OMR IN FRAME FRAME-B /* Områden */
DO:

   ASSIGN                                               
   CMB_OMR = INPUT CMB_OMR.                
   FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR NO-LOCK NO-ERROR.
   RUN omrhj_UI IN kundoffproch (INPUT omrtemp.OMRADE,
                                 OUTPUT FILL-IN-FOR,
                                 OUTPUT FILL-IN-KADR,
                                 OUTPUT FILL-IN-KPNR,
                                 OUTPUT FILL-IN-KORT). 
   DISPLAY  FILL-IN-FOR FILL-IN-KADR FILL-IN-KPNR FILL-IN-KORT WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-2
ON CHOOSE OF FBTN_SKRIV IN FRAME FRAME-B /* Skriv ut */
DO: 
   RUN SKRIVVAL.W (INPUT FALSE).       
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO: 
      {muswait.i}
      EMPTY TEMP-TABLE skapa_mtrl NO-ERROR. 
      CREATE skapa_mtrl.
      ASSIGN
      skapa_mtrl.LEVNAMN = INPUT FILL-IN-KUNDNAMN
      skapa_mtrl.LKONTAKT = INPUT FILL-IN-LKONTAKT
      skapa_mtrl.LTELE = INPUT FILL-IN-LTELE
      skapa_mtrl.LADR = INPUT FILL-IN-LADR
      skapa_mtrl.LPNR = INPUT FILL-IN-LPNR
      skapa_mtrl.LORT = INPUT FILL-IN-LORT
      skapa_mtrl.FORE = INPUT FILL-IN-FOR
      skapa_mtrl.KADR = INPUT FILL-IN-KADR
      skapa_mtrl.KPNR = INPUT FILL-IN-KPNR
      skapa_mtrl.KORT = INPUT FILL-IN-KORT
      skapa_mtrl.BOX = INPUT FILL-IN-BOX
      skapa_mtrl.FAX = INPUT FILL-IN-FAX
      skapa_mtrl.KIKONTAKT = INPUT FILL-IN-KIKONTAKT                        
      skapa_mtrl.KITELE = INPUT FILL-IN-KITELE               
      skapa_mtrl.KOM = INPUT ED_ANM                
      skrivut = TRUE.
      {AVBGOM.I}
      RUN VISAKUND2.W (INPUT TABLE skapa_mtrl).
      {AVBFRAM.I}
   END.     
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-2
ON MOUSE-MENU-CLICK OF FBTN_SKRIV IN FRAME FRAME-B /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA WINDOW-2
ON CHOOSE OF FBTN_VISA IN FRAME FRAME-B /* Visa */
DO: 
   IF musz = TRUE THEN musz = FALSE.
   {muswait.i}   
   ASSIGN    
   skrivut = FALSE. 
   EMPTY TEMP-TABLE skapa_mtrl NO-ERROR. 
   CREATE skapa_mtrl.
   ASSIGN
   skapa_mtrl.LEVNAMN = INPUT FILL-IN-KUNDNAMN
   skapa_mtrl.LKONTAKT = INPUT FILL-IN-LKONTAKT
   skapa_mtrl.LTELE = INPUT FILL-IN-LTELE
   skapa_mtrl.LADR = INPUT FILL-IN-LADR
   skapa_mtrl.LPNR = INPUT FILL-IN-LPNR
   skapa_mtrl.LORT = INPUT FILL-IN-LORT
   skapa_mtrl.FORE = INPUT FILL-IN-FOR
   skapa_mtrl.KADR = INPUT FILL-IN-KADR
   skapa_mtrl.KPNR = INPUT FILL-IN-KPNR
   skapa_mtrl.KORT = INPUT FILL-IN-KORT
   skapa_mtrl.BOX = INPUT FILL-IN-BOX
   skapa_mtrl.FAX = INPUT FILL-IN-FAX
   skapa_mtrl.KIKONTAKT = INPUT FILL-IN-KIKONTAKT                        
   skapa_mtrl.KITELE = INPUT FILL-IN-KITELE               
   skapa_mtrl.KOM = INPUT ED_ANM. 
   {AVBGOM.I}
   RUN VISAKUND2.W (INPUT TABLE skapa_mtrl).
   {AVBFRAM.I}
   {musarrow.i}   
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
   IF Guru.Konstanter:appcon THEN DO:
      RUN PERSONALAPP.P PERSISTENT SET persproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.   
      
   END.
   ELSE DO:
      RUN PERSONALAPP.P PERSISTENT SET persproch.  
   END.
   RUN jurp_UI IN persproch (INPUT Guru.Konstanter:globanv,OUTPUT TABLE jurperstemp,OUTPUT TABLE judavdtemp).
   status-ok = CMB_LEV:DELETE("0").
   CMB_LEV:LABEL = Guru.Konstanter:gbestk.
   /*BESTSKAP.P val = 1*/
   FIND FIRST omrtemp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE omrtemp THEN DO:
      {OMRHMT.I}
   END.                                           
   FIND FIRST bestkundallt NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bestkundallt THEN DO:    
      bestvad = 1.
      {BESTHMT.I}      
   END.
   FOR EACH bestkundallt NO-LOCK.
      ASSIGN  
      status-ok = CMB_LEV:ADD-LAST(bestkundallt.BESTNAMN)IN FRAME {&FRAME-NAME}.            
   END.                                                   
   FIND FIRST bestkundallt NO-LOCK NO-ERROR.   
   ASSIGN
   CMB_LEV:SCREEN-VALUE = bestkundallt.BESTNAMN 
   CMB_OMR:LIST-ITEMS = "". 
   FOR EACH judavdtemp,         
   EACH omrtemp WHERE omrtemp.AVDELNINGNR = judavdtemp.AVDELNINGNR.
      status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN). 
      CMB_OMR:SCREEN-VALUE = omrtemp.NAMN.
   END.

   FIND FIRST omrtemp WHERE omrtemp.OMRADE = globomr
   NO-LOCK NO-ERROR. 
   IF NOT AVAILABLE omrtemp THEN DO:
      FIND FIRST omrtemp USE-INDEX OMR NO-LOCK NO-ERROR.
   END.
   CMB_OMR:LABEL = Guru.Konstanter:gomrk.   
   ASSIGN  
   CMB_OMR:SCREEN-VALUE = omrtemp.NAMN. 
   RUN anvpers_UI IN kundoffproch (INPUT Guru.Konstanter:globanv, OUTPUT FILL-IN-KIKONTAKT,OUTPUT FILL-IN-KITELE).  
   RUN enable_UI.   
   {FRMSIZE.I}   
   {musarrow.i}   
   {WIN_M_SLUT.I}
   APPLY "VALUE-CHANGED" TO CMB_LEV.  
   APPLY "VALUE-CHANGED" TO CMB_OMR.                                          
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY FILL-IN-KUNDNAMN FILL-IN-LKONTAKT FILL-IN-LTELE FILL-IN-LADR 
          FILL-IN-LPNR FILL-IN-LORT CMB_LEV CMB_OMR FILL-IN-FOR FILL-IN-KADR 
          FILL-IN-BOX FILL-IN-KPNR FILL-IN-KORT FILL-IN-FAX FILL-IN-KIKONTAKT 
          FILL-IN-KITELE ED_ANM 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  ENABLE FILL-IN-KUNDNAMN FILL-IN-LKONTAKT FILL-IN-LTELE FILL-IN-LADR 
         FILL-IN-LPNR FILL-IN-LORT FBTN_VISA CMB_LEV CMB_OMR FBTN_SKRIV 
         FILL-IN-FOR FILL-IN-KADR FILL-IN-BOX FILL-IN-KPNR FILL-IN-KORT 
         FILL-IN-FAX FILL-IN-KIKONTAKT FILL-IN-KITELE ED_ANM BTN_AVB 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

