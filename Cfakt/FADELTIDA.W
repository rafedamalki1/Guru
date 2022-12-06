&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
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

&SCOPED-DEFINE NEW 
{GLOBVAR2DEL1.I}
{FAKTTEMP.I}

{BEFTEMP.I}
DEFINE INPUT PARAMETER brwrec AS ROWID NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR bytpers.

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
DEFINE SHARED VARIABLE klocka AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE FILL-IN-TOMDAT AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.
DEFINE VARIABLE kollstart AS DECIMAL NO-UNDO.
DEFINE VARIABLE kollslut AS DECIMAL NO-UNDO.     
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE resber AS DECIMAL NO-UNDO.

DEFINE BUFFER sumbuff FOR sumtidtemp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BRW_BEF

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES befattningstemp

/* Definitions for BROWSE BRW_BEF                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_BEF befattningstemp.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BEF 
&Scoped-define QUERY-STRING-BRW_BEF FOR EACH befattningstemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_BEF OPEN QUERY BRW_BEF FOR EACH befattningstemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_BEF befattningstemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BEF befattningstemp


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BRW_BEF}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_BEF FILL-IN_START FILL-IN_LUNCH Btn_OK ~
Btn_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_PERSONALKOD FILL-IN_DATUM ~
FILL-IN_AONR FILL-IN_DELNR FILL-IN_START FILL-IN_SLUT FILL-IN_LUNCH ~
FILL-IN_VIBEFATTNING 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD klockan100 Dialog-Frame 
FUNCTION klockan100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_AVB 
     LABEL "Avbryt" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Ok" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN_AONR AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_DATUM AS DATE FORMAT "99/99/99" 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_DELNR AS INTEGER FORMAT ">99" INITIAL 0 
     LABEL "Delnr" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_LUNCH AS DECIMAL FORMAT ">9.99" INITIAL 0 
     LABEL "Lunch" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.

DEFINE VARIABLE FILL-IN_PERSONALKOD AS CHARACTER FORMAT "x(5)" 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.

DEFINE VARIABLE FILL-IN_SLUT AS DECIMAL FORMAT "99.99" INITIAL 0 
     LABEL "Slut" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.

DEFINE VARIABLE FILL-IN_START AS DECIMAL FORMAT "99.99" INITIAL 0 
     LABEL "Start" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.

DEFINE VARIABLE FILL-IN_VIBEFATTNING AS CHARACTER FORMAT "x(15)" 
     LABEL "Befattning" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_BEF FOR 
      befattningstemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_BEF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BEF Dialog-Frame _STRUCTURED
  QUERY BRW_BEF NO-LOCK DISPLAY
      befattningstemp.NAMN FORMAT "x(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 22.88 BY 6.96.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BRW_BEF AT ROW 2.25 COL 29.13
     FILL-IN_PERSONALKOD AT ROW 1.5 COL 12.88 COLON-ALIGNED
     FILL-IN_DATUM AT ROW 2.67 COL 12.88 COLON-ALIGNED
     FILL-IN_AONR AT ROW 3.92 COL 12.88 COLON-ALIGNED
     FILL-IN_DELNR AT ROW 5.08 COL 12.88 COLON-ALIGNED
     FILL-IN_START AT ROW 6.33 COL 12.88 COLON-ALIGNED
     FILL-IN_SLUT AT ROW 7.5 COL 12.88 COLON-ALIGNED
     FILL-IN_LUNCH AT ROW 8.67 COL 12.88 COLON-ALIGNED
     FILL-IN_VIBEFATTNING AT ROW 9.92 COL 12.88 COLON-ALIGNED HELP
          "BEFATTNINGSKOD"
     Btn_OK AT ROW 12.04 COL 23
     Btn_AVB AT ROW 12.04 COL 38
     SPACE(1.49) SKIP(0.12)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Dela upp tidposter"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_AVB.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Temp-Tables and Buffers:
      TABLE: befattningstemp T "?" NO-UNDO temp-db befattningstemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE Custom                                                   */
/* BROWSE-TAB BRW_BEF 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       BRW_BEF:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_AONR IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_DATUM IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_DELNR IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_PERSONALKOD IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_SLUT IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_VIBEFATTNING IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BEF
/* Query rebuild information for BROWSE BRW_BEF
     _TblList          = "Temp-Tables.befattningstemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.befattningstemp.NAMN
"befattningstemp.NAMN" ? ? "character" ? ? ? ? ? ? no "" no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_BEF */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON END-ERROR OF FRAME Dialog-Frame /* Dela upp tidposter */
DO:
   {BORTBRWPROC.I}
  APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON ENDKEY OF FRAME Dialog-Frame /* Dela upp tidposter */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Dela upp tidposter */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_BEF
&Scoped-define SELF-NAME BRW_BEF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_BEF Dialog-Frame
ON VALUE-CHANGED OF BRW_BEF IN FRAME Dialog-Frame
DO:
    status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
    FILL-IN_VIBEFATTNING = befattningstemp.NAMN.
    DISPLAY FILL-IN_VIBEFATTNING WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_AVB Dialog-Frame
ON CHOOSE OF Btn_AVB IN FRAME Dialog-Frame /* Avbryt */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Ok */
DO:
   ASSIGN
   FILL-IN_AONR = INPUT FRAME {&FRAME-NAME} FILL-IN_AONR
   FILL-IN_DATUM = INPUT FILL-IN_DATUM
   FILL-IN_DELNR = INPUT FILL-IN_DELNR
   FILL-IN_LUNCH = INPUT FILL-IN_LUNCH
   FILL-IN_PERSONALKOD = INPUT FILL-IN_PERSONALKOD
   FILL-IN_SLUT = INPUT FILL-IN_SLUT
   FILL-IN_START = INPUT FILL-IN_START
   FILL-IN_VIBEFATTNING = INPUT FILL-IN_VIBEFATTNING.
   IF FILL-IN_SLUT <= FILL-IN_START THEN DO:
      MESSAGE "Sluttiden kan inte vara mindre än starttiden! "  VIEW-AS ALERT-BOX.      
      RETURN NO-APPLY.
   END.
   IF FILL-IN_SLUT > 24.00 THEN DO:
      MESSAGE "Klockan kan inte vara större än 24.00!" VIEW-AS ALERT-BOX.      
      RETURN NO-APPLY.
   END.
   IF FILL-IN_START > 24.00 THEN DO:
      MESSAGE "Klockan kan inte vara större än 24.00!" VIEW-AS ALERT-BOX.      
      RETURN NO-APPLY.
   END.       
   RUN klar_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.
   {BORTBRWPROC.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SLUT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SLUT Dialog-Frame
ON MOUSE-MENU-CLICK OF FILL-IN_SLUT IN FRAME Dialog-Frame /* Slut */
DO:
   klocka = INPUT FILL-IN_SLUT.
   
   {AVBGOMD.I}
   RUN KLOCKAN.W.
   {AVBFRAMD.I}
   
   IF klocka = 00.00 THEN klocka = 24.00.
   FILL-IN_SLUT = klocka.
   DISPLAY FILL-IN_SLUT WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_START
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_START Dialog-Frame
ON MOUSE-MENU-CLICK OF FILL-IN_START IN FRAME Dialog-Frame /* Start */
DO:
   klocka = INPUT FILL-IN_START.
   
   {AVBGOMD.I}
   RUN KLOCKAN.W.
   {AVBFRAMD.I}
   
   IF klocka = 00.00 THEN klocka = 24.00.
   FILL-IN_START = klocka.
   DISPLAY FILL-IN_START WITH FRAME {&FRAME-NAME}.

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
   FIND sumtidtemp WHERE ROWID(sumtidtemp) = brwrec NO-LOCK NO-ERROR.
   ASSIGN
   FILL-IN_AONR:LABEL = Guru.Konstanter:gaok        
   FILL-IN_AONR = sumtidtemp.AONR
   FILL-IN_DATUM = sumtidtemp.DATUM
   FILL-IN_DELNR = sumtidtemp.DELNR
   FILL-IN_LUNCH = 0
   FILL-IN_PERSONALKOD = sumtidtemp.PERSONALKOD
   FILL-IN_SLUT = sumtidtemp.SLUT
   FILL-IN_START = sumtidtemp.START
   kollslut = sumtidtemp.SLUT
   kollstart = sumtidtemp.START
   FILL-IN_VIBEFATTNING = sumtidtemp.VIBEFATTNING.
   &Scoped-define FORMATNAMN FILL-IN_AONR   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN_DELNR   
   {DELNRFORMAT.I}
   RUN enable_UI.       
   {FRMSIZED.I}
   FIND FIRST befattningstemp WHERE befattningstemp.BEFATTNING = sumtidtemp.BEFATTNING NO-LOCK NO-ERROR.
   RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(befattningstemp)).              
   RUN lastselectdyn_UI IN brwproc[1].            
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
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_BEF:HANDLE IN FRAME {&FRAME-NAME}).
  

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
  DISPLAY FILL-IN_PERSONALKOD FILL-IN_DATUM FILL-IN_AONR FILL-IN_DELNR 
          FILL-IN_START FILL-IN_SLUT FILL-IN_LUNCH FILL-IN_VIBEFATTNING 
      WITH FRAME Dialog-Frame.
  ENABLE BRW_BEF FILL-IN_START FILL-IN_LUNCH Btn_OK Btn_AVB 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE klar_UI Dialog-Frame 
PROCEDURE klar_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   IF kollslut = FILL-IN_SLUT AND kollstart = FILL-IN_START THEN RETURN.
   IF FILL-IN_START < sumtidtemp.START THEN DO:
      MESSAGE "Denna registrering kan inte börja före den du delar upp." 
      VIEW-AS ALERT-BOX.
      ASSIGN
      musz = TRUE.
      RETURN.
   END.
   FIND sumbuff WHERE ROWID(sumbuff) = brwrec NO-ERROR.
   FIND FIRST bytpers WHERE 
   bytpers.PERSONALKOD = sumbuff.PERSONALKOD AND  
   bytpers.AONR = sumbuff.AONR AND
   bytpers.DELNR = sumbuff.DELNR AND  
   bytpers.DATUM = sumbuff.DATUM AND      
   bytpers.START = sumbuff.START AND
   bytpers.SLUT = sumbuff.SLUT NO-ERROR.
   IF NOT AVAILABLE bytpers THEN CREATE bytpers.      
   IF sumbuff.LUNCH - FILL-IN_LUNCH >= 0 THEN sumbuff.LUNCH = sumbuff.LUNCH - FILL-IN_LUNCH.
   sumbuff.SLUT = FILL-IN_START.
   IF sumbuff.OTIMMAR = 0 AND sumbuff.RESTIM = 0 THEN DO:
      sumbuff.TIMMAR = (klockan100(sumbuff.SLUT) - klockan100(sumbuff.START)) - sumbuff.LUNCH.
      sumbuff.BELOPP = sumbuff.TIMMAR * sumbuff.PRISA.
   END.
   ELSE IF sumbuff.OTIMMAR NE 0 THEN DO:
      resber = sumbuff.OBELOPP / sumbuff.OTIMMAR.   
      sumbuff.OTIMMAR = (klockan100(sumbuff.SLUT) - klockan100(sumbuff.START)) - sumbuff.LUNCH.
      sumbuff.OBELOPP = resber * sumbuff.OTIMMAR.
      sumbuff.OPRIS = resber.         
   END.
   ELSE IF sumbuff.RESTIM NE 0 THEN DO:
      resber = sumbuff.RESKOSTDEC / sumbuff.RESTIM.   
      sumbuff.RESTIM = (klockan100(sumbuff.SLUT) - klockan100(sumbuff.START)) - sumbuff.LUNCH.
      sumbuff.RESKOSTDEC = resber * sumbuff.RESTIM.
      sumbuff.RESPRIS = resber.         
   END.
   ASSIGN
   bytpers.PERSONALKOD = sumbuff.PERSONALKOD   
   bytpers.AONR = sumbuff.AONR
   bytpers.DELNR = sumbuff.DELNR   
   bytpers.DATUM = sumbuff.DATUM
   bytpers.BEFATTNING = sumbuff.BEFATTNING
   bytpers.START = sumbuff.START
   bytpers.SLUT = sumbuff.SLUT
   bytpers.GSTART = sumbuff.GSTART
   bytpers.GSLUT = sumbuff.GSLUT
   bytpers.BYT = TRUE.
   
   
   CREATE sumtidtemp.
   ASSIGN 
   sumtidtemp.PERSONALKOD = sumbuff.PERSONALKOD
   sumtidtemp.NAMN = sumbuff.NAMN 
   sumtidtemp.AONR = sumbuff.AONR
   sumtidtemp.DELNR = sumbuff.DELNR
   sumtidtemp.TIMMAR = sumbuff.TIMMAR
   sumtidtemp.DATUM = sumbuff.DATUM
   sumtidtemp.START = FILL-IN_START
   sumtidtemp.SLUT = FILL-IN_SLUT
   sumtidtemp.GSTART = sumbuff.GSTART
   sumtidtemp.GSLUT = sumbuff.GSLUT
   sumtidtemp.BELOPP = 0 
   sumtidtemp.LONKOST = 0
   sumtidtemp.OBELOPP = 0 
   sumtidtemp.OTIMMAR = 0 
   sumtidtemp.TBELOPP = 0
   sumtidtemp.TPRIS = 0             
   sumtidtemp.BEFATTNING = befattningstemp.BEFATTNING 
   sumtidtemp.VIBEFATTNING = FILL-IN_VIBEFATTNING    
   sumtidtemp.PERSBEF = sumbuff.PERSONALKOD + " " + FILL-IN_VIBEFATTNING
   sumtidtemp.OTEXTID = ""
   sumtidtemp.VIOBEFATTNING = "" 
   sumtidtemp.PERSMASK = sumbuff.PERSMASK
   sumtidtemp.PRISA = sumbuff.PRISA  
   sumtidtemp.OPRIS = sumbuff.OPRIS
   sumtidtemp.PRISTYP = sumbuff.PRISTYP 
   sumtidtemp.TRAKTANTAL = 0  
   sumtidtemp.TRAKTKOD = ""
   sumtidtemp.VITRAKT = ""
   sumtidtemp.ENDAGS = sumbuff.ENDAGS
   sumtidtemp.LONTILLANTAL = 0 
   sumtidtemp.LONTILLAGG = "" 
   sumtidtemp.VILART = "" 
   sumtidtemp.MED = sumbuff.MED    
   sumtidtemp.RESTIM = 0
   sumtidtemp.RESKOSTDEC = 0
   sumtidtemp.LUNCH = FILL-IN_LUNCH
   sumtidtemp.RESPRIS = sumbuff.RESPRIS
   sumtidtemp.OANT1 = 0   
   sumtidtemp.ANDMED = sumbuff.ANDMED.
   
   IF sumtidtemp.OTIMMAR = 0 AND sumtidtemp.RESTIM = 0 THEN DO:
      sumtidtemp.TIMMAR = (klockan100(sumtidtemp.SLUT) - klockan100(sumtidtemp.START)) - sumtidtemp.LUNCH.
      sumtidtemp.BELOPP = sumtidtemp.TIMMAR * sumtidtemp.PRISA.
   END.
   ELSE IF sumtidtemp.OTIMMAR NE 0 THEN DO:
      resber = sumtidtemp.OBELOPP / sumtidtemp.OTIMMAR.   
      sumtidtemp.OTIMMAR = (klockan100(sumtidtemp.SLUT) - klockan100(sumtidtemp.START)) - sumtidtemp.LUNCH.
      sumtidtemp.OBELOPP = resber * sumtidtemp.OTIMMAR.
      sumtidtemp.OPRIS = resber.         
   END.
   ELSE IF sumtidtemp.RESTIM NE 0 THEN DO:
      resber = sumtidtemp.RESKOSTDEC / sumtidtemp.RESTIM.   
      sumtidtemp.RESTIM = (klockan100(sumtidtemp.SLUT) - klockan100(sumtidtemp.START)) - sumtidtemp.LUNCH.
      sumtidtemp.RESKOSTDEC = resber * sumtidtemp.RESTIM.
      sumtidtemp.RESPRIS = resber.         
   END.   
   FIND FIRST bytpers WHERE 
   bytpers.PERSONALKOD = sumtidtemp.PERSONALKOD AND  
   bytpers.AONR = sumtidtemp.AONR AND
   bytpers.DELNR = sumtidtemp.DELNR AND  
   bytpers.DATUM = sumtidtemp.DATUM AND      
   bytpers.START = sumtidtemp.START AND
   bytpers.SLUT = sumtidtemp.SLUT NO-ERROR.      
   IF NOT AVAILABLE bytpers THEN CREATE bytpers.
   ASSIGN
   bytpers.PERSONALKOD = sumtidtemp.PERSONALKOD   
   bytpers.AONR = sumtidtemp.AONR
   bytpers.DELNR = sumtidtemp.DELNR   
   bytpers.DATUM = sumtidtemp.DATUM
   bytpers.BEFATTNING = befattningstemp.BEFATTNING
   bytpers.START = sumtidtemp.START
   bytpers.SLUT = sumtidtemp.SLUT
   bytpers.GSTART = sumtidtemp.GSTART
   bytpers.GSLUT = sumtidtemp.GSLUT
   bytpers.BYT = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION klockan100 Dialog-Frame 
FUNCTION klockan100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

