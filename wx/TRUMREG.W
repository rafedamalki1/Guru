
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

  Created: 95/05/18 -  9:13 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{LAGERTRUM.I}
&Scoped-define NEW 
&Scoped-define SHARED 
DEFINE INPUT-OUTPUT  PARAMETER TABLE FOR   elagertrum.
DEFINE INPUT  PARAMETER kvarmeter AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER lplats AS INTEGER.

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}


DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO. 
DEFINE VARIABLE varant AS INTEGER NO-UNDO.  
DEFINE VARIABLE varenr AS CHARACTER NO-UNDO. 
DEFINE VARIABLE varben AS CHARACTER NO-UNDO. 
DEFINE VARIABLE varlev AS CHARACTER NO-UNDO. 
DEFINE VARIABLE varbestnr AS INTEGER NO-UNDO. 
DEFINE VARIABLE varpris AS DECIMAL NO-UNDO. 
DEFINE VARIABLE varenh AS CHARACTER NO-UNDO. 
DEFINE VARIABLE varbestd AS DATE NO-UNDO. 
DEFINE VARIABLE varbestall AS CHARACTER NO-UNDO.  
DEFINE VARIABLE vardep AS INTEGER NO-UNDO. 
DEFINE VARIABLE varbered AS LOGICAL NO-UNDO.
DEFINE VARIABLE lagerpris AS DECIMAL NO-UNDO.
DEFINE VARIABLE leveransapph AS HANDLE NO-UNDO.
DEFINE VARIABLE enrdepsaldo AS INTEGER NO-UNDO.
DEFINE VARIABLE hjsaldo AS INTEGER NO-UNDO.
DEFINE VARIABLE nytrum AS LOGICAL NO-UNDO.

DEFINE VARIABLE nylagpl AS INTEGER NO-UNDO.

/*{EGENBEN.I}*/
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-VILAGERPLATS FILL-IN-ENR FILL-IN-BEN ~
FILL-IN-ENHET FILL-IN-PRIS FILL-IN-BEST FILL-IN-BANT btn_ok BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-LAGERPLATS FILL-IN-VILAGERPLATS ~
FILL-IN-ENR FILL-IN-BEN FILL-IN-ENHET FILL-IN-PRIS FILL-IN-BEST ~
FILL-IN-BANT 

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

DEFINE BUTTON btn_ok 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-BANT AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Meter" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEST AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Antal" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENHET AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enhet" 
     VIEW-AS FILL-IN 
     SIZE 11.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LAGERPLATS AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Trumma nr" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PRIS AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 10.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-VILAGERPLATS AS CHARACTER FORMAT "X(256)":U 
     LABEL "Trumma namn" 
     VIEW-AS FILL-IN 
     SIZE 15.5 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN-LAGERPLATS AT ROW 1.5 COL 12 COLON-ALIGNED
     FILL-IN-VILAGERPLATS AT ROW 2.79 COL 12 COLON-ALIGNED
     FILL-IN-ENR AT ROW 4.08 COL 12 COLON-ALIGNED
     FILL-IN-BEN AT ROW 5.38 COL 12 COLON-ALIGNED
     FILL-IN-ENHET AT ROW 6.71 COL 12 COLON-ALIGNED
     FILL-IN-PRIS AT ROW 8 COL 12 COLON-ALIGNED WIDGET-ID 2
     FILL-IN-BEST AT ROW 9.33 COL 12 COLON-ALIGNED
     FILL-IN-BANT AT ROW 9.33 COL 34.38 COLON-ALIGNED
     btn_ok AT ROW 11.63 COL 17.25
     BTN_AVB AT ROW 11.63 COL 32.25
     SPACE(2.62) SKIP(0.82)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Registrera trumma":L
         DEFAULT-BUTTON btn_ok.


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

/* SETTINGS FOR FILL-IN FILL-IN-LAGERPLATS IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Registrera trumma */
DO:
  {muswait.i}
   IF VALID-HANDLE(leveransapph) THEN DELETE PROCEDURE leveransapph.         
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   musz = TRUE.
   {BORTBRWPROC.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO: 
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.   
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ok DIALOG-1
ON CHOOSE OF btn_ok IN FRAME DIALOG-1 /* Ok */
DO:   
   DEFINE VARIABLE lagerpok AS LOGICAL NO-UNDO.
   
   ASSIGN
   FILL-IN-LAGERPLATS = INPUT FILL-IN-LAGERPLATS
   FILL-IN-VILAGERPLATS = INPUT FILL-IN-VILAGERPLATS
   FILL-IN-ENR = INPUT FILL-IN-ENR
   FILL-IN-BEN = INPUT FILL-IN-BEN
   FILL-IN-ENHET = INPUT FILL-IN-ENHET
   FILL-IN-BEST = INPUT FILL-IN-BEST
   FILL-IN-BANT = INPUT FILL-IN-BANT
   FILL-IN-PRIS = INPUT FILL-IN-PRIS.
   
   /* heltokigt -bort 20190618
   nylagpl = INTEGER(FILL-IN-VILAGERPLATS) NO-ERROR.
   IF nylagpl > 0 THEN FILL-IN-LAGERPLATS = INTEGER(FILL-IN-VILAGERPLATS).*/ 
   IF FILL-IN-VILAGERPLATS = "" OR FILL-IN-VILAGERPLATS = "E" THEN DO:
      MESSAGE "Trummanamn får inte vara blank"
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-VILAGERPLATS IN FRAME {&FRAME-NAME}.
      RETURN.
   END.
   IF Guru.Konstanter:globforetag = "snat" THEN DO:
      /*EXXX= el  SXXX= fiber DXXX= underhåll*/
      IF SUBSTRING(FILL-IN-VILAGERPLATS,1,1) = "E" OR SUBSTRING(FILL-IN-VILAGERPLATS,1,1) = "D" OR SUBSTRING(FILL-IN-VILAGERPLATS,1,1) = "S" THEN.
      ELSE DO:
         MESSAGE "Trummanamn ska vara Exxx för eltrummor Sxxx för fibertrummor och Dxxx för underhåll"
         VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-VILAGERPLATS IN FRAME {&FRAME-NAME}.
         RETURN.
      END.
      IF LENGTH(FILL-IN-VILAGERPLATS) = 4  THEN.
      ELSE DO:
         MESSAGE "Trummanamn ska vara Exxx för eltrummor Sxxx för fibertrummor och Dxxx för underhåll"
         VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-VILAGERPLATS IN FRAME {&FRAME-NAME}.
         RETURN.
      END.
      
   END. 
   IF FILL-IN-ENR = "" THEN DO:
      MESSAGE "Enr får inte vara blank"
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-ENR IN FRAME {&FRAME-NAME}.
      RETURN.
   END.
   IF FILL-IN-BEN = "" THEN DO:
      MESSAGE "Benämning får inte vara blank"
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
      RETURN.
   END.
   IF FILL-IN-ENHET = "" THEN DO:
      MESSAGE "Enhet får inte vara blank"
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-ENHET IN FRAME {&FRAME-NAME}.
      RETURN.
   END.
   IF FILL-IN-BEST = 0 THEN DO:
      MESSAGE "Antal får inte vara 0"
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-BEST IN FRAME {&FRAME-NAME}.
      RETURN.
   END.
   IF FILL-IN-BANT:HIDDEN = FALSE THEN DO:
      IF kvarmeter > 0 AND FILL-IN-BEST > kvarmeter THEN DO:
         MESSAGE "Antal kan inte vara större än beställningsantal /returantal " + STRING(FILL-IN-BANT) + "m"
         VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-BEST IN FRAME {&FRAME-NAME}.
         RETURN.
      END.   
      
      IF FILL-IN-BEST > FILL-IN-BANT THEN DO:
         MESSAGE "Antal kan inte vara större än beställningsantal /returantal " + STRING(FILL-IN-BANT) + "m"
         VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-BEST IN FRAME {&FRAME-NAME}.
         RETURN.
      END.
   END.
   IF nytrum = TRUE THEN DO:
      RUN hsalenr_UI IN leveransapph (INPUT FILL-IN-ENR,INPUT Guru.GlobalaVariabler:GuruVdepnr, OUTPUT enrdepsaldo).
      hjsaldo = 0.
      FOR EACH lagertrum WHERE lagertrum.ENR = FILL-IN-ENR AND lagertrum.TRUMMA = "S" NO-LOCK:
         hjsaldo = hjsaldo + lagertrum.LAGMETER.
      END.      
      hjsaldo = hjsaldo + FILL-IN-BEST.
      
      IF hjsaldo > enrdepsaldo THEN DO:
         MESSAGE "Saldo i lager för enr " + FILL-IN-ENR + "  är: " + STRING(enrdepsaldo) + ". Med denna trumma skulle saldot bli: " + STRING (hjsaldo)
         VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-VILAGERPLATS IN FRAME {&FRAME-NAME}.
         RETURN.
      END.
   END.
   FIND FIRST lagertrum WHERE lagertrum.TRUMMAPLATS = FILL-IN-VILAGERPLATS AND lagertrum.TRUMMANR NE FILL-IN-LAGERPLATS AND lagertrum.LAGMETER > 0
   AND lagertrum.TRUMMA = "S"  USE-INDEX TRUMMANR NO-ERROR.
   /*det ska gå att lägga upp samma trumma om det är 0 i lagmeter, dvs inventeing är gjordLena 20191105*/
   IF AVAILABLE lagertrum THEN DO:
      MESSAGE "Trumma namn " + FILL-IN-VILAGERPLATS  + " finns redan upplagd på trumma " + STRING(lagertrum.TRUMMANR)
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-VILAGERPLATS IN FRAME {&FRAME-NAME}.
      RETURN.
   END.   
   ELSE DO: 
      FIND FIRST lagertrum WHERE lagertrum.TRUMMAPLATS = FILL-IN-VILAGERPLATS AND lagertrum.TRUMMANR NE FILL-IN-LAGERPLATS AND lagertrum.LAGMETER = 0
      AND lagertrum.TRUMMA = "S"  USE-INDEX TRUMMANR NO-ERROR.
      /*det ska gå att lägga upp samma trumma om det är 0 i lagmeter, dvs inventeing är gjordLena 20191105*/
      IF AVAILABLE lagertrum THEN DO:
         IF lagertrum.ENR NE FILL-IN-ENR  THEN DO:
            MESSAGE "Trumma namn " + FILL-IN-VILAGERPLATS  + " är upplagd med enr " + lagertrum.ENR + " . Nyupplägg med detta Trumma namn måste ha samma enr." 
            VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-VILAGERPLATS IN FRAME {&FRAME-NAME}.
            RETURN.
            END.
      END.   
   END.   
   FIND FIRST lagertrum WHERE lagertrum.TRUMMAPLATS = FILL-IN-VILAGERPLATS AND lagertrum.TRUMMANR NE FILL-IN-LAGERPLATS AND lagertrum.LAGMETER > 0
   AND lagertrum.TRUMMA = "S"  USE-INDEX TRUMMANR NO-ERROR.
   /*det ska gå att lägga upp samma trumma om det är 0 i lagmeter, dvs inventeing är gjordLena 20191105*/
   IF AVAILABLE lagertrum THEN DO:
      MESSAGE "Trumma namn " + FILL-IN-VILAGERPLATS  + " finns redan upplagd på trumma " + STRING(lagertrum.TRUMMANR)
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-VILAGERPLATS IN FRAME {&FRAME-NAME}.
      RETURN.
   END.
     
   /*FIND FIRST lagertrum WHERE lagertrum.TRUMMANR = FILL-IN-LAGERPLATS USE-INDEX TRUMMANR NO-ERROR.
   IF AVAILABLE lagertrum THEN DO:
      MESSAGE "Trumma nr " + FILL-IN-LAGERPLATS +  " " + FILL-IN-VILAGERPLATS + " finns redan upplagd"
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-VILAGERPLATS IN FRAME {&FRAME-NAME}.
      RETURN.
   END.*/
   lagerpok = FALSE. 
   IF elagertrum.ENR = ""  THEN DO:
      ASSIGN
      elagertrum.ENR = FILL-IN-ENR
      elagertrum.BENAMNING = FILL-IN-BEN
      elagertrum.ENHET = FILL-IN-ENHET
      elagertrum.STMETER = FILL-IN-BEST
      elagertrum.LAGMETER = FILL-IN-BEST.
      IF FILL-IN-PRIS > 0 THEN elagertrum.PRIS = FILL-IN-PRIS.
      ELSE elagertrum.PRIS = lagerpris.
   END.   
   ELSE DO:
      ASSIGN
      elagertrum.STMETER = FILL-IN-BEST
      elagertrum.LAGMETER = FILL-IN-BEST.
   END.
   ASSIGN 
   elagertrum.TRUMMA = "S"
   elagertrum.TRUMMANR = FILL-IN-LAGERPLATS
   elagertrum.TRUMMAPLATS = FILL-IN-VILAGERPLATS
   elagertrum.DEPNR = Guru.GlobalaVariabler:GuruVdepnr.
   lplats = FILL-IN-LAGERPLATS.
   FIND FIRST lagertrum WHERE lagertrum.TRUMMANR = elagertrum.TRUMMANR AND lagertrum.TRUMMA = "S" NO-LOCK NO-ERROR.
   IF NOT AVAILABLE lagertrum THEN CREATE lagertrum.
   BUFFER-COPY elagertrum TO lagertrum. 
   RUN SparafaltTrumma_UI IN  Guru.GlobalaVariabler:ClienttdSetapph (INPUT TABLE lagertrum,INPUT TABLE aotrum).
   APPLY "GO" TO FRAME {&FRAME-NAME}.     
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ENR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR DIALOG-1
ON LEAVE OF FILL-IN-ENR IN FRAME DIALOG-1 /* Enr */
DO:
   
   FILL-IN-ENR = INPUT FILL-IN-ENR.
   IF FILL-IN-BEN =  ""  THEN DO:   
      EMPTY TEMP-TABLE felmeddtemp NO-ERROR. 
      EMPTY TEMP-TABLE elagertrum2 NO-ERROR. 
      IF Guru.Konstanter:globforetag = "SNAT"  THEN DO:            
         IF SUBSTRING(FILL-IN-ENR,1,1) NE "E" THEN FILL-IN-ENR = "E" + FILL-IN-ENR.
         DISPLAY  FILL-IN-ENR WITH FRAME {&FRAME-NAME}.                                     
      END.
      RUN henr_UI IN leveransapph (INPUT FILL-IN-ENR,INPUT Guru.GlobalaVariabler:GuruVdepnr, OUTPUT TABLE elagertrum2, OUTPUT TABLE felmeddtemp).
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX .
         DELETE felmeddtemp.
         RETURN NO-APPLY.
      END.
      FIND FIRST  elagertrum2 NO-LOCK NO-ERROR.
      IF AVAILABLE elagertrum2 THEN DO:
         ASSIGN
         FILL-IN-BEN = elagertrum2.BENAMNING
         FILL-IN-ENHET = elagertrum2.ENHET
         FILL-IN-PRIS = elagertrum2.PRIS.
         lagerpris = elagertrum2.PRIS.
         DISPLAY FILL-IN-BEN FILL-IN-ENHET FILL-IN-PRIS WITH FRAME {&FRAME-NAME}.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-PRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PRIS DIALOG-1
ON LEAVE OF FILL-IN-PRIS IN FRAME DIALOG-1 /* Pris */
DO:
  FILL-IN-PRIS = INPUT FILL-IN-PRIS.
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
   lplats = 0.
   FILL-IN-BANT:LABEL = "Totalt antal".
   nytrum = FALSE.
   FIND FIRST elagertrum NO-LOCK NO-ERROR.
   IF NOT AVAILABLE elagertrum THEN DO:
      nytrum = TRUE. 
      CREATE elagertrum.
      ASSIGN 
      elagertrum.DATUM = TODAY.      
      /*IF Guru.Konstanter:globforetag = "snat" THEN FILL-IN-VILAGERPLATS = "E".*/
   END.
      
   
   ASSIGN
   FILL-IN-ENR:LABEL = Guru.Konstanter:genk
   FILL-IN-BEN = elagertrum.BENAMNING
   FILL-IN-ENHET = elagertrum.ENHET
   FILL-IN-ENR = elagertrum.ENR  
   FILL-IN-BEST = kvarmeter    
   FILL-IN-BANT = elagertrum.STMETER
   FILL-IN-PRIS = elagertrum.PRIS    
   lagerpris = elagertrum.PRIS
   /*FILL-IN-LAGERPLATS = 0*/
   varant = elagertrum.STMETER.
   musz = FALSE. 
   FIND LAST lagertrum  USE-INDEX TRUMMANR NO-ERROR.
   IF AVAILABLE lagertrum THEN FILL-IN-LAGERPLATS = lagertrum.TRUMMANR + 1.
   ELSE FILL-IN-LAGERPLATS = 1.
   /*IF Guru.Konstanter:globforetag = "snat" AND FILL-IN-VILAGERPLATS = "" THEN FILL-IN-VILAGERPLATS = "E".         
   IF FILL-IN-VILAGERPLATS = "" THEN FILL-IN-VILAGERPLATS = STRING(FILL-IN-LAGERPLATS).*/
   
   RUN enable_UI.       
   
   IF FILL-IN-ENR = "" THEN DO:
      ENABLE FILL-IN-ENR FILL-IN-BEST WITH FRAME {&FRAME-NAME}.
      DISABLE  FILL-IN-BEN FILL-IN-ENHET WITH FRAME {&FRAME-NAME}.
      FILL-IN-BANT:HIDDEN = TRUE.
   END.
   ELSE DO:      
      DISABLE  FILL-IN-ENR FILL-IN-BEN FILL-IN-ENHET FILL-IN-PRIS FILL-IN-BANT WITH FRAME {&FRAME-NAME}.
   END.
   {FRMSIZED.I}
   {musarrow.i}
   {DIA_M_SLUT.I}
   APPLY "ENTRY" TO FILL-IN-VILAGERPLATS IN FRAME {&FRAME-NAME}.
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
   IF Guru.Konstanter:appcon THEN DO:
      RUN LEVAPPV.P PERSISTENT SET leveransapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN LEVAPPV.P PERSISTENT SET leveransapph.
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
  DISPLAY FILL-IN-LAGERPLATS FILL-IN-VILAGERPLATS FILL-IN-ENR FILL-IN-BEN 
          FILL-IN-ENHET FILL-IN-PRIS FILL-IN-BEST FILL-IN-BANT 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN-VILAGERPLATS FILL-IN-ENR FILL-IN-BEN FILL-IN-ENHET 
         FILL-IN-PRIS FILL-IN-BEST FILL-IN-BANT btn_ok BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

=======
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

  Created: 95/05/18 -  9:13 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{LAGERTRUM.I}
&Scoped-define NEW 
&Scoped-define SHARED 
DEFINE INPUT-OUTPUT  PARAMETER TABLE FOR   elagertrum.
DEFINE INPUT  PARAMETER kvarmeter AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER lplats AS INTEGER.

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}


DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO. 
DEFINE VARIABLE varant AS INTEGER NO-UNDO.  
DEFINE VARIABLE varenr AS CHARACTER NO-UNDO. 
DEFINE VARIABLE varben AS CHARACTER NO-UNDO. 
DEFINE VARIABLE varlev AS CHARACTER NO-UNDO. 
DEFINE VARIABLE varbestnr AS INTEGER NO-UNDO. 
DEFINE VARIABLE varpris AS DECIMAL NO-UNDO. 
DEFINE VARIABLE varenh AS CHARACTER NO-UNDO. 
DEFINE VARIABLE varbestd AS DATE NO-UNDO. 
DEFINE VARIABLE varbestall AS CHARACTER NO-UNDO.  
DEFINE VARIABLE vardep AS INTEGER NO-UNDO. 
DEFINE VARIABLE varbered AS LOGICAL NO-UNDO.
DEFINE VARIABLE lagerpris AS DECIMAL NO-UNDO.
DEFINE VARIABLE leveransapph AS HANDLE NO-UNDO.
DEFINE VARIABLE enrdepsaldo AS INTEGER NO-UNDO.
DEFINE VARIABLE hjsaldo AS INTEGER NO-UNDO.
DEFINE VARIABLE nytrum AS LOGICAL NO-UNDO.

DEFINE VARIABLE nylagpl AS INTEGER NO-UNDO.

/*{EGENBEN.I}*/
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-VILAGERPLATS FILL-IN-ENR FILL-IN-BEN ~
FILL-IN-ENHET FILL-IN-PRIS FILL-IN-BEST FILL-IN-BANT btn_ok BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-LAGERPLATS FILL-IN-VILAGERPLATS ~
FILL-IN-ENR FILL-IN-BEN FILL-IN-ENHET FILL-IN-PRIS FILL-IN-BEST ~
FILL-IN-BANT 

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

DEFINE BUTTON btn_ok 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-BANT AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Meter" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEST AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Antal" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENHET AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enhet" 
     VIEW-AS FILL-IN 
     SIZE 11.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LAGERPLATS AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Trumma nr" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PRIS AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 10.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-VILAGERPLATS AS CHARACTER FORMAT "X(256)":U 
     LABEL "Trumma namn" 
     VIEW-AS FILL-IN 
     SIZE 15.5 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN-LAGERPLATS AT ROW 1.5 COL 12 COLON-ALIGNED
     FILL-IN-VILAGERPLATS AT ROW 2.79 COL 12 COLON-ALIGNED
     FILL-IN-ENR AT ROW 4.08 COL 12 COLON-ALIGNED
     FILL-IN-BEN AT ROW 5.38 COL 12 COLON-ALIGNED
     FILL-IN-ENHET AT ROW 6.71 COL 12 COLON-ALIGNED
     FILL-IN-PRIS AT ROW 8 COL 12 COLON-ALIGNED WIDGET-ID 2
     FILL-IN-BEST AT ROW 9.33 COL 12 COLON-ALIGNED
     FILL-IN-BANT AT ROW 9.33 COL 34.38 COLON-ALIGNED
     btn_ok AT ROW 11.63 COL 17.25
     BTN_AVB AT ROW 11.63 COL 32.25
     SPACE(2.62) SKIP(0.82)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Registrera trumma":L
         DEFAULT-BUTTON btn_ok.


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

/* SETTINGS FOR FILL-IN FILL-IN-LAGERPLATS IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Registrera trumma */
DO:
  {muswait.i}
   IF VALID-HANDLE(leveransapph) THEN DELETE PROCEDURE leveransapph.         
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   musz = TRUE.
   {BORTBRWPROC.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO: 
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.   
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ok DIALOG-1
ON CHOOSE OF btn_ok IN FRAME DIALOG-1 /* Ok */
DO:   
   DEFINE VARIABLE lagerpok AS LOGICAL NO-UNDO.
   
   ASSIGN
   FILL-IN-LAGERPLATS = INPUT FILL-IN-LAGERPLATS
   FILL-IN-VILAGERPLATS = INPUT FILL-IN-VILAGERPLATS
   FILL-IN-ENR = INPUT FILL-IN-ENR
   FILL-IN-BEN = INPUT FILL-IN-BEN
   FILL-IN-ENHET = INPUT FILL-IN-ENHET
   FILL-IN-BEST = INPUT FILL-IN-BEST
   FILL-IN-BANT = INPUT FILL-IN-BANT
   FILL-IN-PRIS = INPUT FILL-IN-PRIS.
   
   /* heltokigt -bort 20190618
   nylagpl = INTEGER(FILL-IN-VILAGERPLATS) NO-ERROR.
   IF nylagpl > 0 THEN FILL-IN-LAGERPLATS = INTEGER(FILL-IN-VILAGERPLATS).*/ 
   IF FILL-IN-VILAGERPLATS = "" OR FILL-IN-VILAGERPLATS = "E" THEN DO:
      MESSAGE "Trummanamn får inte vara blank"
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-VILAGERPLATS IN FRAME {&FRAME-NAME}.
      RETURN.
   END.
   IF Guru.Konstanter:globforetag = "snat" THEN DO:
      /*EXXX= el  SXXX= fiber DXXX= underhåll*/
      IF SUBSTRING(FILL-IN-VILAGERPLATS,1,1) = "E" OR SUBSTRING(FILL-IN-VILAGERPLATS,1,1) = "D" OR SUBSTRING(FILL-IN-VILAGERPLATS,1,1) = "S" THEN.
      ELSE DO:
         MESSAGE "Trummanamn ska vara Exxx för eltrummor Sxxx för fibertrummor och Dxxx för underhåll"
         VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-VILAGERPLATS IN FRAME {&FRAME-NAME}.
         RETURN.
      END.
      IF LENGTH(FILL-IN-VILAGERPLATS) = 4  THEN.
      ELSE DO:
         MESSAGE "Trummanamn ska vara Exxx för eltrummor Sxxx för fibertrummor och Dxxx för underhåll"
         VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-VILAGERPLATS IN FRAME {&FRAME-NAME}.
         RETURN.
      END.
      
   END. 
   IF FILL-IN-ENR = "" THEN DO:
      MESSAGE "Enr får inte vara blank"
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-ENR IN FRAME {&FRAME-NAME}.
      RETURN.
   END.
   IF FILL-IN-BEN = "" THEN DO:
      MESSAGE "Benämning får inte vara blank"
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
      RETURN.
   END.
   IF FILL-IN-ENHET = "" THEN DO:
      MESSAGE "Enhet får inte vara blank"
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-ENHET IN FRAME {&FRAME-NAME}.
      RETURN.
   END.
   IF FILL-IN-BEST = 0 THEN DO:
      MESSAGE "Antal får inte vara 0"
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-BEST IN FRAME {&FRAME-NAME}.
      RETURN.
   END.
   IF FILL-IN-BANT:HIDDEN = FALSE THEN DO:
      IF kvarmeter > 0 AND FILL-IN-BEST > kvarmeter THEN DO:
         MESSAGE "Antal kan inte vara större än beställningsantal /returantal " + STRING(FILL-IN-BANT) + "m"
         VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-BEST IN FRAME {&FRAME-NAME}.
         RETURN.
      END.   
      
      IF FILL-IN-BEST > FILL-IN-BANT THEN DO:
         MESSAGE "Antal kan inte vara större än beställningsantal /returantal " + STRING(FILL-IN-BANT) + "m"
         VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-BEST IN FRAME {&FRAME-NAME}.
         RETURN.
      END.
   END.
   IF nytrum = TRUE THEN DO:
      RUN hsalenr_UI IN leveransapph (INPUT FILL-IN-ENR,INPUT Guru.GlobalaVariabler:GuruVdepnr, OUTPUT enrdepsaldo).
      hjsaldo = 0.
      FOR EACH lagertrum WHERE lagertrum.ENR = FILL-IN-ENR AND lagertrum.TRUMMA = "S" NO-LOCK:
         hjsaldo = hjsaldo + lagertrum.LAGMETER.
      END.      
      hjsaldo = hjsaldo + FILL-IN-BEST.
      
      IF hjsaldo > enrdepsaldo THEN DO:
         MESSAGE "Saldo i lager för enr " + FILL-IN-ENR + "  är: " + STRING(enrdepsaldo) + ". Med denna trumma skulle saldot bli: " + STRING (hjsaldo)
         VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-VILAGERPLATS IN FRAME {&FRAME-NAME}.
         RETURN.
      END.
   END.
   FIND FIRST lagertrum WHERE lagertrum.TRUMMAPLATS = FILL-IN-VILAGERPLATS AND lagertrum.TRUMMANR NE FILL-IN-LAGERPLATS AND lagertrum.LAGMETER > 0
   AND lagertrum.TRUMMA = "S"  USE-INDEX TRUMMANR NO-ERROR.
   /*det ska gå att lägga upp samma trumma om det är 0 i lagmeter, dvs inventeing är gjordLena 20191105*/
   IF AVAILABLE lagertrum THEN DO:
      MESSAGE "Trumma namn " + FILL-IN-VILAGERPLATS  + " finns redan upplagd på trumma " + STRING(lagertrum.TRUMMANR)
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-VILAGERPLATS IN FRAME {&FRAME-NAME}.
      RETURN.
   END.   
   ELSE DO: 
      FIND FIRST lagertrum WHERE lagertrum.TRUMMAPLATS = FILL-IN-VILAGERPLATS AND lagertrum.TRUMMANR NE FILL-IN-LAGERPLATS AND lagertrum.LAGMETER = 0
      AND lagertrum.TRUMMA = "S"  USE-INDEX TRUMMANR NO-ERROR.
      /*det ska gå att lägga upp samma trumma om det är 0 i lagmeter, dvs inventeing är gjordLena 20191105*/
      IF AVAILABLE lagertrum THEN DO:
         IF lagertrum.ENR NE FILL-IN-ENR  THEN DO:
            MESSAGE "Trumma namn " + FILL-IN-VILAGERPLATS  + " är upplagd med enr " + lagertrum.ENR + " . Nyupplägg med detta Trumma namn måste ha samma enr." 
            VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-VILAGERPLATS IN FRAME {&FRAME-NAME}.
            RETURN.
            END.
      END.   
   END.   
   FIND FIRST lagertrum WHERE lagertrum.TRUMMAPLATS = FILL-IN-VILAGERPLATS AND lagertrum.TRUMMANR NE FILL-IN-LAGERPLATS AND lagertrum.LAGMETER > 0
   AND lagertrum.TRUMMA = "S"  USE-INDEX TRUMMANR NO-ERROR.
   /*det ska gå att lägga upp samma trumma om det är 0 i lagmeter, dvs inventeing är gjordLena 20191105*/
   IF AVAILABLE lagertrum THEN DO:
      MESSAGE "Trumma namn " + FILL-IN-VILAGERPLATS  + " finns redan upplagd på trumma " + STRING(lagertrum.TRUMMANR)
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-VILAGERPLATS IN FRAME {&FRAME-NAME}.
      RETURN.
   END.
     
   /*FIND FIRST lagertrum WHERE lagertrum.TRUMMANR = FILL-IN-LAGERPLATS USE-INDEX TRUMMANR NO-ERROR.
   IF AVAILABLE lagertrum THEN DO:
      MESSAGE "Trumma nr " + FILL-IN-LAGERPLATS +  " " + FILL-IN-VILAGERPLATS + " finns redan upplagd"
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-VILAGERPLATS IN FRAME {&FRAME-NAME}.
      RETURN.
   END.*/
   lagerpok = FALSE. 
   IF elagertrum.ENR = ""  THEN DO:
      ASSIGN
      elagertrum.ENR = FILL-IN-ENR
      elagertrum.BENAMNING = FILL-IN-BEN
      elagertrum.ENHET = FILL-IN-ENHET
      elagertrum.STMETER = FILL-IN-BEST
      elagertrum.LAGMETER = FILL-IN-BEST.
      IF FILL-IN-PRIS > 0 THEN elagertrum.PRIS = FILL-IN-PRIS.
      ELSE elagertrum.PRIS = lagerpris.
   END.   
   ELSE DO:
      ASSIGN
      elagertrum.STMETER = FILL-IN-BEST
      elagertrum.LAGMETER = FILL-IN-BEST.
   END.
   ASSIGN 
   elagertrum.TRUMMA = "S"
   elagertrum.TRUMMANR = FILL-IN-LAGERPLATS
   elagertrum.TRUMMAPLATS = FILL-IN-VILAGERPLATS
   elagertrum.DEPNR = Guru.GlobalaVariabler:GuruVdepnr.
   lplats = FILL-IN-LAGERPLATS.
   FIND FIRST lagertrum WHERE lagertrum.TRUMMANR = elagertrum.TRUMMANR AND lagertrum.TRUMMA = "S" NO-LOCK NO-ERROR.
   IF NOT AVAILABLE lagertrum THEN CREATE lagertrum.
   BUFFER-COPY elagertrum TO lagertrum. 
   RUN SparafaltTrumma_UI IN  Guru.GlobalaVariabler:ClienttdSetapph (INPUT TABLE lagertrum,INPUT TABLE aotrum).
   APPLY "GO" TO FRAME {&FRAME-NAME}.     
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ENR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR DIALOG-1
ON LEAVE OF FILL-IN-ENR IN FRAME DIALOG-1 /* Enr */
DO:
   
   FILL-IN-ENR = INPUT FILL-IN-ENR.
   IF FILL-IN-BEN =  ""  THEN DO:   
      EMPTY TEMP-TABLE felmeddtemp NO-ERROR. 
      EMPTY TEMP-TABLE elagertrum2 NO-ERROR. 
      IF Guru.Konstanter:globforetag = "SNAT"  THEN DO:            
         IF SUBSTRING(FILL-IN-ENR,1,1) NE "E" THEN FILL-IN-ENR = "E" + FILL-IN-ENR.
         DISPLAY  FILL-IN-ENR WITH FRAME {&FRAME-NAME}.                                     
      END.
      RUN henr_UI IN leveransapph (INPUT FILL-IN-ENR,INPUT Guru.GlobalaVariabler:GuruVdepnr, OUTPUT TABLE elagertrum2, OUTPUT TABLE felmeddtemp).
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX .
         DELETE felmeddtemp.
         RETURN NO-APPLY.
      END.
      FIND FIRST  elagertrum2 NO-LOCK NO-ERROR.
      IF AVAILABLE elagertrum2 THEN DO:
         ASSIGN
         FILL-IN-BEN = elagertrum2.BENAMNING
         FILL-IN-ENHET = elagertrum2.ENHET
         FILL-IN-PRIS = elagertrum2.PRIS.
         lagerpris = elagertrum2.PRIS.
         DISPLAY FILL-IN-BEN FILL-IN-ENHET FILL-IN-PRIS WITH FRAME {&FRAME-NAME}.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-PRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PRIS DIALOG-1
ON LEAVE OF FILL-IN-PRIS IN FRAME DIALOG-1 /* Pris */
DO:
  FILL-IN-PRIS = INPUT FILL-IN-PRIS.
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
   lplats = 0.
   FILL-IN-BANT:LABEL = "Totalt antal".
   nytrum = FALSE.
   FIND FIRST elagertrum NO-LOCK NO-ERROR.
   IF NOT AVAILABLE elagertrum THEN DO:
      nytrum = TRUE. 
      CREATE elagertrum.
      ASSIGN 
      elagertrum.DATUM = TODAY.      
      /*IF Guru.Konstanter:globforetag = "snat" THEN FILL-IN-VILAGERPLATS = "E".*/
   END.
      
   
   ASSIGN
   FILL-IN-ENR:LABEL = Guru.Konstanter:genk
   FILL-IN-BEN = elagertrum.BENAMNING
   FILL-IN-ENHET = elagertrum.ENHET
   FILL-IN-ENR = elagertrum.ENR  
   FILL-IN-BEST = kvarmeter    
   FILL-IN-BANT = elagertrum.STMETER
   FILL-IN-PRIS = elagertrum.PRIS    
   lagerpris = elagertrum.PRIS
   /*FILL-IN-LAGERPLATS = 0*/
   varant = elagertrum.STMETER.
   musz = FALSE. 
   FIND LAST lagertrum  USE-INDEX TRUMMANR NO-ERROR.
   IF AVAILABLE lagertrum THEN FILL-IN-LAGERPLATS = lagertrum.TRUMMANR + 1.
   ELSE FILL-IN-LAGERPLATS = 1.
   /*IF Guru.Konstanter:globforetag = "snat" AND FILL-IN-VILAGERPLATS = "" THEN FILL-IN-VILAGERPLATS = "E".         
   IF FILL-IN-VILAGERPLATS = "" THEN FILL-IN-VILAGERPLATS = STRING(FILL-IN-LAGERPLATS).*/
   
   RUN enable_UI.       
   
   IF FILL-IN-ENR = "" THEN DO:
      ENABLE FILL-IN-ENR FILL-IN-BEST WITH FRAME {&FRAME-NAME}.
      DISABLE  FILL-IN-BEN FILL-IN-ENHET WITH FRAME {&FRAME-NAME}.
      FILL-IN-BANT:HIDDEN = TRUE.
   END.
   ELSE DO:      
      DISABLE  FILL-IN-ENR FILL-IN-BEN FILL-IN-ENHET FILL-IN-PRIS FILL-IN-BANT WITH FRAME {&FRAME-NAME}.
   END.
   {FRMSIZED.I}
   {musarrow.i}
   {DIA_M_SLUT.I}
   APPLY "ENTRY" TO FILL-IN-VILAGERPLATS IN FRAME {&FRAME-NAME}.
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
   IF Guru.Konstanter:appcon THEN DO:
      RUN LEVAPPV.P PERSISTENT SET leveransapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN LEVAPPV.P PERSISTENT SET leveransapph.
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
  DISPLAY FILL-IN-LAGERPLATS FILL-IN-VILAGERPLATS FILL-IN-ENR FILL-IN-BEN 
          FILL-IN-ENHET FILL-IN-PRIS FILL-IN-BEST FILL-IN-BANT 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN-VILAGERPLATS FILL-IN-ENR FILL-IN-BEN FILL-IN-ENHET 
         FILL-IN-PRIS FILL-IN-BEST FILL-IN-BANT btn_ok BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

>>>>>>> branch 'master' of file:///\\server05\delad\REMOTEGURU\GuruRemote.git
