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

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{LAGERTRUM.I}
&Scoped-define NEW 
&Scoped-define SHARED 
{SPECMTRLTEMP.I}
DEFINE NEW SHARED TEMP-TABLE off_mtrl NO-UNDO     
   FIELD TOTALT AS DECIMAL.
/**/
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
DEFINE VARIABLE utlop AS INTEGER NO-UNDO.
DEFINE VARIABLE mtrlbapph AS HANDLE NO-UNDO.
DEFINE VARIABLE nytt_bestnr AS INTEGER NO-UNDO.
DEFINE VARIABLE motdatum AS DATE NO-UNDO. 




DEFINE INPUT  PARAMETER TABLE FOR  eaotrum.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-VILAGERPLATS FILL-IN-AONR ~
FILL-IN-DELNR FILL-IN-ENR FILL-IN-BEN FILL-IN-ENHET FILL-IN-PRIS ~
FILL-IN-DATUMUT FILL-IN-STARTM FILL-IN-UTTAGET btn_ok BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-LAGERPLATS FILL-IN-VILAGERPLATS ~
FILL-IN-AONR FILL-IN-DELNR FILL-IN-ENR FILL-IN-BEN FILL-IN-ENHET ~
FILL-IN-PRIS FILL-IN-DATUMUT FILL-IN-STARTM FILL-IN-UTTAGET 

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

DEFINE VARIABLE FILL-IN-AONR AS CHARACTER FORMAT "X(6)":U 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DATUMUT AS DATE FORMAT "99/99/99":U 
     LABEL "Uttaget" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DELNR AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "Delnr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENHET AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enhet" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

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
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-STARTM AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Lager" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-UTTAGET AS INTEGER FORMAT "->>>>9":U INITIAL 0 
     LABEL "Utgått" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-VILAGERPLATS AS CHARACTER FORMAT "X(256)":U 
     LABEL "Trumma namn" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN-LAGERPLATS AT ROW 1.5 COL 15.5 COLON-ALIGNED
     FILL-IN-VILAGERPLATS AT ROW 2.67 COL 15.5 COLON-ALIGNED
     FILL-IN-AONR AT ROW 3.83 COL 15.5 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-DELNR AT ROW 5 COL 15.5 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-ENR AT ROW 6.17 COL 15.5 COLON-ALIGNED
     FILL-IN-BEN AT ROW 7.33 COL 15.5 COLON-ALIGNED
     FILL-IN-ENHET AT ROW 8.5 COL 15.5 COLON-ALIGNED
     FILL-IN-PRIS AT ROW 9.67 COL 15.5 COLON-ALIGNED WIDGET-ID 2
     FILL-IN-DATUMUT AT ROW 10.83 COL 15.5 COLON-ALIGNED
     FILL-IN-STARTM AT ROW 12 COL 15.5 COLON-ALIGNED
     FILL-IN-UTTAGET AT ROW 13.33 COL 15.5 COLON-ALIGNED
     btn_ok AT ROW 15.13 COL 22.75
     BTN_AVB AT ROW 15.13 COL 37.75
     SPACE(0.99) SKIP(0.40)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Uttag från Trumma nr":L
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
ON END-ERROR OF FRAME DIALOG-1 /* Uttag från Trumma nr */
DO:
  {muswait.i}
   IF VALID-HANDLE(mtrlbapph) THEN DELETE PROCEDURE mtrlbapph.      
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
   DEBUGGER:SET-BREAK().   
   ASSIGN
   FILL-IN-UTTAGET = INPUT FILL-IN-UTTAGET.   
   musz = FALSE.
   IF FILL-IN-UTTAGET > eaotrum.STMETER THEN DO:
      MESSAGE "Uttaget kan inte vara större än vad som finns på aktuell trumma"
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-UTTAGET IN FRAME {&FRAME-NAME}.
      RETURN.
   END.
   IF FILL-IN-UTTAGET < 0 THEN DO:   
      MESSAGE "Skall det verkligen läggas till meter på trumman?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL TITLE "Uttag från " + Guru.Konstanter:gaok + " " + eaotrum.AONR UPDATE svar2 AS LOGICAL.         
      IF svar2 THEN DO:            
      END.
      ELSE DO:
        APPLY "ENTRY" TO FILL-IN-UTTAGET IN FRAME {&FRAME-NAME}.
        RETURN.
      END.      
   END.   
   IF FILL-IN-UTTAGET = 0 THEN DO:   
      MESSAGE "Skall uttaget verkligen vara 0?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL TITLE "Uttag från " + Guru.Konstanter:gaok + " " + eaotrum.AONR UPDATE svar AS LOGICAL.         
      IF svar THEN DO:      
         musz = TRUE.
      END.
      ELSE DO:
        APPLY "ENTRY" TO FILL-IN-UTTAGET IN FRAME {&FRAME-NAME}.
        RETURN.
      END.      
   END.   
   IF musz = TRUE THEN DO:
      musz = FALSE.
      IF AVAILABLE eaotrum THEN DO:
         /*ta bort lagerplatskoppling och BESTDEPAO om det inte blev något uttag från trumman*/
         RUN kollpplatstr_UI IN mtrlbapph (INPUT Guru.GlobalaVariabler:GuruVdepnr,INPUT Guru.GlobalaVariabler:GuruProjdepnr,INPUT TABLE eaotrum).
         /*sätt aokopp = false på alla poster med lagerplats*/
         FOR EACH lagertrum WHERE lagertrum.TRUMMANR = eaotrum.TRUMMANR:
            ASSIGN
            lagertrum.aokopp = FALSE.
            IF lagertrum.TRUMMA BEGINS "U" THEN.
            ELSE DO:
               ASSIGN
               lagertrum.LAGERPLATSI = 0
               lagertrum.LAGERPLATSC = "".
            END.   
         END.
        
         RUN SparafaltTrumma_UI IN  Guru.GlobalaVariabler:ClienttdSetapph (INPUT TABLE lagertrum,INPUT TABLE aotrum).
      END.                                  
   END.    
   ELSE DO:         
      IF AVAILABLE eaotrum THEN DO:
        
         /*trumman måste vara mottagen för att  kunna skicks tillbaka*/
         RUN kollmottrumma_UI IN mtrlbapph (INPUT Guru.GlobalaVariabler:GuruVdepnr,INPUT Guru.GlobalaVariabler:GuruProjdepnr,INPUT TABLE eaotrum, OUTPUT motdatum).
         IF motdatum = ? THEN DO:
            MESSAGE "Trumman är ännu ej mottagen. Gå till Leveransstatus för att göra detta innan."  VIEW-AS ALERT-BOX.            
            RETURN.                 
         END.   
        
         ASSIGN
         eaotrum.UTMETER = FILL-IN-UTTAGET
         eaotrum.DATUMIN = TODAY
         eaotrum.STMETER = eaotrum.STMETER -  eaotrum.UTMETER .
      END.
      FIND FIRST lagertrum WHERE lagertrum.TRUMMANR = eaotrum.TRUMMANR AND lagertrum.TRUMMA = "S" NO-ERROR.
      IF AVAILABLE lagertrum THEN DO:
         utlop = lagertrum.LOPNR + 1.
         ASSIGN 
         lagertrum.LAGMETER = eaotrum.STMETER
         lagertrum.UTMETER  = lagertrum.UTMETER + eaotrum.UTMETER
         lagertrum.LOPNR  = utlop
         lagertrum.AOKOPP  = FALSE
         lagertrum.LAGERPLATSI = 0
         lagertrum.LAGERPLATSC = "".
         
         
      END.
      RUN SparafaltTrumma_UI IN  Guru.GlobalaVariabler:ClienttdSetapph (INPUT TABLE lagertrum,INPUT TABLE aotrum).         
      
      CREATE lagertrum.
      ASSIGN
      lagertrum.TRUMMANR = eaotrum.TRUMMANR
      lagertrum.TRUMMAPLATS = eaotrum.TRUMMAPLATS
      lagertrum.trumma = "U" + STRING(utlop)
      lagertrum.ENR = eaotrum.ENR 
      lagertrum.BENAMNING = eaotrum.BENAMNING
      lagertrum.ENHET = eaotrum.ENHET      
      lagertrum.UTMETER = eaotrum.UTMETER
      lagertrum.LAGMETER = eaotrum.STMETER 
      lagertrum.AONR = eaotrum.AONR
      lagertrum.DELNR = eaotrum.DELNR
      lagertrum.DATUM = TODAY
      lagertrum.LOPNR  = utlop
      lagertrum.AOKOPP = FALSE
      lagertrum.DEPNR = Guru.GlobalaVariabler:GuruVdepnr
      lagertrum.LAGERPLATSI = eaotrum.LAGERPLATSI
      lagertrum.LAGERPLATSC = eaotrum.LAGERPLATSC
      lagertrum.PRIS = eaotrum.PRIS
      lagertrum.ANVANDARE =  Guru.Konstanter:globanv.   
      RUN SparafaltTrumma_UI IN  Guru.GlobalaVariabler:ClienttdSetapph (INPUT TABLE lagertrum,INPUT TABLE aotrum).
      FOR EACH lagertrum WHERE lagertrum.TRUMMANR = eaotrum.TRUMMANR:
         lagertrum.aokopp = FALSE.  
      END.
      RUN SparafaltTrumma_UI IN  Guru.GlobalaVariabler:ClienttdSetapph (INPUT TABLE lagertrum,INPUT TABLE aotrum).
      EMPTY TEMP-TABLE espec_mtrl NO-ERROR. 
      EMPTY TEMP-TABLE off_mtrl NO-ERROR. 
      CREATE espec_mtrl.      
      ASSIGN
      espec_mtrl.ENR = eaotrum.ENR
      espec_mtrl.BENAMNING = eaotrum.BENAMNING
      espec_mtrl.ENHET = eaotrum.ENHET
      espec_mtrl.SALDO = eaotrum.UTMETER.                           
      FIND FIRST lagertrum WHERE lagertrum.TRUMMANR = eaotrum.TRUMMANR AND lagertrum.trumma = "s" NO-ERROR.
      IF AVAILABLE lagertrum THEN DO:
          espec_mtrl.NPRIS = lagertrum.PRIS.
      END.
      
      RUN uppmtrldeptr_UI IN mtrlbapph (INPUT Guru.GlobalaVariabler:GuruVdepnr,INPUT eaotrum.ENR , INPUT FILL-IN-UTTAGET  ).                      
      RUN bestnrhmt_UI IN mtrlbapph (INPUT Guru.GlobalaVariabler:GuruVdepnr,OUTPUT nytt_bestnr).
      DEBUGGER:SET-BREAK().
      /*försök hitta rätt FIFO-post trumma  Lena 20191022*/             
      RUN okubesttrum_UI IN mtrlbapph (INPUT eaotrum.AONR, INPUT eaotrum.DELNR,INPUT Guru.GlobalaVariabler:GuruVdepnr , INPUT " " ,INPUT nytt_bestnr ,INPUT " ",
                               INPUT Guru.Konstanter:globanv, INPUT TABLE espec_mtrl , INPUT TABLE off_mtrl).
      /*RUN okubest_UI IN mtrlbapph (INPUT eaotrum.AONR, INPUT eaotrum.DELNR,INPUT Guru.GlobalaVariabler:GuruVdepnr , INPUT " " ,INPUT nytt_bestnr ,INPUT " ",
                               INPUT Guru.Konstanter:globanv, INPUT TABLE espec_mtrl , INPUT TABLE off_mtrl).*/
                               
      
                               
      RUN tillbyggplats_UI IN mtrlbapph (INPUT Guru.GlobalaVariabler:GuruVdepnr,INPUT Guru.GlobalaVariabler:GuruProjdepnr ,INPUT TABLE  eaotrum, INPUT utlop  ).                                                                          
      /*
      Uppdatera saldo i lager !!!!!!!!  MTRLDEP.SALDO
      */
   END.   
      APPLY "GO" TO FRAME {&FRAME-NAME}.     
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AONR DIALOG-1
ON ENTRY OF FILL-IN-AONR IN FRAME DIALOG-1 /* Aonr */
DO:
   FILL-IN-AONR = INPUT FILL-IN-AONR.   
   
   
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
   ASSIGN   
   FILL-IN-AONR:LABEL = Guru.Konstanter:gaok.    
   FIND FIRST eaotrum NO-LOCK NO-ERROR.
   ASSIGN
   FILL-IN-ENR:LABEL = Guru.Konstanter:genk
   FILL-IN-AONR = eaotrum.AONR
   FILL-IN-DELNR = eaotrum.DELNR
   FILL-IN-LAGERPLATS = eaotrum.TRUMMANR
   FILL-IN-VILAGERPLATS = eaotrum.TRUMMAPLATS
   FILL-IN-BEN = eaotrum.BENAMNING
   FILL-IN-ENHET = eaotrum.ENHET
   FILL-IN-ENR = eaotrum.ENR
   FILL-IN-DATUMUT = eaotrum.DATUMUT 
   FILL-IN-STARTM = eaotrum.STMETER
   FILL-IN-PRIS = eaotrum.PRIS 
   FILL-IN-UTTAGET = 0.    
   RUN enable_UI.       
   DISABLE FILL-IN-AONR FILL-IN-DELNR FILL-IN-LAGERPLATS FILL-IN-VILAGERPLATS FILL-IN-ENR FILL-IN-BEN FILL-IN-ENHET FILL-IN-DATUMUT 
   FILL-IN-STARTM FILL-IN-PRIS WITH FRAME {&FRAME-NAME}.
   
   {FRMSIZED.I}
   {musarrow.i}
   {DIA_M_SLUT.I}
   APPLY "ENTRY" TO FILL-IN-UTTAGET IN FRAME {&FRAME-NAME}.
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
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph.
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
  DISPLAY FILL-IN-LAGERPLATS FILL-IN-VILAGERPLATS FILL-IN-AONR FILL-IN-DELNR 
          FILL-IN-ENR FILL-IN-BEN FILL-IN-ENHET FILL-IN-PRIS FILL-IN-DATUMUT 
          FILL-IN-STARTM FILL-IN-UTTAGET 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN-VILAGERPLATS FILL-IN-AONR FILL-IN-DELNR FILL-IN-ENR 
         FILL-IN-BEN FILL-IN-ENHET FILL-IN-PRIS FILL-IN-DATUMUT FILL-IN-STARTM 
         FILL-IN-UTTAGET btn_ok BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

