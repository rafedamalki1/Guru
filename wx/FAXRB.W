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

  Created: 95/09/15 -  2:57 pm

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
&Scoped-define NEW                  
DEFINE NEW SHARED VARIABLE str AS CHARACTER FORMAT "X(92)" NO-UNDO. 
DEFINE NEW SHARED VARIABLE str2 AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE NEW SHARED VARIABLE utslutvar AS LOGICAL NO-UNDO.



DEFINE NEW SHARED VARIABLE rb-begin-page AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE rb-end-page AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE globforetag AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE jmlist AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.   
DEFINE SHARED VARIABLE faxut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE valut AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.         
DEFINE SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE avdatum AS DATE NO-UNDO.      
DEFINE SHARED VARIABLE globsidl LIKE ANVANDARE.SIDL NO-UNDO.
DEFINE SHARED VARIABLE globsids LIKE ANVANDARE.SIDS NO-UNDO. 
DEFINE VARIABLE rb-print AS CHARACTER NO-UNDO.
DEFINE VARIABLE rbvar AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE result AS CHARACTER  NO-UNDO.
DEFINE VARIABLE startkund AS INTEGER  NO-UNDO.
DEFINE VARIABLE slutkund AS INTEGER  NO-UNDO.
DEFINE VARIABLE startdatum AS DATE  NO-UNDO.
DEFINE VARIABLE slutdatum AS DATE  NO-UNDO.
DEFINE VARIABLE succ AS LOGICAL  NO-UNDO.
DEFINE VARIABLE datval AS LOGICAL  NO-UNDO.
DEFINE VARIABLE sida2var AS LOGICAL  NO-UNDO.
DEFINE VARIABLE sidnr AS INTEGER NO-UNDO.
DEFINE VARIABLE raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE stde AS CHARACTER  NO-UNDO.
DEFINE VARIABLE botvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE makkoll AS CHARACTER NO-UNDO.   
DEFINE VARIABLE raknare2 AS INTEGER NO-UNDO. 
DEFINE VARIABLE ednum AS INTEGER NO-UNDO.
DEFINE VARIABLE tidrb AS INTEGER NO-UNDO.
DEFINE VARIABLE faxrb AS INTEGER NO-UNDO.
DEFINE VARIABLE berrb AS INTEGER NO-UNDO. 
DEFINE VARIABLE faxuppd AS LOGICAL NO-UNDO.
DEFINE VARIABLE allval AS LOGICAL NO-UNDO.
DEFINE VARIABLE faxuppdspar AS LOGICAL NO-UNDO. 
DEFINE VARIABLE persrec AS RECID NO-UNDO.     
DEFINE VARIABLE ednum3 AS INTEGER NO-UNDO. 
DEFINE VARIABLE fnamn LIKE PERSONALTAB.FORNAMN NO-UNDO.
DEFINE VARIABLE enamn LIKE PERSONALTAB.EFTERNAMN NO-UNDO.
DEFINE VARIABLE tidskick AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE varmed AS INTEGER NO-UNDO.
DEFINE VARIABLE utvarnr LIKE UTFAXNUMMER.FAXNUMMER NO-UNDO.
DEFINE VARIABLE sidutval AS LOGICAL NO-UNDO.
DEFINE QUERY utfaxq FOR UTFAXNUMMER. 
DEFINE QUERY utrubq FOR UTRUBRIK. 
DEFINE QUERY utrubq1 FOR UTRUBRIK1. 
DEFINE QUERY utrubq2 FOR UTRUBRIK2.    
DEFINE QUERY utobjq FOR UTOBJEKT.    
DEFINE QUERY persq FOR PERSONALTAB.   
DEFINE QUERY makq FOR MAKLARE.    
DEFINE QUERY ompq FOR OMRPERS.         
DEFINE BUFFER persbuff FOR PERSONALTAB.
DEFINE TEMP-TABLE medeltemp
   FIELD OMRADE LIKE OMRADETAB.OMRADE
   FIELD BELOPP AS INTEGER
   FIELD ANTAL AS INTEGER
   INDEX OMR IS PRIMARY OMRADE.
DEFINE TEMP-TABLE botyptemp
   FIELD BOTYP LIKE OMRPERS.BOTYP
   INDEX BOTYP IS PRIMARY BOTYP.
DEFINE TEMP-TABLE sidpers
   FIELD KUNDNR LIKE PERSONALTAB.KUNDNR
   FIELD PERSONALKOD LIKE PERSONALTAB.PERSONALKOD
   FIELD SIDOR AS INTEGER
   INDEX SIDOR IS PRIMARY SIDOR KUNDNR.

DEFINE SHARED TEMP-TABLE valpers
   FIELD PERSONALKOD LIKE PERSONALTAB.PERSONALKOD.

DEFINE QUERY valq FOR valpers,PERSONALTAB.
DEFINE BUFFER sidpersbuff FOR sidpers.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-VINST

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-35 BTN_NAS BTN_AVS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_NAS 
     LABEL "Nästa":L 
     SIZE 12 BY 1.5.

DEFINE RECTANGLE RECT-35
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 76.5 BY 2.5
     BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-VINST
     BTN_NAS AT ROW 1.5 COL 14
     BTN_AVS AT ROW 1.5 COL 49.88
     RECT-35 AT ROW 1 COL 4.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.25 ROW 1.41
         SIZE 83.13 BY 6.68
         BGCOLOR 8 .


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
         TITLE              = "Sammanställning"
         COLUMN             = 11.88
         ROW                = 6.5
         HEIGHT             = 7.09
         WIDTH              = 83.63
         MAX-HEIGHT         = 25
         MAX-WIDTH          = 88.25
         VIRTUAL-HEIGHT     = 25
         VIRTUAL-WIDTH      = 88.25
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WINDOW-2
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-VINST
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-VINST
/* Query rebuild information for FRAME FRAME-VINST
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-VINST */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS WINDOW-2
ON CHOOSE OF BTN_AVS IN FRAME FRAME-VINST /* Avsluta */
DO:              
   
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS WINDOW-2
ON GO OF BTN_AVS IN FRAME FRAME-VINST /* Avsluta */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NAS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NAS WINDOW-2
ON CHOOSE OF BTN_NAS IN FRAME FRAME-VINST /* Nästa */
DO:
   {muswait.i}                    
   
   ASSIGN
   stde = ""
   botvar = "".                 
   NASTA:           
   REPEAT:          
      status-mus2 = SESSION:SET-WAIT-STATE("GENERAL").  
      RUN nasta_UI.          
      IF NOT AVAILABLE PERSONALTAB THEN DO:   
         status-mus2 = SESSION:SET-WAIT-STATE("").         
         musz = TRUE.
         LEAVE NASTA.
      END.
      ELSE DO:  
         berrb = ETIME.
         WINDOW-2:TITLE = "SPEKULANT - " + STRING(PERSONALTAB.KUNDNR) + " " + STRING(TIME,"HH:MM:SS").                    
         ASSIGN
         stde = ""
         botvar = "".        
         RUN huvud_UI.
         RUN summa2_UI.                           
         FIND FIRST UTOBJEKT WHERE UTOBJEKT.PERSONALKOD = PERSONALTAB.PERSONALKOD 
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE UTOBJEKT THEN DO:
            NEXT NASTA.         
         END.    
         RUN text_UI. 
         status-mus2 = SESSION:SET-WAIT-STATE("").                              
         LEAVE NASTA.          
      END.                                                          
   END.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      APPLY "WINDOW-CLOSE":U TO WINDOW-2. 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NAS WINDOW-2
ON GO OF BTN_NAS IN FRAME FRAME-VINST /* Nästa */
DO:
   RETURN.
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
   datval = FALSE.
   rb-begin-page = 1.
   rb-end-page = 99999.
   RUN ZEEKPATH.P.
   IF skrivut = TRUE THEN DO:
      /*RUN VALSKRIV.W (OUTPUT rb-print).*/
      RUN ZSKRIVVA.W.
      IF musz = TRUE THEN DO:
         musz = FALSE.
         skrivut = FALSE.
         APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
         LEAVE MAIN-BLOCK.  
      END.
      rb-print = SESSION:PRINTER-NAME.
   END.
   MESSAGE "SKA SENASTE FAXDATUM UPPDATERAS?" VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO 
   UPDATE val1 AS LOGICAL.
   CASE val1:
      WHEN TRUE THEN DO:
         faxuppd = TRUE.                           
      END.
      WHEN FALSE THEN DO:
        faxuppd = FALSE.  
      END.
   END CASE. 
   IF jmlist = TRUE THEN DO:
      MESSAGE "SKA TENDENS UPPDATERAS?" VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO 
      UPDATE upjm AS LOGICAL.
   END.   
   MESSAGE "SKALL SÖKPARAMETRAR VISAS?" VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO 
   UPDATE val2 AS LOGICAL.
   CASE val2:
      WHEN TRUE THEN DO:
         utslutvar = TRUE.                           
      END.
      WHEN FALSE THEN DO:
         utslutvar = FALSE.  
      END.
   END CASE.             
       
   ASSIGN
   stde = ""
   botvar = "".            
    
str=
"=====================================================================================".      
   {muswait.i}
   IF valut = FALSE THEN DO:
      MESSAGE "Vill du ta alla kunder svara JA!" SKIP
      "Vill du välja start och slut kund svara NEJ!"
      VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO 
      UPDATE uppval AS LOGICAL.
      CASE uppval:
          WHEN TRUE THEN DO:
             allval = TRUE.                           
          END.
          WHEN FALSE THEN DO:   
             status-mus2 = SESSION:SET-WAIT-STATE("").
             RUN STARKUND.W (OUTPUT startkund, OUTPUT slutkund) . 
             allval = FALSE.             
             status-mus2 = SESSION:SET-WAIT-STATE("GENERAL").
          END.
      END CASE. 
      IF faxut = FALSE AND skrivut = FALSE THEN DO: 
         IF allval = TRUE THEN DO:
            OPEN QUERY persq FOR EACH PERSONALTAB WHERE PERSONALTAB.AKTIV = TRUE 
            USE-INDEX KUNDNR NO-LOCK.   
         END.
         ELSE DO:
            OPEN QUERY persq FOR EACH PERSONALTAB WHERE PERSONALTAB.AKTIV = TRUE AND
            PERSONALTAB.KUNDNR >= startkund AND PERSONALTAB.KUNDNR <= slutkund             
            USE-INDEX KUNDNR NO-LOCK.   
         END. 
      END.
      ELSE IF faxut = TRUE THEN DO:
         IF allval = TRUE THEN DO: 
            OPEN QUERY persq FOR EACH PERSONALTAB WHERE 
            PERSONALTAB.AKTIV = TRUE AND PERSONALTAB.TELEFON2 NE "" AND
            PERSONALTAB.FAXSTART NE "0000" AND PERSONALTAB.FAXSLUT NE "0000"
            USE-INDEX KUNDNR NO-LOCK.                        
         END.
         ELSE DO: 
            OPEN QUERY persq FOR EACH PERSONALTAB WHERE PERSONALTAB.AKTIV = TRUE AND 
            PERSONALTAB.TELEFON2 NE "" AND
            PERSONALTAB.KUNDNR >= startkund AND PERSONALTAB.KUNDNR <= slutkund AND
            PERSONALTAB.FAXSTART NE "0000" AND PERSONALTAB.FAXSLUT NE "0000"
            USE-INDEX KUNDNR NO-LOCK.                         
         END.
      END.
      ELSE IF skrivut = TRUE THEN DO:    
         MESSAGE "DU HAR VALT SKRIVUT. VILL DU SKRIVA UT ENDAST BREV KUNDER?" 
         VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO UPDATE val3 AS LOGICAL.
         CASE val3:      
            WHEN TRUE THEN DO:
               IF allval = TRUE THEN DO:
                  OPEN QUERY persq FOR EACH PERSONALTAB WHERE PERSONALTAB.AKTIV = TRUE AND 
                  PERSONALTAB.TELEFON2 = "" 
                  USE-INDEX KUNDNR NO-LOCK.
               END.
               ELSE DO:
                  OPEN QUERY persq FOR EACH PERSONALTAB WHERE PERSONALTAB.AKTIV = TRUE AND 
                  PERSONALTAB.TELEFON2 = "" AND
                  PERSONALTAB.KUNDNR >= startkund AND PERSONALTAB.KUNDNR <= slutkund 
                  USE-INDEX KUNDNR NO-LOCK.
               END.                                         
            END.
            WHEN FALSE THEN DO:
               IF allval = TRUE THEN DO:
                  OPEN QUERY persq FOR EACH PERSONALTAB WHERE PERSONALTAB.AKTIV = TRUE 
                  USE-INDEX KUNDNR NO-LOCK.   
               END.
               ELSE DO:
                  OPEN QUERY persq FOR EACH PERSONALTAB WHERE PERSONALTAB.AKTIV = TRUE AND
                  PERSONALTAB.KUNDNR >= startkund AND PERSONALTAB.KUNDNR <= slutkund                  
                  USE-INDEX KUNDNR NO-LOCK.   
               END.   
            END.
         END CASE.
         IF utslutvar = FALSE THEN DO:
            MESSAGE "VILL DU SKRIVA UT RAPPORTERNA EFTER ANTAL SIDOR?" 
            VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO UPDATE val4 AS LOGICAL.
            CASE val4:      
               WHEN TRUE THEN DO:
                  sidutval = TRUE.
               END.
               WHEN FALSE THEN DO:
                  sidutval = FALSE.
               END.
            END CASE.
         END.              
      END.  
      GET LAST persq NO-LOCK.
   END.
   ELSE DO:
      MESSAGE "Vill välja start och slutdatum ?"
      VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO 
      UPDATE uppval1 AS LOGICAL.
      CASE uppval1:
          WHEN TRUE THEN DO:
             datval = TRUE.                           
             RUN STARDAT.W (OUTPUT startdatum, OUTPUT slutdatum).
          END.
          WHEN FALSE THEN DO:   
             datval = FALSE.             
          END.
      END CASE.
      OPEN QUERY valq FOR EACH valpers, 
      EACH PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valpers.PERSONALKOD 
      USE-INDEX KUNDNR NO-LOCK.
      GET LAST valq NO-LOCK.
   END.   
   status-mus2 = SESSION:SET-WAIT-STATE("GENERAL").
   IF AVAILABLE PERSONALTAB THEN DO: 
      persrec = RECID(PERSONALTAB).  
      /*
      IF PERSONALTAB.TELEFON2 = "" THEN DO:      
         FIND FIRST MEDDELANDE WHERE MEDDELANDE.MOTTAGARE = "KUNDBREV"
         USE-INDEX MEDD NO-LOCK NO-ERROR.
         IF AVAILABLE MEDDELANDE THEN DO:                          
            ednum = MEDDELANDE.ANTALRADER.                  
         END.
      END.    
      ELSE DO:      
         FIND FIRST MEDDELANDE WHERE MEDDELANDE.MOTTAGARE = "KUNDFAX"
         USE-INDEX MEDD NO-LOCK NO-ERROR.
         IF AVAILABLE MEDDELANDE THEN DO:
            ednum = MEDDELANDE.ANTALRADER. 
         END.
      END.
      */ 
   END.
   IF sidutval = TRUE THEN DO:
      RUN sidor_UI.
   END.  
   ELSE DO:    
      IF valut = FALSE THEN GET FIRST persq NO-LOCK.     
      ELSE GET FIRST valq NO-LOCK.         
      FAX: 
      REPEAT:      
         IF NOT AVAILABLE PERSONALTAB THEN DO:                  
            LEAVE FAX. 
         END.
         berrb = ETIME.
         WINDOW-2:TITLE = "SPEKULANT - " + STRING(PERSONALTAB.KUNDNR) + " " + STRING(TIME,"HH:MM:SS").  
         RUN huvud_UI.
         RUN summa2_UI.                             
         FIND FIRST UTOBJEKT WHERE UTOBJEKT.PERSONALKOD = PERSONALTAB.PERSONALKOD NO-LOCK NO-ERROR.
         IF NOT AVAILABLE UTOBJEKT THEN DO:                    
            ASSIGN
            stde = ""
            botvar = "".
            RUN nasta_UI.                  
            NEXT FAX.
         END. 
         IF faxut = FALSE AND skrivut = FALSE THEN LEAVE FAX.         
         ELSE DO:                          
            IF skrivut = TRUE THEN RUN ut_UI. 
            IF faxut = TRUE THEN RUN faxut_UI.                     
            ASSIGN
            stde = ""
            botvar = "".
            RUN nasta_UI.                                                                            
         END.               
      END.     
      IF faxut = TRUE AND valut = FALSE THEN DO:
         IF allval = TRUE THEN DO: 
            CLOSE QUERY persq.
            OPEN QUERY persq FOR EACH PERSONALTAB WHERE 
            PERSONALTAB.AKTIV = TRUE AND PERSONALTAB.TELEFON2 NE "" AND
            PERSONALTAB.FAXSTART = "0000" AND PERSONALTAB.FAXSLUT = "0000"
            USE-INDEX KUNDNR NO-LOCK.
         END.
         ELSE DO: 
            CLOSE QUERY persq.
            OPEN QUERY persq FOR EACH PERSONALTAB WHERE 
            PERSONALTAB.AKTIV = TRUE AND PERSONALTAB.TELEFON2 NE "" AND
            PERSONALTAB.KUNDNR >= startkund AND PERSONALTAB.KUNDNR <= slutkund AND
            PERSONALTAB.FAXSTART = "0000" AND PERSONALTAB.FAXSLUT = "0000"
            USE-INDEX KUNDNR NO-LOCK.
         END.
         GET FIRST persq NO-LOCK.
         FAXEJTID: 
         REPEAT:      
            IF NOT AVAILABLE PERSONALTAB THEN DO:                  
               LEAVE FAXEJTID. 
            END.
            berrb = ETIME.
            WINDOW-2:TITLE = "SPEKULANT - " + STRING(PERSONALTAB.KUNDNR) + " " + STRING(TIME,"HH:MM:SS"). 
            RUN huvud_UI.
            RUN summa2_UI.                             
            FIND FIRST UTOBJEKT WHERE UTOBJEKT.PERSONALKOD = PERSONALTAB.PERSONALKOD NO-LOCK NO-ERROR.
            IF NOT AVAILABLE UTOBJEKT THEN DO:                    
               ASSIGN
               stde = ""
               botvar = "".
               RUN nasta_UI.                  
               NEXT FAXEJTID.
            END. 
            RUN faxut_UI.                     
            ASSIGN
            stde = ""
            botvar = "".
            RUN nasta_UI.                                                                            
         END.               
      END.      
   END.                                                   
   IF faxut = FALSE AND skrivut = FALSE THEN DO:
      FIND FIRST UTOBJEKT WHERE UTOBJEKT.PERSONALKOD = PERSONALTAB.PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE UTOBJEKT THEN RUN text_UI.  
   END.
   ELSE DO:            
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
      LEAVE MAIN-BLOCK.        
   END.
   RUN enable_UI.          
   {musarrow.i}                                                
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE brevut_UI WINDOW-2 
PROCEDURE brevut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  /*UT*/                                      
   RUN RBBREV.P (INPUT PERSONALTAB.PERSONALKOD).  
   RUN tabort_UI.       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-2 _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-2 _DEFAULT-ENABLE
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
  ENABLE RECT-35 BTN_NAS BTN_AVS 
      WITH FRAME FRAME-VINST IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-VINST}
  VIEW WINDOW-2.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE epost_UI WINDOW-2 
PROCEDURE epost_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   tidrb = ETIME - berrb.
   WINDOW-2:TITLE = "SPEKULANT - " + STRING(PERSONALTAB.KUNDNR) + " " + STRING(TIME,"HH:MM:SS") + " TID SAMMAN. " + STRING(tidrb).
   berrb = ETIME.
   RUN RBMAIL.P (INPUT PERSONALTAB.PERSONALKOD).
   tidrb = ETIME - berrb.
   WINDOW-2:TITLE = "SPEKULANT - " + STRING(PERSONALTAB.KUNDNR) + " " + STRING(TIME,"HH:MM:SS") + " TID RB. " + STRING(tidrb).
   RUN tabort_UI.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE faxut_UI WINDOW-2 
PROCEDURE faxut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
/* RUN RBskriv.P (INPUT PERSONALTAB.PERSONALKOD).
   RUN RBTERM.P (INPUT PERSONALTAB.PERSONALKOD).*/
   tidrb = ETIME - berrb.
   WINDOW-2:TITLE = "SPEKULANT - " + STRING(PERSONALTAB.KUNDNR) + " " + STRING(TIME,"HH:MM:SS") + " TID SAMMAN. " + STRING(tidrb).
   berrb = ETIME.
   RUN RBFAX.P (INPUT PERSONALTAB.PERSONALKOD).
   tidrb = ETIME - berrb.
   WINDOW-2:TITLE = "SPEKULANT - " + STRING(PERSONALTAB.KUNDNR) + " " + STRING(TIME,"HH:MM:SS") + " TID RB. " + STRING(tidrb).
   RUN tabort_UI.                          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud_UI WINDOW-2 
PROCEDURE huvud_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   /*HUVUD*/     
   RUN tabort_UI.                                                                                         
   DO TRANSACTION:
      CREATE UTFAXNUMMER. 
      ASSIGN                             
      UTFAXNUMMER.PERSONALKOD = PERSONALTAB.PERSONALKOD.           
   END.
   IF faxut = TRUE THEN DO TRANSACTION:        
      IF PERSONALTAB.FAXSTART = "" AND PERSONALTAB.FAXSLUT = "" THEN 
      ASSIGN UTFAXNUMMER.FAXNUMMER = "[[fax=" + PERSONALTAB.TELEFON2 + "]]".      
      ELSE IF PERSONALTAB.FAXSTART = "0000" AND PERSONALTAB.FAXSLUT = "0000" THEN
      ASSIGN UTFAXNUMMER.FAXNUMMER = "[[fax=" + PERSONALTAB.TELEFON2 + "]]" + "[[beskrivning=" + "BG" + STRING(PERSONALTAB.KUNDNR) + "]]" .      
      ELSE DO:                  
         tidskick = STRING(TODAY,"99/99/99").
         SUBSTRING(tidskick,1,2) = SUBSTRING(STRING(TODAY,"99/99/99"),7,2).
         SUBSTRING(tidskick,7,2) = SUBSTRING(STRING(TODAY,"99/99/99"),1,2).
         ASSIGN
         UTFAXNUMMER.FAXNUMMER = "[[fax=" + PERSONALTAB.TELEFON2 + "]]" + " " +
         "[[TID=" + tidskick + " " + STRING(PERSONALTAB.FAXSTART,"99:99") + "," + 
         STRING(PERSONALTAB.FAXSLUT,"99:99") + "]]" + "[[beskrivning=" + "BG" + STRING(PERSONALTAB.KUNDNR) + "]]".
      END.
      IF utslutvar = TRUE THEN DO:
         utvarnr = UTFAXNUMMER.FAXNUMMER.         
         UTFAXNUMMER.FAXNUMMER = UTFAXNUMMER.FAXNUMMER + " [[BIFOGA-FAX=C:\PROTEMP8\" + PERSONALTAB.PERSONALKOD + "]]".
      END.
      ASSIGN                  
      UTFAXNUMMER.PERSONALKOD = PERSONALTAB.PERSONALKOD.   
   END.          
   ASSIGN
   fnamn = PERSONALTAB.FORNAMN 
   enamn = PERSONALTAB.EFTERNAMN.
   ASSIGN
   fnamn = TRIM(fnamn)
   enamn = TRIM(enamn). 
   fnamn = CAPS(SUBSTRING(fnamn,1,1)) + LC(SUBSTRING(fnamn,2)). 
   enamn = CAPS(SUBSTRING(enamn,1,1)) + LC(SUBSTRING(enamn,2)).  
   IF INDEX(fnamn,"-") = 0 THEN DO: 
      fnamn = fnamn.
   END.             
   ELSE DO:
      ednum3 = INDEX(fnamn,"-").
      fnamn = CAPS(SUBSTRING(fnamn,1,1)) + LC(SUBSTRING(fnamn,2)).
      fnamn = SUBSTRING(fnamn,1,ednum3) + CAPS(SUBSTRING(fnamn,ednum3 + 1,1)) + LC(SUBSTRING(fnamn,ednum3 + 2)).
   END.
   IF INDEX(enamn,"-") = 0 THEN DO: 
      enamn = enamn.     
   END.             
   ELSE DO:
      ednum3 = INDEX(enamn,"-").
      enamn = CAPS(SUBSTRING(enamn,1,1)) + LC(SUBSTRING(enamn,2)).
      enamn = SUBSTRING(enamn,1,ednum3) + CAPS(SUBSTRING(enamn,ednum3 + 1,1)) + LC(SUBSTRING(enamn,ednum3 + 2)).      
   END.  
   IF INDEX(fnamn," ") = 0 THEN DO: 
      fnamn = fnamn.
   END.             
   ELSE DO:
      ednum3 = INDEX(fnamn," ").
      fnamn = CAPS(SUBSTRING(fnamn,1,1)) + LC(SUBSTRING(fnamn,2)).
      fnamn = SUBSTRING(fnamn,1,ednum3) + CAPS(SUBSTRING(fnamn,ednum3 + 1,1)) + LC(SUBSTRING(fnamn,ednum3 + 2)).
   END.
   IF INDEX(enamn," ") = 0 THEN DO: 
      enamn = enamn.  
   END.             
   ELSE DO:
      ednum3 = INDEX(enamn," ").
      enamn = CAPS(SUBSTRING(enamn,1,1)) + LC(SUBSTRING(enamn,2)).
      enamn = SUBSTRING(enamn,1,ednum3) + CAPS(SUBSTRING(enamn,ednum3 + 1,1)) + LC(SUBSTRING(enamn,ednum3 + 2)).      
   END.    
   
   IF PERSONALTAB.FORETAG NE "" THEN DO TRANSACTION:      
      
      IF SUBSTRING(PERSONALTAB.FORETAG,1,3) = "C/O" THEN DO:
         ASSIGN
         UTFAXNUMMER.NAMN = fnamn + " " + enamn.          
         UTFAXNUMMER.EFTERNAMN = PERSONALTAB.FORETAG.      
         
      END.     
      ELSE DO:
         ASSIGN
         UTFAXNUMMER.NAMN = "Att " + fnamn + " " + enamn /*+ 
         "                               KUNDNR " + PERSONALTAB.PERSONALKOD */           
         UTFAXNUMMER.EFTERNAMN = "Till " + CAPS(PERSONALTAB.FORETAG).      
      END.
   END.  
   ELSE DO TRANSACTION:      
      ASSIGN      
      UTFAXNUMMER.NAMN = "Till " + fnamn + " " + enamn /*+            
      "                               KUNDNR " + PERSONALTAB.PERSONALKOD*/.
       
   END.  
   IF faxut = FALSE THEN DO TRANSACTION:                      
      ASSIGN
      fnamn = PERSONALTAB.GATUADRESS.       
      ASSIGN
      fnamn = TRIM(fnamn).      
      fnamn = CAPS(SUBSTRING(fnamn,1,1)) + LC(SUBSTRING(fnamn,2)).       
      IF INDEX(fnamn,"-") = 0 THEN DO: 
         fnamn = fnamn.
      END.             
      ELSE DO:
         ednum3 = INDEX(fnamn,"-").
         fnamn = CAPS(SUBSTRING(fnamn,1,1)) + LC(SUBSTRING(fnamn,2)).
         fnamn = SUBSTRING(fnamn,1,ednum3) + CAPS(SUBSTRING(fnamn,ednum3 + 1,1)) + LC(SUBSTRING(fnamn,ednum3 + 2)).
      END.
      IF INDEX(fnamn," ") = 0 THEN DO: 
         fnamn = fnamn.
      END.             
      ELSE DO:
         ednum3 = INDEX(fnamn," ").
         fnamn = CAPS(SUBSTRING(fnamn,1,1)) + LC(SUBSTRING(fnamn,2)).
         fnamn = SUBSTRING(fnamn,1,ednum3) + CAPS(SUBSTRING(fnamn,ednum3 + 1,1)) + LC(SUBSTRING(fnamn,ednum3 + 2)).
      END.
      ASSIGN
      UTFAXNUMMER.LOGGA = fnamn 
      UTFAXNUMMER.MEDD = STRING(CAPS(PERSONALTAB.POSTNUMMER),"XXX XX")  + " " + CAPS(PERSONALTAB.POSTADRESS).
   END.                           
    
   /*
   IF PERSONALTAB.TELEFON2 = "" THEN DO:      
      FIND FIRST MEDDELANDE WHERE MEDDELANDE.MOTTAGARE = "KUNDBREV"
      USE-INDEX MEDD NO-LOCK NO-ERROR.
      IF AVAILABLE MEDDELANDE THEN DO TRANSACTION:         
         UTFAXNUMMER.MEDD = MEDDELANDE.MEDD.
      END.
   END.    
   ELSE DO:      
      FIND FIRST MEDDELANDE WHERE MEDDELANDE.MOTTAGARE = "KUNDFAX"
      USE-INDEX MEDD NO-LOCK NO-ERROR.
      IF AVAILABLE MEDDELANDE THEN DO TRANSACTION:         
         UTFAXNUMMER.MEDD = MEDDELANDE.MEDD.
      END.
   END.
   */                                               
   OPEN QUERY makq FOR EACH MAKLARE NO-LOCK.
   GET FIRST makq.       
   ASSIGN         
   makkoll = ""
   raknare = 1
   raknare2 = 4.
                     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nasta_UI WINDOW-2 
PROCEDURE nasta_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   RUN tabort_UI.
   NASTA:
   REPEAT:
      IF valut = FALSE THEN GET NEXT persq NO-LOCK.        
      ELSE GET NEXT valq NO-LOCK. 
      IF AVAILABLE PERSONALTAB THEN DO:
         IF PERSONALTAB.PERSONALKOD = "1011" THEN DO:
            NEXT NASTA.                 
         END.         
      END.
      LEAVE NASTA.
   END.                  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sidor2_UI WINDOW-2 
PROCEDURE sidor2_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   rb-begin-page = 2.
   rb-end-page = 2.
   faxuppd = faxuppdspar.
   MESSAGE "NU KÖRS DE SOM HAR TVÅ SIDOR OCH DERAS ANDRA SIDA" VIEW-AS ALERT-BOX.
   OPEN QUERY sid2q FOR EACH sidpersbuff WHERE sidpersbuff.SIDOR = 2  
   USE-INDEX SIDOR NO-LOCK.
   GET FIRST sid2q NO-LOCK.
   DO WHILE AVAILABLE(sidpersbuff):              
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = sidpersbuff.PERSONALKOD
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
      WINDOW-2:TITLE = 
      "SPEKULANT - " + STRING(PERSONALTAB.KUNDNR) + " ANTAL SIDOR " + STRING(sidpersbuff.SIDOR) + " ANDRA SIDAN".
      RUN huvud_UI.
      RUN summa2_UI.                             
      FIND FIRST UTOBJEKT WHERE UTOBJEKT.PERSONALKOD = PERSONALTAB.PERSONALKOD NO-LOCK NO-ERROR.
      IF NOT AVAILABLE UTOBJEKT THEN DO:                    
         ASSIGN
         stde = ""
         botvar = "".
      END. 
      ELSE RUN ut_UI. 
      ASSIGN
      stde = ""
      botvar = "".
      GET NEXT sid2q NO-LOCK.                      
   END.
   CLOSE QUERY sid2q.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sidor_UI WINDOW-2 
PROCEDURE sidor_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   faxuppdspar = faxuppd.
   faxuppd = FALSE.   
   GET FIRST persq NO-LOCK.
   DO WHILE AVAILABLE(PERSONALTAB):   
      IF PERSONALTAB.PERSONALKOD = "1011" THEN musz = musz.
      ELSE DO:
         WINDOW-2:TITLE = 
         "BERÄKNINGAR FÖR ANTALET SIDOR FÖR " + STRING(PERSONALTAB.KUNDNR).
         RUN huvud_UI.
         RUN summa2_UI.                             
         FIND FIRST UTOBJEKT WHERE UTOBJEKT.PERSONALKOD = PERSONALTAB.PERSONALKOD NO-LOCK NO-ERROR.
         IF NOT AVAILABLE UTOBJEKT THEN DO:                    
            ASSIGN
            stde = ""
            botvar = "".
         END. 
         ELSE DO:
            RUN brevut_UI.
            rbvar = wtidvar + "rbrun.out".            
            RUN aderb/_getini.p ("faxen2","RO-PAGES","",
            OUTPUT result,rbvar, OUTPUT succ).                        
            CREATE sidpers.
            ASSIGN
            sidpers.sidor = INTEGER(result)
            sidpers.PERSONALKOD = PERSONALTAB.PERSONALKOD
            sidpers.KUNDNR = PERSONALTAB.KUNDNR.
            ASSIGN
            stde = ""
            botvar = "".
         END.
      END.
      GET NEXT persq NO-LOCK.                  
   END.   
   sidnr = 0.
   sida2var = FALSE.
   faxuppd = faxuppdspar.   
   
   OPEN QUERY sidq FOR EACH sidpers USE-INDEX SIDOR NO-LOCK.
   GET FIRST sidq NO-LOCK.
   DO WHILE AVAILABLE(sidpers):     
      IF sidnr NE sidpers.SIDOR THEN DO:         
         IF sidpers.SIDOR = 2 THEN DO:
            MESSAGE "NU KÖRS DE SOM HAR " sidpers.SIDOR " SIDOR" VIEW-AS ALERT-BOX.
            MESSAGE "SKALL ALLA FÖRSTA SIDOR SKRIVAS UT SEPARAT ?" VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO 
            UPDATE val10 AS LOGICAL.
            CASE val10:
               WHEN TRUE THEN DO:
                  faxuppd = FALSE.                  
                  sida2var = TRUE.
                  rb-begin-page = 1.
                  rb-end-page = 1.                           
               END.
               WHEN FALSE THEN DO:
                  sida2var = FALSE.
                  rb-begin-page = 1.
                  rb-end-page = 99999.  
               END.
            END CASE.
         END.
         ELSE IF sidnr = 2 AND sidpers.SIDOR >= 3 AND sida2var = TRUE THEN DO:
            /*ANDRA SIDAN KÖRS NU. */
            sida2var = FALSE.
            RUN sidor2_UI.
            MESSAGE "NU KÖRS DE SOM HAR " sidpers.SIDOR " SIDOR" VIEW-AS ALERT-BOX.
            rb-begin-page = 1.
            rb-end-page = 99999.
         END.
         ELSE DO:
            MESSAGE "NU KÖRS DE SOM HAR " sidpers.SIDOR " SIDOR" VIEW-AS ALERT-BOX.
            rb-begin-page = 1.
            rb-end-page = 99999.
         END.
         sidnr = sidpers.SIDOR.   
      END.  
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = sidpers.PERSONALKOD
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
      IF rb-end-page = 1 THEN DO:
         faxuppd = FALSE.
         WINDOW-2:TITLE = 
         "SPEKULANT - " + STRING(PERSONALTAB.KUNDNR) + " ANTAL SIDOR " + STRING(sidpers.SIDOR) + " FÖRSTA SIDAN".
      END.
      ELSE DO:
         WINDOW-2:TITLE = 
         "SPEKULANT - " + STRING(PERSONALTAB.KUNDNR) + " ANTAL SIDOR " + STRING(sidpers.SIDOR).       
      END.       
      RUN huvud_UI.
      RUN summa2_UI.                             
      FIND FIRST UTOBJEKT WHERE UTOBJEKT.PERSONALKOD = PERSONALTAB.PERSONALKOD NO-LOCK NO-ERROR.
      IF NOT AVAILABLE UTOBJEKT THEN DO:                    
         ASSIGN
         stde = ""
         botvar = "".
      END. 
      ELSE RUN ut_UI. 
      ASSIGN
      stde = ""
      botvar = "".
      GET NEXT sidq NO-LOCK.                      
   END.
   IF sida2var = TRUE THEN DO:
      sida2var = FALSE.
      RUN sidor2_UI.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sokk_UI WINDOW-2 
PROCEDURE sokk_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DO TRANSACTION:
      CREATE UTFAXNUMMER2. 
      ASSIGN                             
      UTFAXNUMMER2.EFTERNAMN = UTFAXNUMMER.EFTERNAMN
      UTFAXNUMMER2.FORETAG = UTFAXNUMMER.FORETAG
      UTFAXNUMMER2.FORNAMN = UTFAXNUMMER.FORNAMN
      UTFAXNUMMER2.LOGGA = UTFAXNUMMER.LOGGA
      UTFAXNUMMER2.MEDD = UTFAXNUMMER.MEDD
      UTFAXNUMMER2.NAMN = UTFAXNUMMER.NAMN      
      UTFAXNUMMER2.PERSONALKOD = UTFAXNUMMER.PERSONALKOD.                 
   END.
   IF faxut = TRUE THEN DO TRANSACTION:
      ASSIGN              
      /*UTFAXNUMMER2.FAXNUMMER = utvarnr*/
      UTFAXNUMMER2.FAXNUMMER2 = "[[SPARA=C:\PROTEMP8\" + PERSONALTAB.PERSONALKOD + "]]".
   END.      
   OPEN QUERY omrpersq FOR EACH OMRPERS WHERE 
   OMRPERS.PERSONALKOD = PERSONALTAB.PERSONALKOD USE-INDEX PEROMR NO-LOCK.     
   GET FIRST omrpersq NO-LOCK.
   DO WHILE AVAILABLE(OMRPERS) TRANSACTION:   
      CREATE UTSLUT.
      FIND FIRST botyptemp WHERE botyptemp.BOTYP = OMRPERS.BOTYP NO-LOCK NO-ERROR.
      IF NOT AVAILABLE botyptemp THEN DO:         
         ASSIGN
         UTSLUT.STRECK = "-"
         UTSLUT.BOTYP = CAPS(OMRPERS.BOTYP).
         IF UTSLUT.BOTYP = "BOSTADSRÄTTER" THEN DO:
            UTSLUT.ORDNING = 2.
            IF PERSONALTAB.BPRIS = 0 THEN UTSLUT.BPRIS = "".
            ELSE UTSLUT.BPRIS = STRING(PERSONALTAB.BPRIS).
            IF PERSONALTAB.MINPRISBO = 0 THEN UTSLUT.MINPRISBO = "".
            ELSE UTSLUT.MINPRISBO = STRING(PERSONALTAB.MINPRISBO).
            IF PERSONALTAB.MAXBOBO = 0 THEN UTSLUT.MAXBOBO = "".
            ELSE UTSLUT.MAXBOBO = STRING(PERSONALTAB.MAXBOBO).
            IF PERSONALTAB.MINBOBO = 0 THEN UTSLUT.MINBOBO = "".
            ELSE UTSLUT.MINBOBO = STRING(PERSONALTAB.MINBOBO).
            IF PERSONALTAB.RUM2 = 0 THEN UTSLUT.RUM2 = "".
            ELSE UTSLUT.RUM2 = STRING(PERSONALTAB.RUM2).
            IF PERSONALTAB.RUM = 0 THEN UTSLUT.RUM = "".
            ELSE UTSLUT.RUM = STRING(PERSONALTAB.RUM).
         END.
         ELSE IF UTSLUT.BOTYP = "BRF-Radhus" THEN DO:
            UTSLUT.ORDNING = 3.
            IF PERSONALTAB.BPRIS = 0 THEN UTSLUT.BPRIS = "".
            ELSE UTSLUT.BPRIS = STRING(PERSONALTAB.BPRIS).
            IF PERSONALTAB.MINPRISBO = 0 THEN UTSLUT.MINPRISBO = "".
            ELSE UTSLUT.MINPRISBO = STRING(PERSONALTAB.MINPRISBO).
            IF PERSONALTAB.MAXBOBO = 0 THEN UTSLUT.MAXBOBO = "".
            ELSE UTSLUT.MAXBOBO = STRING(PERSONALTAB.MAXBOBO).
            IF PERSONALTAB.MINBOBO = 0 THEN UTSLUT.MINBOBO = "".
            ELSE UTSLUT.MINBOBO = STRING(PERSONALTAB.MINBOBO).
            IF PERSONALTAB.RUM2 = 0 THEN UTSLUT.RUM2 = "".
            ELSE UTSLUT.RUM2 = STRING(PERSONALTAB.RUM2).
            IF PERSONALTAB.RUM = 0 THEN UTSLUT.RUM = "".
            ELSE UTSLUT.RUM = STRING(PERSONALTAB.RUM).
         END.
         ELSE DO:
            UTSLUT.ORDNING = 1.
            IF PERSONALTAB.MAXBOVI = 0 THEN UTSLUT.MAXBOBO = "".
            ELSE UTSLUT.MAXBOBO = STRING(PERSONALTAB.MAXBOVI).
            IF PERSONALTAB.BOAREA = 0 THEN UTSLUT.MINBOBO = "".
            ELSE UTSLUT.MINBOBO = STRING(PERSONALTAB.BOAREA).
            IF PERSONALTAB.VPRIS = 0 THEN UTSLUT.BPRIS = "".
            ELSE UTSLUT.BPRIS = STRING(PERSONALTAB.VPRIS).
            IF PERSONALTAB.MINPRISVI = 0 THEN UTSLUT.MINPRISBO = "".
            ELSE UTSLUT.MINPRISBO = STRING(PERSONALTAB.MINPRISVI).                  
         END.         
         CREATE botyptemp.
         botyptemp.BOTYP = CAPS(OMRPERS.BOTYP).
      END.
      ASSIGN      
      UTSLUT.NAMN = ""
      UTSLUT.BOTYP2 = CAPS(OMRPERS.BOTYP)
      UTSLUT.OMRADE = OMRPERS.OMRADE
      UTSLUT.PERSONALKOD = OMRPERS.PERSONALKOD.
      UTSLUT.KOPPLING = OMRPERS.OMRADE + OMRPERS.BOTYP + PERSONALTAB.PERSONALKOD.   
      IF UTSLUT.BOTYP2 = "BOSTADSRÄTTER" THEN DO:
         UTSLUT.ORDNING = 2.         
      END.
      ELSE IF UTSLUT.BOTYP2 = "BRF-Radhus" THEN DO:
         UTSLUT.ORDNING = 3.         
      END.
      ELSE DO:
         UTSLUT.ORDNING = 1.                           
      END.
      GET NEXT omrpersq NO-LOCK.
   END.         
   FOR EACH UTSLUT:
      FIND FIRST OMRADETAB WHERE OMRADETAB.STADSDEL = UTSLUT.OMRADE AND
      OMRADETAB.OMRADE = UTSLUT.OMRADE 
      USE-INDEX STADOMR NO-LOCK NO-ERROR.                        
      IF AVAILABLE OMRADETAB THEN DO:
         ASSIGN
         UTSLUT.NAMN = CAPS(OMRADETAB.NAMN).
      END.   
   END.      
   FOR EACH botyptemp:
      CREATE UTSLUT.
      ASSIGN
      UTSLUT.PERSONALKOD = PERSONALTAB.PERSONALKOD
      UTSLUT.KOPPLING = "ÖÖÖ" + botyptemp.BOTYP + PERSONALTAB.PERSONALKOD         
      UTSLUT.OMRADE = "ÖÖÖ"
      UTSLUT.BOTYP2 = CAPS(botyptemp.BOTYP).
      IF UTSLUT.BOTYP2 = "BOSTADSRÄTTER" THEN DO:
         ASSIGN
         UTSLUT.ORDNING = 2                                           
         UTSLUT.NAMN = "----------------"                          
         UTSLUT.BPRIS = "------------"
         UTSLUT.MINPRISBO = "------------"
         UTSLUT.MAXBOBO = "-----------"
         UTSLUT.MINBOBO = "-----"
         UTSLUT.RUM2 = "-------"
         UTSLUT.RUM =  "-------".
      END.
      ELSE IF UTSLUT.BOTYP2 = "BRF-Radhus" THEN DO:
         ASSIGN
         UTSLUT.ORDNING = 3                                           
         UTSLUT.NAMN = "----------------"                          
         UTSLUT.BPRIS = "------------"
         UTSLUT.MINPRISBO = "------------"
         UTSLUT.MAXBOBO = "-----------"
         UTSLUT.MINBOBO = "-----"
         UTSLUT.RUM2 = "-------"
         UTSLUT.RUM =  "-------".
      END.
      ELSE DO:
         ASSIGN
         UTSLUT.ORDNING = 1                  
         UTSLUT.NAMN = "----------------"                          
         UTSLUT.BPRIS = "------------"
         UTSLUT.MINPRISBO = "------------"
         UTSLUT.MAXBOBO = "-----------"
         UTSLUT.MINBOBO = "------------".         
      END.
      DEF VAR i AS INTEGER.
      i = 1.
      DO WHILE i < 3 :
         i = i + 1.
         CREATE UTSLUT.         
         ASSIGN
         UTSLUT.PERSONALKOD = PERSONALTAB.PERSONALKOD
         UTSLUT.KOPPLING = "ÖÖÖÖ" + botyptemp.BOTYP + PERSONALTAB.PERSONALKOD        
         UTSLUT.OMRADE = "ÖÖÖÖ"
         UTSLUT.BOTYP2 = CAPS(botyptemp.BOTYP).
         UTSLUT.NAMN = CHR(10).
         IF UTSLUT.BOTYP2 = "BOSTADSRÄTTER" THEN DO:
            ASSIGN
            UTSLUT.ORDNING = 2.          
         END.
         ELSE IF UTSLUT.BOTYP2 = "BRF-Radhus" THEN DO:
            ASSIGN
            UTSLUT.ORDNING = 3.          
         END.
         ELSE DO:
            ASSIGN
            UTSLUT.ORDNING = 1.                           
         END.
      END.      
      i = 1.
      DO WHILE i < 3 :
         i = i + 1.
         CREATE UTSLUT.         
         ASSIGN
         UTSLUT.PERSONALKOD = PERSONALTAB.PERSONALKOD
         UTSLUT.KOPPLING = "ÖÖ" + botyptemp.BOTYP + PERSONALTAB.PERSONALKOD        
         UTSLUT.OMRADE = "ÖÖ"
         UTSLUT.BOTYP2 = CAPS(botyptemp.BOTYP).
         UTSLUT.NAMN = CHR(10).
         IF UTSLUT.BOTYP2 = "BOSTADSRÄTTER" THEN DO:
            ASSIGN
            UTSLUT.ORDNING = 2.          
         END.
         ELSE IF UTSLUT.BOTYP2 = "BRF-Radhus" THEN DO:
            ASSIGN
            UTSLUT.ORDNING = 3.          
         END.
         ELSE DO:
            ASSIGN
            UTSLUT.ORDNING = 1.                           
         END.
      END.                          
   END.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE summa2_UI WINDOW-2 
PROCEDURE summa2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/         
   str =  
   "____________________________________________________________________________________________".                                                      
   str2 =  
  
   "________________________________________________________________________________________________________________________________________________".                                                      
   IF datval = TRUE THEN DO:
      OPEN QUERY omp2q 
      FOR EACH OMRPERS WHERE OMRPERS.PERSONALKOD = PERSONALTAB.PERSONALKOD NO-LOCK, 
      EACH HUSOBJ WHERE HUSOBJ.STADSDEL = OMRPERS.OMRADE AND HUSOBJ.BOTYP = OMRPERS.BOTYP AND 
      HUSOBJ.ANDATUM >= startdatum AND HUSOBJ.ANDATUM <= slutdatum
      NO-LOCK.
   END.
   ELSE DO:
      OPEN QUERY omp2q 
      FOR EACH OMRPERS WHERE OMRPERS.PERSONALKOD = PERSONALTAB.PERSONALKOD NO-LOCK,
      EACH HUSOBJ WHERE HUSOBJ.STADSDEL = OMRPERS.OMRADE AND 
      HUSOBJ.BOTYP = OMRPERS.BOTYP AND HUSOBJ.ANDATUM > PERSONALTAB.FAXAT 
      NO-LOCK.
   END.
   GET FIRST omp2q NO-LOCK.
   DO WHILE AVAILABLE(HUSOBJ):      
/*      WINDOW-2:TITLE = "SPEKULANT - " + STRING(PERSONALTAB.KUNDNR,">>>>>>9") + " " + STRING(TIME,"HH:MM:SS") + " OMRÅDE " + OMRPERS.OMRADE.*/
      IF HUSOBJ.BOTYP = "BOSTADSRÄTTER" OR HUSOBJ.BOTYP = "BRF-Radhus" THEN DO:
         IF HUSOBJ.RUM = 0 THEN musz = musz.
         ELSE DO:       
            IF PERSONALTAB.RUM2 = 0 AND PERSONALTAB.RUM = 0 THEN musz = musz.
            ELSE DO:         
               IF PERSONALTAB.RUM = 0 THEN DO:
                  IF HUSOBJ.RUM <= PERSONALTAB.RUM2 THEN musz = FALSE.
                  ELSE musz = TRUE. 
               END.
               ELSE IF PERSONALTAB.RUM2 = 0 THEN DO:
                  IF HUSOBJ.RUM >= PERSONALTAB.RUM THEN musz = FALSE.
                  ELSE musz = TRUE.
               END.
               ELSE DO:
                  IF HUSOBJ.RUM >= PERSONALTAB.RUM AND 
                  HUSOBJ.RUM <= PERSONALTAB.RUM2 THEN musz = FALSE.
                  ELSE musz = TRUE.
               END.
            END.
         END.              
         IF musz = FALSE THEN DO:                
            IF HUSOBJ.PRIS = 0 THEN musz = musz.
            ELSE DO:   
               IF PERSONALTAB.BPRIS = 0 AND PERSONALTAB.MINPRISBO = 0 THEN musz = musz.   
               ELSE DO:
                  IF PERSONALTAB.MINPRISBO = 0 THEN DO:  
                     IF HUSOBJ.PRIS <= PERSONALTAB.BPRIS THEN musz = FALSE.
                     ELSE musz = TRUE.
                  END.
                  ELSE IF PERSONALTAB.BPRIS = 0 THEN DO:
                     IF HUSOBJ.PRIS >= PERSONALTAB.MINPRISBO THEN musz = FALSE.
                     ELSE musz = TRUE.
                  END.                 
                  ELSE DO:   
                     IF HUSOBJ.PRIS >= PERSONALTAB.MINPRISBO AND 
                     HUSOBJ.PRIS <= PERSONALTAB.BPRIS THEN musz = FALSE.
                     ELSE musz = TRUE.
                  END.                         
               END.
            END.
         END.     
         IF musz = FALSE THEN DO:                
            IF HUSOBJ.BOAREA = 0 THEN musz = musz.
            ELSE DO:   
               IF PERSONALTAB.MAXBOBO = 0 AND PERSONALTAB.MINBOBO = 0 THEN musz = musz.   
               ELSE DO:
                  IF PERSONALTAB.MINBOBO = 0 THEN DO:  
                     IF HUSOBJ.BOAREA <= PERSONALTAB.MAXBOBO THEN musz = FALSE.
                     ELSE musz = TRUE.
                  END.
                  ELSE IF PERSONALTAB.MAXBOBO = 0 THEN DO:
                     IF HUSOBJ.BOAREA >= PERSONALTAB.MINBOBO THEN musz = FALSE.
                     ELSE musz = TRUE.
                  END.                 
                  ELSE DO:   
                     IF HUSOBJ.BOAREA >= PERSONALTAB.MINBOBO AND 
                     HUSOBJ.BOAREA <= PERSONALTAB.MAXBOBO THEN musz = FALSE.
                     ELSE musz = TRUE.
                  END.                         
               END.
            END.
         END.
      END.   
      ELSE DO:          
         IF HUSOBJ.BOAREA = 0 THEN musz = musz.
         ELSE DO:   
            IF PERSONALTAB.BOAREA = 0 AND PERSONALTAB.MAXBOVI = 0 THEN musz = musz.   
            ELSE DO:
               IF PERSONALTAB.BOAREA = 0 THEN DO:  
                  IF HUSOBJ.BOAREA <= PERSONALTAB.MAXBOVI THEN musz = FALSE.
                  ELSE musz = TRUE.
               END.
               ELSE IF PERSONALTAB.MAXBOVI = 0 THEN DO:
                  IF HUSOBJ.BOAREA >= PERSONALTAB.BOAREA THEN musz = FALSE.
                  ELSE musz = TRUE.
               END.                 
               ELSE DO:   
                  IF HUSOBJ.BOAREA >= PERSONALTAB.BOAREA AND 
                  HUSOBJ.BOAREA <= PERSONALTAB.MAXBOVI THEN musz = FALSE.
                  ELSE musz = TRUE.
               END.                                         
            END.
         END.              
         IF musz = FALSE THEN DO:                
            IF HUSOBJ.PRIS = 0 THEN musz = musz.
            ELSE DO:   
               IF PERSONALTAB.VPRIS = 0 AND PERSONALTAB.MINPRISVI = 0 THEN musz = musz.   
               ELSE DO:
                  IF PERSONALTAB.MINPRISVI = 0 THEN DO:  
                     IF HUSOBJ.PRIS <= PERSONALTAB.VPRIS THEN musz = FALSE.
                     ELSE musz = TRUE.
                  END.
                  ELSE IF PERSONALTAB.VPRIS = 0 THEN DO:
                     IF HUSOBJ.PRIS >= PERSONALTAB.MINPRISVI THEN musz = FALSE.
                     ELSE musz = TRUE.
                  END.                 
                  ELSE DO:   
                     IF HUSOBJ.PRIS >= PERSONALTAB.MINPRISVI AND 
                     HUSOBJ.PRIS <= PERSONALTAB.VPRIS THEN musz = FALSE.
                     ELSE musz = TRUE.
                  END.                         
               END.
            END.
         END.  
      END.                          
      IF musz = FALSE THEN DO:                  
         fnamn = TRIM(HUSOBJ.NAMN).   
         fnamn = CAPS(SUBSTRING(fnamn,1,1)) + LC(SUBSTRING(fnamn,2)).     
         IF INDEX(fnamn,"-") = 0 THEN DO: 
            fnamn = fnamn.
         END.             
         ELSE DO:
            ednum3 = INDEX(fnamn,"-").
            fnamn = CAPS(SUBSTRING(fnamn,1,1)) + LC(SUBSTRING(fnamn,2)).
            fnamn = SUBSTRING(fnamn,1,ednum3) + CAPS(SUBSTRING(fnamn,ednum3 + 1,1)) + LC(SUBSTRING(fnamn,ednum3 + 2)).
         END.
         IF INDEX(fnamn," ") = 0 THEN DO: 
            fnamn = fnamn.
         END.             
         ELSE DO:
            ednum3 = INDEX(fnamn," ").
            fnamn = CAPS(SUBSTRING(fnamn,1,1)) + LC(SUBSTRING(fnamn,2)).
            fnamn = SUBSTRING(fnamn,1,ednum3) + CAPS(SUBSTRING(fnamn,ednum3 + 1,1)) + LC(SUBSTRING(fnamn,ednum3 + 2)).
         END. 
         enamn = TRIM(HUSOBJ.GATADR).   
         enamn = CAPS(SUBSTRING(enamn,1,1)) + LC(SUBSTRING(enamn,2)).     
         IF INDEX(enamn,"-") = 0 THEN DO: 
            enamn = enamn.
         END.             
         ELSE DO:
            ednum3 = INDEX(enamn,"-").
            enamn = CAPS(SUBSTRING(enamn,1,1)) + LC(SUBSTRING(enamn,2)).
            enamn = SUBSTRING(enamn,1,ednum3) + CAPS(SUBSTRING(enamn,ednum3 + 1,1)) + LC(SUBSTRING(enamn,ednum3 + 2)).
         END.
         IF INDEX(enamn," ") = 0 THEN DO: 
            enamn = enamn.
         END.             
         ELSE DO:
            ednum3 = INDEX(enamn," ").
            enamn = CAPS(SUBSTRING(enamn,1,1)) + LC(SUBSTRING(enamn,2)).
            enamn = SUBSTRING(enamn,1,ednum3) + CAPS(SUBSTRING(enamn,ednum3 + 1,1)) + LC(SUBSTRING(enamn,ednum3 + 2)).
         END.           
         IF HUSOBJ.BOTYP = "BOSTADSRÄTTER" OR HUSOBJ.BOTYP = "BRF-Radhus" THEN DO TRANSACTION:  
            CREATE UTOBJEKT.
            ASSIGN     
            UTOBJEKT.ORDNING = 2
            UTOBJEKT.PERSONALKOD = PERSONALTAB.PERSONALKOD   
            UTOBJEKT.MAKLAREKOD = HUSOBJ.PERSONALKOD         
            UTOBJEKT.TELEFON = HUSOBJ.TELEFON
            UTOBJEKT.STADSDEL = HUSOBJ.STADSDEL 
            UTOBJEKT.KOPPLING = HUSOBJ.STADSDEL + HUSOBJ.BOTYP + PERSONALTAB.PERSONALKOD                    
            UTOBJEKT.NAMN = fnamn
            UTOBJEKT.GATADR = enamn            
            UTOBJEKT.BOTYP = HUSOBJ.BOTYP
            UTOBJEKT.PRIS = HUSOBJ.PRIS
            UTOBJEKT.OMRADE = HUSOBJ.OMRADE   
            UTOBJEKT.HUSOBJEKT = HUSOBJ.HUSOBJEKT   
            UTOBJEKT.BOAREA = HUSOBJ.BOAREA
            UTOBJEKT.RUM = HUSOBJ.RUM 
            UTOBJEKT.BIYTA = HUSOBJ.MANAVG   /*OBS !!*/
            UTOBJEKT.FIRMA = HUSOBJ.FIRMA   
            /* UTOBJEKT.TOMTYTA = HUSOBJ.TOMTYTA*/
            UTOBJEKT.TOTYTA = HUSOBJ.TOTYTA
            UTOBJEKT.BYGGAR = HUSOBJ.BYGGAR 
            UTOBJEKT.MANAVG = HUSOBJ.MANAVG.
            IF HUSOBJ.BOTYP = "BRF-Radhus" THEN DO:
               ASSIGN                  
               UTOBJEKT.ORDNING = 3
               UTOBJEKT.BIYTA = HUSOBJ.BIYTA        /*OBS!*/
               UTOBJEKT.TOTYTA = HUSOBJ.MANAVG.
            END.
            IF jmlist = TRUE AND HUSOBJ.MANAVG > 0 THEN DO:
               IF UTOBJEKT.PRIS > 0 AND UTOBJEKT.BOAREA > 0 THEN DO:
                  UTOBJEKT.BYGGAR = 
                  (UTOBJEKT.PRIS * 0.049 + HUSOBJ.MANAVG * 12) / UTOBJEKT.BOAREA.              
                  FIND FIRST medeltemp WHERE 
                  medeltemp.OMRADE = HUSOBJ.STADSDEL 
                  USE-INDEX OMR NO-ERROR.
                  IF NOT AVAILABLE medeltemp THEN CREATE medeltemp.
                  ASSIGN
                  medeltemp.OMRADE = HUSOBJ.STADSDEL
                  medeltemp.BELOPP = medeltemp.BELOPP + UTOBJEKT.BYGGAR
                  medeltemp.ANTAL = medeltemp.ANTAL + 1.
               END.
            END.
            IF UTOBJEKT.PRIS > 0 AND UTOBJEKT.BOAREA > 0 THEN DO:              
               UTOBJEKT.PRISKVM = 1000 * (UTOBJEKT.PRIS / UTOBJEKT.BOAREA).    
            END.                                                       
         END.
         ELSE DO TRANSACTION:
            CREATE UTOBJEKT.
            ASSIGN    
            UTOBJEKT.ORDNING = 1
            UTOBJEKT.PERSONALKOD = PERSONALTAB.PERSONALKOD   
            UTOBJEKT.MAKLAREKOD = HUSOBJ.PERSONALKOD         
            UTOBJEKT.TELEFON = HUSOBJ.TELEFON
            UTOBJEKT.STADSDEL = HUSOBJ.STADSDEL 
            UTOBJEKT.KOPPLING = HUSOBJ.STADSDEL + HUSOBJ.BOTYP + PERSONALTAB.PERSONALKOD                     
            UTOBJEKT.NAMN = fnamn
            UTOBJEKT.GATADR = enamn            
            UTOBJEKT.BOTYP = HUSOBJ.BOTYP
            UTOBJEKT.PRIS = HUSOBJ.PRIS
            UTOBJEKT.OMRADE = HUSOBJ.OMRADE   
            UTOBJEKT.HUSOBJEKT = HUSOBJ.HUSOBJEKT   
            UTOBJEKT.BOAREA = HUSOBJ.BOAREA
            UTOBJEKT.RUM = HUSOBJ.RUM 
            UTOBJEKT.BIYTA = HUSOBJ.BIYTA
            UTOBJEKT.FIRMA = HUSOBJ.FIRMA   
            UTOBJEKT.TOMTYTA = HUSOBJ.TOMTYTA
            UTOBJEKT.TOTYTA = HUSOBJ.TOTYTA
            UTOBJEKT.BYGGAR = HUSOBJ.BYGGAR 
            UTOBJEKT.MANAVG = HUSOBJ.MANAVG.               
            IF UTOBJEKT.PRIS > 0 AND UTOBJEKT.BOAREA > 0 THEN DO:              
               UTOBJEKT.PRISKVM = 1000 * (UTOBJEKT.PRIS / UTOBJEKT.BOAREA).    
            END.
         END.                                       
         DO TRANSACTION:
            IF UTOBJEKT.PRIS = 0 THEN UTOBJEKT.PRIS = ?.
            IF UTOBJEKT.RUM = 0 THEN UTOBJEKT.RUM = ?. 
            IF UTOBJEKT.BOAREA = 0 THEN UTOBJEKT.BOAREA = ?. 
            IF UTOBJEKT.BIYTA = 0 THEN UTOBJEKT.BIYTA = ?.
            IF UTOBJEKT.TOTYTA = 0 THEN UTOBJEKT.TOTYTA = ?.
            IF UTOBJEKT.TOMTYTA = 0 THEN UTOBJEKT.TOMTYTA = ?.
            IF UTOBJEKT.BYGGAR = 0 THEN UTOBJEKT.BYGGAR = ?.    
         END.
      END.
      musz = FALSE.                           
      GET NEXT omp2q NO-LOCK.
   END.            
   CLOSE QUERY omp2q.
   IF jmlist = TRUE THEN DO TRANSACTION:
      FOR EACH medeltemp: 
         FIND FIRST TENDENS WHERE TENDENS.OMRADE = medeltemp.OMRADE 
         USE-INDEX OMR EXCLUSIVE-LOCK NO-ERROR.
         IF NOT AVAILABLE TENDENS THEN DO:
            CREATE TENDENS.
            ASSIGN    
            TENDENS.DATUM = TODAY 
            TENDENS.MEDEL = medeltemp.BELOPP / medeltemp.ANTAL
            TENDENS.OMRADE = medeltemp.OMRADE.
         END.
         CREATE UTOBJEKT.
         ASSIGN     
         UTOBJEKT.ORDNING = 2
         UTOBJEKT.PERSONALKOD = PERSONALTAB.PERSONALKOD   
         UTOBJEKT.STADSDEL = medeltemp.OMRADE 
         UTOBJEKT.KOPPLING = medeltemp.OMRADE + "BOSTADSRÄTTER" + PERSONALTAB.PERSONALKOD                      
         UTOBJEKT.NAMN = "" 
         UTOBJEKT.GATADR = ""             
         UTOBJEKT.BOTYP = "BOSTADSRÄTTER"         
         UTOBJEKT.OMRADE = "ÖÖ".
         IF UTOBJEKT.PRIS = 0 THEN UTOBJEKT.PRIS = ?.
         IF UTOBJEKT.RUM = 0 THEN UTOBJEKT.RUM = ?. 
         IF UTOBJEKT.BOAREA = 0 THEN UTOBJEKT.BOAREA = ?. 
         IF UTOBJEKT.BIYTA = 0 THEN UTOBJEKT.BIYTA = ?.
         IF UTOBJEKT.TOTYTA = 0 THEN UTOBJEKT.TOTYTA = ?.
         IF UTOBJEKT.TOMTYTA = 0 THEN UTOBJEKT.TOMTYTA = ?.
         IF UTOBJEKT.BYGGAR = 0 THEN UTOBJEKT.BYGGAR = ?.    
         
         CREATE UTOBJEKT.
         ASSIGN     
         UTOBJEKT.ORDNING = 2
         UTOBJEKT.PERSONALKOD = PERSONALTAB.PERSONALKOD   
         UTOBJEKT.STADSDEL = medeltemp.OMRADE 
         UTOBJEKT.KOPPLING = medeltemp.OMRADE + "BOSTADSRÄTTER" + PERSONALTAB.PERSONALKOD                     
         UTOBJEKT.NAMN = "Medelvärde" 
         UTOBJEKT.GATADR = ""             
         UTOBJEKT.BOTYP = "BOSTADSRÄTTER"         
         UTOBJEKT.OMRADE = "ÖÖÖ".
         IF UTOBJEKT.PRIS = 0 THEN UTOBJEKT.PRIS = ?.
         IF UTOBJEKT.RUM = 0 THEN UTOBJEKT.RUM = ?. 
         IF UTOBJEKT.BOAREA = 0 THEN UTOBJEKT.BOAREA = ?. 
         IF UTOBJEKT.BIYTA = 0 THEN UTOBJEKT.BIYTA = ?.
         IF UTOBJEKT.TOTYTA = 0 THEN UTOBJEKT.TOTYTA = ?.
         IF UTOBJEKT.TOMTYTA = 0 THEN UTOBJEKT.TOMTYTA = ?.
         IF UTOBJEKT.BYGGAR = 0 THEN UTOBJEKT.BYGGAR = ?.    
         IF medeltemp.ANTAL > 0 THEN DO:
            UTOBJEKT.GATADR =
            STRING(medeltemp.BELOPP / medeltemp.ANTAL,">>>9") + " kr".                        
         END.
         CREATE UTOBJEKT.
         ASSIGN     
         UTOBJEKT.ORDNING = 2
         UTOBJEKT.PERSONALKOD = PERSONALTAB.PERSONALKOD   
         UTOBJEKT.STADSDEL = medeltemp.OMRADE 
         UTOBJEKT.KOPPLING = medeltemp.OMRADE + "BOSTADSRÄTTER" + PERSONALTAB.PERSONALKOD                     
         UTOBJEKT.NAMN = "Tendens" 
         UTOBJEKT.GATADR = ""             
         UTOBJEKT.BOTYP = "BOSTADSRÄTTER"         
         UTOBJEKT.OMRADE = "ÖÖÖ".
         IF UTOBJEKT.PRIS = 0 THEN UTOBJEKT.PRIS = ?.
         IF UTOBJEKT.RUM = 0 THEN UTOBJEKT.RUM = ?. 
         IF UTOBJEKT.BOAREA = 0 THEN UTOBJEKT.BOAREA = ?. 
         IF UTOBJEKT.BIYTA = 0 THEN UTOBJEKT.BIYTA = ?.
         IF UTOBJEKT.TOTYTA = 0 THEN UTOBJEKT.TOTYTA = ?.
         IF UTOBJEKT.TOMTYTA = 0 THEN UTOBJEKT.TOMTYTA = ?.
         IF UTOBJEKT.BYGGAR = 0 THEN UTOBJEKT.BYGGAR = ?.    
         IF TENDENS.MEDEL > 0 THEN DO:
            varmed = 
            ((medeltemp.BELOPP / medeltemp.ANTAL) - TENDENS.MEDEL) / TENDENS.MEDEL * 100.
            IF varmed >= 0 THEN DO:
               UTOBJEKT.GATADR = " + " + STRING(varmed) + "%".
            END.
            ELSE DO:
               varmed = ABS(varmed).
               UTOBJEKT.GATADR = " - " + STRING(varmed) + "%".                   
            END.            
         END.               
         IF upjm = TRUE THEN DO:
            ASSIGN    
            TENDENS.DATUM = TODAY 
            TENDENS.MEDEL = medeltemp.BELOPP / medeltemp.ANTAL
            TENDENS.OMRADE = medeltemp.OMRADE.
         END.
         DELETE medeltemp. 
      END.
   END.
   OPEN QUERY utobjq FOR EACH UTOBJEKT WHERE UTOBJEKT.PERSONALKOD = PERSONALTAB.PERSONALKOD 
   USE-INDEX OBJ NO-LOCK BY UTOBJEKT.BOTYP BY UTOBJEKT.STADSDEL.
   GET FIRST utobjq NO-LOCK.
   DO WHILE AVAILABLE(UTOBJEKT): 
      IF UTOBJEKT.BOTYP = "BOSTADSRÄTTER" THEN musz = musz.
      ELSE IF UTOBJEKT.BOTYP = "BRF-Radhus" THEN musz = musz.
      ELSE DO:   
         IF botvar = UTOBJEKT.BOTYP AND stde = UTOBJEKT.STADSDEL THEN botvar = botvar.
         ELSE DO:
            FIND FIRST OMRADETAB WHERE OMRADETAB.STADSDEL = UTOBJEKT.STADSDEL AND
            OMRADETAB.OMRADE = UTOBJEKT.STADSDEL 
            USE-INDEX STADOMR NO-LOCK NO-ERROR.                  
            IF AVAILABLE OMRADETAB THEN DO TRANSACTION:                     
               FIND FIRST UTRUBRIK WHERE UTRUBRIK.KOPPLING = UTOBJEKT.KOPPLING NO-LOCK NO-ERROR.             
               IF NOT AVAILABLE UTRUBRIK THEN DO:
                  CREATE UTRUBRIK. 
                  ASSIGN         
                  UTRUBRIK.KOPPLING = UTOBJEKT.KOPPLING              
                  UTRUBRIK.BOTYP = CAPS(UTOBJEKT.BOTYP) + " " + CAPS(OMRADETAB.NAMN)
                  UTRUBRIK.NAMN = CAPS(OMRADETAB.NAMN).                                               
                  ASSIGN
                  stde = ""
                  botvar = "".     
                  CREATE UTRUBRIK1.
                  ASSIGN        
                  UTRUBRIK1.KOPPLING = UTOBJEKT.KOPPLING                               
                  UTRUBRIK1.BI-MAN = "Bi"
                  UTRUBRIK1.BOAREA = "Bo"
                  UTRUBRIK1.BOTYP = "Område" 
                  UTRUBRIK1.BRUKS = "Bruks"
                  UTRUBRIK1.BYGG = "Bygg" 
                  UTRUBRIK1.MAK = "Mäk"
                  UTRUBRIK1.NAMN = "Adress"
                  UTRUBRIK1.PRIS = "Pris"
                  UTRUBRIK1.RUM = "Rum" 
                  UTRUBRIK1.TELEFON = "Telefon"
                  UTRUBRIK1.TOMT = "Tomt".               
                  CREATE UTRUBRIK2.
                  ASSIGN                   
                  UTRUBRIK2.KOPPLING = UTOBJEKT.KOPPLING 
                  UTRUBRIK2.AREA1 = "area" 
                  UTRUBRIK2.AREA2 = "area"  
                  UTRUBRIK2.AREA3 = "area" 
                  UTRUBRIK2.AREA4 = "area"    /*ANVÄNDS EJ*/  
                  UTRUBRIK2.ARTAL = "år"   
                  UTRUBRIK2.MAKLARE = "lare"
                  UTRUBRIK2.TKR = "tkr" 
                  UTRUBRIK2.AVGIFT = "area" .                                                     
               END.
            END.
            ELSE MESSAGE UTOBJEKT.STADSDEL "MÅSTE VARA FEL UPPLAGGT" VIEW-AS ALERT-BOX.
            ASSIGN
            botvar = UTOBJEKT.BOTYP 
            stde = UTOBJEKT.STADSDEL.         
         END.
      END.
      GET NEXT utobjq NO-LOCK.  
   END.   
   CLOSE QUERY utobjq.
   OPEN QUERY utobjbrq FOR EACH UTOBJEKT WHERE UTOBJEKT.PERSONALKOD = PERSONALTAB.PERSONALKOD AND UTOBJEKT.BOTYP = "BOSTADSRÄTTER" 
   USE-INDEX BOTYP NO-LOCK BY UTOBJEKT.BOTYP BY UTOBJEKT.STADSDEL.
   GET FIRST utobjbrq NO-LOCK.
   DO WHILE AVAILABLE(UTOBJEKT):        
      IF botvar = UTOBJEKT.BOTYP AND stde = UTOBJEKT.STADSDEL THEN botvar = botvar.
      ELSE DO:         
         FIND FIRST OMRADETAB WHERE OMRADETAB.STADSDEL = UTOBJEKT.STADSDEL AND
         OMRADETAB.OMRADE = UTOBJEKT.STADSDEL
         USE-INDEX STADOMR NO-LOCK NO-ERROR.         
         IF AVAILABLE OMRADETAB THEN DO TRANSACTION:            
            FIND FIRST UTRUBRIK WHERE UTRUBRIK.KOPPLING = UTOBJEKT.KOPPLING NO-LOCK NO-ERROR.             
            IF NOT AVAILABLE UTRUBRIK THEN DO:
               CREATE UTRUBRIK.                                
               ASSIGN         
               UTRUBRIK.KOPPLING = UTOBJEKT.KOPPLING              
               UTRUBRIK.BOTYP = CAPS(UTOBJEKT.BOTYP) + " " + CAPS(OMRADETAB.NAMN)
               UTRUBRIK.NAMN = CAPS(OMRADETAB.NAMN).                                                                               
               ASSIGN
               stde = ""
               botvar = "".
               CREATE UTRUBRIK1.
               ASSIGN                       
               UTRUBRIK1.KOPPLING = UTOBJEKT.KOPPLING                              
               UTRUBRIK1.BI-MAN = "Månads"  
               UTRUBRIK1.BOAREA = "Bo"
               UTRUBRIK1.BOTYP = "Område" 
               UTRUBRIK1.BRUKS = ""
               UTRUBRIK1.BYGG = "Bygg" 
               UTRUBRIK1.MAK = "Mäk"
               UTRUBRIK1.NAMN = "Adress"
               UTRUBRIK1.PRIS = "Pris"
               UTRUBRIK1.RUM = "Rum" 
               UTRUBRIK1.TELEFON = "Telefon"
               UTRUBRIK1.TOMT = "".  
               IF jmlist = TRUE THEN  UTRUBRIK1.BYGG = "Värd".                                                      
               CREATE UTRUBRIK2.
               ASSIGN                   
               UTRUBRIK2.KOPPLING = UTOBJEKT.KOPPLING
               UTRUBRIK2.AREA1 = "" 
               UTRUBRIK2.AREA2 = ""  
               UTRUBRIK2.AREA3 = "area" 
               UTRUBRIK2.AREA4 = ""   
               UTRUBRIK2.AVGIFT = "avgift" 
               UTRUBRIK2.ARTAL = "år"   
               UTRUBRIK2.MAKLARE = "lare"
               UTRUBRIK2.TKR = "tkr".
               IF jmlist = TRUE THEN UTRUBRIK2.ARTAL = "".                                                                 
            END.
         END.
         ELSE MESSAGE UTOBJEKT.STADSDEL "MÅSTE VARA FEL UPPLAGGT" VIEW-AS ALERT-BOX.
         ASSIGN
         botvar = UTOBJEKT.BOTYP 
         stde = UTOBJEKT.STADSDEL.
      END.
      GET NEXT utobjbrq NO-LOCK.            
   END.
   CLOSE QUERY utobjbrq.
   OPEN QUERY utobjbrq FOR EACH UTOBJEKT WHERE UTOBJEKT.PERSONALKOD = PERSONALTAB.PERSONALKOD AND UTOBJEKT.BOTYP = "BRF-Radhus" 
   USE-INDEX BOTYP NO-LOCK BY UTOBJEKT.BOTYP BY UTOBJEKT.STADSDEL.
   GET FIRST utobjbrq NO-LOCK.
   DO WHILE AVAILABLE(UTOBJEKT):        
      IF botvar = UTOBJEKT.BOTYP AND stde = UTOBJEKT.STADSDEL THEN botvar = botvar.
      ELSE DO:         
         FIND FIRST OMRADETAB WHERE OMRADETAB.STADSDEL = UTOBJEKT.STADSDEL AND
         OMRADETAB.OMRADE = UTOBJEKT.STADSDEL
         USE-INDEX STADOMR NO-LOCK NO-ERROR.         
         IF AVAILABLE OMRADETAB THEN DO TRANSACTION:
            FIND FIRST UTRUBRIK WHERE UTRUBRIK.KOPPLING = UTOBJEKT.KOPPLING NO-LOCK NO-ERROR.             
            IF NOT AVAILABLE UTRUBRIK THEN DO:
               CREATE UTRUBRIK.                                
               ASSIGN         
               UTRUBRIK.KOPPLING = UTOBJEKT.KOPPLING              
               UTRUBRIK.BOTYP = CAPS(UTOBJEKT.BOTYP) + " " + CAPS(OMRADETAB.NAMN)
               UTRUBRIK.NAMN = CAPS(OMRADETAB.NAMN).                                                                               
               ASSIGN
               stde = ""
               botvar = "".
               CREATE UTRUBRIK1.
               ASSIGN                       
               UTRUBRIK1.KOPPLING = UTOBJEKT.KOPPLING                              
               UTRUBRIK1.BI-MAN = "Bi"  
               UTRUBRIK1.BOAREA = "Bo"
               UTRUBRIK1.BOTYP = "Område" 
               UTRUBRIK1.BRUKS = "Månads"
               UTRUBRIK1.BYGG = "Bygg" 
               UTRUBRIK1.MAK = "Mäk"
               UTRUBRIK1.NAMN = "Adress"
               UTRUBRIK1.PRIS = "Pris"
               UTRUBRIK1.RUM = "Rum" 
               UTRUBRIK1.TELEFON = "Telefon"
               UTRUBRIK1.TOMT = "".  
               IF jmlist = TRUE THEN  UTRUBRIK1.BYGG = "Värd".                                                      
               CREATE UTRUBRIK2.
               ASSIGN                   
               UTRUBRIK2.KOPPLING = UTOBJEKT.KOPPLING
               UTRUBRIK2.AREA1 = "avgift" 
               UTRUBRIK2.AREA2 = ""  
               UTRUBRIK2.AREA3 = "area" 
               UTRUBRIK2.AREA4 = ""   
               UTRUBRIK2.AVGIFT = "area" 
               UTRUBRIK2.ARTAL = "år"   
               UTRUBRIK2.MAKLARE = "lare"
               UTRUBRIK2.TKR = "tkr".
               IF jmlist = TRUE THEN UTRUBRIK2.ARTAL = "".                                                                 
            END.
         END.
         ELSE MESSAGE UTOBJEKT.STADSDEL "MÅSTE VARA FEL UPPLAGGT" VIEW-AS ALERT-BOX.
         ASSIGN
         botvar = UTOBJEKT.BOTYP 
         stde = UTOBJEKT.STADSDEL.
      END.
      GET NEXT utobjbrq NO-LOCK.            
   END.
   CLOSE QUERY utobjbrq.
   IF faxuppd = TRUE THEN DO:
      DO TRANSACTION:
         FIND FIRST persbuff WHERE persbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
         USE-INDEX PERSONALKOD EXCLUSIVE-LOCK NO-ERROR.  
         ASSIGN                  
         persbuff.ANTALFAX = persbuff.ANTALFAX + 1
         persbuff.FAXAT = TODAY.
         IF persrec = RECID(persbuff) THEN DO:  
           /*
            IF persbuff.TELEFON2 = "" THEN DO:      
               FIND FIRST MEDDELANDE WHERE MEDDELANDE.MOTTAGARE = "KUNDBREV"
               USE-INDEX MEDD EXCLUSIVE-LOCK NO-ERROR.
               IF AVAILABLE MEDDELANDE THEN DELETE MEDDELANDE.       
            END.    
            ELSE DO:      
               FIND FIRST MEDDELANDE WHERE MEDDELANDE.MOTTAGARE = "KUNDFAX"
               USE-INDEX MEDD EXCLUSIVE-LOCK NO-ERROR.
               IF AVAILABLE MEDDELANDE THEN DELETE MEDDELANDE.    
            END.
            */   
         END. 
      END.
   END.
   IF utslutvar = TRUE THEN RUN sokk_UI.              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE summa_UI WINDOW-2 
PROCEDURE summa_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   str =  
   "____________________________________________________________________________________________".                                                      
   str2 =  
  
   "________________________________________________________________________________________________________________________________________________".                                                      
  
   OPEN QUERY ompq 
   FOR EACH OMRPERS WHERE OMRPERS.PERSONALKOD = PERSONALTAB.PERSONALKOD 
   USE-INDEX PEROMR NO-LOCK.
   GET FIRST ompq NO-LOCK.
   DO WHILE AVAILABLE(OMRPERS):      
      IF datval = TRUE THEN DO:      
         OPEN QUERY objq FOR EACH HUSOBJ WHERE HUSOBJ.STADSDEL = OMRPERS.OMRADE AND 
         HUSOBJ.BOTYP = OMRPERS.BOTYP AND 
         HUSOBJ.ANDATUM >= startdatum AND HUSOBJ.ANDATUM <= slutdatum
         USE-INDEX FAX NO-LOCK.
      END.
      ELSE DO:
         OPEN QUERY objq FOR EACH HUSOBJ WHERE HUSOBJ.STADSDEL = OMRPERS.OMRADE AND 
         HUSOBJ.BOTYP = OMRPERS.BOTYP AND HUSOBJ.ANDATUM > PERSONALTAB.FAXAT 
         USE-INDEX FAX NO-LOCK.
      END.
      GET FIRST objq  NO-LOCK.
      DO WHILE AVAILABLE(HUSOBJ):         
         IF HUSOBJ.BOTYP = "BOSTADSRÄTTER" OR HUSOBJ.BOTYP = "BRF-Radhus" THEN DO:
            IF HUSOBJ.RUM = 0 THEN musz = musz.
            ELSE DO:       
               IF PERSONALTAB.RUM2 = 0 AND PERSONALTAB.RUM = 0 THEN musz = musz.
               ELSE DO:         
                  IF PERSONALTAB.RUM = 0 THEN DO:
                     IF HUSOBJ.RUM <= PERSONALTAB.RUM2 THEN musz = FALSE.
                     ELSE musz = TRUE. 
                  END.
                  ELSE IF PERSONALTAB.RUM2 = 0 THEN DO:
                     IF HUSOBJ.RUM >= PERSONALTAB.RUM THEN musz = FALSE.
                     ELSE musz = TRUE.
                  END.
                  ELSE DO:
                     IF HUSOBJ.RUM >= PERSONALTAB.RUM AND 
                     HUSOBJ.RUM <= PERSONALTAB.RUM2 THEN musz = FALSE.
                     ELSE musz = TRUE.
                  END.
               END.
            END.              
            IF musz = FALSE THEN DO:                
               IF HUSOBJ.PRIS = 0 THEN musz = musz.
               ELSE DO:   
                  IF PERSONALTAB.BPRIS = 0 AND PERSONALTAB.MINPRISBO = 0 THEN musz = musz.   
                  ELSE DO:
                     IF PERSONALTAB.MINPRISBO = 0 THEN DO:  
                        IF HUSOBJ.PRIS <= PERSONALTAB.BPRIS THEN musz = FALSE.
                        ELSE musz = TRUE.
                     END.
                     ELSE IF PERSONALTAB.BPRIS = 0 THEN DO:
                        IF HUSOBJ.PRIS >= PERSONALTAB.MINPRISBO THEN musz = FALSE.
                        ELSE musz = TRUE.
                     END.                 
                     ELSE DO:   
                        IF HUSOBJ.PRIS >= PERSONALTAB.MINPRISBO AND 
                        HUSOBJ.PRIS <= PERSONALTAB.BPRIS THEN musz = FALSE.
                        ELSE musz = TRUE.
                     END.                         
                  END.
               END.
            END.     
            IF musz = FALSE THEN DO:                
               IF HUSOBJ.BOAREA = 0 THEN musz = musz.
               ELSE DO:   
                  IF PERSONALTAB.MAXBOBO = 0 AND PERSONALTAB.MINBOBO = 0 THEN musz = musz.   
                  ELSE DO:
                     IF PERSONALTAB.MINBOBO = 0 THEN DO:  
                        IF HUSOBJ.BOAREA <= PERSONALTAB.MAXBOBO THEN musz = FALSE.
                        ELSE musz = TRUE.
                     END.
                     ELSE IF PERSONALTAB.MAXBOBO = 0 THEN DO:
                        IF HUSOBJ.BOAREA >= PERSONALTAB.MINBOBO THEN musz = FALSE.
                        ELSE musz = TRUE.
                     END.                 
                     ELSE DO:   
                        IF HUSOBJ.BOAREA >= PERSONALTAB.MINBOBO AND 
                        HUSOBJ.BOAREA <= PERSONALTAB.MAXBOBO THEN musz = FALSE.
                        ELSE musz = TRUE.
                     END.                         
                  END.
               END.
            END.
         END.   
         ELSE DO:          
            IF HUSOBJ.BOAREA = 0 THEN musz = musz.
            ELSE DO:   
               IF PERSONALTAB.BOAREA = 0 AND PERSONALTAB.MAXBOVI = 0 THEN musz = musz.   
               ELSE DO:
                  IF PERSONALTAB.BOAREA = 0 THEN DO:  
                     IF HUSOBJ.BOAREA <= PERSONALTAB.MAXBOVI THEN musz = FALSE.
                     ELSE musz = TRUE.
                  END.
                  ELSE IF PERSONALTAB.MAXBOVI = 0 THEN DO:
                     IF HUSOBJ.BOAREA >= PERSONALTAB.BOAREA THEN musz = FALSE.
                     ELSE musz = TRUE.
                  END.                 
                  ELSE DO:   
                     IF HUSOBJ.BOAREA >= PERSONALTAB.BOAREA AND 
                     HUSOBJ.BOAREA <= PERSONALTAB.MAXBOVI THEN musz = FALSE.
                     ELSE musz = TRUE.
                  END.                                         
               END.
            END.              
            IF musz = FALSE THEN DO:                
               IF HUSOBJ.PRIS = 0 THEN musz = musz.
               ELSE DO:   
                  IF PERSONALTAB.VPRIS = 0 AND PERSONALTAB.MINPRISVI = 0 THEN musz = musz.   
                  ELSE DO:
                     IF PERSONALTAB.MINPRISVI = 0 THEN DO:  
                        IF HUSOBJ.PRIS <= PERSONALTAB.VPRIS THEN musz = FALSE.
                        ELSE musz = TRUE.
                     END.
                     ELSE IF PERSONALTAB.VPRIS = 0 THEN DO:
                        IF HUSOBJ.PRIS >= PERSONALTAB.MINPRISVI THEN musz = FALSE.
                        ELSE musz = TRUE.
                     END.                 
                     ELSE DO:   
                        IF HUSOBJ.PRIS >= PERSONALTAB.MINPRISVI AND 
                        HUSOBJ.PRIS <= PERSONALTAB.VPRIS THEN musz = FALSE.
                        ELSE musz = TRUE.
                     END.                         
                  END.
               END.
            END.  
         END.                          
         IF musz = FALSE THEN DO:                  
            fnamn = TRIM(HUSOBJ.NAMN).   
            fnamn = CAPS(SUBSTRING(fnamn,1,1)) + LC(SUBSTRING(fnamn,2)).     
            IF INDEX(fnamn,"-") = 0 THEN DO: 
               fnamn = fnamn.
            END.             
            ELSE DO:
               ednum3 = INDEX(fnamn,"-").
               fnamn = CAPS(SUBSTRING(fnamn,1,1)) + LC(SUBSTRING(fnamn,2)).
               fnamn = SUBSTRING(fnamn,1,ednum3) + CAPS(SUBSTRING(fnamn,ednum3 + 1,1)) + LC(SUBSTRING(fnamn,ednum3 + 2)).
            END.
            IF INDEX(fnamn," ") = 0 THEN DO: 
               fnamn = fnamn.
            END.             
            ELSE DO:
               ednum3 = INDEX(fnamn," ").
               fnamn = CAPS(SUBSTRING(fnamn,1,1)) + LC(SUBSTRING(fnamn,2)).
               fnamn = SUBSTRING(fnamn,1,ednum3) + CAPS(SUBSTRING(fnamn,ednum3 + 1,1)) + LC(SUBSTRING(fnamn,ednum3 + 2)).
            END. 
            enamn = TRIM(HUSOBJ.GATADR).   
            enamn = CAPS(SUBSTRING(enamn,1,1)) + LC(SUBSTRING(enamn,2)).     
            IF INDEX(enamn,"-") = 0 THEN DO: 
               enamn = enamn.
            END.             
            ELSE DO:
               ednum3 = INDEX(enamn,"-").
               enamn = CAPS(SUBSTRING(enamn,1,1)) + LC(SUBSTRING(enamn,2)).
               enamn = SUBSTRING(enamn,1,ednum3) + CAPS(SUBSTRING(enamn,ednum3 + 1,1)) + LC(SUBSTRING(enamn,ednum3 + 2)).
            END.
            IF INDEX(enamn," ") = 0 THEN DO: 
               enamn = enamn.
            END.             
            ELSE DO:
               ednum3 = INDEX(enamn," ").
               enamn = CAPS(SUBSTRING(enamn,1,1)) + LC(SUBSTRING(enamn,2)).
               enamn = SUBSTRING(enamn,1,ednum3) + CAPS(SUBSTRING(enamn,ednum3 + 1,1)) + LC(SUBSTRING(enamn,ednum3 + 2)).
            END.           
            IF HUSOBJ.BOTYP = "BOSTADSRÄTTER" OR HUSOBJ.BOTYP = "BRF-Radhus" THEN DO TRANSACTION:  
               CREATE UTOBJEKT.
               ASSIGN     
               UTOBJEKT.ORDNING = 2
               UTOBJEKT.PERSONALKOD = PERSONALTAB.PERSONALKOD   
               UTOBJEKT.MAKLAREKOD = HUSOBJ.PERSONALKOD         
               UTOBJEKT.TELEFON = HUSOBJ.TELEFON
               UTOBJEKT.STADSDEL = HUSOBJ.STADSDEL 
               UTOBJEKT.KOPPLING = HUSOBJ.STADSDEL + HUSOBJ.BOTYP + PERSONALTAB.PERSONALKOD                    
               UTOBJEKT.NAMN = fnamn
               UTOBJEKT.GATADR = enamn            
               UTOBJEKT.BOTYP = HUSOBJ.BOTYP
               UTOBJEKT.PRIS = HUSOBJ.PRIS
               UTOBJEKT.OMRADE = HUSOBJ.OMRADE   
               UTOBJEKT.HUSOBJEKT = HUSOBJ.HUSOBJEKT   
               UTOBJEKT.BOAREA = HUSOBJ.BOAREA
               UTOBJEKT.RUM = HUSOBJ.RUM 
               UTOBJEKT.BIYTA = HUSOBJ.MANAVG   /*OBS !!*/
               UTOBJEKT.FIRMA = HUSOBJ.FIRMA   
              /* UTOBJEKT.TOMTYTA = HUSOBJ.TOMTYTA*/
               UTOBJEKT.TOTYTA = HUSOBJ.TOTYTA
               UTOBJEKT.BYGGAR = HUSOBJ.BYGGAR 
               UTOBJEKT.MANAVG = HUSOBJ.MANAVG.
               IF HUSOBJ.BOTYP = "BRF-Radhus" THEN DO:
                  ASSIGN                  
                  UTOBJEKT.ORDNING = 3
                  UTOBJEKT.BIYTA = HUSOBJ.BIYTA        /*OBS!*/
                  UTOBJEKT.TOTYTA = HUSOBJ.MANAVG.
               END.
               IF jmlist = TRUE AND HUSOBJ.MANAVG > 0 THEN DO:
                  IF UTOBJEKT.PRIS > 0 AND UTOBJEKT.BOAREA > 0 THEN DO:
                     UTOBJEKT.BYGGAR = 
                     (UTOBJEKT.PRIS * 0.049 + HUSOBJ.MANAVG * 12) / UTOBJEKT.BOAREA.              
                     FIND FIRST medeltemp WHERE 
                     medeltemp.OMRADE = HUSOBJ.STADSDEL 
                     USE-INDEX OMR NO-ERROR.
                     IF NOT AVAILABLE medeltemp THEN CREATE medeltemp.
                     ASSIGN
                     medeltemp.OMRADE = HUSOBJ.STADSDEL
                     medeltemp.BELOPP = medeltemp.BELOPP + UTOBJEKT.BYGGAR
                     medeltemp.ANTAL = medeltemp.ANTAL + 1.
                  END.
               END.
               IF UTOBJEKT.PRIS > 0 AND UTOBJEKT.BOAREA > 0 THEN DO:              
                  UTOBJEKT.PRISKVM = 1000 * (UTOBJEKT.PRIS / UTOBJEKT.BOAREA).    
               END.                                                       
            END.
            ELSE DO TRANSACTION:
               CREATE UTOBJEKT.
               ASSIGN    
               UTOBJEKT.ORDNING = 1
               UTOBJEKT.PERSONALKOD = PERSONALTAB.PERSONALKOD   
               UTOBJEKT.MAKLAREKOD = HUSOBJ.PERSONALKOD         
               UTOBJEKT.TELEFON = HUSOBJ.TELEFON
               UTOBJEKT.STADSDEL = HUSOBJ.STADSDEL 
               UTOBJEKT.KOPPLING = HUSOBJ.STADSDEL + HUSOBJ.BOTYP + PERSONALTAB.PERSONALKOD                     
               UTOBJEKT.NAMN = fnamn
               UTOBJEKT.GATADR = enamn            
               UTOBJEKT.BOTYP = HUSOBJ.BOTYP
               UTOBJEKT.PRIS = HUSOBJ.PRIS
               UTOBJEKT.OMRADE = HUSOBJ.OMRADE   
               UTOBJEKT.HUSOBJEKT = HUSOBJ.HUSOBJEKT   
               UTOBJEKT.BOAREA = HUSOBJ.BOAREA
               UTOBJEKT.RUM = HUSOBJ.RUM 
               UTOBJEKT.BIYTA = HUSOBJ.BIYTA
               UTOBJEKT.FIRMA = HUSOBJ.FIRMA   
               UTOBJEKT.TOMTYTA = HUSOBJ.TOMTYTA
               UTOBJEKT.TOTYTA = HUSOBJ.TOTYTA
               UTOBJEKT.BYGGAR = HUSOBJ.BYGGAR 
               UTOBJEKT.MANAVG = HUSOBJ.MANAVG.               
               IF UTOBJEKT.PRIS > 0 AND UTOBJEKT.BOAREA > 0 THEN DO:              
                  UTOBJEKT.PRISKVM = 1000 * (UTOBJEKT.PRIS / UTOBJEKT.BOAREA).    
               END.
            END.                                       
            DO TRANSACTION:
               IF UTOBJEKT.PRIS = 0 THEN UTOBJEKT.PRIS = ?.
               IF UTOBJEKT.RUM = 0 THEN UTOBJEKT.RUM = ?. 
               IF UTOBJEKT.BOAREA = 0 THEN UTOBJEKT.BOAREA = ?. 
               IF UTOBJEKT.BIYTA = 0 THEN UTOBJEKT.BIYTA = ?.
               IF UTOBJEKT.TOTYTA = 0 THEN UTOBJEKT.TOTYTA = ?.
               IF UTOBJEKT.TOMTYTA = 0 THEN UTOBJEKT.TOMTYTA = ?.
               IF UTOBJEKT.BYGGAR = 0 THEN UTOBJEKT.BYGGAR = ?.    
            END.
         END.
         musz = FALSE.
         GET NEXT objq  NO-LOCK.          
      END.                       
      CLOSE QUERY objq.                     
      GET NEXT ompq NO-LOCK.
   END.            
   CLOSE QUERY ompq.
   IF jmlist = TRUE THEN DO TRANSACTION:
      FOR EACH medeltemp: 
         FIND FIRST TENDENS WHERE TENDENS.OMRADE = medeltemp.OMRADE 
         USE-INDEX OMR EXCLUSIVE-LOCK NO-ERROR.
         IF NOT AVAILABLE TENDENS THEN DO:
            CREATE TENDENS.
            ASSIGN    
            TENDENS.DATUM = TODAY 
            TENDENS.MEDEL = medeltemp.BELOPP / medeltemp.ANTAL
            TENDENS.OMRADE = medeltemp.OMRADE.
         END.
         CREATE UTOBJEKT.
         ASSIGN     
         UTOBJEKT.ORDNING = 2
         UTOBJEKT.PERSONALKOD = PERSONALTAB.PERSONALKOD   
         UTOBJEKT.STADSDEL = medeltemp.OMRADE 
         UTOBJEKT.KOPPLING = medeltemp.OMRADE + "BOSTADSRÄTTER" + PERSONALTAB.PERSONALKOD                      
         UTOBJEKT.NAMN = "" 
         UTOBJEKT.GATADR = ""             
         UTOBJEKT.BOTYP = "BOSTADSRÄTTER"         
         UTOBJEKT.OMRADE = "ÖÖ".
         IF UTOBJEKT.PRIS = 0 THEN UTOBJEKT.PRIS = ?.
         IF UTOBJEKT.RUM = 0 THEN UTOBJEKT.RUM = ?. 
         IF UTOBJEKT.BOAREA = 0 THEN UTOBJEKT.BOAREA = ?. 
         IF UTOBJEKT.BIYTA = 0 THEN UTOBJEKT.BIYTA = ?.
         IF UTOBJEKT.TOTYTA = 0 THEN UTOBJEKT.TOTYTA = ?.
         IF UTOBJEKT.TOMTYTA = 0 THEN UTOBJEKT.TOMTYTA = ?.
         IF UTOBJEKT.BYGGAR = 0 THEN UTOBJEKT.BYGGAR = ?.    
         
         CREATE UTOBJEKT.
         ASSIGN     
         UTOBJEKT.ORDNING = 2
         UTOBJEKT.PERSONALKOD = PERSONALTAB.PERSONALKOD   
         UTOBJEKT.STADSDEL = medeltemp.OMRADE 
         UTOBJEKT.KOPPLING = medeltemp.OMRADE + "BOSTADSRÄTTER" + PERSONALTAB.PERSONALKOD                     
         UTOBJEKT.NAMN = "Medelvärde" 
         UTOBJEKT.GATADR = ""             
         UTOBJEKT.BOTYP = "BOSTADSRÄTTER"         
         UTOBJEKT.OMRADE = "ÖÖÖ".
         IF UTOBJEKT.PRIS = 0 THEN UTOBJEKT.PRIS = ?.
         IF UTOBJEKT.RUM = 0 THEN UTOBJEKT.RUM = ?. 
         IF UTOBJEKT.BOAREA = 0 THEN UTOBJEKT.BOAREA = ?. 
         IF UTOBJEKT.BIYTA = 0 THEN UTOBJEKT.BIYTA = ?.
         IF UTOBJEKT.TOTYTA = 0 THEN UTOBJEKT.TOTYTA = ?.
         IF UTOBJEKT.TOMTYTA = 0 THEN UTOBJEKT.TOMTYTA = ?.
         IF UTOBJEKT.BYGGAR = 0 THEN UTOBJEKT.BYGGAR = ?.    
         IF medeltemp.ANTAL > 0 THEN DO:
            UTOBJEKT.GATADR =
            STRING(medeltemp.BELOPP / medeltemp.ANTAL,">>>9") + " kr".                        
         END.
         CREATE UTOBJEKT.
         ASSIGN     
         UTOBJEKT.ORDNING = 2
         UTOBJEKT.PERSONALKOD = PERSONALTAB.PERSONALKOD   
         UTOBJEKT.STADSDEL = medeltemp.OMRADE 
         UTOBJEKT.KOPPLING = medeltemp.OMRADE + "BOSTADSRÄTTER" + PERSONALTAB.PERSONALKOD                     
         UTOBJEKT.NAMN = "Tendens" 
         UTOBJEKT.GATADR = ""             
         UTOBJEKT.BOTYP = "BOSTADSRÄTTER"         
         UTOBJEKT.OMRADE = "ÖÖÖ".
         IF UTOBJEKT.PRIS = 0 THEN UTOBJEKT.PRIS = ?.
         IF UTOBJEKT.RUM = 0 THEN UTOBJEKT.RUM = ?. 
         IF UTOBJEKT.BOAREA = 0 THEN UTOBJEKT.BOAREA = ?. 
         IF UTOBJEKT.BIYTA = 0 THEN UTOBJEKT.BIYTA = ?.
         IF UTOBJEKT.TOTYTA = 0 THEN UTOBJEKT.TOTYTA = ?.
         IF UTOBJEKT.TOMTYTA = 0 THEN UTOBJEKT.TOMTYTA = ?.
         IF UTOBJEKT.BYGGAR = 0 THEN UTOBJEKT.BYGGAR = ?.    
         IF TENDENS.MEDEL > 0 THEN DO:
            varmed = 
            ((medeltemp.BELOPP / medeltemp.ANTAL) - TENDENS.MEDEL) / TENDENS.MEDEL * 100.
            IF varmed >= 0 THEN DO:
               UTOBJEKT.GATADR = " + " + STRING(varmed) + "%".
            END.
            ELSE DO:
               varmed = ABS(varmed).
               UTOBJEKT.GATADR = " - " + STRING(varmed) + "%".                   
            END.            
         END.               
         IF upjm = TRUE THEN DO:
            ASSIGN    
            TENDENS.DATUM = TODAY 
            TENDENS.MEDEL = medeltemp.BELOPP / medeltemp.ANTAL
            TENDENS.OMRADE = medeltemp.OMRADE.
         END.
         DELETE medeltemp. 
      END.
   END.
   OPEN QUERY utobjq FOR EACH UTOBJEKT WHERE UTOBJEKT.PERSONALKOD = PERSONALTAB.PERSONALKOD 
   USE-INDEX OBJ NO-LOCK BY UTOBJEKT.BOTYP BY UTOBJEKT.STADSDEL.
   GET FIRST utobjq NO-LOCK.
   DO WHILE AVAILABLE(UTOBJEKT): 
      IF UTOBJEKT.BOTYP = "BOSTADSRÄTTER" THEN musz = musz.
      ELSE IF UTOBJEKT.BOTYP = "BRF-Radhus" THEN musz = musz.
      ELSE DO:   
         IF botvar = UTOBJEKT.BOTYP AND stde = UTOBJEKT.STADSDEL THEN botvar = botvar.
         ELSE DO:
            FIND FIRST OMRADETAB WHERE OMRADETAB.STADSDEL = UTOBJEKT.STADSDEL AND
            OMRADETAB.OMRADE = UTOBJEKT.STADSDEL 
            USE-INDEX STADOMR NO-LOCK NO-ERROR.                  
            IF AVAILABLE OMRADETAB THEN DO TRANSACTION:                     
               FIND FIRST UTRUBRIK WHERE UTRUBRIK.KOPPLING = UTOBJEKT.KOPPLING NO-LOCK NO-ERROR.             
               IF NOT AVAILABLE UTRUBRIK THEN DO:
                  CREATE UTRUBRIK. 
                  ASSIGN         
                  UTRUBRIK.KOPPLING = UTOBJEKT.KOPPLING              
                  UTRUBRIK.BOTYP = CAPS(UTOBJEKT.BOTYP) + " " + CAPS(OMRADETAB.NAMN)
                  UTRUBRIK.NAMN = CAPS(OMRADETAB.NAMN).                                               
                  ASSIGN
                  stde = ""
                  botvar = "".     
                  CREATE UTRUBRIK1.
                  ASSIGN        
                  UTRUBRIK1.KOPPLING = UTOBJEKT.KOPPLING                               
                  UTRUBRIK1.BI-MAN = "Bi"
                  UTRUBRIK1.BOAREA = "Bo"
                  UTRUBRIK1.BOTYP = "Område" 
                  UTRUBRIK1.BRUKS = "Bruks"
                  UTRUBRIK1.BYGG = "Bygg" 
                  UTRUBRIK1.MAK = "Mäk"
                  UTRUBRIK1.NAMN = "Adress"
                  UTRUBRIK1.PRIS = "Pris"
                  UTRUBRIK1.RUM = "Rum" 
                  UTRUBRIK1.TELEFON = "Telefon"
                  UTRUBRIK1.TOMT = "Tomt".               
                  CREATE UTRUBRIK2.
                  ASSIGN                   
                  UTRUBRIK2.KOPPLING = UTOBJEKT.KOPPLING 
                  UTRUBRIK2.AREA1 = "area" 
                  UTRUBRIK2.AREA2 = "area"  
                  UTRUBRIK2.AREA3 = "area" 
                  UTRUBRIK2.AREA4 = "area"    /*ANVÄNDS EJ*/  
                  UTRUBRIK2.ARTAL = "år"   
                  UTRUBRIK2.MAKLARE = "lare"
                  UTRUBRIK2.TKR = "tkr" 
                  UTRUBRIK2.AVGIFT = "area" .                                                     
               END.
            END.
            ELSE MESSAGE UTOBJEKT.STADSDEL "MÅSTE VARA FEL UPPLAGGT" VIEW-AS ALERT-BOX.
            ASSIGN
            botvar = UTOBJEKT.BOTYP 
            stde = UTOBJEKT.STADSDEL.         
         END.
      END.
      GET NEXT utobjq NO-LOCK.  
   END.   
   CLOSE QUERY utobjq.
   OPEN QUERY utobjbrq FOR EACH UTOBJEKT WHERE UTOBJEKT.PERSONALKOD = PERSONALTAB.PERSONALKOD AND UTOBJEKT.BOTYP = "BOSTADSRÄTTER" 
   USE-INDEX BOTYP NO-LOCK BY UTOBJEKT.BOTYP BY UTOBJEKT.STADSDEL.
   GET FIRST utobjbrq NO-LOCK.
   DO WHILE AVAILABLE(UTOBJEKT):        
      IF botvar = UTOBJEKT.BOTYP AND stde = UTOBJEKT.STADSDEL THEN botvar = botvar.
      ELSE DO:         
         FIND FIRST OMRADETAB WHERE OMRADETAB.STADSDEL = UTOBJEKT.STADSDEL AND
         OMRADETAB.OMRADE = UTOBJEKT.STADSDEL
         USE-INDEX STADOMR NO-LOCK NO-ERROR.         
         IF AVAILABLE OMRADETAB THEN DO TRANSACTION:            
            FIND FIRST UTRUBRIK WHERE UTRUBRIK.KOPPLING = UTOBJEKT.KOPPLING NO-LOCK NO-ERROR.             
            IF NOT AVAILABLE UTRUBRIK THEN DO:
               CREATE UTRUBRIK.                                
               ASSIGN         
               UTRUBRIK.KOPPLING = UTOBJEKT.KOPPLING              
               UTRUBRIK.BOTYP = CAPS(UTOBJEKT.BOTYP) + " " + CAPS(OMRADETAB.NAMN)
               UTRUBRIK.NAMN = CAPS(OMRADETAB.NAMN).                                                                               
               ASSIGN
               stde = ""
               botvar = "".
               CREATE UTRUBRIK1.
               ASSIGN                       
               UTRUBRIK1.KOPPLING = UTOBJEKT.KOPPLING                              
               UTRUBRIK1.BI-MAN = "Månads"  
               UTRUBRIK1.BOAREA = "Bo"
               UTRUBRIK1.BOTYP = "Område" 
               UTRUBRIK1.BRUKS = ""
               UTRUBRIK1.BYGG = "Bygg" 
               UTRUBRIK1.MAK = "Mäk"
               UTRUBRIK1.NAMN = "Adress"
               UTRUBRIK1.PRIS = "Pris"
               UTRUBRIK1.RUM = "Rum" 
               UTRUBRIK1.TELEFON = "Telefon"
               UTRUBRIK1.TOMT = "".  
               IF jmlist = TRUE THEN  UTRUBRIK1.BYGG = "Värd".                                                      
               CREATE UTRUBRIK2.
               ASSIGN                   
               UTRUBRIK2.KOPPLING = UTOBJEKT.KOPPLING
               UTRUBRIK2.AREA1 = "" 
               UTRUBRIK2.AREA2 = ""  
               UTRUBRIK2.AREA3 = "area" 
               UTRUBRIK2.AREA4 = ""   
               UTRUBRIK2.AVGIFT = "avgift" 
               UTRUBRIK2.ARTAL = "år"   
               UTRUBRIK2.MAKLARE = "lare"
               UTRUBRIK2.TKR = "tkr".
               IF jmlist = TRUE THEN UTRUBRIK2.ARTAL = "".                                                                 
            END.
         END.
         ELSE MESSAGE UTOBJEKT.STADSDEL "MÅSTE VARA FEL UPPLAGGT" VIEW-AS ALERT-BOX.
         ASSIGN
         botvar = UTOBJEKT.BOTYP 
         stde = UTOBJEKT.STADSDEL.
      END.
      GET NEXT utobjbrq NO-LOCK.            
   END.
   CLOSE QUERY utobjbrq.
   OPEN QUERY utobjbrq FOR EACH UTOBJEKT WHERE UTOBJEKT.PERSONALKOD = PERSONALTAB.PERSONALKOD AND UTOBJEKT.BOTYP = "BRF-Radhus" 
   USE-INDEX BOTYP NO-LOCK BY UTOBJEKT.BOTYP BY UTOBJEKT.STADSDEL.
   GET FIRST utobjbrq NO-LOCK.
   DO WHILE AVAILABLE(UTOBJEKT):        
      IF botvar = UTOBJEKT.BOTYP AND stde = UTOBJEKT.STADSDEL THEN botvar = botvar.
      ELSE DO:         
         FIND FIRST OMRADETAB WHERE OMRADETAB.STADSDEL = UTOBJEKT.STADSDEL AND
         OMRADETAB.OMRADE = UTOBJEKT.STADSDEL
         USE-INDEX STADOMR NO-LOCK NO-ERROR.         
         IF AVAILABLE OMRADETAB THEN DO TRANSACTION:
            FIND FIRST UTRUBRIK WHERE UTRUBRIK.KOPPLING = UTOBJEKT.KOPPLING NO-LOCK NO-ERROR.             
            IF NOT AVAILABLE UTRUBRIK THEN DO:
               CREATE UTRUBRIK.                                
               ASSIGN         
               UTRUBRIK.KOPPLING = UTOBJEKT.KOPPLING              
               UTRUBRIK.BOTYP = CAPS(UTOBJEKT.BOTYP) + " " + CAPS(OMRADETAB.NAMN)
               UTRUBRIK.NAMN = CAPS(OMRADETAB.NAMN).                                                                               
               ASSIGN
               stde = ""
               botvar = "".
               CREATE UTRUBRIK1.
               ASSIGN                       
               UTRUBRIK1.KOPPLING = UTOBJEKT.KOPPLING                              
               UTRUBRIK1.BI-MAN = "Bi"  
               UTRUBRIK1.BOAREA = "Bo"
               UTRUBRIK1.BOTYP = "Område" 
               UTRUBRIK1.BRUKS = "Månads"
               UTRUBRIK1.BYGG = "Bygg" 
               UTRUBRIK1.MAK = "Mäk"
               UTRUBRIK1.NAMN = "Adress"
               UTRUBRIK1.PRIS = "Pris"
               UTRUBRIK1.RUM = "Rum" 
               UTRUBRIK1.TELEFON = "Telefon"
               UTRUBRIK1.TOMT = "".  
               IF jmlist = TRUE THEN  UTRUBRIK1.BYGG = "Värd".                                                      
               CREATE UTRUBRIK2.
               ASSIGN                   
               UTRUBRIK2.KOPPLING = UTOBJEKT.KOPPLING
               UTRUBRIK2.AREA1 = "avgift" 
               UTRUBRIK2.AREA2 = ""  
               UTRUBRIK2.AREA3 = "area" 
               UTRUBRIK2.AREA4 = ""   
               UTRUBRIK2.AVGIFT = "area" 
               UTRUBRIK2.ARTAL = "år"   
               UTRUBRIK2.MAKLARE = "lare"
               UTRUBRIK2.TKR = "tkr".
               IF jmlist = TRUE THEN UTRUBRIK2.ARTAL = "".                                                                 
            END.
         END.
         ELSE MESSAGE UTOBJEKT.STADSDEL "MÅSTE VARA FEL UPPLAGGT" VIEW-AS ALERT-BOX.
         ASSIGN
         botvar = UTOBJEKT.BOTYP 
         stde = UTOBJEKT.STADSDEL.
      END.
      GET NEXT utobjbrq NO-LOCK.            
   END.
   CLOSE QUERY utobjbrq.
   IF faxuppd = TRUE THEN DO:
      DO TRANSACTION:
         FIND FIRST persbuff WHERE persbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
         USE-INDEX PERSONALKOD EXCLUSIVE-LOCK NO-ERROR.  
         ASSIGN                  
         persbuff.ANTALFAX = persbuff.ANTALFAX + 1
         persbuff.FAXAT = TODAY.
         IF persrec = RECID(persbuff) THEN DO:  
           /*
            IF persbuff.TELEFON2 = "" THEN DO:      
               FIND FIRST MEDDELANDE WHERE MEDDELANDE.MOTTAGARE = "KUNDBREV"
               USE-INDEX MEDD EXCLUSIVE-LOCK NO-ERROR.
               IF AVAILABLE MEDDELANDE THEN DELETE MEDDELANDE.       
            END.    
            ELSE DO:      
               FIND FIRST MEDDELANDE WHERE MEDDELANDE.MOTTAGARE = "KUNDFAX"
               USE-INDEX MEDD EXCLUSIVE-LOCK NO-ERROR.
               IF AVAILABLE MEDDELANDE THEN DELETE MEDDELANDE.    
            END.
            */   
         END. 
      END.
   END.
   IF utslutvar = TRUE THEN RUN sokk_UI.              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tabort_UI WINDOW-2 
PROCEDURE tabort_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/        
   OPEN QUERY utfaxq FOR EACH UTFAXNUMMER WHERE 
   UTFAXNUMMER.PERSONALKOD = PERSONALTAB.PERSONALKOD NO-LOCK.
   DO TRANSACTION:
      GET FIRST utfaxq SHARE-LOCK.
      DO WHILE AVAILABLE(UTFAXNUMMER):                         
         DELETE UTFAXNUMMER.          
         GET NEXT utfaxq SHARE-LOCK.
      END.         
   END.
   OPEN QUERY utobjq FOR EACH UTOBJEKT WHERE UTOBJEKT.PERSONALKOD = 
   PERSONALTAB.PERSONALKOD NO-LOCK.
   DO TRANSACTION:
      GET FIRST utobjq SHARE-LOCK.     
      DO WHILE AVAILABLE(UTOBJEKT):
         OPEN QUERY utrubq FOR EACH UTRUBRIK WHERE UTRUBRIK.KOPPLING =  
         UTOBJEKT.KOPPLING NO-LOCK.
         GET FIRST utrubq SHARE-LOCK.
         DO WHILE AVAILABLE(UTRUBRIK):                   
            DELETE UTRUBRIK. 
            GET NEXT utrubq SHARE-LOCK.
         END.    
         OPEN QUERY utrub1q FOR EACH UTRUBRIK1 WHERE UTRUBRIK1.KOPPLING =  
         UTOBJEKT.KOPPLING NO-LOCK.
         GET FIRST utrub1q SHARE-LOCK.
         DO WHILE AVAILABLE(UTRUBRIK1):       
            DELETE UTRUBRIK1. 
            GET NEXT utrub1q SHARE-LOCK.
         END.   
         OPEN QUERY utrub2q FOR EACH UTRUBRIK2 WHERE UTRUBRIK2.KOPPLING =  
         UTOBJEKT.KOPPLING NO-LOCK.
         GET FIRST utrub2q SHARE-LOCK.
         DO WHILE AVAILABLE(UTRUBRIK2):       
            DELETE UTRUBRIK2. 
            GET NEXT utrub2q SHARE-LOCK.
         END.       
         DELETE UTOBJEKT. 
         GET NEXT utobjq SHARE-LOCK.
      END.   
   END. 
   
   OPEN QUERY utfax2q FOR EACH UTFAXNUMMER2 WHERE UTFAXNUMMER2.PERSONALKOD = 
   PERSONALTAB.PERSONALKOD NO-LOCK.
   DO TRANSACTION:
      GET FIRST utfax2q SHARE-LOCK.
      DO WHILE AVAILABLE(UTFAXNUMMER2):       
         DELETE UTFAXNUMMER2. 
         GET NEXT utfax2q SHARE-LOCK.
      END.   
   END.             
   OPEN QUERY utslutq FOR EACH UTSLUT WHERE UTSLUT.PERSONALKOD = 
   PERSONALTAB.PERSONALKOD NO-LOCK.
   DO TRANSACTION:
      GET FIRST utslutq SHARE-LOCK.
      DO WHILE AVAILABLE(UTSLUT):       
         DELETE UTSLUT. 
         GET NEXT utslutq SHARE-LOCK.
      END.   
   END.    
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE text_UI WINDOW-2 
PROCEDURE text_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  /*TEXT*/
   tidrb = ETIME - berrb.
   WINDOW-2:TITLE = "SPEKULANT - " + STRING(PERSONALTAB.KUNDNR,">>>>>>9") + " " + STRING(TIME,"HH:MM:SS") + " TID SAMMAN. " + STRING(tidrb).
   berrb = ETIME.   
   MESSAGE "SKA SAMMANSTÄLLNINGEN VISAS I EXCEL ?" VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO 
   UPDATE val1 AS LOGICAL.
   CASE val1:
      WHEN TRUE THEN DO:
         RUN RBEXCEL.P (INPUT PERSONALTAB.PERSONALKOD, INPUT Guru.Konstanter:globforetag) .                           
      END.
      WHEN FALSE THEN DO:
           RUN RBTERM.P (INPUT PERSONALTAB.PERSONALKOD).  
      END.
   END CASE.
   

   tidrb = ETIME - berrb.
   WINDOW-2:TITLE = "SPEKULANT - " + STRING(PERSONALTAB.KUNDNR,">>>>>>9") + " " + STRING(TIME,"HH:MM:SS") + " TID RB. " + STRING(tidrb).
   RUN tabort_UI.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI WINDOW-2 
PROCEDURE ut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  /*UT*/                                      
   tidrb = ETIME - berrb.
   WINDOW-2:TITLE = "SPEKULANT - " + STRING(PERSONALTAB.KUNDNR,">>>>>>9") + " " + STRING(TIME,"HH:MM:SS") + " TID SAMMAN. " + STRING(tidrb).
   berrb = ETIME.
   RUN RBSKRIV.P (INPUT PERSONALTAB.PERSONALKOD, INPUT rb-print).  
   tidrb = ETIME - berrb.
   WINDOW-2:TITLE = "SPEKULANT - " + STRING(PERSONALTAB.KUNDNR,">>>>>>9") + " " + STRING(TIME,"HH:MM:SS") + " TID RB. " + STRING(tidrb).
   RUN tabort_UI.       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


