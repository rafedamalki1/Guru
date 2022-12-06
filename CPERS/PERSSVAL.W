&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
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

  Created: 95/07/04 - 11:31 am

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

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{VPERSONALTEMP.I}
{EPERSTEMP.I}
DEFINE INPUT PARAMETER TABLE FOR valperstemp.
&Scoped-define NEW
{GLOBVAR2DEL1.I}
{REGVAR.I}

&Scoped-define SHARED SHARED
{AVTAL.I}
&Scoped-define NEW NEW

{KODERAVT.I}
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec2 AS RECID NO-UNDO. 
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE NEW SHARED TEMP-TABLE tidut1
   FIELD UT AS CHARACTER FORMAT "X(90)".
DEFINE NEW SHARED TEMP-TABLE tidut2
   FIELD UT AS CHARACTER FORMAT "X(90)".
DEFINE NEW SHARED TEMP-TABLE tidut3
   FIELD UT AS CHARACTER FORMAT "X(90)".
   
DEFINE VARIABLE vpers AS CHARACTER FORMAT "X(5)".
DEFINE VARIABLE vomr AS CHARACTER FORMAT "X(6)".
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(80)" NO-UNDO.
DEFINE VARIABLE dispens AS LOGICAL NO-UNDO.
DEFINE VARIABLE lakdispens AS LOGICAL NO-UNDO.
DEFINE VARIABLE avarfor AS INTEGER NO-UNDO.
DEFINE VARIABLE spafr AS LOGICAL NO-UNDO.
DEFINE VARIABLE avkfaktor AS DECIMAL NO-UNDO.
DEFINE VARIABLE sefaktor AS DECIMAL NO-UNDO.
DEFINE VARIABLE dardatum AS DATE  NO-UNDO.
DEFINE VARIABLE dargodk AS CHARACTER NO-UNDO.

DEFINE VARIABLE dmandatum AS DATE  NO-UNDO.
DEFINE VARIABLE dmangodk AS CHARACTER NO-UNDO.

DEFINE VARIABLE d48datum AS DATE  NO-UNDO.
DEFINE VARIABLE d48godk AS CHARACTER NO-UNDO.   
DEFINE VARIABLE a50datum AS DATE  NO-UNDO.
DEFINE VARIABLE a50godk AS CHARACTER NO-UNDO.
DEFINE VARIABLE a50tim AS INTEGER NO-UNDO.

DEFINE VARIABLE dispensm AS LOGICAL NO-UNDO.
DEFINE VARIABLE dispens48 AS LOGICAL NO-UNDO.
DEFINE VARIABLE ftro AS LOGICAL NO-UNDO.
DEFINE VARIABLE tillit AS LOGICAL NO-UNDO.
DEFINE VARIABLE lonvaxatk AS LOGICAL NO-UNDO.
DEFINE VARIABLE plfanns AS LOGICAL NO-UNDO.
DEFINE VARIABLE ater50 AS LOGICAL NO-UNDO.
DEFINE VARIABLE eorgnr  AS CHARACTER NO-UNDO.
DEFINE VARIABLE eforetag AS CHARACTER NO-UNDO.
DEFINE VARIABLE pkod AS CHARACTER NO-UNDO.
DEFINE VARIABLE sealltid AS LOGICAL NO-UNDO.
DEFINE VARIABLE sedatum AS DATE  NO-UNDO.
DEFINE VARIABLE segodk AS CHARACTER NO-UNDO.
DEFINE VARIABLE nofall AS LOGICAL NO-UNDO.
DEFINE VARIABLE nodatum AS DATE NO-UNDO.
DEFINE VARIABLE nogodk AS CHARACTER NO-UNDO.

DEFINE VARIABLE asfaktapph AS HANDLE NO-UNDO.
DEFINE VARIABLE restuserh AS HANDLE NO-UNDO.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-B

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD_SORTPERS BTN_VPER BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS RAD_SORTPERS 

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

DEFINE BUTTON BTN_VPER 
     LABEL "Visa":L 
     SIZE 14 BY 1.

DEFINE {&NEW} SHARED VARIABLE RAD_SORTPERS AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Alla personaluppgifter och avtal", 1
          

     SIZE 38.5 BY 3.92 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-B
     RAD_SORTPERS AT ROW 3.38 COL 16.13 NO-LABEL
     BTN_VPER AT ROW 8 COL 56.13
     BTN_AVB AT ROW 21.13 COL 56.13
     "Visa personaluppgifter:" VIEW-AS TEXT
          SIZE 26.5 BY 1.5 AT ROW 1.29 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 71 BY 21.38.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: ansvarigtemp T "?" NO-UNDO temp-db ansvarigtemp
      TABLE: godkannartemp T "?" NO-UNDO temp-db godkannartemp
      TABLE: omrtemp T "?" NO-UNDO temp-db omrtemp
      TABLE: valperstemp T "?" NO-UNDO temp-db valperstemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Personal"
         HEIGHT             = 21.42
         WIDTH              = 71.25
         MAX-HEIGHT         = 27.25
         MAX-WIDTH          = 100
         VIRTUAL-HEIGHT     = 27.25
         VIRTUAL-WIDTH      = 100
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WINDOW-2
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-B
                                                                        */
/* SETTINGS FOR RADIO-SET RAD_SORTPERS IN FRAME FRAME-B
   SHARED                                                               */
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


&Scoped-define SELF-NAME BTN_VPER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VPER WINDOW-2
ON CHOOSE OF BTN_VPER IN FRAME FRAME-B /* Visa */
DO:    
   ASSIGN RAD_SORTPERS = INPUT RAD_SORTPERS.
   {muswait.i}
   DEBUGGER:SET-BREAK().
   IF RAD_SORTPERS = 1  THEN DO:
      EMPTY TEMP-TABLE eperstemp NO-ERROR. 
      
      FOR EACH valperstemp:
         CREATE eperstemp.
         BUFFER-COPY valperstemp TO eperstemp.
         ASSIGN eperstemp.EPOST = SUBSTRING(valperstemp.PERSONSOK,20).
         FIND FIRST aavtaltemp WHERE aavtaltemp.AVTALTYP = eperstemp.BEFATTNING AND aavtaltemp.TYP = "BEF" NO-LOCK NO-ERROR.
         IF AVAILABLE aavtaltemp THEN DO:
            eperstemp.VIBEFATTNING = aavtaltemp.AVTALNAMN.
         END.                     
         IF eperstemp.OVERTIDUTTAG = "K" THEN ASSIGN eperstemp.OVUT  = "Komp".
         IF eperstemp.OVERTIDUTTAG = "Ö" THEN ASSIGN eperstemp.OVUT = "Över".
         IF eperstemp.OVERTIDUTTAG = "F" THEN ASSIGN eperstemp.OVUT = "Flex".
         IF eperstemp.OVERTIDUTTAG = "I" THEN ASSIGN eperstemp.OVUT = "Ejöv".
         
         pkod = eperstemp.PERSONALKOD.                   
         regdatum = TODAY.
         /*regdatum = 05/02/2022.*/ 
         RUN REGDAG.P.            
         RUN REGVEC.P.
         {SLUTARBW.I}
         ASSIGN
         eperstemp.REGSTART = regstart
         eperstemp.REGSLUT = regslut
         eperstemp.LUNCHSTARTEN = lunchstarten
         eperstemp.LUNCHSLUTET = lunchslutet.
         
         IF Guru.Konstanter:globforetag = "gkal" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
            IF eperstemp.OMREGTID = 0 THEN eperstemp.SEMFOR = FALSE.
            ELSE IF eperstemp.OMREGTID = 1 THEN eperstemp.SEMFOR = TRUE.
            RUN havafor IN asfaktapph (INPUT eperstemp.PERSONALKOD, OUTPUT avarfor).
            eperstemp.AVAFOR = avarfor.   
            RUN hdispens IN asfaktapph (INPUT eperstemp.PERSONALKOD, OUTPUT dispens, OUTPUT dardatum, OUTPUT dargodk).
            ASSIGN                  
            eperstemp.ODISPENS = dispens
            eperstemp.DARDATUM = STRING(dardatum)
            eperstemp.DARGODK = dargodk.   
            RUN hdispensm IN asfaktapph (INPUT eperstemp.PERSONALKOD, OUTPUT dispensm, OUTPUT dmandatum, OUTPUT dmangodk).
            ASSIGN
            eperstemp.OMANDISPENS = dispensm
            eperstemp.DMANDATUM = STRING(dmandatum)
            eperstemp.DMANGODK = dmangodk.
            RUN lonvaxatk IN asfaktapph (INPUT eperstemp.PERSONALKOD, OUTPUT lonvaxatk).
            eperstemp.LONVATK = lonvaxatk.                                            
         END.                                       
         IF Guru.Konstanter:globforetag = "MISV"  THEN DO:   
            RUN hdispens48 IN asfaktapph (INPUT eperstemp.PERSONALKOD, OUTPUT dispens48, OUTPUT d48datum, OUTPUT d48godk).
            ASSIGN     
            eperstemp.O48DISPENS = dispens48
            eperstemp.D48DATUM = STRING(d48datum)
            eperstemp.D48GODK = d48godk.                                 
         END.
         IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "elpa" THEN DO:   
            RUN lakdispens IN asfaktapph (INPUT eperstemp.PERSONALKOD, OUTPUT lakdispens).
            eperstemp.DISPELAK = lakdispens.                              
         END.
         IF Guru.Konstanter:globforetag = "gkal" THEN DO:   
            RUN spfrisk IN asfaktapph (INPUT eperstemp.PERSONALKOD, OUTPUT spafr).
            eperstemp.SPARRFR = spafr.    
            RUN fortro IN asfaktapph (INPUT eperstemp.PERSONALKOD, OUTPUT ftro).
            eperstemp.FORTRO = ftro.
            RUN tillit IN asfaktapph (INPUT eperstemp.PERSONALKOD, OUTPUT tillit).
            eperstemp.TILLIT = tillit.
                                                                     
         END.          
         IF  Guru.Konstanter:globforetag = "sund" THEN DO:
            RUN fortro IN asfaktapph (INPUT eperstemp.PERSONALKOD, OUTPUT ftro).
            eperstemp.FORTRO = ftro.
            
         END.   
         IF Guru.Konstanter:varforetypval[46] = 1 THEN DO:     
            RUN hfaktor IN asfaktapph (INPUT eperstemp.PERSONALKOD, OUTPUT avkfaktor, OUTPUT sefaktor).   
            IF avkfaktor > 0 THEN  ASSIGN eperstemp.AKFAKTOR = avkfaktor.
            ELSE eperstemp.AKFAKTOR = 1.            
            IF sefaktor > 0 THEN ASSIGN eperstemp.SFAKTOR = avkfaktor.
            ELSE ASSIGN eperstemp.SFAKTOR = 1.
         END.
         IF Guru.Konstanter:globforetag = "SNAT"  THEN DO:   
            RUN htillatnodf IN asfaktapph (INPUT eperstemp.PERSONALKOD, OUTPUT nofall, OUTPUT nodatum, OUTPUT nogodk).
            ASSIGN 
            eperstemp.NODFALL = nofall
            eperstemp.NODDATUM = STRING(nodatum) 
            eperstemp.NODGODK = nogodk.                                      
         END.
         IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "snat" OR Guru.Konstanter:globforetag = "misv" THEN DO:
            
            RUN ater50_UI IN asfaktapph (INPUT eperstemp.PERSONALKOD, OUTPUT ater50,OUTPUT a50tim, OUTPUT a50datum, OUTPUT a50godk).
            ASSIGN                     
            eperstemp.ATER50 = ater50
            eperstemp.A50DATUM = STRING(a50datum)
            eperstemp.A50GODK = a50godk
            eperstemp.A50TIM = a50tim.
            
                              
         END.
         IF Guru.Konstanter:varforetypval[2] = 0 THEN DO:            
            RUN sealltid_UI IN asfaktapph (INPUT eperstemp.PERSONALKOD, OUTPUT sealltid, OUTPUT sedatum, OUTPUT seanv).
            ASSIGN                     
            eperstemp.SEALLTID = sealltid
            eperstemp.SEDATUM = STRING(sedatum)
            eperstemp.SEANV = seanv.            
                              
         END.           
                           
         IF Guru.Konstanter:varforetypchar[9] = "1" THEN DO:            
            RUN fannsRESTUser_UI IN restuserh(INPUT eperstemp.PERSONALKOD, OUTPUT plfanns).
            IF plfanns = TRUE THEN DO:  
               eperstemp.PLIGGARE = TRUE.
               RUN hmtpersligg IN asfaktapph (INPUT eperstemp.PERSONALKOD, OUTPUT eorgnr,output eforetag).     
               IF eorgnr = "" THEN  ASSIGN eperstemp.ORGNR = "0000000000".
               ELSE ASSIGN eperstemp.ORGNR = eorgnr.     
               eperstemp.FORETAG = eforetag.
               DEBUGGER:SET-BREAK().           
               RUN persGPLsista_UI IN asfaktapph (INPUT-OUTPUT TABLE eperstemp).
            END.                
         END.       
      END.
      RUN pavt_UI IN asfaktapph (INPUT-OUTPUT TABLE eperstemp).
      RUN PERSVIS.W (INPUT 1, INPUT TABLE eperstemp).
   END.   
   ELSE IF RAD_SORTPERS = 2  THEN DO:
      EMPTY TEMP-TABLE koder NO-ERROR. 
      RUN hamtallalart_UI IN asfaktapph ( OUTPUT TABLE koder).
      RUN AVLARTVIS.W (INPUT TABLE koder).
      
   END.
   ELSE IF RAD_SORTPERS = 21  THEN DO:
      DEBUGGER:SET-BREAK().
      EMPTY TEMP-TABLE eperstemp NO-ERROR.
      RUN persGPL_UI IN asfaktapph (OUTPUT TABLE eperstemp).
      
      RUN PERSVIS.W (INPUT 2, INPUT TABLE eperstemp).
   END.     
   ELSE DO:         
      RUN visapers_UI.                 
   END.

   
   {musarrow.i}      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_SORTPERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_SORTPERS WINDOW-2
ON VALUE-CHANGED OF RAD_SORTPERS IN FRAME FRAME-B
DO:
   ASSIGN RAD_SORTPERS = INPUT RAD_SORTPERS.
     
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
   IF VALID-HANDLE(asfaktapph) THEN DELETE PROCEDURE asfaktapph NO-ERROR.
   IF VALID-HANDLE(restuserh) THEN DELETE PROCEDURE restuserh NO-ERROR.
   
   
    {BORTBRWPROC.I}
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
    /*
 borttagna från RAD_SORTPERS enligt PUL Lena 20171128   
"Godkänner tid - Efternamn", 7,
"Godkänner tid - Enhet/Signatur", 8,
"Område - Efternamn", 10,
"Område - Enhet/Signatur", 11,
"Enbart Övertiduttag = ej övertid", 13,
"Enbart Deltid = Ja", 15,
"Enbart Flex  = Ja", 16
   
    */  
   /*IF Guru.Konstanter:globforetag = "gkal" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND"  OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
      RAD_SORTPERS:ADD-LAST("Enbart Spärr arbetstidsförkortning = Ja", 14).
      RAD_SORTPERS:ADD-LAST("Enbart Avvik arbetstidsförk  = Ja", 19).
      RAD_SORTPERS:ADD-LAST("Enbart Dispens övertid  = Ja", 17).                  
   END.                                
   IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "elpa" THEN DO: 
      RAD_SORTPERS:ADD-LAST("Enbart Dispens läkarbesök  = Ja", 18).  
                                    
   END.   
   IF Guru.Konstanter:globforetag = "elpa" OR Guru.Konstanter:globforetag = "gkal" THEN DO:      
      RAD_SORTPERS:ADD-LAST("Enbart Spärr friskvårdstimmar = Ja",20).
   END.*/
   IF Guru.Konstanter:varforetypchar[9] = "1" AND Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79)  THEN DO:
      RAD_SORTPERS:ADD-LAST("All personal med GPL = Ja",21).
   END.
   IF Guru.Konstanter:varforetypval[2] = 0 THEN DO:
      RAD_SORTPERS:ADD-LAST("Alla lönearter per avtal",2).
   END.   
   
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
/*   ASSIGN
   /*ansvarigtemp.PERSONALKOD:READ-ONLY IN BROWSE BRW_ANSV = TRUE    */
   godkannartemp.PERSONALKOD:READ-ONLY IN BROWSE BRW_GODK = TRUE.
   /*RUN DYNBRW.P PERSISTENT SET brwproc[{&LEFT-BROWSE}] 
      (INPUT BRW_ANSV:HANDLE IN FRAME {&FRAME-NAME}).*/
   RUN DYNBRW.P PERSISTENT SET brwproc[{&RIGHT-BROWSE}] 
      (INPUT BRW_GODK:HANDLE IN FRAME {&FRAME-NAME}).*/       
   IF Guru.Konstanter:appcon THEN DO:      
       RUN ASFAKTAPP.P PERSISTENT SET asfaktapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:      
      RUN ASFAKTAPP.P PERSISTENT SET asfaktapph.
     
   END.
   IF NOT VALID-HANDLE(restuserh) THEN DO:
      IF Guru.Konstanter:appcon THEN DO:
          RUN RESTUSERS.P PERSISTENT SET restuserh ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
      END.
      ELSE DO:      
         RUN RESTUSERS.P PERSISTENT SET restuserh.
      END.   
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
  DISPLAY RAD_SORTPERS 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  ENABLE RAD_SORTPERS BTN_VPER BTN_AVB 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visapers_UI WINDOW-2 
PROCEDURE visapers_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {AVBGOM.I}
   RUN UTRAPP3.W (INPUT TABLE valperstemp).  
   {AVBFRAM.I}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

