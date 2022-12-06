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

  Created: 01/12/96 - 11:16 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE NEW SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE aonrrec2 AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE printer LIKE SKRIVARDEF.UTSKRIFTSTYP NO-UNDO.
DEFINE NEW SHARED VARIABLE printer1 LIKE SKRIVARDEF.SKRIVARID NO-UNDO.
DEFINE NEW SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE globlos LIKE ANVANDARE.AV-LOSEN NO-UNDO.
DEFINE NEW SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE NEW SHARED VARIABLE plusaonr LIKE TIDREGITAB.AONR NO-UNDO.
DEFINE NEW SHARED VARIABLE plusdnr LIKE TIDREGITAB.DELNR NO-UNDO.
DEFINE NEW SHARED VARIABLE plusrec AS RECID  NO-UNDO.
DEFINE NEW SHARED VARIABLE plustidrec AS RECID  NO-UNDO.
DEFINE NEW SHARED VARIABLE plustid AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE plusdval LIKE PROGVAL.JANEJ NO-UNDO.
DEFINE NEW SHARED VARIABLE succelval LIKE PROGVAL.JANEJ NO-UNDO.     
DEFINE NEW SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE NEW SHARED VARIABLE regstart LIKE TIDREGITAB.START NO-UNDO. 
DEFINE NEW SHARED VARIABLE regslut LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE NEW SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE avdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE bilforare AS LOGICAL FORMAT "JA/NEJ" NO-UNDO.
DEFINE NEW SHARED VARIABLE enflerdygns AS LOGICAL FORMAT "ENDAGS/FLERDYGNS" NO-UNDO.
DEFINE NEW SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE NEW SHARED VARIABLE klocka LIKE TIDREGITAB.START NO-UNDO.
DEFINE NEW SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE tidtabrec2 AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE muszval AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE vartgamla AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE NEW SHARED VARIABLE frustarten AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE fruslutet AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffestart AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffeslut AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchstarten LIKE ARBETSTIDTAB.LUNCHSTART NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchslutet LIKE ARBETSTIDTAB.LUNCHSLUT NO-UNDO.
DEFINE NEW SHARED VARIABLE regtotalt LIKE TIDREGITAB.TOTALT NO-UNDO.   
DEFINE NEW SHARED VARIABLE regstartsek AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE regslutsek AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE bustart3 LIKE TIDREGITAB.START NO-UNDO.
DEFINE NEW SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.
DEFINE NEW SHARED VARIABLE appcon AS LOGICAL NO-UNDO.


DEFINE NEW SHARED TEMP-TABLE tidapptemp
   FIELD FORETAG LIKE FORETAG.FORETAG
   FIELD ANVANDARE LIKE ANVANDARE.ANVANDARE
   FIELD RECPERS AS RECID
   FIELD RECTID AS RECID
   FIELD DATUM AS DATE.

DEFINE VARIABLE regdatum2 AS DATE NO-UNDO.  
DEFINE VARIABLE regdatum3 AS DATE NO-UNDO.
DEFINE VARIABLE helg AS INTEGER NO-UNDO.  
DEFINE VARIABLE anr LIKE AONRTAB.AONR NO-UNDO.
DEFINE VARIABLE dnr LIKE AONRTAB.DELNR NO-UNDO.

DEFINE VARIABLE regdagspar AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE VARIABLE justtid AS DECIMAL FORMAT "99.99" NO-UNDO. 
DEFINE BUFFER tidbuff FOR TIDREGITAB.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-41 BTN_NVE FILL-IN_DATUM BTN_FVE ~
BTN_START BTN_AVSL 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_DATUM 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVSL AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 15 BY 2.5.

DEFINE BUTTON BTN_FVE 
     LABEL "-" 
     SIZE 2.5 BY .77.

DEFINE BUTTON BTN_NVE 
     LABEL "+" 
     SIZE 2.5 BY .77.

DEFINE BUTTON BTN_START AUTO-GO 
     LABEL "Starta":L 
     SIZE 15 BY 2.5.

DEFINE VARIABLE FILL-IN_DATUM AS DATE FORMAT "99/99/99" 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 73.5 BY 10
     BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BTN_NVE AT ROW 4.14 COL 42
     FILL-IN_DATUM AT ROW 4.5 COL 29.5 COLON-ALIGNED
     BTN_FVE AT ROW 5 COL 42
     BTN_START AT ROW 7.5 COL 11
     BTN_AVSL AT ROW 7.5 COL 45
     RECT-41 AT ROW 1 COL 1
     "Ange datum för den dagen du vill göra en automatisk tidregistrering" VIEW-AS TEXT
          SIZE 70 BY 2 AT ROW 2 COL 3
     SPACE(2.20) SKIP(7.13)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Automatregistrering".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
                                                                        */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE.

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

&Scoped-define SELF-NAME BTN_AVSL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVSL DIALOG-1
ON CHOOSE OF BTN_AVSL IN FRAME DIALOG-1 /* Avbryt */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE DIALOG-1
ON CHOOSE OF BTN_FVE IN FRAME DIALOG-1 /* - */
DO: 
   ASSIGN
   FILL-IN_DATUM = INPUT FILL-IN_DATUM.   
   FILL-IN_DATUM = FILL-IN_DATUM - 1.      
   DISPLAY FILL-IN_DATUM WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE DIALOG-1
ON CHOOSE OF BTN_NVE IN FRAME DIALOG-1 /* + */
DO:   
   ASSIGN
   FILL-IN_DATUM = INPUT FILL-IN_DATUM.   
   FILL-IN_DATUM = FILL-IN_DATUM + 1.        
   DISPLAY FILL-IN_DATUM WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_START
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_START DIALOG-1
ON CHOOSE OF BTN_START IN FRAME DIALOG-1 /* Starta */
DO:
   {muswait.i}  
   
   FILL-IN_DATUM = INPUT FILL-IN_DATUM.
   regdatum = FILL-IN_DATUM.   
   FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
   Guru.Konstanter:globforetag = FORETAG.FORETAG.
   IF FORETAG.PROGRAM = "PLUSD" THEN plusdval = TRUE.
                                ELSE plusdval = FALSE.
   RUN REGDAG.P.
   helg = 0.
   IF WEEKDAY(regdatum) = 2 THEN helg = 2.   
   OPEN QUERY pq FOR EACH PERSONALTAB WHERE PERSONALTAB.AKTIV = TRUE  
   USE-INDEX PERSONALKOD NO-LOCK.
   GET FIRST pq NO-LOCK.
   DO WHILE AVAILABLE(PERSONALTAB):
      RUN ejreg_UI.   
      GET NEXT pq NO-LOCK.
   END.
/*   MESSAGE "Vill du köra uppföljningen ?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Uppföljning"
   UPDATE answer AS LOGICAL.
   IF answer THEN DO TRANSACTION:      
      {SUMARIN.I} 
   END.*/
   {musarrow.i}   

   RETURN.    
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_DATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DATUM DIALOG-1
ON LEAVE OF FILL-IN_DATUM IN FRAME DIALOG-1 /* Datum */
DO:
   FILL-IN_DATUM = INPUT FILL-IN_DATUM. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DATUM DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN_DATUM IN FRAME DIALOG-1 /* Datum */
DO:
   ASSIGN
   FILL-IN_DATUM = INPUT FILL-IN_DATUM
   regdatum = INPUT FILL-IN_DATUM.
   RUN AlmanBtn.w.
   FILL-IN_DATUM = regdatum.
   DISPLAY FILL-IN_DATUM WITH FRAME {&FRAME-NAME}.
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
   FILL-IN_DATUM = TODAY - 1.      
   IF Guru.Konstanter:globforetag = "NORD" OR Guru.Konstanter:globforetag = "ETA" OR Guru.Konstanter:globforetag = "ESAN"
      OR Guru.Konstanter:globforetag = "ESMA" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
         CREATE SERVER Guru.Konstanter:apphand.
         IF Guru.Konstanter:globforetag = "NORD" THEN DO:
            Guru.Konstanter:appcon = Guru.Konstanter:apphand:CONNECT("-S appesnord -H elpaso.sydkraft.se -N TCP","ELPAO","KAGGEN","").
         END.
         ELSE IF Guru.Konstanter:globforetag = "ETA" THEN DO:
            Guru.Konstanter:appcon = Guru.Konstanter:apphand:CONNECT("-S appeta -H elpaso.sydkraft.se -N TCP","ELPAO","KAGGEN","").
         END.
         ELSE IF Guru.Konstanter:globforetag = "ESAN" THEN DO:
            Guru.Konstanter:appcon = Guru.Konstanter:apphand:CONNECT("-S appesadm -H elpaso.sydkraft.se -N TCP","ELPAO","KAGGEN","").
         END.
         ELSE IF Guru.Konstanter:globforetag = "ESMA" THEN DO:
            Guru.Konstanter:appcon = Guru.Konstanter:apphand:CONNECT("-S appesmal -H elpaso.sydkraft.se -N TCP","ELPAO","KAGGEN","").
         END.
         ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
            CREATE SERVER Guru.Konstanter:apphand.
            Guru.Konstanter:appcon = Guru.Konstanter:apphand:CONNECT("-S appelpool -H Ntserver1 -N TCP","ELPAO","KAGGEN","").
         END.
         IF NOT Guru.Konstanter:appcon THEN DO:
            MESSAGE 
            "Du fick nu en massa fel meddelanden." Skip
            "Dessa meddelanden innebär att du inte kan starta denna funktion!" skip 
            "Kontakta system ansvarig." 
            VIEW-AS ALERT-BOX.
            musz = TRUE.
         END.
         ASSIGN Guru.Konstanter:globanv = "elpao".
      END.   
   RUN enable_UI.
   {musarrow.i}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-1 _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ejreg_UI DIALOG-1 
PROCEDURE ejreg_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
         
      FRAME DIALOG-1:TITLE = "XAutomatregistrering - " + PERSONALTAB.PERSONALKOD + " " + STRING(TIME,"HH:MM").      
      persrec = RECID(PERSONALTAB).      
      FIND FIRST ANSTFORMTAB WHERE ANSTFORM.ANSTALLNING = PERSONALTAB.ANSTALLNING
      USE-INDEX ANSTF NO-LOCK NO-ERROR.
      FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdatum AND
      OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
      IF NOT AVAILABLE OVERAVTAB THEN DO:
         persrec = persrec.
      END.
      ELSE DO:
         IF OVERAVTAB.EQDAG = 7 OR OVERAVTAB.EQDAG = 1 THEN RETURN.
      END.
      RUN REGVEC.P.                    
      IF musz = TRUE THEN DO:
         musz = FALSE.
         RETURN.
      END.
      RUN SLUTARB.P.
      IF regstart = regslut THEN RETURN.
      regdatum3 = regdatum - helg.      
            FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = PERSONALTAB.OMRADE
            NO-LOCK NO-ERROR.
            IF ANSTFORMTAB.KOD BEGINS "K" THEN DO:              
               ASSIGN anr = "1022" + SUBSTRING(OMRADETAB.ORGIDNUM,1,2).
               dnr = INTEGER(SUBSTRING(OMRADETAB.ORGIDNUM,3,2)).
            END.   
            ELSE DO:
               ASSIGN anr = "109990".
               dnr = 1.
            END.   
            FIND AONRTAB WHERE AONRTAB.AONR = anr AND AONRTAB.DELNR = dnr
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE AONRTAB THEN RETURN. 
            FIND FIRST TIMKOSTNADSTAB WHERE TIMKOSTNADSTAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
            TIMKOSTNADSTAB.PRISTYP = AONRTAB.PRISTYP USE-INDEX PRISPERS NO-LOCK NO-ERROR.
            FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND 
            TIDREGITAB.DATUM = regdatum NO-LOCK NO-ERROR.
            
            IF NOT AVAILABLE TIDREGITAB THEN DO TRANSACTION:                                             
               CREATE tidbuff.
               ASSIGN 
               tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD 
               tidbuff.DATUM = regdatum
               tidbuff.PROGRAM = 'EJAUREG' + STRING(TODAY) + "ELPA"
               tidbuff.VECKONUMMER = regvnr 
               tidbuff.DAG = regdagnamn
               tidbuff.START = regstart 
               tidbuff.SLUT = regslut
               tidbuff.AONR = anr 
               tidbuff.DELNR = dnr 
               tidbuff.TRAKTAMENTE = 0 
               tidbuff.OVERTIDUTTAG = PERSONALTAB.OVERTIDUTTAG 
               tidbuff.UTRYCKNING = FALSE
               tidbuff.PRISTYP = AONRTAB.PRISTYP 
               tidbuff.PRIS = TIMKOSTNADSTAB.PRISA 
               tidbuff.TIDLOG = TRUE.
               ASSIGN
               tidtabrec = RECID(tidbuff)
               plustid = 00.00
               plusaonr = tidbuff.AONR
               plusdnr = tidbuff.DELNR
               nytid = regstart.
               RUN TIMSEK.P.          
               ASSIGN
               regstartsek = sekunder
               nytid = regslut.
               RUN TIMSEK.P.         
               ASSIGN
               regslutsek = sekunder
               plustidrec = RECID(tidbuff).
               RUN TOTTID.P.
               ASSIGN tidbuff.TOTALT = nytid.              
               RELEASE TIDREGITAB NO-ERROR.
               RELEASE tidbuff NO-ERROR.
               FOR EACH tidapptemp:
                  DELETE tidapptemp.
               END.
            END.     
            RELEASE TIDREGITAB NO-ERROR.
            RELEASE tidbuff NO-ERROR.  
            CREATE tidapptemp.
            ASSIGN
            tidapptemp.FORETAG = globforetag
            tidapptemp.ANVANDARE = "ELPA"
            tidapptemp.RECPERS = persrec
            tidapptemp.RECTID = tidtabrec
            tidapptemp.DATUM = regdatum.                     
            IF Guru.Konstanter:appcon THEN DO:                           
               RUN TIDUPPDE.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
              (INPUT TABLE tidapptemp).
            END.
            ELSE DO:
              RUN TIDUPPDE.P 
              (INPUT TABLE tidapptemp).                  
            END.
            RUN FELTEXT.P.
            musz = FALSE.             
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-1 _DEFAULT-ENABLE
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
  DISPLAY FILL-IN_DATUM 
      WITH FRAME DIALOG-1.
  ENABLE RECT-41 BTN_NVE FILL-IN_DATUM BTN_FVE BTN_START BTN_AVSL 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nastatid_UI DIALOG-1 
PROCEDURE nastatid_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND PREV TIDREGITAB WHERE
   TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND   
   TIDREGITAB.DATUM < regdatum3 AND TIDREGITAB.TIDLOG = TRUE AND
   TIDREGITAB.OKOD1 = "" AND TIDREGITAB.PRISTYP NE "RESTID..." 
/*   TIDREGITAB.START GE regstart AND TIDREGITAB.SLUT LE regslut */
   USE-INDEX PSTART NO-LOCK NO-ERROR.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


