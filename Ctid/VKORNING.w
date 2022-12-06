&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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
/*          This .W file was created with the Progress AppBuilder.      */
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
&Scoped-define SHARED 
{LONEDEF.I}
&Scoped-define NEW   
&Scoped-define SHARED SHARED
{GLOBVAR2DEL1.I}
{REGVAR.I}
{SOKDEF.I}

{STANSLONEDEF.I}
&SCOPED-DEFINE NEW NEW
&SCOPED-DEFINE SHARED SHARED
{BLOB.I}
DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
DEFINE VARIABLE blobproch AS HANDLE NO-UNDO.


DEFINE NEW SHARED VARIABLE korlage AS CHARACTER FORMAT "X(50)" NO-UNDO.
DEFINE NEW SHARED VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE man AS INTEGER FORMAT "99" NO-UNDO.
DEFINE NEW SHARED VARIABLE tperiod AS INTEGER FORMAT "999" NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE outanvanv AS CHARACTER NO-UNDO.
DEFINE VARIABLE outdatornamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE vknummer AS CHARACTER FORMAT "X(4)" NO-UNDO.
DEFINE VARIABLE manval AS LOGICAL NO-UNDO.
DEFINE VARIABLE vtidrec AS RECID.
DEFINE VARIABLE veckrec AS RECID.
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(41)" NO-UNDO.
DEFINE VARIABLE prognamn2 AS CHARACTER FORMAT "X(41)" NO-UNDO.
DEFINE VARIABLE samvar AS CHARACTER NO-UNDO.

/*NYTT STANS*/
DEFINE VARIABLE fnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE fnamn2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommando2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sparmapp AS CHARACTER NO-UNDO.
DEFINE VARIABLE radrakn AS INTEGER NO-UNDO.
DEFINE VARIABLE iColumn                 AS INTEGER INITIAL 0.
DEFINE VARIABLE cColumn                 AS CHARACTER.
DEFINE VARIABLE cRange                  AS CHARACTER.
DEFINE VARIABLE chExcelApplication      AS COM-HANDLE.
DEFINE VARIABLE chWorkbook              AS COM-HANDLE.
DEFINE VARIABLE chWorksheet             AS COM-HANDLE.
DEFINE VARIABLE chChart                 AS COM-HANDLE.
DEFINE VARIABLE chWorksheetRange        AS COM-HANDLE.
DEFINE VARIABLE felexcel AS LOGICAL NO-UNDO.
DEFINE VARIABLE tider AS CHARACTER NO-UNDO.
DEFINE VARIABLE traff AS LOGICAL NO-UNDO.


{TIDUTTTNEW.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE FILL-IN-VECK AS CHARACTER FORMAT "X(41)":U 
     VIEW-AS FILL-IN 
     SIZE 51.5 BY 4.5
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-VV AS CHARACTER FORMAT "X(4)":U 
     LABEL "Ange kontrollord för ekonomi- och lönesammanställning" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FILL-IN-VECK AT ROW 1.75 COL 1.5 NO-LABEL
     FILL-IN-VV AT ROW 6.79 COL 55.13 COLON-ALIGNED AUTO-RETURN 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 79.88 BY 21.23.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Ekonomi- och lönesammanställning"
         HEIGHT             = 7.63
         WIDTH              = 64.63
         MAX-HEIGHT         = 21.25
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 21.25
         VIRTUAL-WIDTH      = 80
         RESIZE             = yes
         SCROLL-BARS        = no
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
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN FILL-IN-VECK IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       FILL-IN-VECK:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-VV IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-VV:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Ekonomi- och lönesammanställning */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Ekonomi- och lönesammanställning */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {WIN_M_START.I}
   {muswait.i}     
   
   RUN enable_UI.
   {FRMSIZE.I}      
   regdatum = TODAY.
   RUN REGVEC.P.      
   IF YEAR(TODAY) < 2000 THEN vknummer = "V" + STRING(regvnr, "999").
   ELSE vknummer = "w" + STRING(TODAY,"99999999").
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 77.
   {SOKANROP.I}      
   {SOKSTART.I}
   IF soktemp.SOKCHAR[1]  = "UTBI" THEN musz = TRUE. 
   IF musz = FALSE THEN DO:
      ASSIGN
      soktemp.SOKVAL = 6
      soktemp.SOKCHAR[1] = vknummer.
      {SOKANROP.I}      
      IF soktemp.SOKINT[1]  = 0 THEN musz = musz. 
      ELSE DO:
         musz = TRUE.
         MESSAGE soktemp.SOKCHAR[1] VIEW-AS ALERT-BOX.                     
      END.
   END.
   IF musz = FALSE THEN DO:
     RUN INLOAPI.P (OUTPUT outanvanv, OUTPUT outdatornamn).
      
      IF TRIM(outdatornamn) = "acer" THEN.
      /*CCC TA BORT!!!!*/
      ELSE IF Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79)  THEN. 
      ELSE IF NOT Guru.Konstanter:appcon THEN DO:
         {APCONFEL.I}               
         musz = TRUE.            
      END.
      
   END.
   IF musz = FALSE THEN DO:
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 7.
      {SOKANROP.I}            
      IF soktemp.SOKINT[1]  = 1 THEN DO:
         MESSAGE soktemp.SOKCHAR[1]
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val AS LOGICAL.
         CASE val:
            WHEN TRUE THEN DO:
               musz = FALSE.              
            END.
            WHEN FALSE THEN musz = TRUE.
         END CASE. 
      END.
         
   END.   
   IF musz = FALSE THEN DO:
      IF gvisatidpermanad = TRUE THEN RUN VKORNDAT.W.
      {WIN_M_SLUT.I}
      IF musz = FALSE THEN DO:
         {SOKSTART.I}
         ASSIGN
         soktemp.SOKVAL = 8
         soktemp.SOKINT[1] = 1.
         {SOKANROP.I} 
         IF soktemp.SOKINT[1] = 1 THEN DO:
            MESSAGE soktemp.SOKCHAR[1]
            VIEW-AS ALERT-BOX.                          
            musz = TRUE.
         END.                  
      END.
      IF musz = TRUE THEN musz = FALSE.
      ELSE DO:
         RUN INLOAPI.P (OUTPUT outanvanv, OUTPUT outdatornamn).
         {SOKSTART.I}
         ASSIGN
         soktemp.SOKVAL = 9
         soktemp.SOKCHAR[1] = Guru.Konstanter:globanv + " " + outanvanv + " " + outdatornamn.
         {SOKANROP.I} 
         IF WEEKDAY(TODAY) = 1 THEN regdatum = TODAY - 7.
         IF WEEKDAY(TODAY) = 2 THEN regdatum = TODAY - 1.
         IF WEEKDAY(TODAY) = 3 THEN regdatum = TODAY - 2.
         IF WEEKDAY(TODAY) = 4 THEN regdatum = TODAY - 3.
         IF WEEKDAY(TODAY) = 5 THEN regdatum = TODAY - 4.
         IF WEEKDAY(TODAY) = 6 THEN regdatum = TODAY - 5.
         IF WEEKDAY(TODAY) = 7 THEN regdatum = TODAY - 6.            
         FILL-IN-VECK = "EKO.- OCH LÖNESAMMANST. ÄR STARTAD " + STRING(TIME,"HH:MM").
         DISPLAY FILL-IN-VECK WITH FRAME {&FRAME-NAME}.
         FILL-IN-VECK:HIDDEN = FALSE. 
         RUN vecko_UI.
         MESSAGE "Eko.- och lönesammanst. är färdig " + STRING(TIME,"HH:MM") 
         VIEW-AS ALERT-BOX.       
      END.
   END.          
   musz = FALSE.
   status-musdia = SESSION:SET-WAIT-STATE("").
   IF status-musdia = status-musdia THEN LEAVE MAIN-BLOCK.
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mallut_UI C-Win 
PROCEDURE mallut_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER bolag AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER sparmapp AS CHARACTER NO-UNDO.
DEFINE VARIABLE ServerFilNamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE  stansfillong AS MEMPTR NO-UNDO.
   IF Guru.Konstanter:appcon THEN RUN FINNSTABELL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT "BLOBINFO", OUTPUT bloblog).
   ELSE RUN FINNSTABELL.P (INPUT "BLOBINFO", OUTPUT bloblog).
   IF bloblog = TRUE THEN DO:
      
      DEFINE VARIABLE resid AS INTEGER NO-UNDO.
      IF NOT VALID-HANDLE(blobproch) THEN DO:
         {FINNSDYNBLOB.I}
      END. 
      RUN blobfil_UI IN blobproch (INPUT fnamn, OUTPUT resid).
      IF resid = ? THEN DO:
         kommando = SEARCH(fnamn).      
      END.
      ELSE DO:
         FIND FIRST blobinfotemp WHERE blobinfotemp.ID = resid NO-LOCK NO-ERROR.
         RUN blobopen_UI IN blobproch (INPUT blobinfotemp.FILNAMN, OUTPUT kommando).      
      END.
      
   END.
   ELSE kommando = SEARCH(fnamn).     
   IF kommando = ? THEN DO:
      MESSAGE "Hittade inte " fnamn VIEW-AS ALERT-BOX.
      RETURN.       
   END.  
   kommando2 = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
   sparmapp = kommando2. 
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN kommando2 = webclienttempdir.
   OS-CREATE-DIR VALUE(kommando2) NO-ERROR.
           
   kommando2 = kommando2 + fnamn2.   
   OS-COPY VALUE(kommando) VALUE(kommando2).   
   kommando = kommando2.

   
   Guru.Konstanter:AmericanSet().
   
   CREATE "Excel.Application" chExcelApplication.
   chExcelApplication:Visible = FALSE NO-ERROR.   
   
   {OPENEXCEL.I}
   chWorkbook = chExcelApplication:Workbooks:OPEN(kommando)  NO-ERROR.
   chWorkSheet = chExcelApplication:Sheets:Item(1)  NO-ERROR.        
   radrakn = 1.         
   FOR EACH perskoll WHERE perskoll.VIJUDID = bolag :
      traff = FALSE.
      FOR EACH stanslonefil WHERE stanslonefil.PPERSONNUMMER = perskoll.PNR  USE-INDEX PPERSONNUMMER2 NO-LOCK:         
         IF stanslonefil.HEROSORT = "TILLÄGG" THEN DO:
            traff =  TRUE.
            radrakn = radrakn + 1.
            ASSIGN                        
            iColumn = radrakn            
            cColumn = STRING(iColumn).
            cRange = "A" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.PPERSONNUMMER).
            cRange = "B" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT perskoll.ANSTNR).
            cRange = "C" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT perskoll.PERSONALKOD).
            cRange = "D" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT (perskoll.FORNAMN + " " + perskoll.EFTERNAMN )).
            cRange = "E" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT perskoll.OMRADE).
            cRange = "F" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.PLONTILLAGG).
            
            cRange = "G" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.KLARTEXT).                 
            cRange = "H" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.PLONTILLANTAL).
            cRange = "I" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.PDATUM).
            cRange = "J" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.PDATUM).
            cRange = "Q" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.RESMAL).
         END.
         IF stanslonefil.HEROSORT = "RESA" THEN DO:
            traff =  TRUE.
            radrakn = radrakn + 1.
            ASSIGN                        
            iColumn = radrakn            
            cColumn = STRING(iColumn).
            cRange = "A" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.PPERSONNUMMER).
            cRange = "B" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT perskoll.ANSTNR).
            cRange = "C" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT perskoll.PERSONALKOD).
            cRange = "D" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT (perskoll.FORNAMN + " " + perskoll.EFTERNAMN )).
            cRange = "E" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT perskoll.OMRADE).               
            cRange = "I" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.PDATUM).
            cRange = "J" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.PDATUM).
            cRange = "Q" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.RESMAL).
         END.   
         IF stanslonefil.HEROSORT = "ÖVERTID" THEN DO:
            traff =  TRUE.
            radrakn = radrakn + 1.
            ASSIGN                        
            iColumn = radrakn            
            cColumn = STRING(iColumn).
            cRange = "A" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.PPERSONNUMMER).
            cRange = "B" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT perskoll.ANSTNR).
            cRange = "C" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT perskoll.PERSONALKOD).
            cRange = "D" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT (perskoll.FORNAMN + " " + perskoll.EFTERNAMN )).
            cRange = "E" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT perskoll.OMRADE).
            cRange = "G" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT "ÖVERTID").
                                    
            cRange = "I" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.PDATUM).
            cRange = "J" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.PDATUM).
            cRange = "K" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.START).
            cRange = "L" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.SLUT).
            cRange = "M" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.OVERTIDUTTAG).
            cRange = "N" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.TOTALT).
            cRange = "Q" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.RESMAL).
            
         END.
         IF stanslonefil.HEROSORT = "TIMTID" THEN DO:
            traff =  TRUE.
            radrakn = radrakn + 1.
            ASSIGN                        
            iColumn = radrakn            
            cColumn = STRING(iColumn).
            cRange = "A" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.PPERSONNUMMER).
            cRange = "B" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT perskoll.ANSTNR).
            cRange = "C" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT perskoll.PERSONALKOD).
            cRange = "D" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT (perskoll.FORNAMN + " " + perskoll.EFTERNAMN )).
            cRange = "E" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT perskoll.OMRADE).            
            cRange = "G" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT "TIMTID").                                    
            cRange = "I" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.PDATUM).
            cRange = "J" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.PDATUM).
            cRange = "K" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.START).
            cRange = "L" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.SLUT).            
            cRange = "N" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.TOTALT).
         END.
         IF stanslonefil.HEROSORT = "RESTID" THEN DO:
            traff =  TRUE.
            radrakn = radrakn + 1.
            ASSIGN                        
            iColumn = radrakn            
            cColumn = STRING(iColumn).
            cRange = "A" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.PPERSONNUMMER).
            cRange = "B" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT perskoll.ANSTNR).
            cRange = "C" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT perskoll.PERSONALKOD).
            cRange = "D" + cColumn.                        
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT (perskoll.FORNAMN + " " + perskoll.EFTERNAMN )).
            cRange = "E" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT perskoll.OMRADE).            
            cRange = "G" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT "RESTID").                                    
            cRange = "I" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.PDATUM).
            cRange = "J" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.PDATUM).
            cRange = "K" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.START).
            cRange = "L" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.SLUT).            
            cRange = "N" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.TOTALT).
            cRange = "Q" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.RESMAL).
         END.
         IF stanslonefil.HEROSORT = "BEREDSKAP" THEN DO:
            traff =  TRUE.
            radrakn = radrakn + 1.
            ASSIGN                        
            iColumn = radrakn            
            cColumn = STRING(iColumn).
            cRange = "A" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.PPERSONNUMMER).
            cRange = "B" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT perskoll.ANSTNR).
            cRange = "C" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT perskoll.PERSONALKOD).
            cRange = "D" + cColumn.                        
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT (perskoll.FORNAMN + " " + perskoll.EFTERNAMN )).
            cRange = "E" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT perskoll.OMRADE).            
            cRange = "G" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT "BEREDSKAP").                                    
            cRange = "I" + cColumn.                        
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.STARTDATUM).
            cRange = "J" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.SLUTDATUM).
            cRange = "K" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.BERSTART).
            cRange = "L" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.BERSLUT).            
            cRange = "Q" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.RESMAL).
         END.
         IF stanslonefil.HEROSORT = "LÖNEAVDRAG" THEN DO:
            /*MINDRE ÄN -10 TIM PÅ FLEXEN*/
            traff =  TRUE.
            radrakn = radrakn + 1.
            ASSIGN                        
            iColumn = radrakn            
            cColumn = STRING(iColumn).
            cRange = "A" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.PPERSONNUMMER).
            cRange = "B" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT perskoll.ANSTNR).
            cRange = "C" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT perskoll.PERSONALKOD).
            cRange = "D" + cColumn.                        
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT (perskoll.FORNAMN + " " + perskoll.EFTERNAMN )).
            cRange = "E" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT perskoll.OMRADE).            
            cRange = "G" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT "LÖNEAVDRAG FLEX").
            
            cRange = "H" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.PLONTILLANTAL).
            cRange = "I" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.PDATUM).
            cRange = "J" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT stanslonefil.PDATUM).            
         END.
         
      END.
      FOR EACH franvarotemp WHERE SUBSTRING(franvarotemp.pnr,3) = perskoll.PNR  NO-LOCK:
         traff =  TRUE.
         radrakn = radrakn + 1.         
         ASSIGN                        
         iColumn = radrakn            
         cColumn = STRING(iColumn).
         cRange = "A" + cColumn.            
         RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT perskoll.PNR).
         cRange = "B" + cColumn.            
         RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT perskoll.ANSTNR).
         cRange = "C" + cColumn.            
         RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT perskoll.PERSONALKOD).
         cRange = "D" + cColumn.            
         RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT (perskoll.FORNAMN + " " + perskoll.EFTERNAMN )).
         cRange = "E" + cColumn.            
         RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT perskoll.OMRADE).            
         cRange = "G" + cColumn.            
         RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT franvarotemp.ORT).            
         cRange = "I" + cColumn.            
         RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT franvarotemp.FRAN).
         cRange = "J" + cColumn.            
         RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT franvarotemp.TILL).
         IF franvarotemp.TOTTID > 0 THEN DO:
            cRange = "O" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT franvarotemp.TOTTID).
         END.
         ELSE IF franvarotemp.TTID > 0 THEN DO:
            cRange = "K" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT franvarotemp.START).
            cRange = "L" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT franvarotemp.SLUT).
            cRange = "N" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT franvarotemp.TIMMAR).
         END.   
         ELSE DO:
           /* INGET SKA UT? 
           cRange = "J" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT franvarotemp.START).
            cRange = "K" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT franvarotemp.SLUT).
            cRange = "M" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT franvarotemp.TTID).
            cRange = "N" + cColumn.            
            RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT franvarotemp.TOTTID).*/
    
         END.   
         cRange = "Q" + cColumn.            
         RUN utexcel_UI (INPUT "ARIAL",INPUT 12,INPUT franvarotemp.KOMMENTAR ).
      END.
      IF traff = TRUE THEN DO:
         ASSIGN
         traff = FALSE
         radrakn = radrakn + 1.
      END.   
   END.   
   
          
   Guru.Konstanter:EuropeanAmericanReset().
   
   chExcelApplication:displayalerts = FALSE.   
   
   chWorkbook:SaveAs(kommando,,,,,,,,,).        
   chExcelApplication:Visible = TRUE   NO-ERROR.    
   
   chExcelApplication:displayalerts = TRUE.   
   RELEASE OBJECT chWorksheetRange NO-ERROR.                
   RELEASE OBJECT chWorksheet NO-ERROR. 
   /*  Den som gör körningen vill ha upp filerna på skärmen*/
   /* visa dem direkt på skärm men spara dem också
   NO-RETURN-VALUE chWorkbook:CLOSE() no-error.
   NO-RETURN-VALUE chExcelApplication:QUIT().*/
   RELEASE OBJECT chExcelApplication NO-ERROR.      
   RELEASE OBJECT chWorkbook NO-ERROR.
   RELEASE OBJECT chWorksheet NO-ERROR.
       
   ServerFilNamn = "D:\DELAD\PRO10S\BACKEXPORT\lonelnat\" + fnamn2.                          
 
    
    
   COPY-LOB FROM FILE kommando TO stansfillong. 
   IF NOT VALID-HANDLE(blobproch) THEN DO:
      {FINNSDYNBLOB.I}
   END. 
   RUN BlobFilTillServer_UI IN blobproch (INPUT ServerFilNamn, INPUT stansfillong).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE utexcel_UI C-Win 
PROCEDURE utexcel_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER typsnitt AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER storlek AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER varde AS CHARACTER NO-UNDO.
   IF varde = "" THEN RETURN.
   IF varde = ? THEN RETURN.
   ASSIGN
   chWorkSheet:Range(cRange):Font:NAME = typsnitt NO-ERROR.
   chWorkSheet:Range(cRange):Font:SIZE = storlek  NO-ERROR.
   chWorkSheet:Range(cRange):Value = varde NO-ERROR.  
   {EXCELFEL.I}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vecko_UI C-Win 
PROCEDURE vecko_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}   
   /* VKORNING.P*/               
   IF Guru.Konstanter:globforetag = "GKAL" THEN samvar = "\\goliat\DELAD\server\pro9s\KALESAMM.TXT".   
   IF Guru.Konstanter:globforetag = "LULE" THEN samvar = "D:\elpool\DELAD\PRO9s\EXPORT\LON\LULESAMM.TXT".
   IF Guru.Konstanter:globforetag = "SUND" THEN samvar = "D:\DELAD\SERVER\PRO10S\SULESAMM.TXT".
   IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
      /*SNATBERGET*/
      samvar = "D:\DELAD\SERVER\PRO10S\SNATLESAMM.TXT".
      samvar = REPLACE(samvar,"D:\DELAD\SERVER\PRO10S\","D:\DELAD\PRO10S\").
         
   END.     
   IF Guru.Konstanter:globforetag = "MISV" THEN DO:
      samvar = "C:\elpool\delad\pro10s\MILESAMM.TXT".
      IF Guru.Konstanter:AppSpringSet[1] = "misvstb" THEN samvar = "D:\elpool\delad\pro10s\MILESAMM.TXT".
   END.   
   IF Guru.Konstanter:globforetag = "GRAN" THEN samvar = "\\granguru\guru_ser\server\pro9s\GRLESAMM.TXT".
   IF Guru.Konstanter:globforetag = "elpa" THEN samvar = "\\pc122\delad\pro9s\elLESAMM.TXT".     
   IF Guru.Konstanter:appcon = TRUE THEN DO:
      RUN LESAMMAN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT 1,INPUT samvar,OUTPUT TABLE tidut).
   END.
   ELSE DO:
      RUN LESAMMAN.P (INPUT 1,INPUT samvar,OUTPUT TABLE tidut).
   END.
   IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:                  
      manval = FALSE.               
      /* 20171205 sista körning till lön. Ta bort schema- tillägg- frånvaro -filer. Även flex ska bort
      FILL-IN-VECK = "Nu startar arbetsschema-sammanställningen " + STRING(TIME,"HH:MM").  
      DISPLAY FILL-IN-VECK VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
      PAUSE 0.
      RUN GKSCHFLEX.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT samvar,INPUT vkdatum, INPUT Guru.Konstanter:globanv, INPUT "" ).                          
      FILL-IN-VECK = "Nu startar lönesammanställning " + STRING(TIME,"HH:MM").  
      DISPLAY FILL-IN-VECK VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
      PAUSE 0.      
      RUN GKLOFLEX.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT samvar,INPUT vkdatum, INPUT Guru.Konstanter:globanv, INPUT "").
      FILL-IN-VECK = "Nu startar frånvarosammanställning " + STRING(TIME,"HH:MM").
      DISPLAY  FILL-IN-VECK VIEW-AS TEXT  WITH FRAME {&FRAME-NAME} NO-LABELS.
      PAUSE 0.     
      RUN GKFRFLEX.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT samvar,INPUT vkdatum, INPUT Guru.Konstanter:globanv,INPUT "").*/
      /*låt flexen köras tills kalmar säger något annat 20180208 Lena*/
      FILL-IN-VECK = "Nu startar flexsammanställning " + STRING(TIME,"HH:MM").
      DISPLAY  FILL-IN-VECK VIEW-AS TEXT  WITH FRAME {&FRAME-NAME} NO-LABELS.
      PAUSE 0.
      RUN GKALFL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT samvar,INPUT vkdatum, INPUT Guru.Konstanter:globanv).         
      FILL-IN-VECK = "Nu startar ekonomiutläsningen " + STRING(TIME,"HH:MM").
      DISPLAY  FILL-IN-VECK VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
      PAUSE 0.
      RUN GKALEKOT.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      /*     KÖRLOG       TIDFELAR    TID TOM       SKARP  VECKOKORD*/
      (INPUT samvar,INPUT FALSE,INPUT vkdatum, INPUT TRUE,INPUT "").     
      
      FILL-IN-VECK = "Nu startar rättningssammanställningen " + STRING(TIME,"HH:MM").
      DISPLAY  FILL-IN-VECK VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
      PAUSE 0.      
      RUN GKALEKOT.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT samvar,INPUT TRUE,INPUT vkdatum, INPUT TRUE,INPUT "").   
   END.               
   
   IF Guru.Konstanter:globforetag = 'GRAN' OR Guru.Konstanter:globforetag = 'GADM'  OR Guru.Konstanter:globforetag = "ccGKAL" OR Guru.Konstanter:globforetag = "celpa"
     THEN DO:      
      manval = FALSE.    
      IF gvisatidpermanad = TRUE THEN DO:
         IF MONTH(vkdatum) NE MONTH(vkdatum + 1) THEN DO:            
            MESSAGE "Vill du starta skapa lönefil direkt efter ekonomi- och lönesammanställningen ?" SKIP      
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE valg AS LOGICAL.
            CASE valg:
               WHEN TRUE THEN DO:
                  manval = TRUE.              
               END.
               WHEN FALSE THEN manval = FALSE.
            END CASE.
         END.   
      END.      
      IF Guru.Konstanter:globforetag = 'GRAN' OR Guru.Konstanter:globforetag = 'GADM'  OR Guru.Konstanter:globforetag = "cGKAL" OR Guru.Konstanter:globforetag = "Celpa"
        THEN DO:      
         FILL-IN-VECK = "Nu startar lönesammanställning " + STRING(TIME,"HH:MM").  
         DISPLAY FILL-IN-VECK VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
         PAUSE 0.
         RUN GRANVE1.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT samvar,INPUT vkdatum, INPUT gvisatidpermanad, INPUT Guru.Konstanter:globforetag).     
         FILL-IN-VECK = "Nu startar frånvarosammanställning " + STRING(TIME,"HH:MM").
         DISPLAY  FILL-IN-VECK VIEW-AS TEXT  WITH FRAME {&FRAME-NAME} NO-LABELS.
         PAUSE 0.          
         RUN GRANFR1.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT samvar,INPUT vkdatum, INPUT gvisatidpermanad, INPUT Guru.Konstanter:globforetag).
      END.
      
      IF Guru.Konstanter:globforetag = "ccGKAL" THEN DO:
         RUN GKALEKOT.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         /*     KÖRLOG       TIDFELAR    TID TOM       SKARP  VECKOKORD*/
         (INPUT samvar,INPUT FALSE,INPUT vkdatum, INPUT TRUE,INPUT "").     
         FILL-IN-VECK = "Nu startar ekonomiutläsningen " + STRING(TIME,"HH:MM").
         DISPLAY  FILL-IN-VECK VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
         PAUSE 0.
         FILL-IN-VECK = "Nu startar rättningssammanställningen " + STRING(TIME,"HH:MM").
         DISPLAY  FILL-IN-VECK VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
         PAUSE 0.         
         RUN GKALEKOT.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT samvar,INPUT TRUE,INPUT vkdatum, INPUT TRUE,INPUT "").     
      END.
      
   END. 
   /*IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "celpa"  THEN DO:            
      IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
         /*    luleå kör ekonomikörning i autolule.p , men innan
          lönekörning görs måste ytterligare en ekonomikörning göras för
               att skicka det som har godkänts sedan igår kväll*/
         FILL-IN-VECK = "Nu startar ekonomisammanställning " + STRING(TIME,"HH:MM").
         DISPLAY  FILL-IN-VECK VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
         PAUSE 0.                  
         RUN LULEEKOSTART.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT samvar,INPUT TODAY, INPUT TRUE,INPUT FALSE,INPUT "").                  
         
         
         FILL-IN-VECK = "Nu startar lönesammanställning " + STRING(TIME,"HH:MM").  
         DISPLAY FILL-IN-VECK VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
         PAUSE 0.
         RUN LULEVE1.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT samvar,INPUT vkdatum, INPUT gvisatidpermanad, INPUT Guru.Konstanter:globforetag,INPUT "" ).     
         FILL-IN-VECK = "Nu startar frånvarosammanställning " + STRING(TIME,"HH:MM").
         DISPLAY  FILL-IN-VECK VIEW-AS TEXT  WITH FRAME {&FRAME-NAME} NO-LABELS.
         PAUSE 0.          
         RUN LULEFR1.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT samvar,INPUT vkdatum, INPUT gvisatidpermanad, INPUT Guru.Konstanter:globforetag,INPUT "").
         
         FILL-IN-VECK = "Nu skapas lönefilen " + STRING(TIME,"HH:MM").
         DISPLAY FILL-IN-VECK VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
         PAUSE 0.
         RUN LULEMAN.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT samvar,INPUT vkdatum, INPUT gvisatidpermanad, INPUT Guru.Konstanter:globforetag,INPUT man ,INPUT "").           
         RUN LULEFR2.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT samvar,INPUT vkdatum, INPUT gvisatidpermanad, INPUT Guru.Konstanter:globforetag,INPUT man,INPUT "").           
      END.                         
      IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "celpa" THEN DO:
         /*flextid*/
         RUN LULEFL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT samvar,INPUT vkdatum, INPUT Guru.Konstanter:globanv).
      END.
   END.*/    
   
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "gamSNAT" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:                  
      manval = FALSE.         
      FILL-IN-VECK = "Nu startar arbetsschema-sammanställningen " + STRING(TIME,"HH:MM").  
      DISPLAY FILL-IN-VECK VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
      PAUSE 0.
      RUN SUSCHHOGIA.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT samvar,INPUT vkdatum, INPUT Guru.Konstanter:globanv, INPUT "" ).           
      FILL-IN-VECK = "Nu startar flexkörning " + STRING(TIME,"HH:MM").
      RUN FLXMAN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT samvar,INPUT vkdatum).             
      FILL-IN-VECK = "Nu startar lönesammanställning " + STRING(TIME,"HH:MM").  
      DISPLAY FILL-IN-VECK VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
      PAUSE 0.      
      RUN SULOHOGIA.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT samvar,INPUT vkdatum, INPUT Guru.Konstanter:globanv, INPUT "").
      FILL-IN-VECK = "Nu startar frånvarosammanställning " + STRING(TIME,"HH:MM").
      DISPLAY  FILL-IN-VECK VIEW-AS TEXT  WITH FRAME {&FRAME-NAME} NO-LABELS.
      PAUSE 0.     
      RUN SUFRHOGIA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT samvar,INPUT vkdatum, INPUT Guru.Konstanter:globanv,INPUT "").     
      FILL-IN-VECK = "Nu startar ekonomisammanställning " + STRING(TIME,"HH:MM").
      DISPLAY  FILL-IN-VECK VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
      PAUSE 0.            
      RUN SUNDEKO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT samvar,INPUT vkdatum,INPUT "").                                                                                    
      FILL-IN-VECK = "Nu startar rättningssammanställning " + STRING(TIME,"HH:MM").  
      DISPLAY FILL-IN-VECK VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
      PAUSE 0.      
      RUN SUFEEKO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT samvar,INPUT vkdatum,INPUT "").    
      IF Guru.Konstanter:globforetag = "SUND" THEN DO:
         FILL-IN-VECK = "Nu startar överläsning till Insikt " + STRING(TIME,"HH:MM").
         DISPLAY  FILL-IN-VECK VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
         PAUSE 0.            
         RUN SUNDINSIKTTID.p ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
                
      END.
      
      IF Guru.Konstanter:globforetag = "SNAT"  THEN DO:                
         FILL-IN-VECK = "Nu startar kompsaldosammanställning " + STRING(TIME,"HH:MM").  
         DISPLAY FILL-IN-VECK VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
         PAUSE 0.      
         RUN KOMPSMAN.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT samvar,INPUT vkdatum, INPUT Guru.Konstanter:globanv, INPUT "").
      END.   
   END.
   
   IF Guru.Konstanter:globforetag = "SNAT"  THEN DO:                  
      manval = FALSE.                          
      FILL-IN-VECK = "Nu startar flexkörning " + STRING(TIME,"HH:MM").
      
      RUN FLXMAN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT samvar,INPUT vkdatum).
                   
      FILL-IN-VECK = "Nu startar lönesammanställning " + STRING(TIME,"HH:MM").  
      DISPLAY FILL-IN-VECK VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
      PAUSE 0.   
      
      RUN SULOHOGIASTA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT   
      (INPUT samvar,INPUT vkdatum, INPUT Guru.Konstanter:globanv, INPUT "", OUTPUT TABLE stanslonefil ).
      
      FILL-IN-VECK = "Nu startar frånvarosammanställning " + STRING(TIME,"HH:MM").
      DISPLAY  FILL-IN-VECK VIEW-AS TEXT  WITH FRAME {&FRAME-NAME} NO-LABELS.
      PAUSE 0.     
             
      RUN SUFRHOGIASTA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT  
      (INPUT samvar,INPUT vkdatum, INPUT Guru.Konstanter:globanv,INPUT "", INPUT-OUTPUT TABLE stanslonefil, OUTPUT TABLE perskoll, OUTPUT TABLE franvarotemp).      
      tider = REPLACE(STRING(TIME,"HH:MM"),":","").
      fnamn = "MallStanstid.XLSX".
      fnamn2 = "MallStanstidElnat" + STRING(TODAY,"99999999") + tider +  ".XLSX".
      RUN mallut_UI (INPUT "ELNÄT", OUTPUT sparmapp).
      
      tider = REPLACE(STRING(TIME,"HH:MM"),":","").
      fnamn = "MallStanstid.XLSX".
      fnamn2 = "MallStanstidServaNet" + STRING(TODAY,"99999999") + tider +  ".XLSX".      
      RUN mallut_UI (INPUT "ServaNet", OUTPUT sparmapp ).
      
      FILL-IN-VECK = "De skapade Excelfierna ligger nu i mappen: " +  sparmapp + "  De sparas även på servern. " + STRING(TIME,"HH:MM").
      DISPLAY  FILL-IN-VECK VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
      PAUSE 2.           
      FILL-IN-VECK = "Nu startar ekonomisammanställning " + STRING(TIME,"HH:MM").
      DISPLAY  FILL-IN-VECK VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
      PAUSE 0.                  
      RUN SUNDEKO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT samvar,INPUT vkdatum,INPUT "").                                                                                    
      FILL-IN-VECK = "Nu startar rättningssammanställning " + STRING(TIME,"HH:MM").  
      DISPLAY FILL-IN-VECK VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
      PAUSE 0.            
      RUN SUFEEKO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT samvar,INPUT vkdatum,INPUT "").    
                           
      FILL-IN-VECK = "Nu startar kompsaldosammanställning " + STRING(TIME,"HH:MM").  
      DISPLAY FILL-IN-VECK VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
      PAUSE 0.            
      RUN KOMPSMAN.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT samvar,INPUT vkdatum, INPUT Guru.Konstanter:globanv, INPUT "").
      
   END. 
   IF Guru.Konstanter:globforetag = "XMISV" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:                  
      manval = FALSE.         
      FILL-IN-VECK = "Nu startar arbetsschema-sammanställningen " + STRING(TIME,"HH:MM").  
      DISPLAY FILL-IN-VECK VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
      PAUSE 0.
      RUN SUSCHHOGIA.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT samvar,INPUT vkdatum, INPUT Guru.Konstanter:globanv, INPUT "" ).           
      FILL-IN-VECK = "Nu startar flexkörning " + STRING(TIME,"HH:MM").
      RUN FLXMAN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT samvar,INPUT vkdatum).             
      FILL-IN-VECK = "Nu startar lönesammanställning " + STRING(TIME,"HH:MM").  
      DISPLAY FILL-IN-VECK VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
      PAUSE 0.
      RUN SULOHOGIA.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT samvar,INPUT vkdatum, INPUT Guru.Konstanter:globanv, INPUT "").
      FILL-IN-VECK = "Nu startar frånvarosammanställning " + STRING(TIME,"HH:MM").
      DISPLAY  FILL-IN-VECK VIEW-AS TEXT  WITH FRAME {&FRAME-NAME} NO-LABELS.
      PAUSE 0.     
      RUN SUFRHOGIA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT samvar,INPUT vkdatum, INPUT Guru.Konstanter:globanv,INPUT "").                      
      FILL-IN-VECK = "Nu startar ekonomisammanställning " + STRING(TIME,"HH:MM").
      DISPLAY  FILL-IN-VECK VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
      PAUSE 0.      
      IF Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:
         RUN MISVEKON.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT samvar,INPUT vkdatum,INPUT "").                                                              
      END.                           
   END.
   IF Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:
      manval = FALSE.
      IF MONTH(vkdatum) NE MONTH(vkdatum + 1) THEN DO:            
         MESSAGE "Vill du skapa lönefiler direkt efter ekonomi-sammanställningen ?" SKIP      
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE valg1 AS LOGICAL.
         CASE valg1:
            WHEN TRUE THEN DO:
               manval = TRUE.              
            END.
            WHEN FALSE THEN manval = FALSE.
         END CASE.
      END.
     /*ekonomi varje vecka  lönekörning 1 gång per månad
      misv kör varje vecka, men innan lönekörning görs måste ytterligare en ekonomikörning göras för
      att skicka det som har godkänts sedan sist*/
      FILL-IN-VECK = "Nu startar ekonomisammanställning " + STRING(TIME,"HH:MM").
      DISPLAY  FILL-IN-VECK VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
      PAUSE 0.                  
   
      RUN MISVEKON.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT samvar,INPUT vkdatum,INPUT "").  
      
      IF Guru.Konstanter:appcon THEN DO:
         RUN VKSATT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT samvar,INPUT Guru.Konstanter:globforetag,INPUT gvisatidpermanad,INPUT vkdatum,
         INPUT vknummer,INPUT regdatum).
      END.
      ELSE DO:
         RUN VKSATT.P 
         (INPUT samvar,INPUT Guru.Konstanter:globforetag,INPUT gvisatidpermanad,INPUT vkdatum,
         INPUT vknummer,INPUT regdatum).
      END.                                                                               
      IF manval = TRUE THEN DO:         
         FILL-IN-VECK = "Nu startar arbetsschema-sammanställningen " + STRING(TIME,"HH:MM").  
         DISPLAY FILL-IN-VECK VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
         PAUSE 0.
         RUN SUSCHHOGIA.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT samvar,INPUT vkdatum, INPUT Guru.Konstanter:globanv, INPUT "" ).           
         FILL-IN-VECK = "Nu startar flexkörning " + STRING(TIME,"HH:MM").
         RUN FLXMAN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT samvar,INPUT vkdatum).             
         FILL-IN-VECK = "Nu startar lönesammanställning " + STRING(TIME,"HH:MM").  
         DISPLAY FILL-IN-VECK VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
         PAUSE 0.
         RUN SULOHOGIAM.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT samvar,INPUT vkdatum, INPUT Guru.Konstanter:globanv, INPUT "").
         FILL-IN-VECK = "Nu startar frånvarosammanställning " + STRING(TIME,"HH:MM").
         DISPLAY  FILL-IN-VECK VIEW-AS TEXT  WITH FRAME {&FRAME-NAME} NO-LABELS.
         PAUSE 0.     
         RUN SUFRHOGIAM.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT samvar,INPUT vkdatum, INPUT Guru.Konstanter:globanv,INPUT "").                      
         FILL-IN-VECK = "Nu startar ekonomisammanställning " + STRING(TIME,"HH:MM").
         DISPLAY  FILL-IN-VECK VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
         PAUSE 0.      
      END.                             
   END.
    
   regdatum = TODAY.
   RUN REGVEC.P.           
   /* luleå:s  veckonatt uppdateras av ekonomikörningen istället*/
   IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "celpa"  THEN regdatum = regdatum.
   ELSE DO:   
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 11
      soktemp.SOKINT[1] = regvnr
      soktemp.SOKCHAR[1] = vknummer.
      {SOKANROP.I}      
      IF WEEKDAY(TODAY) = 1 THEN regdatum = TODAY - 7.
      IF WEEKDAY(TODAY) = 2 THEN regdatum = TODAY - 1.
      IF WEEKDAY(TODAY) = 3 THEN regdatum = TODAY - 2.
      IF WEEKDAY(TODAY) = 4 THEN regdatum = TODAY - 3.
      IF WEEKDAY(TODAY) = 5 THEN regdatum = TODAY - 4.
      IF WEEKDAY(TODAY) = 6 THEN regdatum = TODAY - 5.
      IF WEEKDAY(TODAY) = 7 THEN regdatum = TODAY - 6.          
   END.
   FILL-IN-VECK = "Nu stoppas all ändring av tidsedlar " + STRING(TIME,"HH:MM").
   DISPLAY  FILL-IN-VECK VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
   PAUSE 0.
   IF Guru.Konstanter:globforetag = "LULE"  OR Guru.Konstanter:globforetag = "celpa"  THEN DO:      
      vknummer = "".
   END.
   IF Guru.Konstanter:globforetag = "MISV" AND manval = TRUE THEN DO:
      /*OM ÄVEN LÖNEKÖRNING GÖRS*/
      vknummer = "".
      IF Guru.Konstanter:appcon THEN DO:
         RUN VKSATT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT samvar,INPUT Guru.Konstanter:globforetag,INPUT gvisatidpermanad,INPUT vkdatum,
         INPUT vknummer,INPUT regdatum).
      END.
      ELSE DO:
         RUN VKSATT.P 
         (INPUT samvar,INPUT Guru.Konstanter:globforetag,INPUT gvisatidpermanad,INPUT vkdatum,
         INPUT vknummer,INPUT regdatum).
      END.
   END.
   ELSE IF Guru.Konstanter:globforetag = "MISV" AND manval = FALSE THEN. 
   ELSE DO:    
      IF Guru.Konstanter:appcon THEN DO:
         RUN VKSATT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT samvar,INPUT Guru.Konstanter:globforetag,INPUT gvisatidpermanad,INPUT vkdatum,
         INPUT vknummer,INPUT regdatum).
      END.
      ELSE DO:
         RUN VKSATT.P 
         (INPUT samvar,INPUT Guru.Konstanter:globforetag,INPUT gvisatidpermanad,INPUT vkdatum,
         INPUT vknummer,INPUT regdatum).
      END.
   END.   
   /* luleå:s veckonatt uppdateras av ekonomikörningen istället*/
   IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "celpa"  THEN regdatum = regdatum.
   ELSE DO:   
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 12.
      {SOKANROP.I}      
   END.
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 8
   soktemp.SOKINT[1] = 2.
   {SOKANROP.I}        
   
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "celpa"  THEN DO:
      /*fakt*/
      IF Guru.Konstanter:appcon THEN DO:                                                    
         RUN SUMDAG.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
      END.
      ELSE DO:
         RUN SUMDAG.P.
      END.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:      
         IF Guru.Konstanter:appcon THEN DO:
            RUN OBEORDLIST.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT samvar).
            RUN VECKOVFLEX.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT samvar).
            IF Guru.Konstanter:globforetag = "SUND" THEN DO:
               RUN LAKINTYGSU.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT samvar).
            END.   
         END.
      END.
      IF Guru.Konstanter:globforetag = "MISV" THEN DO:      
         IF Guru.Konstanter:appcon THEN DO:
            RUN OBEORDLIST.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT samvar).            
         END.
      END.      
   END.   
   {musarrow.i}  
   IF Guru.Konstanter:appcon = TRUE THEN DO:
      RUN LESAMMAN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT 2,INPUT samvar,OUTPUT TABLE tidut).
   END.
   ELSE DO:
       RUN LESAMMAN.P 
      (INPUT 2,INPUT samvar,OUTPUT TABLE tidut).
   END.
   RUN LESAMMV.W.      

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

