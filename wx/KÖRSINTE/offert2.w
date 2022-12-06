&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-1



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 08/16/96 - 11:37 am

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
{GLOBVAR2DEL1.I}
{SOKMTRL.I}
DEFINE NEW SHARED VARIABLE kundoffproch AS HANDLE NO-UNDO. /* KUNDOFFAPP.P */
DEFINE NEW SHARED VARIABLE offe AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE avb AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE levapph AS HANDLE NO-UNDO.     /*LEVAPP.P*/
DEFINE SHARED VARIABLE kalksprec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE valkalknr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE finnskoff AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE bestant AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vald_kundlev AS CHARACTER  NO-UNDO.
DEFINE SHARED VARIABLE vald_lev AS CHARACTER  NO-UNDO.  
DEFINE SHARED VARIABLE huvudlev AS CHARACTER FORMAT "X(25)" NO-UNDO.  
DEFINE SHARED VARIABLE sok AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE sokant AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE mtrl_recid AS RECID NO-UNDO.
DEFINE VARIABLE forsta AS RECID NO-UNDO.
DEFINE VARIABLE antbest AS DECIMAL  NO-UNDO. 
DEFINE VARIABLE levvar AS CHARACTER  NO-UNDO.                     

DEFINE VARIABLE nettooff AS DECIMAL FORMAT "->9.99" NO-UNDO.
DEFINE VARIABLE offlev AS CHARACTER  NO-UNDO.         

DEFINE VARIABLE openqvar AS CHARACTER NO-UNDO.
&Scoped-define SHARED SHARED
{LEVTEMP.I}    
   
/*{EGENBEN.I}*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_MTRL

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES bmtrl_mtrl

/* Definitions for BROWSE BRW_MTRL                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL bmtrl_mtrl.Enr bmtrl_mtrl.Benamning ~
bmtrl_mtrl.Enhet bmtrl_mtrl.BERKVANT bmtrl_mtrl.NPRIS bmtrl_mtrl.SUMMA 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL 
&Scoped-define QUERY-STRING-BRW_MTRL FOR EACH bmtrl_mtrl NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRL OPEN QUERY BRW_MTRL FOR EACH bmtrl_mtrl NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL bmtrl_mtrl
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL bmtrl_mtrl


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_MTRL FBTN_SKAPA FBTN_VISA FBTN_SKRIV ~
CMB_LEV BTN_OFF BTN_BOFF FBTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS CMB_LEV 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BOFF 
     LABEL "Totalt":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_OFF 
     LABEL "Per artikel":L 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_OK AUTO-END-KEY 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_SKAPA 
     LABEL "Skapa Best.":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_SKRIV 
     LABEL "Skriv ut" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE VARIABLE CMB_LEV AS CHARACTER FORMAT "X(15)":U 
     LABEL "Leverantörer" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_MTRL FOR 
      bmtrl_mtrl SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL WINDOW-1 _STRUCTURED
  QUERY BRW_MTRL NO-LOCK DISPLAY
      bmtrl_mtrl.Enr COLUMN-LABEL "Enr" FORMAT "X(11)":U
      bmtrl_mtrl.Benamning COLUMN-LABEL "Benämning" FORMAT "x(256)":U
            WIDTH 25
      bmtrl_mtrl.Enhet COLUMN-LABEL "Enhet" FORMAT "x(5)":U
      bmtrl_mtrl.BERKVANT COLUMN-LABEL "Antal" FORMAT "->>>>9.99":U
      bmtrl_mtrl.NPRIS COLUMN-LABEL "Netto pris" FORMAT ">>>>>9.99":U
      bmtrl_mtrl.SUMMA COLUMN-LABEL "Summa" FORMAT ">>>>>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 79.5 BY 26
         TITLE "Materielspecifikation".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_MTRL AT ROW 1.5 COL 1.5
     FBTN_SKAPA AT ROW 8 COL 82
     FBTN_VISA AT ROW 9.08 COL 82
     FBTN_SKRIV AT ROW 10.21 COL 82
     CMB_LEV AT ROW 27.92 COL 14 COLON-ALIGNED
     BTN_OFF AT ROW 27.92 COL 35.5
     BTN_BOFF AT ROW 27.92 COL 49.13
     FBTN_OK AT ROW 27.92 COL 63
     BTN_AVB AT ROW 27.92 COL 82
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 96 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db bmtrl_mtrl
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Offertpris"
         HEIGHT             = 28.42
         WIDTH              = 96
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 96
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 96
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
/* SETTINGS FOR WINDOW WINDOW-1
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
/* BROWSE-TAB BRW_MTRL 1 FRAME-A */
ASSIGN 
       BRW_MTRL:MAX-DATA-GUESS IN FRAME FRAME-A         = 10000
       BRW_MTRL:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE
       BRW_MTRL:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

ASSIGN 
       BTN_OFF:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL
/* Query rebuild information for BROWSE BRW_MTRL
     _TblList          = "Temp-Tables.bmtrl_mtrl"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.bmtrl_mtrl.Enr
"bmtrl_mtrl.Enr" "Enr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.bmtrl_mtrl.Benamning
"bmtrl_mtrl.Benamning" "Benämning" "x(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.bmtrl_mtrl.Enhet
"bmtrl_mtrl.Enhet" "Enhet" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.bmtrl_mtrl.BERKVANT
"bmtrl_mtrl.BERKVANT" "Antal" "->>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.bmtrl_mtrl.NPRIS
"bmtrl_mtrl.NPRIS" "Netto pris" ">>>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.bmtrl_mtrl.SUMMA
"bmtrl_mtrl.SUMMA" "Summa" ">>>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_MTRL
&Scoped-define SELF-NAME BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_MTRL IN FRAME FRAME-A /* Materielspecifikation */
DO:
   FIND off_mtrl WHERE off_mtrl.LEVKOD = vald_lev NO-ERROR.
   IF NOT AVAILABLE off_mtrl THEN DO: 
      EMPTY TEMP-TABLE ekund_mtrl NO-ERROR. 
      EMPTY TEMP-TABLE emtrl_mtrl NO-ERROR. 
      CREATE emtrl_mtrl.
      BUFFER-COPY bmtrl_mtrl TO emtrl_mtrl.
      RUN OPART2.W (INPUT TRUE, INPUT-OUTPUT TABLE ekund_mtrl,INPUT-OUTPUT TABLE emtrl_mtrl).
      FIND FIRST emtrl_mtrl NO-LOCK NO-ERROR.
      BUFFER-COPY emtrl_mtrl TO bmtrl_mtrl.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(bmtrl_mtrl)).
      RUN openbdyn_UI IN brwproc[1] (INPUT "").
      RUN lastselectdyn_UI IN brwproc[1].    
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avbryt */
DO: 
   IF finnskoff = TRUE THEN DO:
      MESSAGE "OBS! Vill du spara dina ändringar?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL TITLE "Spara ändringar?" UPDATE svar AS LOGICAL.         
      IF svar THEN DO:
         APPLY "CHOOSE" TO FBTN_OK IN FRAME {&FRAME-NAME}.
      END.
      ELSE IF NOT svar THEN DO:       
         APPLY "CLOSE":U TO THIS-PROCEDURE.   
      END.                    
      ELSE DO:
         musz = musz.
      END.     
   END.
   ELSE DO:   
      APPLY "CLOSE":U TO THIS-PROCEDURE.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BOFF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BOFF WINDOW-1
ON CHOOSE OF BTN_BOFF IN FRAME FRAME-A /* Totalt */
DO: 
   musz = TRUE.
   FIND FIRST off_mtrl WHERE off_mtrl.LEVKOD = vald_lev NO-ERROR.
   IF AVAILABLE off_mtrl THEN DO:
      offe = TRUE.
      
      RUN OPTOT2.W (INPUT ROWID(off_mtrl)).
      
   END.
   ELSE DO:
      offe = FALSE.                        
      
      RUN OPTOT2.W (INPUT ROWID(bmtrl_mtrl)).
      
   END.   
   FIND FIRST off_mtrl WHERE off_mtrl.LEVKOD = vald_lev NO-LOCK NO-ERROR.
   IF AVAILABLE off_mtrl THEN DO:
      BTN_OFF:HIDDEN = TRUE.
   END.
   ELSE DO:
      BTN_OFF:HIDDEN = FALSE.
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OFF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OFF WINDOW-1
ON CHOOSE OF BTN_OFF IN FRAME FRAME-A /* Per artikel */
DO:    
   
   ASSIGN
   antal_valda = BRW_MTRL:NUM-SELECTED-ROWS
   antal_raknare = 1.    
   DO WHILE antal_raknare LE antal_valda:   
      status-ok = BRW_MTRL:FETCH-SELECTED-ROW(antal_raknare).  
      EMPTY TEMP-TABLE ekund_mtrl NO-ERROR. 
      EMPTY TEMP-TABLE emtrl_mtrl NO-ERROR. 
      CREATE emtrl_mtrl.
      BUFFER-COPY bmtrl_mtrl TO emtrl_mtrl.
      RUN OPART2.W (INPUT TRUE, INPUT-OUTPUT TABLE ekund_mtrl,INPUT-OUTPUT TABLE emtrl_mtrl).
      FIND FIRST emtrl_mtrl NO-LOCK NO-ERROR.
      BUFFER-COPY emtrl_mtrl TO bmtrl_mtrl.
      antal_raknare = antal_raknare + 1.   
   END.
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   ASSIGN
   status-ok = BRW_MTRL:DESELECT-ROWS().  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEV WINDOW-1
ON VALUE-CHANGED OF CMB_LEV IN FRAME FRAME-A /* Leverantörer */
DO:
   ASSIGN                                               
   CMB_LEV = INPUT CMB_LEV.                
   FIND FIRST levtemp WHERE levtemp.LEVNAMN = CMB_LEV NO-LOCK NO-ERROR.
   vald_lev = levtemp.LEVKOD.
   
   FIND FIRST off_mtrl WHERE off_mtrl.LEVKOD = vald_lev NO-LOCK NO-ERROR.
   IF AVAILABLE off_mtrl THEN DO:
      BTN_OFF:HIDDEN = TRUE.
   END.
   ELSE DO:
      BTN_OFF:HIDDEN = FALSE.
   END.      
      
   openqvar = "bmtrl_mtrl.LEVKOD = " + "'" + vald_lev + "'".
   RUN setcolsortvar_UI IN brwproc[1] (INPUT openqvar).
   RUN openbdynspec_UI IN brwproc[1].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_OK WINDOW-1
ON CHOOSE OF FBTN_OK IN FRAME FRAME-A /* Ok */
DO:
   IF finnskoff = TRUE THEN DO:
      RUN bestmtrlspar_UI IN kundoffproch (INPUT FALSE,INPUT varforetypval[1],INPUT valkalknr,INPUT TABLE bmtrl_mtrl,INPUT TABLE off_mtrl).                  
   END.
   ELSE DO:   
      RETURN.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKAPA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKAPA WINDOW-1
ON CHOOSE OF FBTN_SKAPA IN FRAME FRAME-A /* Skapa Best. */
DO:   
   {muswait.i}
   {AVBGOM.I}
   RUN LEVTRP2.W.
   {AVBFRAM.I}
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-1
ON CHOOSE OF FBTN_SKRIV IN FRAME FRAME-A /* Skriv ut */
DO: 
   RUN SKRIVVAL.W (INPUT FALSE).       
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO: 
      {muswait.i}                
      skrivut = TRUE.
      {AVBGOM.I}
      RUN VISALOFF.W.
      {AVBFRAM.I}
   END.     
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-1
ON MOUSE-MENU-CLICK OF FBTN_SKRIV IN FRAME FRAME-A /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA WINDOW-1
ON CHOOSE OF FBTN_VISA IN FRAME FRAME-A /* Visa */
DO:     
   {muswait.i}
   IF musz = TRUE THEN musz = FALSE.   
   ASSIGN    
   skrivut = FALSE.   
   {AVBGOM.I}
   RUN VISALOFF.W.
   {AVBFRAM.I}   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   IF VALID-HANDLE(kundoffproch) THEN DELETE PROCEDURE kundoffproch.
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
   {ALLSTARTDYN.I}
   FIND FIRST levtemp NO-ERROR.
   IF NOT AVAILABLE levtemp THEN DO:
      RUN levhmt_UI IN levapph (OUTPUT TABLE levtemp).             
   END.
   {muswait.i}
   bmtrl_mtrl.ENR:LABEL IN BROWSE BRW_MTRL = Guru.Konstanter:genk.
   IF finnskoff = TRUE THEN DO:
      RUN bestmtrlhmt_UI IN kundoffproch (INPUT valkalknr,OUTPUT TABLE bmtrl_mtrl,OUTPUT TABLE off_mtrl).           
   END.
   ELSE DO:
      finnskoff = finnskoff.
   END.                  
   FOR EACH bmtrl_mtrl.
      bmtrl_mtrl.SUMMA = bmtrl_mtrl.NPRIS * bmtrl_mtrl.BERKVANT.
   END.   
   status-ok = CMB_LEV:DELETE("0"). 
   FIND FIRST bmtrl_mtrl WHERE bmtrl_mtrl.LEVKOD = vald_kundlev NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bmtrl_mtrl THEN DO:
      FIND FIRST bmtrl_mtrl NO-LOCK NO-ERROR. 
   END.
   IF AVAILABLE bmtrl_mtrl THEN DO: 
      vald_lev = bmtrl_mtrl.LEVKOD.
      FOR EACH bmtrl_mtrl BREAK BY bmtrl_mtrl.LEVKOD : 
         IF LAST-OF(bmtrl_mtrl.LEVKOD) THEN DO:
            FIND FIRST levtemp WHERE levtemp.LEVKOD = bmtrl_mtrl.LEVKOD NO-LOCK NO-ERROR.
            IF AVAILABLE levtemp THEN DO:
               ASSIGN  
               status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.               
            END.
         END.
      END.                                       
      FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_lev NO-LOCK NO-ERROR.     
      ASSIGN       
      CMB_LEV:SCREEN-VALUE = levtemp.LEVNAMN.
      openqvar = "bmtrl_mtrl.LEVKOD = " + "'" + vald_lev + "'".
      RUN setcolsortvar_UI IN brwproc[1] (INPUT openqvar).
      RUN openbdynspec_UI IN brwproc[1].
   END.
   ELSE DO:
      MESSAGE "Det finns inget materiel att sätta offertpris på." VIEW-AS ALERT-BOX.      
   END. 
   
   RUN enable_UI.   
   {FRMSIZE.I}  
   ASSIGN
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_AVB:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = FBTN_OK:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   ASSIGN
   Guru.GlobalaVariabler:colrighth = BTN_BOFF:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   ASSIGN
   Guru.GlobalaVariabler:colrighth = BTN_OFF:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   ASSIGN
   Guru.GlobalaVariabler:colrighth = CMB_LEV:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).

   FIND FIRST off_mtrl WHERE off_mtrl.LEVKOD = vald_lev NO-LOCK NO-ERROR.
   IF AVAILABLE off_mtrl THEN DO:
      BTN_OFF:HIDDEN = TRUE.
   END.
   ELSE DO:
      BTN_OFF:HIDDEN = FALSE.
   END.            
   {musarrow.i}    
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_MTRL:HANDLE IN FRAME {&FRAME-NAME}).
   RUN setdefaultcolbyname_UI IN brwproc[1] (INPUT "ENR").
   IF Guru.Konstanter:appcon THEN DO:
      RUN KUNDOFFAPP.P PERSISTENT SET kundoffproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.      
   END.
   ELSE DO:
      RUN KUNDOFFAPP.P PERSISTENT SET kundoffproch.      
   END.                         

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-1  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
  THEN DELETE WIDGET WINDOW-1.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-1  _DEFAULT-ENABLE
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
  DISPLAY CMB_LEV 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE BRW_MTRL FBTN_SKAPA FBTN_VISA FBTN_SKRIV CMB_LEV BTN_OFF BTN_BOFF 
         FBTN_OK BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

