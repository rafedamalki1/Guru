&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-1


/* Temp-Table and Buffer definitions                                    */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 11/21/96 -  1:20 pm

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

{MTRLTEMP.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{LAGERTRUM.I}
{DEPATEMP.I}
{LEVTEMP.I}
{HOPPSEK2W.I}
DEFINE NEW SHARED VARIABLE avb AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE vald_best AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO. 
DEFINE SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE depatitta AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE levtill AS INTEGER NO-UNDO.
DEFINE VARIABLE leveransapph AS HANDLE NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE bortenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE svar1 AS LOGICAL NO-UNDO. 
DEFINE VARIABLE invdat AS DATE NO-UNDO.
DEFINE VARIABLE lagervar AS LOGICAL NO-UNDO.
DEFINE VARIABLE lplats AS INTEGER NO-UNDO.
DEFINE VARIABLE kvarmeter AS INTEGER NO-UNDO.
&Scoped-define NEW 
&Scoped-define SHARED SHARED 
{BESTMTRLDEP.I}

DEFINE TEMP-TABLE ebest_mtrl NO-UNDO LIKE  best_mtrl.
/*DEFINE TEMP-TABLE lptemp NO-UNDO
   FIELD ENR AS CHARACTER 
   FIELD TRUMMANR AS INTEGER
   FIELD LEVKOD AS CHARACTER
   FIELD RMETER AS INTEGER 
   FIELD KMETER AS INTEGER
   FIELD METER AS INTEGER  
   INDEX ENR ENR. 

DEFINE TEMP-TABLE lptemp2 NO-UNDO
   FIELD ENR AS CHARACTER 
   FIELD TRUMMANR AS INTEGER   
   INDEX ENR ENR.*/ 

   
DEFINE BUFFER bmtrlbuf FOR best_mtrl.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_MOT

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES best_mtrl

/* Definitions for BROWSE BRW_MOT                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_MOT best_mtrl.Enr best_mtrl.BENAMNING ~
best_mtrl.Antal best_mtrl.PRIS best_mtrl.REST 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MOT best_mtrl.Antal ~
best_mtrl.PRIS 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_MOT best_mtrl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_MOT best_mtrl
&Scoped-define QUERY-STRING-BRW_MOT FOR EACH best_mtrl NO-LOCK ~
    BY best_mtrl.Enr
&Scoped-define OPEN-QUERY-BRW_MOT OPEN QUERY BRW_MOT FOR EACH best_mtrl NO-LOCK ~
    BY best_mtrl.Enr.
&Scoped-define TABLES-IN-QUERY-BRW_MOT best_mtrl
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MOT best_mtrl


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_MOT BTN_AVB BRW_MOT BTN_TRUMMA BTN_REST ~
BTN_AND BTN_BORT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AND DEFAULT 
     LABEL "Ändra antal" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_AVB DEFAULT 
     LABEL "Avbryt" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_BORT DEFAULT 
     LABEL "Ta bort" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_MOT DEFAULT 
     LABEL "Mottagit" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_PRISOK DEFAULT 
     LABEL "Spara uppdatering" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_REST DEFAULT 
     LABEL "Rest" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_TRUMMA DEFAULT 
     LABEL "Trumma" 
     SIZE 12 BY 1 TOOLTIP "Lägg markerat enr på trumma"
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_MOT FOR 
      best_mtrl SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_MOT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MOT WINDOW-1 _STRUCTURED
  QUERY BRW_MOT NO-LOCK DISPLAY
      best_mtrl.Enr COLUMN-LABEL "Enr" FORMAT "X(11)":U
      best_mtrl.BENAMNING COLUMN-LABEL "Benämning" FORMAT "x(25)":U
      best_mtrl.Antal COLUMN-LABEL "Antal" FORMAT ">>>,>>9":U
      best_mtrl.PRIS COLUMN-LABEL "Pris" FORMAT ">>>>99.99":U
      best_mtrl.REST COLUMN-LABEL "Rest" FORMAT "Ja/Nej":U
  ENABLE
      best_mtrl.Antal
      best_mtrl.PRIS
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH MULTIPLE SIZE 61.5 BY 23.5
         TITLE "Specifikation av beställning".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BTN_MOT AT ROW 27.88 COL 43.88
     BTN_AVB AT ROW 27.88 COL 58.75
     BRW_MOT AT ROW 1.5 COL 1.5
     BTN_TRUMMA AT ROW 25.96 COL 2.25
     BTN_REST AT ROW 25.96 COL 15
     BTN_AND AT ROW 25.96 COL 27.75
     BTN_BORT AT ROW 25.96 COL 40.5
     BTN_PRISOK AT ROW 27.88 COL 29 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 72.25 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db best_mtrl
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 28.42
         WIDTH              = 72.38
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 72.38
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 72.38
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
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB BRW_MOT BTN_AVB FRAME-A */
ASSIGN 
       BRW_MOT:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR BUTTON BTN_PRISOK IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       BTN_PRISOK:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       BTN_TRUMMA:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MOT
/* Query rebuild information for BROWSE BRW_MOT
     _TblList          = "Temp-Tables.best_mtrl"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.best_mtrl.Enr|yes"
     _FldNameList[1]   > Temp-Tables.best_mtrl.Enr
"best_mtrl.Enr" "Enr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.best_mtrl.BENAMNING
"best_mtrl.BENAMNING" "Benämning" "x(25)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.best_mtrl.Antal
"best_mtrl.Antal" "Antal" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.best_mtrl.PRIS
"best_mtrl.PRIS" "Pris" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.best_mtrl.REST
"best_mtrl.REST" "Rest" ? "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MOT */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_MOT
&Scoped-define SELF-NAME BRW_MOT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MOT WINDOW-1
ON ROW-LEAVE OF BRW_MOT IN FRAME FRAME-A /* Specifikation av beställning */
DO:
  IF AVAILABLE best_mtrl THEN DO:
     IF Guru.Konstanter:mtrlsekvar[6] = FALSE THEN RUN visa_UI. 
     ASSIGN          
     best_mtrl.PRIS = INPUT BROWSE BRW_MOT best_mtrl.PRIS.
     IF Guru.Konstanter:mtrlsekvar[6] = FALSE THEN RUN visa_UI.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MOT WINDOW-1
ON VALUE-CHANGED OF BRW_MOT IN FRAME FRAME-A /* Specifikation av beställning */
DO:
   status-ok = BRW_MOT:SELECT-FOCUSED-ROW() IN FRAME FRAME-A.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME best_mtrl.Antal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL best_mtrl.Antal BRW_MOT _BROWSE-COLUMN WINDOW-1
ON ANY-KEY OF best_mtrl.Antal IN BROWSE BRW_MOT /* Antal */
DO:
  IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      IF AVAILABLE best_mtrl THEN DO:      
         IF best_mtrl.ANTAL NE INPUT BROWSE BRW_MOT best_mtrl.ANTAL THEN DO:             
            best_mtrl.ANTAL = INPUT BROWSE BRW_MOT best_mtrl.ANTAL .
            RUN visa_UI.
            
         END.
      END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL best_mtrl.Antal BRW_MOT _BROWSE-COLUMN WINDOW-1
ON ENTRY OF best_mtrl.Antal IN BROWSE BRW_MOT /* Antal */
DO:
   IF AVAILABLE best_mtrl THEN DO: 
     RUN visa_UI.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL best_mtrl.Antal BRW_MOT _BROWSE-COLUMN WINDOW-1
ON LEAVE OF best_mtrl.Antal IN BROWSE BRW_MOT /* Antal */
DO:
  IF AVAILABLE best_mtrl THEN DO:      
     FIND FIRST bmtrlbuf WHERE bmtrlbuf.ENR = best_mtrl.ENR AND bmtrlbuf.REST = TRUE NO-LOCK NO-ERROR. 
      IF AVAILABLE bmtrlbuf THEN DO:
         MESSAGE  LC(Guru.Konstanter:genk) + " - " + best_mtrl.ENR + "är restrapporterat och går ej att ändra antal för "
         VIEW-AS ALERT-BOX.
      END.
      ELSE IF best_mtrl.REST = TRUE THEN DO:
         MESSAGE  LC(Guru.Konstanter:genk) + " - " + best_mtrl.ENR + "är restrapporterat och går ej att ändra antal för "
         VIEW-AS ALERT-BOX.
      END.
      ELSE DO:
         IF best_mtrl.ANTAL NE INPUT BROWSE BRW_MOT best_mtrl.ANTAL THEN DO:          
            best_mtrl.ANTAL = INPUT BROWSE BRW_MOT best_mtrl.ANTAL .
            RUN visa_UI.         
         END.
      END.   
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME best_mtrl.PRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL best_mtrl.PRIS BRW_MOT _BROWSE-COLUMN WINDOW-1
ON ANY-KEY OF best_mtrl.PRIS IN BROWSE BRW_MOT /* Pris */
DO:
  IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      IF AVAILABLE best_mtrl THEN DO:      
         IF best_mtrl.PRIS NE INPUT BROWSE BRW_MOT best_mtrl.PRIS THEN DO:             
            best_mtrl.PRIS = INPUT BROWSE BRW_MOT best_mtrl.PRIS.
            IF Guru.Konstanter:mtrlsekvar[6] = FALSE THEN RUN visa_UI.
            
         END.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL best_mtrl.PRIS BRW_MOT _BROWSE-COLUMN WINDOW-1
ON ENTRY OF best_mtrl.PRIS IN BROWSE BRW_MOT /* Pris */
DO:
  IF AVAILABLE best_mtrl THEN DO: 
     IF Guru.Konstanter:mtrlsekvar[6] = FALSE THEN RUN visa_UI.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL best_mtrl.PRIS BRW_MOT _BROWSE-COLUMN WINDOW-1
ON LEAVE OF best_mtrl.PRIS IN BROWSE BRW_MOT /* Pris */
DO:
  IF AVAILABLE best_mtrl THEN DO:                
      IF best_mtrl.PRIS NE INPUT BROWSE BRW_MOT best_mtrl.PRIS THEN DO:          
         best_mtrl.PRIS = INPUT BROWSE BRW_MOT best_mtrl.PRIS.
         IF Guru.Konstanter:mtrlsekvar[6] = FALSE THEN RUN visa_UI.         
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND WINDOW-1
ON CHOOSE OF BTN_AND IN FRAME FRAME-A /* Ändra antal */
DO:
   {muswait.i}
   antal_valda = BRW_MOT:NUM-SELECTED-ROWS.
   antal_raknare = 1.    
   DO WHILE antal_raknare LE antal_valda:   
      status-ok = BRW_MOT:FETCH-SELECTED-ROW(antal_raknare).
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(best_mtrl)).
      FIND FIRST bmtrlbuf WHERE bmtrlbuf.ENR = best_mtrl.ENR AND bmtrlbuf.REST = TRUE NO-LOCK NO-ERROR. 
      IF AVAILABLE bmtrlbuf THEN DO:
         MESSAGE  LC(Guru.Konstanter:genk) + " - " + best_mtrl.ENR + "är restrapporterat och går ej att ändra antal för "
         VIEW-AS ALERT-BOX.
      END.
      ELSE IF best_mtrl.REST = TRUE THEN DO:
         MESSAGE  LC(Guru.Konstanter:genk) + " - " + best_mtrl.ENR + "är restrapporterat och går ej att ändra antal för "
         VIEW-AS ALERT-BOX.
      END.
      ELSE DO:     
         RUN ANBANTV.W (INPUT 1, INPUT ROWID(best_mtrl)).
      END.
      
      antal_raknare = antal_raknare + 1.   
   END.
   RUN openbdynspec_UI IN brwproc[1].    
   RUN lastselectdyn_UI IN brwproc[1].
   RUN title_UI IN brwproc[1].
   status-ok = BRW_MOT:DESELECT-ROWS().   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avbryt */
DO:  
   IF depatitta = FALSE THEN DO:
      MESSAGE "OBS! Vill du spara dina ändringar?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL TITLE "Spara ändringar?" UPDATE svar.         
      IF svar THEN DO:
         APPLY "CHOOSE" TO BTN_MOT IN FRAME {&FRAME-NAME}.
      END.
      ELSE IF NOT svar THEN DO:       
         
         
         FOR EACH lptemp2 :
            FIND FIRST lagertrum  WHERE lagertrum.TRUMMANR = lptemp2.TRUMMANR AND lagertrum.TRUMMA = "S" NO-LOCK NO-ERROR.
            IF AVAILABLE lagertrum THEN DELETE lagertrum.
         END.
         FIND FIRST lptemp2  WHERE NO-LOCK NO-ERROR.
         IF AVAILABLE lptemp2 THEN DO:
            RUN SparafaltTrumma_UI IN  Guru.GlobalaVariabler:ClienttdSetapph (INPUT TABLE lagertrum,INPUT TABLE aotrum).
         END.   
         EMPTY TEMP-TABLE lptemp2 NO-ERROR.
         musz = TRUE. 
         APPLY "CLOSE":U TO THIS-PROCEDURE.   
      END.                                  
   END.
   ELSE DO:
      musz = TRUE.
      APPLY "CLOSE":U TO THIS-PROCEDURE.   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-1
ON CHOOSE OF BTN_BORT IN FRAME FRAME-A /* Ta bort */
DO:
   {muswait.i} 
   antal_valda = BRW_MOT:NUM-SELECTED-ROWS.
   antal_raknare = 1.    
   DO WHILE antal_raknare LE antal_valda:   
      status-ok = BRW_MOT:FETCH-SELECTED-ROW(antal_raknare). 
      IF best_mtrl.REST = TRUE THEN DO: 
         MESSAGE "Vill du ta bort restnotering för " + LC(Guru.Konstanter:genk) + " - " + STRING(best_mtrl.ENR) VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO UPDATE svar.
         IF svar THEN DO: 
            FIND FIRST bmtrlbuf WHERE bmtrlbuf.ENR = best_mtrl.ENR 
            AND bmtrlbuf.REST = FALSE NO-ERROR.
            ASSIGN
            bmtrlbuf.ANTAL = bmtrlbuf.antal + best_mtrl.ANTAL.    
            DELETE best_mtrl.
            RUN selnextprevrow_UI IN brwproc[1].
         END.                                               
      END.
      ELSE DO:
         MESSAGE "Vill du ta bort " + LC(Guru.Konstanter:genk) + " - " + STRING(best_mtrl.ENR) + " - från beställningen." 
         VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO UPDATE svar. 
         IF svar THEN DO:
            bortenr = best_mtrl.ENR.
            FIND FIRST bmtrlbuf WHERE bmtrlbuf.ENR = best_mtrl.ENR 
            AND bmtrlbuf.REST = TRUE NO-LOCK NO-ERROR.
            IF AVAILABLE bmtrlbuf THEN DO:
               MESSAGE "Det finns en restnotering för detta " + LC(Guru.Konstanter:genk) + " som också kommer att tas bort."
               VIEW-AS ALERT-BOX.    
               FOR EACH best_mtrl WHERE best_mtrl.ENR = bortenr:
                  DELETE best_mtrl. 
               END.              
            END.
            ELSE DO:
               DELETE best_mtrl.
            END.   
            RUN selnextprevrow_UI IN brwproc[1].
         END.                                                             
      END. 
      antal_raknare = antal_raknare + 1. 
   END.       
   RUN openbdynspec_UI IN brwproc[1].    
   RUN lastselectdyn_UI IN brwproc[1].
   RUN title_UI IN brwproc[1].
   {musarrow.i} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_MOT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MOT WINDOW-1
ON CHOOSE OF BTN_MOT IN FRAME FRAME-A /* Mottagit */
DO:
   {muswait.i}
   FOR EACH best_mtrl WHERE best_mtrl.ANTAL = 0.
      DELETE best_mtrl.
   END.       
   DEFINE VARIABLE varning AS LOGICAL NO-UNDO.
   DEFINE VARIABLE varningenr AS CHARACTER NO-UNDO.
   DEFINE VARIABLE nollpris AS LOGICAL NO-UNDO.
   DEFINE VARIABLE finnsej AS LOGICAL NO-UNDO.
   DEFINE VARIABLE finnsejenr AS CHARACTER NO-UNDO.
   DEFINE VARIABLE deplev AS CHARACTER NO-UNDO.
   
   varning = FALSE.
   nollpris = FALSE.
   finnsej = FALSE.
   finnsejenr = "".
   IF best_mtrl.PRIS:VISIBLE IN BROWSE BRW_MOT = TRUE THEN DO:   
      FOR EACH best_mtrl:
         EMPTY TEMP-TABLE emtrltemp NO-ERROR.          
         RUN hmtskap_UI IN leveransapph (INPUT best_mtrl.ENR,INPUT best_mtrl.LEVKOD,OUTPUT TABLE emtrltemp). 
         FIND FIRST emtrltemp NO-LOCK NO-ERROR.
         IF AVAILABLE emtrltemp THEN DO:
            IF best_mtrl.PRIS NE emtrltemp.NPRIS THEN DO:
               varning = TRUE.
               varningenr = best_mtrl.ENR.                 
            END.            
         END.
         ELSE DO:
            finnsej = TRUE.
            finnsejenr = best_mtrl.ENR + " lev " + best_mtrl.LEVKOD.
         END.   
         IF best_mtrl.PRIS = 0 THEN nollpris = TRUE.
         
         /*kalmar depåändring abc nu*/
         IF Guru.Konstanter:globforetag = "GKAL" AND varning = TRUE THEN DO:
            
            RUN kollmtrldep_UI IN leveransapph (INPUT vald_depa,INPUT best_mtrl.ENR,OUTPUT deplev).                                    
            IF deplev = "10" THEN DO:
                varning = FALSE.
                finnsej = FALSE.
                finnsejenr = "".                
            END.                   
         END.             
      END.
      IF varning = TRUE THEN DO:
         IF varningenr NE "" THEN  MESSAGE "Ni har ändrat pris på " + varningenr +  " jämfört med vad prislistan säger. Är detta korrekt?"         
         VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO TITLE "Uppdatera Priser?" UPDATE svar.
         ELSE   MESSAGE "Ni har ändrat pris på någon artikel jämfört med vad prislistan säger. Är detta korrekt?"         
         VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO TITLE "Uppdatera Priser?" UPDATE svar.        
         IF NOT svar THEN DO:           
            RETURN NO-APPLY.
         END.
      END.
      IF  finnsej = TRUE THEN DO:
         MESSAGE "Enr " + finnsejenr + "finns ej i leverantörkatalog. Vill du ändå genomföra mottagningen?"
         VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO TITLE "Enr finns ej i katalog" UPDATE svar1.         
         IF NOT svar1 THEN DO:           
            RETURN NO-APPLY.
         END.
      END.   
      IF nollpris = TRUE THEN DO:
         MESSAGE "Pris på mottagna artiklar kan inte vara 0 kr"
         VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
         
   END.
   
   IF Guru.Konstanter:globforetag = "snat" THEN DO:      
      FOR EACH best_mtrl NO-LOCK:
         FIND FIRST lagertrum  WHERE lagertrum.ENR = best_mtrl.ENR AND lagertrum.TRUMMA = "S" /*AND lagertrum.LAGMETER > 0*/ NO-LOCK NO-ERROR.
         IF AVAILABLE lagertrum THEN DO:
            /*om inga trummor upplagda via retur skall frågan fås.*/
            FIND FIRST lptemp2 WHERE lptemp2.ENR = best_mtrl.ENR  NO-ERROR.
            IF NOT AVAILABLE lptemp2 THEN DO:   
               RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(best_mtrl)).                         
               status-ok = BRW_MOT:DESELECT-ROWS() NO-ERROR.               
               RUN lastselectdyn_UI IN brwproc[1].
               MESSAGE "Enr " + best_mtrl.ENR + " finns lagd på trumma. Vill du lägga in dessa meter på en ny trumma ?" 
               VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL TITLE "Ny trumma?"
               UPDATE svar.         
               IF svar THEN DO:
                  APPLY "CHOOSE" TO BTN_TRUMMA IN FRAME FRAME-A. 
                  RETURN.               
               END.               
               ELSE IF NOT svar THEN DO:
               END.
               ELSE DO:
                  RETURN   NO-APPLY.
               END.
            END.        
         END.                    
      END.
   END.
   
   
   RUN ok_UI IN leveransapph (INPUT vald_depa, INPUT vald_best, INPUT vald_lev, INPUT TABLE best_mtrl, OUTPUT invdat).
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "SOLE" 
     OR 
   Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "LULE"  OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "BODE" OR Guru.Konstanter:globforetag = "KRAF" THEN DO:        
      /*levnamn ne "" då är det en projektbeställning och skall ej på verka lagret*/
      FOR EACH best_mtrl WHERE best_mtrl.REST = FALSE AND best_mtrl.LEVNAMN = "":
         EMPTY TEMP-TABLE ebest_mtrl  NO-ERROR. 
         CREATE ebest_mtrl.
         BUFFER-COPY best_mtrl TO ebest_mtrl.
         RUN ok2_UI IN leveransapph (INPUT vald_depa, INPUT TABLE ebest_mtrl, INPUT invdat, INPUT ebest_mtrl.ENR, OUTPUT lagervar).
         IF lagervar = FALSE THEN DO:
            FIND FIRST ebest_mtrl NO-LOCK NO-ERROR.
            svar = FALSE.
            MESSAGE "Materiel med " + LC(Guru.Konstanter:genk) + ":" + ebest_mtrl.ENR + " och Benämningen:" +
            ebest_mtrl.BENAMNING + " lagerförs ej. Vill du börja lagerföra materielet?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Ej lagerfört materiel!" 
            UPDATE svar.         
            IF svar THEN DO:
               RUN ok3_UI IN leveransapph (INPUT vald_depa, INPUT TABLE ebest_mtrl, INPUT invdat, INPUT ebest_mtrl.ENR).
            END.
         END.         
         EMPTY TEMP-TABLE ebest_mtrl  NO-ERROR. 
      END.      
   END.  
     APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_PRISOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_PRISOK WINDOW-1
ON CHOOSE OF BTN_PRISOK IN FRAME FRAME-A /* Spara uppdatering */
DO:
   {muswait.i}   
   FIND FIRST best_mtrl WHERE best_mtrl.REST = TRUE NO-LOCK NO-ERROR.
   IF AVAILABLE best_mtrl THEN DO:
      MESSAGE "Spara uppdatering skall bara användas för att uppdatera ändrade priser och antal. Ingen mottagning kan ske samtidigt." SKIP  
              "Du har resrapporterat!"
         VIEW-AS ALERT-BOX.
   END.
   ELSE DO:   
      RUN prisok_UI IN leveransapph (INPUT vald_depa, INPUT vald_best, INPUT vald_lev, INPUT TABLE best_mtrl, OUTPUT invdat).
      APPLY "CLOSE":U TO THIS-PROCEDURE.
   END.        
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_REST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REST WINDOW-1
ON CHOOSE OF BTN_REST IN FRAME FRAME-A /* Rest */
DO:
   {muswait.i}
   antal_valda = BRW_MOT:NUM-SELECTED-ROWS.
   antal_raknare = 1.    
   DO WHILE antal_raknare LE antal_valda:   
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(best_mtrl)).
      status-ok = BRW_MOT:FETCH-SELECTED-ROW(antal_raknare).
      IF best_mtrl.REST = TRUE THEN DO:
         MESSAGE "För att ändra restantalet för " + LC(Guru.Konstanter:genk) + " - " + best_mtrl.ENR + " - Markera samma " + LC(Guru.Konstanter:genk) + " men där rest är Nej."
         VIEW-AS ALERT-BOX.
      END.
      ELSE DO:     
         
         RUN RESTANTV.W (INPUT ROWID(best_mtrl)).
         
      END.   
      antal_raknare = antal_raknare + 1.   
   END.   
   RUN openbdynspec_UI IN brwproc[1].    
   RUN lastselectdyn_UI IN brwproc[1].
   RUN title_UI IN brwproc[1].
   status-ok = BRW_MOT:DESELECT-ROWS().   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_TRUMMA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_TRUMMA WINDOW-1
ON CHOOSE OF BTN_TRUMMA IN FRAME FRAME-A /* Trumma */
DO:
   {muswait.i}
   antal_valda = BRW_MOT:NUM-SELECTED-ROWS.
   antal_raknare = 1.    
   DO WHILE antal_raknare LE antal_valda:   
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(best_mtrl)).
      status-ok = BRW_MOT:FETCH-SELECTED-ROW(antal_raknare).      
      DEBUGGER:SET-BREAK().
      EMPTY TEMP-TABLE elagertrum NO-ERROR. 
      CREATE elagertrum.
      ASSIGN 
      elagertrum.BENAMNING = best_mtrl.BENAMNING
      elagertrum.ENHET = best_mtrl.ENHET
      elagertrum.ENR = best_mtrl.ENR
      elagertrum.STMETER = best_mtrl.ANTAL
      elagertrum.LAGMETER = best_mtrl.ANTAL
      elagertrum.PRIS = best_mtrl.PRIS
      elagertrum.DATUM = TODAY
      elagertrum.ANVANDARE = Guru.Konstanter:globanv.
      
      kvarmeter = best_mtrl.ANTAL.
      /*vilka enr som läggs på trummor, check om samma enr fördelas på två trummor*/
      FIND FIRST lptemp WHERE lptemp.ENR = elagertrum.ENR NO-ERROR.
      IF AVAILABLE lptemp THEN DO:
         kvarmeter = lptemp.KMETER. 
      END.
      IF kvarmeter > 0 THEN DO:         
         RUN TRUMREG.W (INPUT-OUTPUT TABLE elagertrum,INPUT kvarmeter, OUTPUT lplats).
         IF lplats > 0 THEN DO:
            FIND FIRST elagertrum NO-ERROR.
            IF AVAILABLE elagertrum THEN DO:            
               FIND FIRST lptemp WHERE lptemp.ENR = elagertrum.ENR NO-ERROR.
               IF NOT AVAILABLE lptemp  THEN DO:
                  CREATE lptemp.
                  lptemp.TRUMMANR = lplats.
                  lptemp.METER = best_mtrl.ANTAL.
               END.
               ASSIGN
               lptemp.ENR = elagertrum.ENR
               lptemp.RMETER = lptemp.RMETER + elagertrum.STMETER
               lptemp.KMETER = lptemp.METER - lptemp.RMETER.
               /*vilka trummor som lagts upp för att kunna rensa vid avbryt*/
               FIND FIRST lptemp2 WHERE lptemp2.TRUMMANR = lplats NO-ERROR.
               IF NOT AVAILABLE lptemp2  THEN DO:
                  CREATE lptemp2.
                  ASSIGN 
                  lptemp2.TRUMMANR = lplats
                  lptemp2.ENR = lptemp.ENR. 
               END.
               
            END.
         END.
      END.
      ELSE DO:
         MESSAGE "För " +  LC(Guru.Konstanter:genk) + " - " + best_mtrl.ENR + "är redan hela länden registrerad på trumma" VIEW-AS ALERT-BOX.
      END.
      EMPTY TEMP-TABLE elagertrum NO-ERROR. 
      antal_raknare = antal_raknare + 1.   
   END.      
   musz = FALSE.
   status-ok = BRW_MOT:DESELECT-ROWS().   
   RUN lastselectdyn_UI IN brwproc[1].   
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
   {BORTBRWPROC.I}
   IF VALID-HANDLE(leveransapph) THEN DELETE PROCEDURE leveransapph.

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
   
   EMPTY TEMP-TABLE  best_mtrl NO-ERROR. 
   EMPTY TEMP-TABLE lptemp NO-ERROR. 
   EMPTY TEMP-TABLE lptemp2 NO-ERROR. 
   RUN besthmt_UI IN leveransapph (INPUT vald_depa,INPUT vald_best,INPUT vald_lev,OUTPUT TABLE best_mtrl).                
   RUN setcolsortvar_UI IN brwproc[1] (INPUT "").
   RUN openbdynspec_UI IN brwproc[1].         
   RUN title_UI IN brwproc[1].   
   FIND FIRST depatemp WHERE depatemp.DEP-NR = vald_depa.
   FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_lev NO-LOCK NO-ERROR.
   ASSIGN WINDOW-1:TITLE = "Mottagningsrutin beställning nr: " + STRING(vald_best) + 
   " för leverantör " + levtemp.LEVNAMN.    
   /*kalmar depåändring*/
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "BODE" OR Guru.Konstanter:globforetag = "gkal" OR Guru.Konstanter:globforetag = "KRAF" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      best_mtrl.PRIS:VISIBLE IN BROWSE BRW_MOT = TRUE.
   END.
   ELSE DO:
      best_mtrl.PRIS:VISIBLE IN BROWSE BRW_MOT = FALSE.   
   END.
   IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN DO:
      best_mtrl.PRIS:VISIBLE IN BROWSE BRW_MOT = FALSE.   
   END.      
      
   RUN enable_UI.
   BTN_TRUMMA:HIDDEN IN FRAME FRAME-A           = TRUE.     
   IF Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      IF levtill = 1 THEN  BTN_TRUMMA:HIDDEN IN FRAME FRAME-A           = FALSE.
   END.
   
   
   
   /*kalmar depåändring*/
   IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "gkal"  OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "elpa" THEN ENABLE BTN_PRISOK WITH FRAME {&FRAME-NAME}.
   {FRMSIZE.I}
   
   IF depatitta = TRUE THEN DO:
      DISABLE BTN_MOT WITH FRAME {&FRAME-NAME}.
      DISABLE BTN_BORT WITH FRAME {&FRAME-NAME}.
      DISABLE BTN_REST WITH FRAME {&FRAME-NAME}.
      DISABLE BTN_PRISOK WITH FRAME {&FRAME-NAME}.
   END.
   /*APPLY "ENTRY" TO best_mtrl.BENAMNING IN BROWSE BRW_MOT.*/
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
   
   /*dd kalmar depåändring abc nu ska vara ändringbart*/
   IF Guru.Konstanter:globforetag = "cgkal" THEN DO:
      best_mtrl.PRIS:READ-ONLY IN BROWSE BRW_MOT = TRUE.
      
   END. 
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_MOT:HANDLE IN FRAME {&FRAME-NAME}).
   RUN StartSokEnrLev_UI IN brwproc[1] (INPUT THIS-PROCEDURE).      
   IF Guru.Konstanter:appcon THEN DO:
      RUN LEVAPPV.P PERSISTENT SET leveransapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN LEVAPPV.P PERSISTENT SET leveransapph.
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
  ENABLE BTN_MOT BTN_AVB BRW_MOT BTN_TRUMMA BTN_REST BTN_AND BTN_BORT 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE infoes_UI WINDOW-1 
PROCEDURE infoes_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER valenr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER brwname AS CHARACTER NO-UNDO.
   DEFINE VARIABLE valbrw AS INTEGER NO-UNDO.  
   DEFINE VARIABLE levnamnvar AS CHARACTER NO-UNDO.
   IF brwname = "BRW_MOT" THEN valbrw = 1.  
    
   IF valbrw = 1 THEN DO:
      status-ok = BRW_MOT:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.
      IF status-ok THEN DO:
         valenr = best_mtrl.ENR.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = best_mtrl.LEVKOD
         NO-LOCK NO-ERROR.      
      END.
   END.
   
   IF status-ok THEN DO:
      levnamnvar = levtemp.LEVNAMN.
      {LEVLANK.I}      
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visa_UI WINDOW-1 
PROCEDURE visa_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF AVAILABLE best_mtrl THEN DO:      
      DISPLAY
      best_mtrl.ANTAL best_mtrl.PRIS      
      WITH BROWSE BRW_MOT.         
   END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

