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
{aobestnrtab.I}

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER TABLE FOR eaobest_nr_tab.
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
DEFINE VARIABLE leveransaoapph AS HANDLE NO-UNDO.
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
{AOBESTMTRLDEP.I}


/*DEFINE TEMP-TABLE eaobest_mtrl NO-UNDO LIKE  aobest_mtrl.
DEFINE TEMP-TABLE lptemp NO-UNDO
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

   
DEFINE BUFFER bmtrlbuf FOR aobest_mtrl.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_MOTAO

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES aobest_mtrl

/* Definitions for BROWSE BRW_MOTAO                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_MOTAO aobest_mtrl.Enr ~
aobest_mtrl.BENAMNING aobest_mtrl.Antal aobest_mtrl.Pris aobest_mtrl.REST ~
aobest_mtrl.LAGERPLATSI aobest_mtrl.LAGERPLATSC aobest_mtrl.AONRAONR ~
aobest_mtrl.AONRDELNR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MOTAO aobest_mtrl.Antal 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_MOTAO aobest_mtrl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_MOTAO aobest_mtrl
&Scoped-define QUERY-STRING-BRW_MOTAO FOR EACH aobest_mtrl NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_MOTAO OPEN QUERY BRW_MOTAO FOR EACH aobest_mtrl NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_MOTAO aobest_mtrl
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MOTAO aobest_mtrl


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_MOTAO BTN_MOT BTN_AVB BTN_TRUMMA ~
BTN_REST BTN_AND BTN_BORT 

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

DEFINE BUTTON BTN_TRUMMA DEFAULT  NO-CONVERT-3D-COLORS
     LABEL "Trumma" 
     SIZE 12 BY 1 TOOLTIP "Lägg markerat enr på trumma"
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_MOTAO FOR 
      aobest_mtrl SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_MOTAO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MOTAO WINDOW-1 _STRUCTURED
  QUERY BRW_MOTAO NO-LOCK DISPLAY
      aobest_mtrl.Enr FORMAT "X(11)":U
      aobest_mtrl.BENAMNING FORMAT "x(40)":U
      aobest_mtrl.Antal FORMAT ">>>>>9":U
      aobest_mtrl.Pris FORMAT ">>>>99.99":U
      aobest_mtrl.REST FORMAT "Ja/Nej":U
      aobest_mtrl.LAGERPLATSI COLUMN-LABEL "Plats" FORMAT ">>>>9":U
      aobest_mtrl.LAGERPLATSC COLUMN-LABEL "Lagerplats!namn" FORMAT "x(12)":U
      aobest_mtrl.AONRAONR COLUMN-LABEL "Aonr" FORMAT "x(8)":U
      aobest_mtrl.AONRDELNR COLUMN-LABEL "Delnr" FORMAT ">>9":U
  ENABLE
      aobest_mtrl.Antal
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 61.5 BY 23.5
         TITLE "Specifikation av beställning" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_MOTAO AT ROW 1.5 COL 1.5 WIDGET-ID 100
     BTN_MOT AT ROW 27.88 COL 43.88
     BTN_AVB AT ROW 27.88 COL 58.75
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
      TABLE: aobest_mtrl T "?" NO-UNDO temp-db aobest_mtrl
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
/* BROWSE-TAB BRW_MOTAO 1 FRAME-A */
ASSIGN 
       BRW_MOTAO:HIDDEN  IN FRAME FRAME-A                = TRUE.

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

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MOTAO
/* Query rebuild information for BROWSE BRW_MOTAO
     _TblList          = "Temp-Tables.aobest_mtrl"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.aobest_mtrl.Enr
     _FldNameList[2]   = Temp-Tables.aobest_mtrl.BENAMNING
     _FldNameList[3]   > Temp-Tables.aobest_mtrl.Antal
"aobest_mtrl.Antal" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = Temp-Tables.aobest_mtrl.Pris
     _FldNameList[5]   = Temp-Tables.aobest_mtrl.REST
     _FldNameList[6]   > Temp-Tables.aobest_mtrl.LAGERPLATSI
"aobest_mtrl.LAGERPLATSI" "Plats" ">>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.aobest_mtrl.LAGERPLATSC
"aobest_mtrl.LAGERPLATSC" "Lagerplats!namn" "x(12)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.aobest_mtrl.AONRAONR
"aobest_mtrl.AONRAONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.aobest_mtrl.AONRDELNR
"aobest_mtrl.AONRDELNR" "Delnr" ">>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MOTAO */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND WINDOW-1
ON CHOOSE OF BTN_AND IN FRAME FRAME-A /* Ändra antal */
DO:
   {muswait.i}
   antal_valda = BRW_MOTAO:NUM-SELECTED-ROWS.
   antal_raknare = 1.    
   DO WHILE antal_raknare LE antal_valda:   
      status-ok = BRW_MOTAO:FETCH-SELECTED-ROW(antal_raknare).
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(aobest_mtrl)).
      FIND FIRST bmtrlbuf WHERE bmtrlbuf.ENR = aobest_mtrl.ENR AND bmtrlbuf.REST = TRUE NO-LOCK NO-ERROR. 
      IF AVAILABLE bmtrlbuf THEN DO:
         MESSAGE  LC(Guru.Konstanter:genk) + " - " + aobest_mtrl.ENR + "är restrapporterat och går ej att ändra antal för "
         VIEW-AS ALERT-BOX.
      END.
      ELSE IF aobest_mtrl.REST = TRUE THEN DO:
         MESSAGE  LC(Guru.Konstanter:genk) + " - " + aobest_mtrl.ENR + "är restrapporterat och går ej att ändra antal för "
         VIEW-AS ALERT-BOX.
      END.
      ELSE DO:     
         RUN ANBANTV.W (INPUT 2, INPUT ROWID(aobest_mtrl)).
      END.
      
      antal_raknare = antal_raknare + 1.   
   END.
   RUN openbdynspec_UI IN brwproc[1].    
   RUN lastselectdyn_UI IN brwproc[1].
   RUN title_UI IN brwproc[1].
   status-ok = BRW_MOTAO:DESELECT-ROWS().   
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
            FIND FIRST lagertrum WHERE lagertrum.TRUMMANR = lptemp2.TRUMMANR AND lagertrum.TRUMMA = "S" NO-LOCK NO-ERROR.
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
   antal_valda = BRW_MOTAO:NUM-SELECTED-ROWS.
   antal_raknare = 1.    
   DO WHILE antal_raknare LE antal_valda:   
      status-ok = BRW_MOTAO:FETCH-SELECTED-ROW(antal_raknare). 
      IF aobest_mtrl.REST = TRUE THEN DO: 
         MESSAGE "Vill du ta bort restnotering för " + LC(Guru.Konstanter:genk) + " - " + STRING(aobest_mtrl.ENR) VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO UPDATE svar.
         IF svar THEN DO:             
            FIND FIRST bmtrlbuf WHERE bmtrlbuf.ENR = aobest_mtrl.ENR 
            AND bmtrlbuf.REST = FALSE NO-ERROR.
            ASSIGN
            bmtrlbuf.ANTAL = bmtrlbuf.antal + aobest_mtrl.ANTAL.    
            DELETE aobest_mtrl.
            RUN selnextprevrow_UI IN brwproc[1].
         END.
                                             
      END.
      ELSE DO:
         MESSAGE "Vill du ta bort " + LC(Guru.Konstanter:genk) + " - " + STRING(aobest_mtrl.ENR) + " - från beställningen." 
         VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO UPDATE svar. 
         IF svar THEN DO:
            EMPTY TEMP-TABLE eaobest_mtrl  NO-ERROR.
            CREATE eaobest_mtrl.
            BUFFER-COPY aobest_mtrl TO eaobest_mtrl. 
            RUN bortantbestdepao_UI IN leveransaoapph (INPUT vald_depa,INPUT Guru.GlobalaVariabler:GuruProjdepnr, INPUT TABLE eaobest_mtrl).
            EMPTY TEMP-TABLE eaobest_mtrl  NO-ERROR.
            bortenr = aobest_mtrl.ENR.
            FIND FIRST bmtrlbuf WHERE bmtrlbuf.ENR = aobest_mtrl.ENR 
            AND bmtrlbuf.REST = TRUE NO-LOCK NO-ERROR.
            IF AVAILABLE bmtrlbuf THEN DO:
               MESSAGE "Det finns en restnotering för detta " + LC(Guru.Konstanter:genk) + " som också kommer att tas bort."
               VIEW-AS ALERT-BOX.    
               FOR EACH aobest_mtrl WHERE aobest_mtrl.ENR = bortenr:
                  DELETE aobest_mtrl. 
               END.              
            END.
            ELSE DO:
               DELETE aobest_mtrl.
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
   FOR EACH aobest_mtrl WHERE best_mtrl.ANTAL = 0.
      DELETE aobest_mtrl.
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
   /*/*SKA DET GÅ ATT UPPDATERA PRIS? Vad ska då hända?*/
   aobest_mtrl.PRIS:VISIBLE IN BROWSE BRW_MOTAO = FALSE.*/
   /*IF aobest_mtrl.PRIS:VISIBLE IN BROWSE BRW_MOTAO = TRUE THEN DO:   
      FOR EACH aobest_mtrl:
         EMPTY TEMP-TABLE emtrltemp NO-ERROR.          
         RUN hmtskap_UI IN leveransaoapph (INPUT aobest_mtrl.ENR,INPUT aobest_mtrl.LEVKOD,OUTPUT TABLE emtrltemp). 
         FIND FIRST emtrltemp NO-LOCK NO-ERROR.
         IF AVAILABLE emtrltemp THEN DO:
            IF aobest_mtrl.PRIS NE emtrltemp.NPRIS THEN DO:
               varning = TRUE.
               varningenr = aobest_mtrl.ENR.                 
            END.            
         END.
         ELSE DO:
            finnsej = TRUE.
            finnsejenr = aobest_mtrl.ENR + " lev " + aobest_mtrl.LEVKOD.
         END.   
         IF aobest_mtrl.PRIS = 0 THEN nollpris = TRUE.
         
                      
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
         
   END.*/
   
   RUN aook_UI IN leveransaoapph (INPUT vald_depa,INPUT Guru.GlobalaVariabler:GuruProjdepnr, INPUT vald_best, INPUT vald_lev, INPUT TABLE aobest_mtrl, OUTPUT invdat).
   IF Guru.Konstanter:globforetag = "ELPA"  OR 
   Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "LULE"  OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "BODE" OR Guru.Konstanter:globforetag = "KRAF" THEN DO:        
      /*levnamn ne "" då är det en projektbeställning och skall ej på verka lagret*/
      FOR EACH aobest_mtrl WHERE aobest_mtrl.REST = FALSE AND aobest_mtrl.LEVNAMN = "":
         EMPTY TEMP-TABLE eaobest_mtrl  NO-ERROR. 
         CREATE eaobest_mtrl.
         BUFFER-COPY aobest_mtrl TO eaobest_mtrl.
         RUN aook2_UI IN leveransaoapph (INPUT vald_depa,INPUT Guru.GlobalaVariabler:GuruProjdepnr, INPUT TABLE eaobest_mtrl, INPUT invdat, INPUT eaobest_mtrl.ENR ).
                  
         EMPTY TEMP-TABLE eaobest_mtrl  NO-ERROR. 
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
   FIND FIRST aobest_mtrl WHERE aobest_mtrl.REST = TRUE NO-LOCK NO-ERROR.
   IF AVAILABLE aobest_mtrl THEN DO:
      MESSAGE "Spara uppdatering skall bara användas för att uppdatera ändrade priser och antal. Ingen mottagning kan ske samtidigt." SKIP  
              "Du har resrapporterat!"
         VIEW-AS ALERT-BOX.
   END.
   ELSE DO:
         
      RUN aoprisok_UI IN leveransaoapph (INPUT vald_depa,INPUT Guru.GlobalaVariabler:GuruProjdepnr, INPUT vald_best, INPUT vald_lev, INPUT TABLE aobest_mtrl, OUTPUT invdat).
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
   antal_valda = BRW_MOTAO:NUM-SELECTED-ROWS.
   antal_raknare = 1.    
   DO WHILE antal_raknare LE antal_valda:   
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(aobest_mtrl)).
      status-ok = BRW_MOTAO:FETCH-SELECTED-ROW(antal_raknare).
      IF aobest_mtrl.REST = TRUE THEN DO:
         MESSAGE "För att ändra restantalet för " + LC(Guru.Konstanter:genk) + " - " + aobest_mtrl.ENR + " - Markera samma " + LC(Guru.Konstanter:genk) + " men där rest är Nej."
         VIEW-AS ALERT-BOX.
      END.
      ELSE DO:     
         
         RUN RESTANTVAO.W (INPUT ROWID(aobest_mtrl)).
         
      END.   
      antal_raknare = antal_raknare + 1.   
   END.   
   RUN openbdynspec_UI IN brwproc[1].    
   RUN lastselectdyn_UI IN brwproc[1].
   RUN title_UI IN brwproc[1].
   status-ok = BRW_MOTAO:DESELECT-ROWS().   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_TRUMMA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_TRUMMA WINDOW-1
ON CHOOSE OF BTN_TRUMMA IN FRAME FRAME-A /* Trumma */
DO:
   {muswait.i}
   /*antal_valda = BRW_MOTAO:NUM-SELECTED-ROWS.
   antal_raknare = 1.    
   DO WHILE antal_raknare LE antal_valda:   
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(aobest_mtrl)).
      status-ok = BRW_MOTAO:FETCH-SELECTED-ROW(antal_raknare).      
      
      EMPTY TEMP-TABLE elagertrum NO-ERROR. 
      CREATE elagertrum.
      ASSIGN 
      elagertrum.BENAMNING = aobest_mtrl.BENAMNING
      elagertrum.ENHET = aobest_mtrl.ENHET
      elagertrum.ENR = aobest_mtrl.ENR
      elagertrum.STMETER = aobest_mtrl.ANTAL
      elagertrum.LAGMETER = aobest_mtrl.ANTAL
      elagertrum.PRIS = aobest_mtrl.PRIS
      elagertrum.DATUM = TODAY.
      kvarmeter = aobest_mtrl.ANTAL.
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
                  lptemp.METER = aobest_mtrl.ANTAL.
               END.
               ASSIGN
               lptemp.ENR = elagertrum.ENR
               lptemp.RMETER = lptemp.RMETER + elagertrum.STMETER
               lptemp.KMETER = lptemp.METER - lptemp.RMETER.
               /*vilka trummor som lagts upp för att kunna rensa vid avbryt*/
               FIND FIRST lptemp2 WHERE lptemp2.TRUMMANR = lplats NO-ERROR.
               IF NOT AVAILABLE lptemp2  THEN DO:
                  CREATE lptemp2.
                  assign
                  lptemp2.TRUMMANR = lplats
                  lptemp2.ENR = lptemp.ENR. 
               END.
               
            END.
         END.
      END.
      ELSE DO:
         MESSAGE "För " +  LC(Guru.Konstanter:genk) + " - " + aobest_mtrl.ENR + "är redan hela länden registrerad på trumma" VIEW-AS ALERT-BOX.
      END.
      EMPTY TEMP-TABLE elagertrum NO-ERROR. 
      antal_raknare = antal_raknare + 1.   
   END.      
   musz = FALSE.
   status-ok = BRW_MOTAO:DESELECT-ROWS().   
   RUN lastselectdyn_UI IN brwproc[1].*/   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_MOTAO
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
   IF VALID-HANDLE(leveransaoapph) THEN DELETE PROCEDURE leveransaoapph.
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
   
   EMPTY TEMP-TABLE  aobest_mtrl NO-ERROR. 
   EMPTY TEMP-TABLE lptemp NO-ERROR. 
   EMPTY TEMP-TABLE lptemp2 NO-ERROR.
   FIND FIRST eaobest_nr_tab NO-LOCK NO-ERROR.  
         
   RUN aobesthmt_UI IN leveransaoapph ( INPUT TABLE eaobest_nr_tab ,OUTPUT TABLE aobest_mtrl).
   EMPTY TEMP-TABLE eaobest_nr_tab NO-ERROR. 



   /*RUN aobesthmt_UI IN leveransaoapph (INPUT vald_depa,INPUT Guru.GlobalaVariabler:GuruProjdepnr,INPUT vald_best,INPUT vald_lev, OUTPUT TABLE aobest_mtrl).*/                
   RUN setcolsortvar_UI IN brwproc[1] (INPUT "").
   RUN openbdynspec_UI IN brwproc[1].         
   RUN title_UI IN brwproc[1].   
   FIND FIRST depatemp WHERE depatemp.DEP-NR = vald_depa.
   FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_lev NO-LOCK NO-ERROR.
   IF AVAILABLE levtemp THEN DO:
      ASSIGN WINDOW-1:TITLE = "Mottagningsrutin beställning nr: " + STRING(vald_best) + 
      " för leverantör " + levtemp.LEVNAMN.
    END.
    ELSE IF vald_lev = "0" THEN DO: 
       ASSIGN WINDOW-1:TITLE = "Mottagningsrutin beställning nr: " + STRING(vald_best) + 
      " för DEPÅ" .
    END.         
   /*kalmar depåändring*/
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "BODE" OR Guru.Konstanter:globforetag = "gkal" OR Guru.Konstanter:globforetag = "KRAF" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      aobest_mtrl.PRIS:VISIBLE IN BROWSE BRW_MOTAO = TRUE.
   END.
   ELSE DO:
      aobest_mtrl.PRIS:VISIBLE IN BROWSE BRW_MOTAO = FALSE.   
   END.
   /*/*SKA DET GÅ ATT UPPDATERA PRIS? Vad ska då hända?*/
   aobest_mtrl.PRIS:VISIBLE IN BROWSE BRW_MOTAO = FALSE.*/
   IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN DO:
      aobest_mtrl.PRIS:VISIBLE IN BROWSE BRW_MOTAO = FALSE.   
   END.      
      
   RUN enable_UI.   
   IF Guru.Konstanter:globforetag = "snat" THEN DO: 
      FIND FIRST aobest_mtrl  NO-LOCK NO-ERROR.
      IF AVAILABLE aobest_mtrl THEN DO:
         IF aobest_mtrl.TRUMMANR > 0 OR aobest_mtrl.BESTNR > 0 THEN DO:
            BTN_REST:HIDDEN IN FRAME FRAME-A           = TRUE.
            BTN_AND:HIDDEN IN FRAME FRAME-A           = TRUE.
            BTN_BORT:HIDDEN IN FRAME FRAME-A           = TRUE.
         END.   
      END.        
   END.   
   IF Guru.Konstanter:globforetag = "elpa" OR Guru.Konstanter:globforetag = "CSNAT" THEN DO:
      /*INLEVERANS AV TRUMMA SKALL ENBART GÖRAS OM LEVERANSEN SKA IN I LAGER*/
      BTN_TRUMMA:HIDDEN IN FRAME FRAME-A           = FALSE.
   END.    
   ELSE BTN_TRUMMA:HIDDEN IN FRAME FRAME-A           = TRUE.
   
   
   /*kalmar depåändring*/
   IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "gkal"  OR Guru.Konstanter:globforetag = "cSNAT" OR Guru.Konstanter:globforetag = "elpa" THEN ENABLE BTN_PRISOK WITH FRAME {&FRAME-NAME}.
   {FRMSIZE.I}
   
   IF depatitta = TRUE THEN DO:
      DISABLE BTN_MOT WITH FRAME {&FRAME-NAME}.
      DISABLE BTN_BORT WITH FRAME {&FRAME-NAME}.
      DISABLE BTN_REST WITH FRAME {&FRAME-NAME}.
      DISABLE BTN_PRISOK WITH FRAME {&FRAME-NAME}.
   END.
   /*APPLY "ENTRY" TO best_mtrl.BENAMNING IN BROWSE BRW_MOTAO.*/
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
   
   /*dd kalmar depåändring abc sks vara ändringbart*/
   IF Guru.Konstanter:globforetag = "gkal" THEN DO:
      aobest_mtrl.PRIS:READ-ONLY IN BROWSE BRW_MOTAO = TRUE.
      
   END.
   IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
      aobest_mtrl.ANTAL:READ-ONLY IN BROWSE BRW_MOTAO = TRUE.
      
   END. 
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_MOTAO:HANDLE IN FRAME {&FRAME-NAME}).   
   RUN StartSokEnrLev_UI IN brwproc[1] (INPUT THIS-PROCEDURE).   
   IF Guru.Konstanter:appcon THEN DO:
      RUN LEVAPPVAO.P PERSISTENT SET leveransaoapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN LEVAPPVAO.P PERSISTENT SET leveransaoapph.
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
  ENABLE BRW_MOTAO BTN_MOT BTN_AVB BTN_TRUMMA BTN_REST BTN_AND BTN_BORT 
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
   IF brwname = "BRW_MOTAO" THEN valbrw = 1. 
   IF valbrw = 1 THEN DO:
      status-ok = BRW_MOTAO:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.
      IF status-ok THEN DO:
         valenr = aobest_mtrl.ENR.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = aobest_mtrl.LEVKOD
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
   IF AVAILABLE aobest_mtrl THEN DO:      
      DISPLAY
      aobest_mtrl.ANTAL aobest_mtrl.PRIS      
      WITH BROWSE BRW_MOTAO.         
   END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

