&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
/*          This .W file was created with the Progress AppBulder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
{AOTIDKOPP.I}
{BESTSTATTEMP.I}
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR eaotidkopp.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR eaotidslagtemp.
DEFINE INPUT PARAMETER valet AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */

{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}

&Scoped-define SHARED SHARED 
{DIRDEF.I}
{AONRDEF.I}
{AVTALTEMP.I}
{SOKDEF.I}
DEFINE SHARED VARIABLE grundmappvar AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE OKvald AS LOGICAL INITIAL TRUE.
DEFINE VARIABLE mappvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE melvar AS INTEGER NO-UNDO.
DEFINE VARIABLE langd AS INTEGER NO-UNDO.
DEFINE VARIABLE pos1 AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE tidlkopdokh AS HANDLE NO-UNDO.
DEFINE VARIABLE nyttaoejapph AS HANDLE NO-UNDO.                              /* dokhant.P */
DEFINE VARIABLE fil AS CHARACTER NO-UNDO.
DEFINE VARIABLE edmedd AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BRW_AVTAL

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES avtaltemp bestatlevtemp

/* Definitions for BROWSE BRW_AVTAL                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_AVTAL avtaltemp.AVTAL 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AVTAL avtaltemp.AVTAL 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_AVTAL avtaltemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_AVTAL avtaltemp
&Scoped-define QUERY-STRING-BRW_AVTAL FOR EACH avtaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_AVTAL OPEN QUERY BRW_AVTAL FOR EACH avtaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_AVTAL avtaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AVTAL avtaltemp


/* Definitions for BROWSE BRW_STATUS                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_STATUS bestatlevtemp.DATUM ~
bestatlevtemp.TID bestatlevtemp.BESTALLD bestatlevtemp.LEVNAMN ~
bestatlevtemp.BERDATUM bestatlevtemp.ANVANDARE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_STATUS 
&Scoped-define QUERY-STRING-BRW_STATUS FOR EACH bestatlevtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_STATUS OPEN QUERY BRW_STATUS FOR EACH bestatlevtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_STATUS bestatlevtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_STATUS bestatlevtemp


/* Definitions for DIALOG-BOX Dialog-Frame                              */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_AVTAL BRW_STATUS FILL-IN-FILNAMN BTN_FIL ~
ED_MEDD BTN_OK BTN_AVB FILL-IN-NAMN FILL-IN-ANM 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-FILNAMN ED_MEDD FILL-IN-NAMN ~
FILL-IN-ANM 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_FIL 
     LABEL "Välj fil" 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE ED_MEDD AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 55 BY 5.38
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-ANM AS CHARACTER FORMAT "X(256)":U INITIAL "Anmärkning:" 
      VIEW-AS TEXT 
     SIZE 14 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-FILNAMN AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 55 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-NAMN AS CHARACTER FORMAT "X(256)":U INITIAL "Filnamn:" 
      VIEW-AS TEXT 
     SIZE 14 BY .67 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AVTAL FOR 
      avtaltemp SCROLLING.

DEFINE QUERY BRW_STATUS FOR 
      bestatlevtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AVTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AVTAL Dialog-Frame _STRUCTURED
  QUERY BRW_AVTAL NO-LOCK DISPLAY
      avtaltemp.AVTAL COLUMN-LABEL "Dokument" FORMAT "X(30)":U
            WIDTH 49.88
  ENABLE
      avtaltemp.AVTAL
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 53 BY 6.54.

DEFINE BROWSE BRW_STATUS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_STATUS Dialog-Frame _STRUCTURED
  QUERY BRW_STATUS NO-LOCK DISPLAY
      bestatlevtemp.DATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
      bestatlevtemp.TID FORMAT "99.99":U
      bestatlevtemp.BESTALLD FORMAT "X(18)":U
      bestatlevtemp.LEVNAMN COLUMN-LABEL "Leverantör" FORMAT "x(25)":U
      bestatlevtemp.BERDATUM COLUMN-LABEL "Ber.datum" FORMAT "99/99/99":U
      bestatlevtemp.ANVANDARE COLUMN-LABEL "Användare" FORMAT "x(12)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 85 BY 7
         TITLE "Beställningsstatus".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BRW_AVTAL AT ROW 2.08 COL 17.38
     BRW_STATUS AT ROW 8 COL 1.5
     FILL-IN-FILNAMN AT ROW 11.38 COL 16.38 NO-LABEL
     BTN_FIL AT ROW 13 COL 37.88
     ED_MEDD AT ROW 16 COL 16.38 NO-LABEL
     BTN_OK AT ROW 22.25 COL 57.5
     BTN_AVB AT ROW 22.25 COL 72.5
     FILL-IN-NAMN AT ROW 10.54 COL 16.63 NO-LABEL
     FILL-IN-ANM AT ROW 15.17 COL 36.88 NO-LABEL
     SPACE(35.99) SKIP(7.53)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Koppla dokument till tidläge"
         DEFAULT-BUTTON BTN_OK CANCEL-BUTTON BTN_AVB.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Temp-Tables and Buffers:
      TABLE: avtaltemp T "?" NO-UNDO temp-db avtaltemp
      TABLE: bestatlevtemp T "?" NO-UNDO temp-db bestatlevtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_AVTAL 1 Dialog-Frame */
/* BROWSE-TAB BRW_STATUS BRW_AVTAL Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       BRW_STATUS:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

ASSIGN 
       ED_MEDD:RETURN-INSERTED IN FRAME Dialog-Frame  = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-ANM IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-FILNAMN IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-NAMN IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AVTAL
/* Query rebuild information for BROWSE BRW_AVTAL
     _TblList          = "Temp-Tables.avtaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.avtaltemp.AVTAL
"avtaltemp.AVTAL" "Dokument" ? "character" ? ? ? ? ? ? yes ? no no "49.88" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AVTAL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_STATUS
/* Query rebuild information for BROWSE BRW_STATUS
     _TblList          = "Temp-Tables.bestatlevtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.bestatlevtemp.DATUM
"bestatlevtemp.DATUM" "Datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   = Temp-Tables.bestatlevtemp.TID
     _FldNameList[3]   = Temp-Tables.bestatlevtemp.BESTALLD
     _FldNameList[4]   > Temp-Tables.bestatlevtemp.LEVNAMN
"bestatlevtemp.LEVNAMN" "Leverantör" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.bestatlevtemp.BERDATUM
"bestatlevtemp.BERDATUM" "Ber.datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.bestatlevtemp.ANVANDARE
"bestatlevtemp.ANVANDARE" "Användare" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_STATUS */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON END-ERROR OF FRAME Dialog-Frame /* Koppla dokument till tidläge */
DO:
   {muswait.i}
   FOR EACH eaotidkopp:
      DELETE eaotidkopp.
   END.
   FOR EACH eaotidslagtemp:
      DELETE eaotidslagtemp.
   END.  
   {BORTBRWPROC.I}
   IF VALID-HANDLE(tidlkopdokh) THEN DELETE PROCEDURE tidlkopdokh.   
   IF VALID-HANDLE(nyttaoejapph) THEN DELETE PROCEDURE nyttaoejapph.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON ENDKEY OF FRAME Dialog-Frame /* Koppla dokument till tidläge */
DO:
   FOR EACH eaotidkopp:
      DELETE eaotidkopp.
   END.
   FOR EACH eaotidslagtemp:
      DELETE eaotidslagtemp.
   END.  
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Koppla dokument till tidläge */
DO:
   FOR EACH eaotidkopp:
      DELETE eaotidkopp.
   END.
   FOR EACH eaotidslagtemp:
      DELETE eaotidslagtemp.
   END.  
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AVTAL
&Scoped-define SELF-NAME BRW_AVTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AVTAL Dialog-Frame
ON VALUE-CHANGED OF BRW_AVTAL IN FRAME Dialog-Frame
DO:
   status-ok = BRW_AVTAL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF AVAILABLE avtaltemp THEN DO:
      BRW_AVTAL:TOOLTIP = avtaltemp.ANMARK.   
      IF avtaltemp.AVTAL NE "Eget dokument" THEN DO:
         FILL-IN-FILNAMN = "".
         DISPLAY FILL-IN-FILNAMN WITH FRAME {&FRAME-NAME}.
         FILL-IN-NAMN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         FILL-IN-FILNAMN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         BTN_FIL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      END.
      ELSE DO:
         FILL-IN-NAMN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         FILL-IN-FILNAMN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         BTN_FIL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB Dialog-Frame
ON CHOOSE OF BTN_AVB IN FRAME Dialog-Frame /* Avbryt */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FIL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FIL Dialog-Frame
ON CHOOSE OF BTN_FIL IN FRAME Dialog-Frame /* Välj fil */
DO:
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 77.
   {SOKANROP.I}
   mappvar = grundmappvar + eaotidkopp.AONR + STRING(eaotidkopp.DELNR) + "\".
   /*
   IF soktemp.SOKCHAR[1] = "GRANNORD"  THEN DO:
     mappvar = "\\SRV00486\APPL\GURU\DOKUMENT\GRAN\" + eaotidkopp.AONR + STRING(eaotidkopp.DELNR) + "\".                  
      /*
      mappvar = "\\DOKUGRAN\DOKUMENT\NÄT (N)\GURU\DOKUMENT\GRAN\" + aotidkopp.AONR + STRING(aotidkopp.DELNR) + "\".         
      */
   END.
   ELSE IF soktemp.SOKCHAR[1] = "GRANES"  THEN DO:
      mappvar = "\\SRV00486\APPL\GURU\DOKUMENT\GRAN\" + eaotidkopp.AONR + STRING(eaotidkopp.DELNR) + "\".            
         /*
      mappvar = "\\DOKUGRAN\DOKUMENT\NÄT (N)\GURU\DOKUMENT\GRAN\" + aotidkopp.AONR + STRING(aotidkopp.DELNR) + "\".         
      */
   END.
   ELSE IF (globforetag = "GRAN" AND soktemp.SOKCHAR[1] = "UTBI") THEN DO:
      mappvar = "\\SRV00486\APPL\GURU\DOKUMENT\GRANUTBI\" + eaotidkopp.AONR + STRING(eaotidkopp.DELNR) + "\".         
   END.
   ELSE IF soktemp.SOKCHAR[1] = "GRANIT" THEN DO:
      mappvar = "\\SRV00486\APPL\GURU\DOKUMENT\GRANIT\" + eaotidkopp.AONR + STRING(eaotidkopp.DELNR) + "\".         
   END.
   ELSE IF soktemp.SOKCHAR[1] = "GRANADM9" THEN DO:
      mappvar = "\\SRV00486\APPL\GURU\DOKUMENT\GRANADM\" + eaotidkopp.AONR + STRING(eaotidkopp.DELNR) + "\".         
   END.
   ELSE IF soktemp.SOKCHAR[1] = "GRANSOL" THEN DO:
      mappvar = "\\SRV00486\APPL\GURU\DOKUMENT\GRANSOL\" + eaotidkopp.AONR + STRING(eaotidkopp.DELNR) + "\".         
   END.
   ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
      mappvar = "\\SERVER04\d\dokument\DOKUMENT\" + eaotidkopp.AONR + STRING(eaotidkopp.DELNR) + "\".
   END.
   ELSE IF soktemp.SOKCHAR[1] = "LULE" THEN DO:
      mappvar = "N:\Planering\Projekt_GURU\" + eaotidkopp.AONR + STRING(eaotidkopp.DELNR) + "\".
      
   END.
   ELSE IF Guru.Konstanter:globforetag = "LULE" AND soktemp.SOKCHAR[1] = "UTBI" THEN DO:
      mappvar = "N:\Planering\Projekt_GURU\UTBILDNING\" + eaotidkopp.AONR + STRING(eaotidkopp.DELNR) + "\".
   END.
   ELSE IF soktemp.SOKCHAR[1] = "GKAL" THEN DO:
      mappvar = "\\goliat\GURU\DOKUMENT\" + eaotidkopp.AONR + STRING(eaotidkopp.DELNR) + "\".            
      /*mappvar = "\\Prosit.energi-miljo.se\Gemensam\GURU\DOKUMENT\" + aotidkopp.AONR + STRING(aotidkopp.DELNR) + "\". */
   END.
   ELSE IF Guru.Konstanter:globforetag = "GKAL" AND soktemp.SOKCHAR[1] = "UTBI" THEN DO:
      mappvar = "\\goliat\GURU\DOKUMENT\UTBI\" + eaotidkopp.AONR + STRING(eaotidkopp.DELNR) + "\".         
   END.
   ELSE IF Guru.Konstanter:globforetag = "TRAS" THEN DO:   
      mappvar = "C:\PRO9\GURU\DOKUMENT\" + eaotidkopp.AONR + STRING(eaotidkopp.DELNR) + "\".
   END.
   ELSE IF Guru.Konstanter:globforetag = "PINN" THEN DO:   
      mappvar = SESSION:TEMP-DIRECTORY + eaotidkopp.AONR + STRING(eaotidkopp.DELNR) + "\".
   END.
   ELSE DO:
      mappvar = SESSION:TEMP-DIRECTORY.
   END. 
   */   
   SYSTEM-DIALOG GET-FILE filnamn
   TITLE          "Välj den fil som Ni vill koppla"
   FILTERS        "All Files (*.*)"  "*.*"   
   INITIAL-DIR    mappvar
   MUST-EXIST         
   USE-FILENAME
   UPDATE OKvald.   
   IF OKvald = TRUE THEN DO:
      ASSIGN
      FILL-IN-FILNAMN = filnamn.
      DISPLAY FILL-IN-FILNAMN WITH FRAME {&FRAME-NAME}.
      FIND FIRST avtaltemp WHERE avtaltemp.AVTAL = "Eget dokument" AND avtaltemp.LANK = TRUE
      NO-LOCK NO-ERROR.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(avtaltemp)).
      RUN lastselectdyn_UI IN brwproc[1].      
   END.
   ELSE DO:
      FILL-IN-FILNAMN = "".
      DISPLAY FILL-IN-FILNAMN WITH FRAME {&FRAME-NAME}.      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK Dialog-Frame
ON CHOOSE OF BTN_OK IN FRAME Dialog-Frame /* Ok */
DO:

   ASSIGN
   ED_MEDD = INPUT ED_MEDD
   FILL-IN-FILNAMN = INPUT FILL-IN-FILNAMN.
   ASSIGN
   edmedd = ED_MEDD
   fil = FILL-IN-FILNAMN.
   IF AVAILABLE avtaltemp THEN DO:
      RUN btnoku IN tidlkopdokh (INPUT valet,INPUT avtaltemp.AVTAL, INPUT Guru.Konstanter:globforetag,
                                 INPUT eaotidkopp.AONR, 
                                 INPUT eaotidkopp.DELNR,  
      
                                 INPUT eaotidkopp.IDTIDLAG,
                                 INPUT-OUTPUT mappvar,
                                 INPUT-OUTPUT edmedd,
                                 INPUT-OUTPUT fil,
                                 OUTPUT TABLE felmeddtemp).
   END.
   ELSE DO:
      RUN btnoku IN tidlkopdokh (INPUT valet,INPUT "", INPUT Guru.Konstanter:globforetag,
                                 INPUT eaotidkopp.AONR, 
                                 INPUT eaotidkopp.DELNR,  
                                 INPUT eaotidkopp.IDTIDLAG,
                                 INPUT-OUTPUT mappvar,
                                 INPUT-OUTPUT edmedd,
                                 INPUT-OUTPUT fil,
                                 OUTPUT TABLE felmeddtemp).
   END.
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      IF felmeddtemp.VAL = 1 THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX TITLE "Meddelande".               
         DELETE felmeddtemp.   
         RETURN NO-APPLY.
      END.
      IF felmeddtemp.VAL = 2 THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX 
         QUESTION BUTTONS YES-NO TITLE "Kopiera till projektmapp"
         UPDATE answer AS LOGICAL.
         IF answer THEN DO:
            ASSIGN
            langd = LENGTH(fil)
            pos1 = 1
            melvar = INDEX(fil,"\",pos1).
            IF melvar NE 0 THEN DO:
               DO WHILE melvar < langd:                                     
                  ASSIGN
                  pos1 = melvar + 1
                  melvar = INDEX(fil,"\",pos1).
                  IF melvar = 0 THEN melvar = 100000.
               END.                    
            END.               
            mappvar = mappvar + SUBSTRING(fil,pos1,LENGTH(fil) - 1).
            RUN osmappvar IN nyttaoejapph (INPUT fil,INPUT mappvar).                    
         END.
         RUN svaru IN tidlkopdokh (INPUT answer,
                                   INPUT eaotidkopp.AONR, 
                                   INPUT eaotidkopp.DELNR,  
                                   INPUT eaotidkopp.IDTIDLAG,
                                   INPUT-OUTPUT mappvar,
                                   INPUT-OUTPUT fil).                 
      END.
   END.   
   IF mappvar NE ? THEN DO:                                                  
      RUN osmappvar IN nyttaoejapph (INPUT fil,INPUT mappvar).        
      ASSIGN
      fil = mappvar
      mappvar = ?.
   END.   
   FIND FIRST eaotidkopp NO-LOCK NO-ERROR.
   IF valet = 1 THEN DO:
      ASSIGN eaotidkopp.FILNAMN = fil.
   END.
   IF valet = 2 THEN DO:
      IF fil = "" AND edmedd = "" THEN DO:             
         IF AVAILABLE eaotidkopp THEN DELETE eaotidkopp.
      END.
      ELSE DO:
         ASSIGN eaotidkopp.ANMARK = edmedd.
      END.
   END.
   {BORTBRWPROC.I}
   IF VALID-HANDLE(tidlkopdokh) THEN DELETE PROCEDURE tidlkopdokh.  
   IF VALID-HANDLE(nyttaoejapph) THEN DELETE PROCEDURE nyttaoejapph.     
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
   RUN avthmt IN tidlkopdokh (OUTPUT TABLE avtaltemp).
   FRAME Dialog-Frame:TITLE = "Koppla dokument till " + LC(Guru.Konstanter:gtidlk).
   IF valet = 1 THEN ASSIGN FRAME {&FRAME-NAME}:TITLE = "Koppla dokument".                 
   ELSE ASSIGN FRAME {&FRAME-NAME}:TITLE = "Koppla anmärkning".
   FIND FIRST eaotidkopp NO-LOCK NO-ERROR.      
   IF NOT AVAILABLE eaotidkopp THEN DO:
      FIND FIRST eaotidslagtemp NO-LOCK NO-ERROR.   
      IF AVAILABLE eaotidslagtemp THEN DO:
         CREATE eaotidkopp.
         ASSIGN
         eaotidkopp.AONR = eaotidslagtemp.AONR
         eaotidkopp.DELNR = eaotidslagtemp.DELNR
         eaotidkopp.IDTIDLAG = eaotidslagtemp.IDTIDLAG.         
      END.
   END.
   ELSE DO:
      ASSIGN
      ED_MEDD = eaotidkopp.ANMARK
      FILL-IN-FILNAMN = eaotidkopp.FILNAMN.      
   END.
   RUN enable_UI.          
   {FRMSIZED.I}
   IF valet = 1 THEN DO:
      OPEN QUERY BRW_AVTAL FOR EACH avtaltemp WHERE avtaltemp.LANK = TRUE 
      NO-LOCK USE-INDEX ORD.
      GET FIRST BRW_AVTAL NO-LOCK.
     
      IF eaotidkopp.FILNAMN NE "" THEN DO:         
         FIND FIRST avtaltemp WHERE avtaltemp.AVTAL = "Eget dokument" NO-LOCK NO-ERROR.          
         IF AVAILABLE avtaltemp THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(avtaltemp)).
            RUN lastselectdyn_UI IN brwproc[1].  
         END.                  
         BTN_FIL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.
      ELSE DO:
         BTN_FIL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      END.
      ASSIGN
      BRW_STATUS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-ANM:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      ED_MEDD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   ELSE DO:
      ASSIGN
      BRW_AVTAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_FIL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-NAMN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-FILNAMN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      IF AVAILABLE eaotidslagtemp THEN DO:
         IF eaotidslagtemp.IDTIDLAG = "Materiel" OR eaotidslagtemp.IDTIDLAG = "MBESTÄLLT" THEN DO:
            RUN beststatus_UI IN tidlkopdokh (INPUT eaotidkopp.AONR,INPUT eaotidkopp.DELNR, OUTPUT TABLE bestatlevtemp).  
            BRW_STATUS:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
            RUN openbdynspec_UI IN brwproc[2]. 
         END.
         ELSE BRW_STATUS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      END.
      ELSE BRW_STATUS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   APPLY "VALUE-CHANGED" TO BRW_AVTAL.
   {musarrow.i}
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
   ASSIGN
   avtaltemp.AVTAL:READ-ONLY IN BROWSE BRW_AVTAL = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_AVTAL:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_STATUS:HANDLE IN FRAME {&FRAME-NAME}).
   IF Guru.Konstanter:appcon THEN DO:
      RUN TIDLKOPDAPP.P PERSISTENT SET tidlkopdokh ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN TIDLKOPDAPP.P PERSISTENT SET tidlkopdokh.
   END.
  
   IF Guru.Konstanter:varforetypval[21] = 1 THEN DO:
      RUN DOKHANT.P PERSISTENT SET nyttaoejapph.
   END.
   ELSE DO:
      IF Guru.Konstanter:appcon THEN DO:
         RUN DOKHANTAPP.P PERSISTENT SET nyttaoejapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      END.
      ELSE DO:
         RUN DOKHANTAPP.P PERSISTENT SET nyttaoejapph.
      END.   
   END.   
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
  DISPLAY FILL-IN-FILNAMN ED_MEDD FILL-IN-NAMN FILL-IN-ANM 
      WITH FRAME Dialog-Frame.
  ENABLE BRW_AVTAL BRW_STATUS FILL-IN-FILNAMN BTN_FIL ED_MEDD BTN_OK BTN_AVB 
         FILL-IN-NAMN FILL-IN-ANM 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

