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
DEFINE INPUT PARAMETER aonrvar AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER delnrvar AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER nyavtalvar AS LOGICAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER ordnvar AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER idvar AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER lankvar AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER mappvar AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}

&Scoped-define SHARED SHARED 
{DIRDEF.I}
{AONRDEF.I}
{AVTALAONR.I}
{AVTALTEMP.I}
{SOKDEF.I}

DEFINE NEW SHARED VARIABLE brec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE borec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE grundmappvar AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE varhj AS CHARACTER NO-UNDO.
DEFINE VARIABLE extrawinh AS HANDLE NO-UNDO.
DEFINE VARIABLE mappvarhj AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE avtalnum AS INTEGER NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE OKvald AS LOGICAL INITIAL TRUE.
DEFINE VARIABLE melvar AS INTEGER NO-UNDO.
DEFINE VARIABLE langd AS INTEGER NO-UNDO.
DEFINE VARIABLE pos1 AS INTEGER NO-UNDO.
DEFINE VARIABLE nyavtalapph AS HANDLE NO-UNDO.
DEFINE VARIABLE nyttaoejapph AS HANDLE NO-UNDO.

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
&Scoped-define INTERNAL-TABLES avtaltemp

/* Definitions for BROWSE BRW_AVTAL                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_AVTAL avtaltemp.AVTAL 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AVTAL avtaltemp.AVTAL 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_AVTAL avtaltemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_AVTAL avtaltemp
&Scoped-define QUERY-STRING-BRW_AVTAL FOR EACH avtaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_AVTAL OPEN QUERY BRW_AVTAL FOR EACH avtaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_AVTAL avtaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AVTAL avtaltemp


/* Definitions for DIALOG-BOX Dialog-Frame                              */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_AVTAL BTN_DOK FILL-IN-FILNAMN ~
FILL-IN-AVTAL ED_MEDD BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-FILNAMN FILL-IN-AVTAL ED_MEDD 

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

DEFINE BUTTON BTN_DOK 
     LABEL "Välj dokument" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE ED_MEDD AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 55 BY 5.38
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-AVTAL AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FILNAMN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Filnamn" 
     VIEW-AS FILL-IN 
     SIZE 49.13 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AVTAL FOR 
      avtaltemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AVTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AVTAL Dialog-Frame _STRUCTURED
  QUERY BRW_AVTAL NO-LOCK DISPLAY
      avtaltemp.AVTAL FORMAT "X(30)":U WIDTH 49.88
  ENABLE
      avtaltemp.AVTAL
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 55 BY 6.54.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BRW_AVTAL AT ROW 2.08 COL 1.5
     BTN_DOK AT ROW 8 COL 59.5
     FILL-IN-FILNAMN AT ROW 9.75 COL 8.88 COLON-ALIGNED
     FILL-IN-AVTAL AT ROW 13 COL 13.75 NO-LABEL
     ED_MEDD AT ROW 15.46 COL 1.5 NO-LABEL
     BTN_OK AT ROW 22.25 COL 44.5
     BTN_AVB AT ROW 22.25 COL 59.5
     "Anmärkning:" VIEW-AS TEXT
          SIZE 12 BY .67 AT ROW 14.38 COL 16.13
     "Egen text:" VIEW-AS TEXT
          SIZE 13 BY .67 AT ROW 11.92 COL 1.88
     SPACE(59.49) SKIP(10.82)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Nytt tillstånd/avtal"
         DEFAULT-BUTTON BTN_OK CANCEL-BUTTON BTN_AVB.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Temp-Tables and Buffers:
      TABLE: avtaltemp T "?" NO-UNDO temp-db avtaltemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_AVTAL TEXT-2 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       ED_MEDD:RETURN-INSERTED IN FRAME Dialog-Frame  = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-AVTAL IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AVTAL
/* Query rebuild information for BROWSE BRW_AVTAL
     _TblList          = "Temp-Tables.avtaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.avtaltemp.AVTAL
"avtaltemp.AVTAL" ? ? "character" ? ? ? ? ? ? yes ? no no "49.88" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AVTAL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON END-ERROR OF FRAME Dialog-Frame /* Nytt tillstånd/avtal */
DO:
   musz = TRUE.  
   {BORTBRWPROC.I}
   IF VALID-HANDLE(nyavtalapph) THEN DELETE PROCEDURE nyavtalapph.   
   IF VALID-HANDLE(nyttaoejapph) THEN DELETE PROCEDURE nyttaoejapph.   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON ENDKEY OF FRAME Dialog-Frame /* Nytt tillstånd/avtal */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Nytt tillstånd/avtal */
DO:
   {muswait.i}
   musz = TRUE.   
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
      IF SUBSTRING(avtaltemp.AVTAL,1,30) NE "Eget dokument" THEN DO:
            FILL-IN-FILNAMN = "".
            DISPLAY FILL-IN-FILNAMN WITH FRAME {&FRAME-NAME}.
            FILL-IN-FILNAMN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
            BTN_DOK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         END.
         ELSE DO:
            FILL-IN-FILNAMN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
            BTN_DOK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
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


&Scoped-define SELF-NAME BTN_DOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_DOK Dialog-Frame
ON CHOOSE OF BTN_DOK IN FRAME Dialog-Frame /* Välj dokument */
DO:
   ASSIGN
   mappvarhj = aonrvar + STRING(delnrvar) + "\".          
   mappvar = grundmappvar +  mappvarhj.                                         
   FILE-INFO:FILE-NAME = mappvar.
   IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
      MESSAGE "Mappen " mappvar " finns inte. Det är troligen ett sekretessproblem. Kontakta Elpool." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
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
      FIND FIRST avtaltemp WHERE SUBSTRING(avtaltemp.AVTAL,1,30) = "Eget dokument" AND avtaltemp.LANK = lankvar
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
   FILL-IN-AVTAL = INPUT FILL-IN-AVTAL
   FILL-IN-FILNAMN = INPUT FILL-IN-FILNAMN
   ED_MEDD = INPUT ED_MEDD.
   IF FILL-IN-AVTAL = "" THEN DO:
      MESSAGE "Ni måste fylla i en egen text."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
      APPLY "ENTRY" TO FILL-IN-AVTAL IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   IF SUBSTRING(avtaltemp.AVTAL,1,30) = "Eget dokument" THEN DO:
      IF FILL-IN-FILNAMN = "" THEN DO:
         MESSAGE "Ni måste välja ett dokument."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         APPLY "ENTRY" TO FILL-IN-AVTAL IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.      
      IF FILL-IN-FILNAMN BEGINS "www." OR  FILL-IN-FILNAMN BEGINS "http://" THEN.
      ELSE DO:
         FILE-INFO:FILE-NAME = FILL-IN-FILNAMN.                           
         IF SEARCH(FILE-INFO:FULL-PATHNAME) = ? THEN DO:
            MESSAGE FILL-IN-FILNAMN " finns inte. Det är troligen ett sekretessproblem. Kontakta Elpool. Filtyp " FILE-INFO:FILE-TYPE
            VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
         END.
      END.
   END.
   ELSE DO:
      IF nyavtalvar = TRUE THEN DO: 
         IF FILL-IN-FILNAMN BEGINS "www." OR  FILL-IN-FILNAMN BEGINS "http://" THEN.
         ELSE DO:
            FILE-INFO:FILE-NAME = avtaltemp.FILNAMN.                            
            IF SEARCH(FILE-INFO:FULL-PATHNAME) = ? THEN DO:
               MESSAGE avtaltemp.FILNAMN " finns inte. Det är troligen ett sekretessproblem. Kontakta Elpool. Filtyp " FILE-INFO:FILE-TYPE
               VIEW-AS ALERT-BOX.
               RETURN NO-APPLY.
            END.
         END.
      END.
   END.
      /* nyavtalvar = true om ny */
   IF nyavtalvar = TRUE THEN DO: 
      /*lankvar = FALSE är Tillstånd/Avtal*/
      IF lankvar = FALSE THEN DO:      
         FIND LAST avtalaonrtemp WHERE avtalaonrtemp.AONR = aonrvar AND avtalaonrtemp.DELNR = delnrvar AND
         avtalaonrtemp.AVTID = avtaltemp.AVTALID AND avtalaonrtemp.LNK = FALSE 
         USE-INDEX AOAVTAL NO-LOCK NO-ERROR.
         IF AVAILABLE avtalaonrtemp THEN avtalnum = avtalaonrtemp.ORDN + 1.
         ELSE avtalnum = 1.
         RUN skapavtal IN nyavtalapph (INPUT aonrvar,INPUT delnrvar,INPUT avtaltemp.AVTALID,
                                       INPUT avtalnum,INPUT FALSE,INPUT-OUTPUT TABLE avtalaonrtemp). 
         FIND LAST avtalaonrtemp WHERE avtalaonrtemp.AONR = aonrvar AND avtalaonrtemp.DELNR = delnrvar AND
         avtalaonrtemp.AVTID = avtaltemp.AVTALID AND avtalaonrtemp.ORDN = avtalnum AND avtalaonrtemp.LNK = FALSE 
         USE-INDEX AOAVTAL NO-LOCK NO-ERROR.
         
         IF FILL-IN-FILNAMN NE "" AND SUBSTRING(avtaltemp.AVTAL,1,30) = "Eget dokument" THEN DO:
            RUN sokvag_UI.             
         END.
         ELSE DO:
            RUN avtal_UI.
            IF musz = TRUE THEN DO:
               musz = FALSE.
               RETURN NO-APPLY.
            END.
         END.   
      END.
      ELSE DO:
         FIND LAST avtalaonrtemp WHERE avtalaonrtemp.AONR = aonrvar AND avtalaonrtemp.DELNR = delnrvar AND
         avtalaonrtemp.AVTID = avtaltemp.AVTALID AND avtalaonrtemp.LNK = TRUE
         USE-INDEX AOAVTAL NO-LOCK NO-ERROR.
         IF AVAILABLE avtalaonrtemp THEN avtalnum = avtalaonrtemp.ORDN + 1.
         ELSE avtalnum = 1.
         RUN skapavtal IN nyavtalapph (INPUT aonrvar,INPUT delnrvar,INPUT avtaltemp.AVTALID,
                                       INPUT avtalnum,INPUT TRUE,INPUT-OUTPUT TABLE avtalaonrtemp).   
         FIND LAST avtalaonrtemp WHERE avtalaonrtemp.AONR = aonrvar AND avtalaonrtemp.DELNR = delnrvar AND
         avtalaonrtemp.AVTID = avtaltemp.AVTALID AND avtalaonrtemp.ORDN = avtalnum AND avtalaonrtemp.LNK = TRUE NO-LOCK NO-ERROR.
         
         IF FILL-IN-FILNAMN NE "" THEN DO:
            RUN sokvag_UI.             
         END.
         ELSE DO:
            RUN avtal_UI.
            IF musz = TRUE THEN DO:
               musz = FALSE.
               RETURN NO-APPLY.
            END.
         END.
      END.
   END.
   ELSE DO:
      IF lankvar = TRUE THEN DO:     
         IF SUBSTRING(avtaltemp.AVTAL,1,30) = "Eget dokument" THEN DO:
            IF FILL-IN-FILNAMN NE avtalaonrtemp.FIL THEN DO:
               RUN sokvag_UI.             
            END.
         END.
      END.
   END.
   ASSIGN
   ordnvar = avtalaonrtemp.ORDN
   idvar = avtalaonrtemp.AVTID
   avtalaonrtemp.BEN = FILL-IN-AVTAL
   avtalaonrtemp.ANM = ED_MEDD. 
   RUN btnok IN nyavtalapph (INPUT aonrvar,INPUT delnrvar,INPUT avtaltemp.AVTALID,
                             INPUT avtalaonrtemp.ORDN,INPUT FILL-IN-AVTAL,INPUT ED_MEDD,
                             INPUT avtalaonrtemp.FIL,INPUT-OUTPUT TABLE avtalaonrtemp).
   {BORTBRWPROC.I}
   IF VALID-HANDLE(nyavtalapph) THEN DELETE PROCEDURE nyavtalapph. 
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
   FRAME {&FRAME-NAME}:HIDDEN = FALSE.
   RUN avthmt IN nyavtalapph (INPUT aonrvar,INPUT delnrvar,
                              OUTPUT TABLE avtaltemp).
   FIND FIRST avtaltemp  NO-LOCK NO-ERROR.
   IF NOT AVAILABLE avtaltemp THEN DO:
      RUN skapeget IN nyavtalapph (OUTPUT TABLE avtaltemp).         
   END.
   IF nyavtalvar = TRUE THEN DO:
      IF lankvar = FALSE THEN
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Nytt tillstånd/avtal".
      ELSE ASSIGN FRAME {&FRAME-NAME}:TITLE = "Nytt dokument".
   END.
   ELSE DO:      
      FIND FIRST avtalaonrtemp WHERE avtalaonrtemp.AONR = aonrvar AND
      avtalaonrtemp.DELNR = delnrvar AND avtalaonrtemp.AVTID = idvar AND 
      avtalaonrtemp.ORDN = ordnvar NO-LOCK NO-ERROR.
      IF lankvar = FALSE THEN DO:
         ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ändring av tillstånd/avtal " + avtalaonrtemp.BEN.
      END.      
      ELSE DO: 
         ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ändring av dokument " + avtalaonrtemp.BEN
         FILL-IN-FILNAMN = avtalaonrtemp.FIL.
      END.
      ASSIGN
      FILL-IN-AVTAL = avtalaonrtemp.BEN      
      ED_MEDD = avtalaonrtemp.ANM.
   END.
   IF lankvar = FALSE THEN DO:      
      avtaltemp.AVTAL:LABEL IN BROWSE BRW_AVTAL = "Tillstånd/Avtal".
   END.
   ELSE DO:
      avtaltemp.AVTAL:LABEL IN BROWSE BRW_AVTAL = "Dokument".
   END.
   RUN enable_UI.       
   {FRMSIZED.I}
      
   IF lankvar = FALSE THEN DO:
      ASSIGN
      FILL-IN-FILNAMN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_DOK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      varhj = ' WHERE avtaltemp.LANK = FALSE AND SUBSTRING(avtaltemp.AVTAL,35,7) = ""'.
      
      RUN setcolsortvar_UI IN brwproc[1] (INPUT varhj).
      RUN openbdynspec_UI IN brwproc[1].      
   END.
   ELSE DO:      
      varhj = ' WHERE avtaltemp.LANK = true AND SUBSTRING(avtaltemp.AVTAL,35,7) = ""'.
      RUN setcolsortvar_UI IN brwproc[1] (INPUT varhj).
      RUN openbdynspec_UI IN brwproc[1].      
      FIND FIRST avtaltemp WHERE avtaltemp.LANK = TRUE 
      AND SUBSTRING(avtaltemp.AVTAL,35,7) = ""
      USE-INDEX ORD NO-LOCK NO-ERROR.
      IF AVAILABLE avtaltemp THEN APPLY "VALUE-CHANGED" TO BRW_AVTAL
      IN FRAME {&FRAME-NAME}.
   END.
   IF nyavtalvar = FALSE THEN DO:
      FIND FIRST avtaltemp WHERE avtaltemp.AVTALID = avtalaonrtemp.AVTID USE-INDEX ID
      NO-LOCK NO-ERROR.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(avtaltemp)).
      RUN lastselectdyn_UI IN brwproc[1].         
      FILL-IN-FILNAMN = avtalaonrtemp.FIL.
      DISPLAY FILL-IN-FILNAMN WITH FRAME {&FRAME-NAME}.
      DISABLE BRW_AVTAL WITH FRAME {&FRAME-NAME}.
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
   IF Guru.Konstanter:appcon THEN DO:
      RUN NYAVTALAPP.P PERSISTENT SET nyavtalapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NYAVTALAPP.P PERSISTENT SET nyavtalapph.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avtal_UI Dialog-Frame 
PROCEDURE avtal_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
   
   ASSIGN
   mappvarhj = aonrvar + STRING(delnrvar) + "\".          
   mappvar = grundmappvar +  mappvarhj.                                         
   FILE-INFO:FILE-NAME = mappvar.
   IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
      MESSAGE "Mappen " mappvar " finns inte. Det är troligen ett sekretessproblem. Kontakta Elpool." VIEW-AS ALERT-BOX.
      musz = TRUE.
      RETURN.
   END.
   mappvarhj = aonrvar + STRING(delnrvar) + "\" + SUBSTRING(avtaltemp.AVTAL,1,30) + " " + aonrvar + STRING(delnrvar) + STRING(avtalaonrtemp.AVTID) + STRING(avtalaonrtemp.ORDN).          
   mappvar = grundmappvar +  mappvarhj.                                         
   IF SUBSTRING(avtaltemp.FILNAMN,LENGTH(avtaltemp.FILNAMN) - 2,3) = "dot" THEN DO:
      mappvar = mappvar + ".doc".
   END.
   ELSE IF SUBSTRING(avtaltemp.FILNAMN,LENGTH(avtaltemp.FILNAMN) - 2,3) = "xlt" THEN DO:
      mappvar = mappvar + ".xls".               
   END.
   ELSE DO:
      IF INDEX(avtaltemp.FILNAMN,".") > 0 THEN DO: 
         mappvar = mappvar + SUBSTRING(avtaltemp.FILNAMN,INDEX(avtaltemp.FILNAMN,".")).
      END.
      ELSE mappvar = mappvar.          
   END.
   RUN osmappvar IN nyttaoejapph (INPUT avtaltemp.FILNAMN,INPUT mappvar).                      
   ASSIGN
   avtalaonrtemp.FIL = mappvar
   mappvar = ?.

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
  DISPLAY FILL-IN-FILNAMN FILL-IN-AVTAL ED_MEDD 
      WITH FRAME Dialog-Frame.
  ENABLE BRW_AVTAL BTN_DOK FILL-IN-FILNAMN FILL-IN-AVTAL ED_MEDD BTN_OK BTN_AVB 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sokvag_UI Dialog-Frame 
PROCEDURE sokvag_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
   mappvarhj = aonrvar + STRING(delnrvar) + "\".          
   mappvar = grundmappvar +  mappvarhj.                                         
   IF FILL-IN-FILNAMN BEGINS mappvar THEN DO:
      ASSIGN
      avtalaonrtemp.FIL = FILL-IN-FILNAMN
      mappvar = ?.
   END.
   ELSE DO:
      MESSAGE "Vill du kopiera filen till projektmappen?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Kopiera till projektmapp"
      UPDATE answer AS LOGICAL.
      IF answer THEN DO:
         ASSIGN
         langd = LENGTH(FILL-IN-FILNAMN)
         pos1 = 1
         melvar = INDEX(FILL-IN-FILNAMN,"\",pos1).
         IF melvar NE 0 THEN DO:
            DO WHILE melvar < langd:                                     
               ASSIGN
               pos1 = melvar + 1
               melvar = INDEX(FILL-IN-FILNAMN,"\",pos1).
               IF melvar = 0 THEN melvar = 100000.
            END.                    
         END.               
         mappvar = mappvar + SUBSTRING(FILL-IN-FILNAMN,pos1,LENGTH(FILL-IN-FILNAMN) - 1).
         RUN osmappvar IN nyttaoejapph (INPUT filnamn,INPUT mappvar).
         ASSIGN
         avtalaonrtemp.FIL = mappvar
         mappvar = ?.
      END.
      ELSE DO:
         ASSIGN
         avtalaonrtemp.FIL = FILL-IN-FILNAMN
         mappvar = ?.
      END.
   END.   
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

