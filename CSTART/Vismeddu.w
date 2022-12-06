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

  Created: 03/25/96 -  4:43 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}


DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE varmess AS CHARACTER NO-UNDO.
DEFINE VARIABLE varerror AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
{VALDBTEMP.I}
    
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE knappval AS CHARACTER NO-UNDO.

DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE vismeddh AS HANDLE NO-UNDO.
DEFINE VARIABLE meddrec AS RECID NO-UNDO.
DEFINE VARIABLE apphandgran AS HANDLE NO-UNDO.
DEFINE VARIABLE appcongran AS LOGICAL NO-UNDO.
DEFINE VARIABLE mus-hand AS WIDGET-HANDLE NO-UNDO.
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .
DEFINE TEMP-TABLE flermeddtemp
  FIELD EDATUM AS DATE 
  FIELD EMOTAGET AS LOGICAL 
  FIELD MEDD AS CHARACTER 
  FIELD MOTTAGARE AS CHARACTER 
  FIELD SANDARE AS CHARACTER 
  FIELD SDATUM AS DATE
  FIELD FORETAG AS CHARACTER
  FIELD MEDREC AS RECID
  INDEX MEDD IS PRIMARY FORETAG SANDARE SDATUM MOTTAGARE EMOTAGET 
  INDEX MEDD2 FORETAG SANDARE MOTTAGARE  
  INDEX MOTTAGARE FORETAG MOTTAGARE EMOTAGET SANDARE. 
DEFINE QUERY flermeddq FOR flermeddtemp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS EDITOR_MEDD BTN_SKR BTN_SPARA BTN_OK BNT_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_SANDARE FILL-IN_SDATUM EDITOR_MEDD 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BNT_AVB AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK-2 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKR 
     LABEL "Skriv ut" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SPARA 
     LABEL "Spara" 
     SIZE 14 BY 1.

DEFINE VARIABLE EDITOR_MEDD AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 53 BY 20 NO-UNDO.

DEFINE VARIABLE FILL-IN-DB AS CHARACTER FORMAT "X(256)":U 
     LABEL "Databas" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_SANDARE AS CHARACTER FORMAT "x(12)" 
     LABEL "Sändare" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.

DEFINE VARIABLE FILL-IN_SDATUM AS DATE FORMAT "99/99/99" 
     LABEL "Sänt datum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN-DB AT ROW 1.5 COL 36.75 COLON-ALIGNED
     FILL-IN_SANDARE AT ROW 3.5 COL 8.38 COLON-ALIGNED
     FILL-IN_SDATUM AT ROW 3.5 COL 35.88 COLON-ALIGNED
     EDITOR_MEDD AT ROW 5.5 COL 1.5 NO-LABEL
     BTN_SKR AT ROW 8 COL 55.5
     BTN_SPARA AT ROW 25.71 COL 25.5
     BTN_OK AT ROW 25.71 COL 40.5
     BTN_OK-2 AT ROW 25.71 COL 40.5
     BNT_AVB AT ROW 25.71 COL 55.5
     "Meddelande från:" VIEW-AS TEXT
          SIZE 25 BY 1.29 AT ROW 1.5 COL 1.5
          FONT 17
     SPACE(43.74) SKIP(24.20)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Meddelande till Guruanvändare".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON BTN_OK-2 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BTN_OK-2:HIDDEN IN FRAME DIALOG-1           = TRUE.

ASSIGN 
       EDITOR_MEDD:READ-ONLY IN FRAME DIALOG-1        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-DB IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-DB:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_SANDARE IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_SDATUM IN FRAME DIALOG-1
   NO-ENABLE                                                            */
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

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Meddelande till Guruanvändare */
DO:
   RUN avb_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Meddelande till Guruanvändare */
DO:
   RUN avb_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BNT_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BNT_AVB DIALOG-1
ON CHOOSE OF BNT_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
   RUN avb_UI.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:      
   IF vart = "MST" THEN APPLY "GO" TO BTN_OK IN FRAME {&FRAME-NAME}.
   /*EJ LÄSTA MEDELANDEN MEDDSTA.W*/
   ELSE DO:
      GET FIRST flermeddq.          
      IF AVAILABLE flermeddtemp THEN DO:
         IF flermeddtemp.SANDARE = "" THEN. 
         ELSE DO:
            IF flermeddtemp.SANDARE = "FAKT.ADM." OR flermeddtemp.SANDARE = "IFSresponse" THEN DO:
               flermeddtemp.EMOTAGET = TRUE.              
               flermeddtemp.FORETAG = "spara".
               RUN medbort_UI IN vismeddh (INPUT TABLE flermeddtemp).            
            END.
            ELSE DO:
               flermeddtemp.FORETAG = "tabort".
               RUN medbort_UI IN vismeddh (INPUT TABLE flermeddtemp).            
            END.
         END.
         DELETE flermeddtemp.
      END.    
      GET NEXT flermeddq NO-LOCK.
      IF NOT AVAILABLE flermeddtemp THEN DO:
         IF Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79)   THEN DO:
            RUN flermed_UI.      
            RUN visa_UI.
            GET FIRST flermeddq NO-LOCK.
            IF NOT AVAILABLE flermeddtemp THEN DO:
               APPLY "GO" TO BTN_OK IN FRAME {&FRAME-NAME}.
            END.               
         END.
         ELSE APPLY "GO" TO BTN_OK IN FRAME {&FRAME-NAME}.
      END.   
      ELSE DO:      
         ASSIGN 
         EDITOR_MEDD = flermeddtemp.MEDD
         FILL-IN_SANDARE = flermeddtemp.SANDARE 
         FILL-IN_SDATUM = flermeddtemp.SDATUM.
         DISPLAY EDITOR_MEDD FILL-IN_SANDARE FILL-IN_SDATUM WITH FRAME {&FRAME-NAME}.      
      END.
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON GO OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:
   RUN avb_UI.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK-2 DIALOG-1
ON CHOOSE OF BTN_OK-2 IN FRAME DIALOG-1 /* Ok */
DO:      
   {muswait.i}   
   GET FIRST flermeddq.          
   IF flermeddtemp.SANDARE = "FAKT.ADM." OR flermeddtemp.SANDARE = "IFSresponse" THEN DO:
      flermeddtemp.EMOTAGET = TRUE.  
      FIND FIRST valdbtemp WHERE valdbtemp.GFORETAG = flermeddtemp.FORETAG NO-ERROR.
      flermeddtemp.FORETAG = "spara".      
      RUN flermed2_UI (INPUT valdbtemp.GFORETAG,INPUT 5).          
   END.
   ELSE DO:     
      FIND FIRST valdbtemp WHERE valdbtemp.GFORETAG = flermeddtemp.FORETAG NO-ERROR.
      flermeddtemp.FORETAG = "tabort".  
      RUN flermed2_UI (INPUT valdbtemp.GFORETAG,INPUT 5).          
   END.   
   OPEN QUERY flermeddq FOR EACH flermeddtemp NO-LOCK.   
   GET FIRST flermeddq NO-LOCK.               
   {musarrow.i}   
   IF NOT AVAILABLE flermeddtemp THEN DO:
      APPLY "GO" TO BTN_OK-2 IN FRAME {&FRAME-NAME}.
   END.   
   ELSE DO:
      ASSIGN 
      FILL-IN-DB = flermeddtemp.FORETAG
      EDITOR_MEDD = flermeddtemp.MEDD
      FILL-IN_SANDARE = flermeddtemp.SANDARE 
      FILL-IN_SDATUM = flermeddtemp.SDATUM.
      DISPLAY FILL-IN-DB EDITOR_MEDD FILL-IN_SANDARE FILL-IN_SDATUM FILL-IN-DB WITH FRAME {&FRAME-NAME}.       
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK-2 DIALOG-1
ON GO OF BTN_OK-2 IN FRAME DIALOG-1 /* Ok */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKR DIALOG-1
ON CHOOSE OF BTN_SKR IN FRAME DIALOG-1 /* Skriv ut */
DO:
   RUN SKRIVVAL.W (INPUT FALSE).         
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:    
      RUN ut_UI.      
   END.
   {musarrow.i}   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKR DIALOG-1
ON MOUSE-MENU-CLICK OF BTN_SKR IN FRAME DIALOG-1 /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SPARA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SPARA DIALOG-1
ON CHOOSE OF BTN_SPARA IN FRAME DIALOG-1 /* Spara */
DO:      
   IF vart = "MST" THEN APPLY "GO" TO BTN_OK IN FRAME {&FRAME-NAME}.
   /*EJ LÄSTA MEDELANDEN MEDDSTA.W*/
   ELSE DO:
      GET FIRST flermeddq.          
      IF AVAILABLE flermeddtemp THEN DO:
         flermeddtemp.EMOTAGET = TRUE.              
         flermeddtemp.FORETAG = "spara".      
         RUN medbort_UI IN vismeddh (INPUT TABLE flermeddtemp).
         DELETE flermeddtemp.
      END.   
      GET NEXT flermeddq NO-LOCK.
      IF NOT AVAILABLE flermeddtemp THEN DO:
         APPLY "GO" TO BTN_OK IN FRAME {&FRAME-NAME}.
      END.   
      ELSE DO:      
         ASSIGN 
         EDITOR_MEDD = flermeddtemp.MEDD
         FILL-IN_SANDARE = flermeddtemp.SANDARE 
         FILL-IN_SDATUM = flermeddtemp.SDATUM.
         DISPLAY EDITOR_MEDD FILL-IN_SANDARE FILL-IN_SDATUM WITH FRAME {&FRAME-NAME}.      
      END.
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SPARA DIALOG-1
ON GO OF BTN_SPARA IN FRAME DIALOG-1 /* Spara */
DO:
   RUN avb_UI.
   RETURN.
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
   IF Guru.Konstanter:globanv = "SELNDEPA" THEN DO :
      LEAVE MAIN-BLOCK.      
   END.   
   {DIA_M_START.I}
   
   {muswait.i}   
   {VALDBALL.I}
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN APAMEDDU.P PERSISTENT SET vismeddh ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN APAMEDDU.P PERSISTENT SET vismeddh.
   END.   
   IF vart = "MST" THEN DO:
      RUN vmedstatus_UI IN vismeddh (INPUT Guru.GlobalaVariabler:plusrec,OUTPUT TABLE flermeddtemp).
      FIND FIRST flermeddtemp  NO-LOCK NO-ERROR.
      IF AVAILABLE flermeddtemp THEN DO:
         ASSIGN 
         EDITOR_MEDD = flermeddtemp.MEDD
         FILL-IN_SANDARE = flermeddtemp.SANDARE 
         FILL-IN_SDATUM = flermeddtemp.SDATUM.   
      END.
   END.
   ELSE DO:   
      RUN medh_UI IN vismeddh (INPUT Guru.Konstanter:globanv,OUTPUT TABLE flermeddtemp).
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:
         IF SEARCH(Guru.Konstanter:guruvar + "gurumed.txt") NE ? THEN DO:
            /*
            INPUT FROM VALUE(SEARCH(Guru.Konstanter:guruvar + "gurumed.txt")) NO-ECHO.
            REPEAT:
              SET words VIEW-AS EDITOR INNER-CHARS 78 INNER-LINES 80 WITH FRAME DDD WIDTH 80.              
               CREATE intid.   
               ASSIGN intid.TIN = words.            
            END.
            INPUT CLOSE.  
            */
            OS-DELETE VALUE(Guru.Konstanter:guruvar + "gurumed.txt") NO-ERROR.
            
            CREATE flermeddtemp.
            FOR EACH intid:
               IF intid.TIN NE "" THEN flermeddtemp.MEDD = flermeddtemp.MEDD + CHR(10) + intid.TIN.
            END.
            IF flermeddtemp.MEDD = ""  THEN DELETE flermeddtemp.                 
            IF AVAILABLE flermeddtemp THEN DO:
               flermeddtemp.MOTTAGARE = Guru.Konstanter:globanv.
            END.
         END.
      END.
      OPEN QUERY flermeddq FOR EACH flermeddtemp NO-LOCK.   
      GET FIRST flermeddq NO-LOCK.               
      IF NOT AVAILABLE flermeddtemp THEN DO:
         IF Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79)  THEN DO:
            RUN flermed_UI.
            OPEN QUERY flermeddq FOR EACH flermeddtemp NO-LOCK.   
            GET FIRST flermeddq NO-LOCK.
            IF NOT AVAILABLE flermeddtemp THEN DO:
               IF Guru.GlobalaVariabler:retvalkoll = TRUE THEN DO:  
                  RUN SetAppstartingCursor IN Guru.Konstanter:hpApi.
                  Guru.GlobalaVariabler:retvalkoll = FALSE.
               END.
              /*   status-mus2 = SESSION:SET-WAIT-STATE("").      */
               RUN avb_UI.
               LEAVE MAIN-BLOCK.         
            END.   
            ELSE DO:
               ASSIGN 
               FILL-IN-DB = flermeddtemp.FORETAG
               EDITOR_MEDD = flermeddtemp.MEDD
               FILL-IN_SANDARE = flermeddtemp.SANDARE 
               FILL-IN_SDATUM = flermeddtemp.SDATUM.
            END.
         END.
         ELSE DO:
            /*status-mus2 = SESSION:SET-WAIT-STATE("").      */
            IF Guru.GlobalaVariabler:retvalkoll = TRUE  THEN DO:  
               RUN SetAppstartingCursor IN Guru.Konstanter:hpApi.
               Guru.GlobalaVariabler:retvalkoll = FALSE.
            END.
            RUN avb_UI.
            LEAVE MAIN-BLOCK.
         END.   
      END.
      ELSE DO:    
         ASSIGN 
         EDITOR_MEDD = flermeddtemp.MEDD
         FILL-IN_SANDARE = flermeddtemp.SANDARE 
         FILL-IN_SDATUM = flermeddtemp.SDATUM.   
      END.
   END.   
   RUN enable_UI.
   /*       
   {FRMSIZED.I} 
   */
   {musarrow.i} 
   {DIA_M_SLUT.I}
   IF FILL-IN-DB NE "" THEN do:
      DISPLAY FILL-IN-DB WITH FRAME {&FRAME-NAME}. 
      BTN_OK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      BTN_OK-2:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      ENABLE BTN_OK-2 WITH FRAME {&FRAME-NAME}.
   END.
   IF vart = "MST" THEN DO:
      BTN_SPARA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avb_UI DIALOG-1 
PROCEDURE avb_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   IF VALID-HANDLE(vismeddh) THEN DELETE PROCEDURE vismeddh NO-ERROR.
   vismeddh = ?.
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
  DISPLAY FILL-IN_SANDARE FILL-IN_SDATUM EDITOR_MEDD 
      WITH FRAME DIALOG-1.
  ENABLE EDITOR_MEDD BTN_SKR BTN_SPARA BTN_OK BNT_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE flermed2_UI DIALOG-1 
PROCEDURE flermed2_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER gfore AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER vadgora AS INTEGER NO-UNDO.
   DEFINE VARIABLE conappvar AS CHARACTER NO-UNDO.
   CREATE SERVER apphandgran.
   /*
   FIND FIRST valdbtemp WHERE valdbtemp.GFORETAG = gfore NO-ERROR. 
   */
   ASSIGN
   conappvar = valdbtemp.APPCON.
   IF conappvar = "" THEN DO:
      MESSAGE "Kontakta Elpool tel 090/184540 för du kan inte ansluta korrekt!"
      VIEW-AS ALERT-BOX.
   END.
    /*obs case-sensitv -AppService appguru9*/
   ELSE DO:
      appcongran = apphandgran:CONNECT(conappvar,{APPCON1.i},{APPCON2.i},gfore).       
   END.
   IF NOT appcongran THEN DO:
      MESSAGE 
      "Du fick nu en massa fel meddelanden." Skip
      "Dessa meddelanden innebär att det inte går att hämta meddelande från " gfore skip 
      "Kontakta system ansvarig." 
      VIEW-AS ALERT-BOX.             
   END.
   ELSE DO:
      IF vadgora = 5  THEN DO:
         RUN APAMEDDF.P ON apphandgran TRANSACTION DISTINCT 
         (INPUT TABLE flermeddtemp).
      END.
      ELSE DO:
         RUN APAMEDDF.P ON apphandgran TRANSACTION DISTINCT 
         (INPUT TABLE flermeddtemp).
      END.      
      IF appcongran THEN appcongran = apphandgran:DISCONNECT().
      DELETE OBJECT apphandgran NO-ERROR.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE flermed_UI DIALOG-1 
PROCEDURE flermed_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {FLERMED.I}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI DIALOG-1 
PROCEDURE ut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {PRINTSTAENDE.I}           
   DISPLAY TODAY AT  6
   "MEDELANDE FRÅN :" AT 6
   FILL-IN_SANDARE AT 23 NO-LABEL  
           "TILL           :" AT 6 
   Guru.Konstanter:globanv AT 23  NO-LABEL        
           "SÄNT DEN       :" AT 6
   FILL-IN_SDATUM AT 23  NO-LABEL
   EDITOR_MEDD AT 6 VIEW-AS EDITOR SIZE 50 BY 9 NO-LABEL.
   OUTPUT CLOSE.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visa_UI DIALOG-1 
PROCEDURE visa_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   OPEN QUERY flermeddq FOR EACH flermeddtemp NO-LOCK.   
   GET FIRST flermeddq NO-LOCK.               
   IF NOT AVAILABLE flermeddtemp THEN DO:
      musz = musz.          
   END.
   ELSE DO:
      ASSIGN 
      FILL-IN-DB = flermeddtemp.FORETAG
      EDITOR_MEDD = flermeddtemp.MEDD
      FILL-IN_SANDARE = flermeddtemp.SANDARE 
      FILL-IN_SDATUM = flermeddtemp.SDATUM.
      DISPLAY FILL-IN-DB EDITOR_MEDD FILL-IN_SANDARE FILL-IN_SDATUM WITH FRAME {&FRAME-NAME}.
      BTN_OK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      BTN_OK-2:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      ENABLE BTN_OK-2 WITH FRAME {&FRAME-NAME}.
   END.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

