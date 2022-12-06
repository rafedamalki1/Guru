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
DEFINE INPUT  PARAMETER bestoff  AS CHARACTER.

/* Local Variable Definitions ---                                       */ 
{ALLDEF.I}
&Scoped-define NEW                                
{GLOBVAR2DEL1.I}
/*{EGENBEN.I}*/
{ANMARKD.I}
&Scoped-define SHARED SHARED
{DEFBESTU.I} 
{LEVTEMP.I}
{HUVLEVTEMP.I}
{KONVALTEMP.I}
DEFINE SHARED VARIABLE priset AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE detvar AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE datvar AS DATE NO-UNDO.  
DEFINE SHARED VARIABLE alla AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE epostvar AS CHARACTER NO-UNDO.

DEFINE SHARED VARIABLE delbest AS LOGICAL NO-UNDO.

DEFINE SHARED VARIABLE kalkrow AS ROWID NO-UNDO.
DEFINE SHARED VARIABLE mailvar AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE edivar AS LOGICAL NO-UNDO.
DEFINE VARIABLE musz2 AS LOGICAL NO-UNDO.
DEFINE VARIABLE EDITOR_MEDD AS CHARACTER  VIEW-AS EDITOR SCROLLBAR-VERTICAL SIZE 53 BY 9 FONT 25 NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.        
DEFINE VARIABLE arrhjsum AS DECIMAL NO-UNDO.    
DEFINE VARIABLE str AS CHARACTER FORMAT "X(130)" NO-UNDO. 
DEFINE VARIABLE str1 AS CHARACTER FORMAT "X(130)" NO-UNDO.
DEFINE VARIABLE str2 AS CHARACTER FORMAT "X(130)" NO-UNDO. 
DEFINE VARIABLE str0 AS CHARACTER FORMAT "X(130)" NO-UNDO.
DEFINE VARIABLE totalt AS DECIMAL NO-UNDO. 
DEFINE VARIABLE totalt2 AS DECIMAL NO-UNDO. 
DEFINE VARIABLE leverant AS CHARACTER NO-UNDO.
DEFINE VARIABLE lev AS CHARACTER NO-UNDO.  
DEFINE VARIABLE sumpris AS DECIMAL NO-UNDO.    
DEFINE VARIABLE totalsum AS DECIMAL NO-UNDO.
DEFINE VARIABLE sumantal AS INTEGER NO-UNDO.
DEFINE VARIABLE xkordvar AS INTEGER NO-UNDO. 
DEFINE VARIABLE stopvar AS LOGICAL NO-UNDO. 
DEFINE VARIABLE prisvar AS INTEGER NO-UNDO.
DEFINE VARIABLE antvar AS INTEGER NO-UNDO. 
DEFINE VARIABLE antvar2 AS INTEGER NO-UNDO.  
DEFINE VARIABLE firstlev AS CHARACTER NO-UNDO.   
DEFINE VARIABLE mtrlrak AS INTEGER NO-UNDO.  
DEFINE VARIABLE mtrlrak2 AS INTEGER NO-UNDO.
DEFINE VARIABLE radrak AS INTEGER NO-UNDO.   
DEFINE VARIABLE langd AS INTEGER NO-UNDO.  
DEFINE VARIABLE upp AS LOGICAL NO-UNDO.            
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(41)" NO-UNDO.
DEFINE VARIABLE prognamn2 AS CHARACTER FORMAT "X(41)" NO-UNDO.
DEFINE VARIABLE skick AS LOGICAL NO-UNDO.
DEFINE VARIABLE efel AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE bytgrupp AS CHARACTER NO-UNDO. 
DEFINE VARIABLE enrvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE utrec AS RECID NO-UNDO.
DEFINE VARIABLE utrec2 AS RECID NO-UNDO.
DEFINE VARIABLE kant AS INTEGER NO-UNDO.
DEFINE VARIABLE tant AS INTEGER NO-UNDO.
DEFINE VARIABLE uppvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE sidlangd AS INTEGER NO-UNDO.
DEFINE VARIABLE ediraknare AS INTEGER NO-UNDO.
DEFINE VARIABLE anmlangd AS INTEGER NO-UNDO.
DEFINE VARIABLE idvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE numsok AS LOGICAL NO-UNDO.
DEFINE VARIABLE byggnr AS INTEGER NO-UNDO.
DEFINE VARIABLE skapkab AS INTEGER NO-UNDO.
DEFINE VARIABLE mailberapph AS HANDLE NO-UNDO.
DEFINE VARIABLE hmtvalaonr AS CHARACTER NO-UNDO.
DEFINE VARIABLE hmtvaldelnr AS INTEGER NO-UNDO.
DEFINE VARIABLE skickad AS DATE NO-UNDO.
DEFINE VARIABLE rrakn AS INTEGER NO-UNDO.

DEFINE VARIABLE MED_EDITOR AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 53 BY 11.73
     BGCOLOR 8  NO-UNDO.
     
DEFINE TEMP-TABLE Body1
   FIELD UT AS CHARACTER.
   
DEFINE TEMP-TABLE Body2
   FIELD UT AS CHARACTER.


DEFINE TEMP-TABLE felmeddftptemp 
  FIELD FELMEDD AS CHARACTER
  FIELD VAL AS INTEGER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-VINST
&Scoped-define BROWSE-NAME BRW_UT

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tidut

/* Definitions for BROWSE BRW_UT                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_UT tidut.ut 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UT 
&Scoped-define QUERY-STRING-BRW_UT FOR EACH tidut NO-LOCK
&Scoped-define OPEN-QUERY-BRW_UT OPEN QUERY BRW_UT FOR EACH tidut NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_UT tidut
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UT tidut


/* Definitions for FRAME FRAME-VINST                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-VINST ~
    ~{&OPEN-QUERY-BRW_UT}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_AVS 

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
     SIZE 14 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_UT FOR 
      tidut SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_UT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UT WINDOW-2 _STRUCTURED
  QUERY BRW_UT NO-LOCK DISPLAY
      tidut.ut FORMAT "X(132)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 81 BY 20.42.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-VINST
     BRW_UT AT ROW 1.5 COL 1.5
     BTN_AVS AT ROW 22.25 COL 68.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 82.38 BY 22.54.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db tidut
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Specifikation av beställning"
         HEIGHT             = 22.63
         WIDTH              = 82.5
         MAX-HEIGHT         = 25.13
         MAX-WIDTH          = 100
         VIRTUAL-HEIGHT     = 25.13
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
/* SETTINGS FOR FRAME FRAME-VINST
                                                                        */
/* BROWSE-TAB BRW_UT 1 FRAME-VINST */
/* SETTINGS FOR BROWSE BRW_UT IN FRAME FRAME-VINST
   NO-ENABLE                                                            */
ASSIGN 
       BRW_UT:HIDDEN  IN FRAME FRAME-VINST                = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UT
/* Query rebuild information for BROWSE BRW_UT
     _TblList          = "temp-db.tidut"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = temp-db.tidut.ut
     _Query            is OPENED
*/  /* BROWSE BRW_UT */
&ANALYZE-RESUME

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
   {BORTBRWPROC.I}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_UT
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-2 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   IF VALID-HANDLE(mailberapph) THEN DELETE PROCEDURE mailberapph.
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
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   FIND FIRST skapa_mtrl NO-ERROR.
   FIND best_mtrl WHERE best_mtrl.LEVKOD = vald_lev NO-LOCK NO-ERROR.
   FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_lev NO-LOCK NO-ERROR.                                                      
   EMPTY TEMP-TABLE tidut NO-ERROR. 
/*   str=
 * "=============================================================================". 
 *    str1 =
 * "*****************************************************************************".  
 *    str0 = 
 * "-----------------------------------------------------------------------------".     */
 
   str=
"=====". 
   str1 =
"*****".  
   str0 = 
"-----".     
   prognamn = SESSION:TEMP-DIRECTORY.
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN prognamn = webclienttempdir.
   /*
   ÖVERFÖRING TILL RAINDANCE FÖR BORLÄNGE EJ KLART FÖR DRIFTSÄTTNING VÄNTAR PÅ OK FRÅN WMDATA*/
   IF Guru.Konstanter:globforetag = "cBORL" THEN DO:
      EMPTY TEMP-TABLE tidut NO-ERROR.
      
      prognamn = prognamn + STRING(skapa_mtrl.bestnr) + ".txt".     
      prognamn2 = STRING(skapa_mtrl.bestnr) + ".guru".  
      RUN hamtaaonr_UI IN mailberapph (INPUT kalkrow,OUTPUT hmtvalaonr,OUTPUT hmtvaldelnr).      
      RUN kolraindance_UI IN mailberapph (INPUT skapa_mtrl.bestnr,INPUT hmtvalaonr,INPUT hmtvaldelnr, OUTPUT skickad).
      IF skickad NE ? THEN DO:
         MESSAGE "Beställningen är redan skickad till Raindance datum:" + string(skickad,"yymmdd") + " ingen ny fil skapas." 
         VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.
      ELSE DO:      
         FOR EACH trp_mtrl WHERE trp_mtrl.LEVKOD = vald_lev: 
            IF trp_mtrl.DBEST NE "RETUR" THEN DO:
               totalt = totalt + (trp_mtrl.OPRIS * trp_mtrl.ANTAL).      
            END.   
         END. 
         /*STARTRAD*/
         CREATE tidut. 
         ASSIGN
         SUBSTRING(tidut.UT,1) = "H"
         SUBSTRING(tidut.UT,5) = STRING(1111)                          
         SUBSTRING(tidut.UT,25) = STRING(skapa_mtrl.BESTNR)
         SUBSTRING(tidut.UT,45) = STRING(TODAY,"999999")
         SUBSTRING(tidut.UT,51) = STRING(skapa_mtrl.DATUM,"999999")
         SUBSTRING(tidut.UT,57) = SUBSTRING(globanv,1,3)
         SUBSTRING(tidut.UT,67) = STRING(totalt).
         /*ORDERRADER*/
         rrakn = 0.
         FOR EACH trp_mtrl WHERE trp_mtrl.LEVKOD = vald_lev AND trp_mtrl.ANTAL > 0: 
            IF trp_mtrl.DBEST NE "RETUR" THEN DO:                    
               rrakn = rrakn + 1.
               CREATE tidut.      
               ASSIGN  
               SUBSTRING(tidut.UT,1) = "R"
               SUBSTRING(tidut.UT,2) = STRING(rrakn)
               SUBSTRING(tidut.UT,6) = SUBSTRING(trp_mtrl.ENR,1,20)
               SUBSTRING(tidut.UT,26) = STRING(trp_mtrl.ANTAL,">>>>>>>>>9")
               SUBSTRING(tidut.UT,36) = SUBSTRING(trp_mtrl.ENHET,1,6)
               SUBSTRING(tidut.UT,42) = STRING((trp_mtrl.OPRIS * trp_mtrl.ANTAL * 100),">>>>>>>>9")
               SUBSTRING(tidut.UT,57) = STRING(hmtvalaonr) + STRING(hmtvaldelnr)            
               SUBSTRING(tidut.UT,77) = SUBSTRING(trp_mtrl.BENAMNING,1,40).                                                             
            END.   
         END.   
         /*SLUTRAD*/
         CREATE tidut. 
         ASSIGN
         SUBSTRING(tidut.UT,1) = "S"
         SUBSTRING(tidut.UT,2) = STRING(skapa_mtrl.BESTNR).
   
         OUTPUT TO VALUE(prognamn).
         FOR EACH tidut:                                                              
            PUT UNFORMATTED                                                           
            TRIM(tidut.UT) + CHR(10).                                                 
         END.                                                                         
         OUTPUT CLOSE.
         
         RUN sparraindance_UI IN mailberapph (INPUT skapa_mtrl.bestnr,INPUT hmtvalaonr,INPUT hmtvaldelnr).
         RUN FTPFILE.P (INPUT "guru", INPUT "guru", INPUT TRUE, INPUT 1,
                     INPUT prognamn, INPUT prognamn2,
                     INPUT "10.79.16.22", OUTPUT TABLE felmeddftptemp).
         
         FIND FIRST felmeddftptemp NO-LOCK NO-ERROR.
         IF felmeddftptemp.FELMEDD BEGINS "Fil skickad..." THEN DO:
            MESSAGE "Beställningen är nu skickad till Raindance. Filen finns att se under:" + prognamn VIEW-AS ALERT-BOX TITLE "Meddelande".
            DELETE felmeddftptemp.
         END.
         ELSE DO:           
            prognamn = SESSION:TEMP-DIRECTORY.   
            IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN prognamn = webclienttempdir.
            prognamn = prognamn + "edifel.txt".
            MESSAGE "Ett fel inträffade. Kontakta Elpool 090 - 18 45 40." VIEW-AS ALERT-BOX TITLE "Meddelande".
            OUTPUT TO VALUE(prognamn).
            FOR EACH felmeddftptemp:
               PUT UNFORMATTED
               TRIM(felmeddftptemp.FELMEDD) + CHR(10).
               DELETE felmeddftptemp.
            END.
            OUTPUT CLOSE.
         END.        
      END.
   END.
   
   
   RUN skapalista_UI IN mailberapph (INPUT Guru.Konstanter:globforetag, INPUT epostvar, INPUT bestoff, INPUT vald_lev, INPUT kalkrow,
                                     INPUT valaonr, INPUT valomrade, INPUT detvar, INPUT mailvar,
                                     INPUT edivar, INPUT priset, INPUT alla,INPUT datvar,
                                     OUTPUT musz2, INPUT TABLE skapa_mtrl, INPUT TABLE trp_mtrl,
                                     OUTPUT TABLE tidut, INPUT TABLE kon_val,INPUT delbest, OUTPUT TABLE felmeddftptemp).   

   
   musz = musz2.   
   IF musz = TRUE THEN DO:
      musz = FALSE.
      status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").
      APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
      LEAVE MAIN-BLOCK. 
   END.                 
   RUN ut_UI.  
   status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").
   APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
   LEAVE MAIN-BLOCK.       
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
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_UT:HANDLE IN FRAME {&FRAME-NAME}).
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN MAILBERAPP.P PERSISTENT SET mailberapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MAILBERAPP.P PERSISTENT SET mailberapph.
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
  ENABLE BTN_AVS 
      WITH FRAME FRAME-VINST IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-VINST}
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
   EDITOR_MEDD = "".
   prognamn = SESSION:TEMP-DIRECTORY.       
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN prognamn = webclienttempdir.      
   IF Guru.Konstanter:globforetag = "CELPA" THEN DO:
      RUN EPOST2.P (INPUT "",INPUT "niklas@elpool.se",INPUT bestoff,INPUT TABLE tidut,      
      INPUT "",INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globforetag,OUTPUT skick,OUTPUT efel).
      IF efel NE "" THEN MESSAGE efel VIEW-AS ALERT-BOX.         
      IF skick = TRUE THEN RETURN NO-APPLY.     
   END.
   ELSE IF Guru.Konstanter:globforetag = "cSUND" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
      IF bestoff = "beställning" THEN DO:
         /*"order@elef.se"*/
         RUN EPOST2.P (INPUT "",INPUT "niklas@elpool.se",INPUT bestoff,INPUT TABLE tidut,
         INPUT "",INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globforetag,OUTPUT skick,OUTPUT efel).
         IF efel NE "" THEN MESSAGE efel VIEW-AS ALERT-BOX.
         IF skick = TRUE THEN RETURN NO-APPLY.
         ELSE DO:
            MESSAGE "E-posten är nu skickad till utkorgen" VIEW-AS ALERT-BOX.
         END.
      END.
      IF bestoff = "offert" THEN DO:
         /*"offert@elef.se"*/
         RUN EPOST2.P (INPUT "",INPUT "offert@elef.se",INPUT bestoff,INPUT TABLE tidut,
         INPUT "",INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globforetag,OUTPUT skick,OUTPUT efel).
         IF efel NE "" THEN MESSAGE efel VIEW-AS ALERT-BOX.
         IF skick = TRUE THEN RETURN NO-APPLY.
         ELSE DO:
            MESSAGE "E-posten är nu skickad till utkorgen" VIEW-AS ALERT-BOX.
         END.
      END.
   END.
   ELSE IF Guru.Konstanter:globforetag = "VORD"  OR Guru.Konstanter:globforetag = "VAST" OR 
      Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO: 
      FIND FIRST skapa_mtrl NO-ERROR.
      prognamn = prognamn + STRING(skapa_mtrl.bestnr) + ".guru".     
      prognamn2 = STRING(skapa_mtrl.bestnr) + ".guru".                                    
      OUTPUT TO VALUE(prognamn).
      FOR EACH tidut:                                                              
         PUT UNFORMATTED                                                           
         TRIM(tidut.UT) + CHR(10).                                                 
      END.                                                                         
      OUTPUT CLOSE.
      FIND FIRST felmeddftptemp NO-LOCK NO-ERROR.
      IF felmeddftptemp.FELMEDD BEGINS "Fil skickad..." THEN DO:
         MESSAGE "Beställningen är nu skickad. Filen finns att se under:" + prognamn VIEW-AS ALERT-BOX TITLE "Meddelande".
         DELETE felmeddftptemp.
      END.
      ELSE DO:
         prognamn = SESSION:TEMP-DIRECTORY.   
         IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN prognamn = webclienttempdir.
         prognamn = prognamn + "edifel.txt".
         MESSAGE "Ett fel inträffade. Kontakta Elpool 090 - 18 45 40." SKIP
                  "Fel logg " prognamn
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         OUTPUT TO VALUE(prognamn).
         FOR EACH felmeddftptemp:
            PUT UNFORMATTED
            TRIM(felmeddftptemp.FELMEDD) + CHR(10).
            DELETE felmeddftptemp.
         END.
         OUTPUT CLOSE.
      END.            
   END.   
   ELSE IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "elpa"  THEN DO: 
      FIND FIRST skapa_mtrl NO-ERROR.
      prognamn = prognamn + STRING(skapa_mtrl.bestnr) + ".guru".     
      prognamn2 = STRING(skapa_mtrl.bestnr) + ".guru".                                    
      OUTPUT TO VALUE(prognamn).
      FOR EACH tidut:                                                              
         PUT UNFORMATTED                                                           
         TRIM(tidut.UT) + CHR(10).                                                 
      END.                                                                         
      OUTPUT CLOSE.
      MESSAGE "Beställningen är nu skickad. Filen finns att se under:" + prognamn VIEW-AS ALERT-BOX TITLE "Meddelande".            
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

