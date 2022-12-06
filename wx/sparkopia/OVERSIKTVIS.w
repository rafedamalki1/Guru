
&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
{MUATEMP.I}
{SUMTEMP.I}
{ALLDEF.I}
{GLOBVAR2DEL1.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED 
  
{MARKVAL.I}
DEFINE TEMP-TABLE uppfoltemp   
   FIELD FORETAG AS CHARACTER
   FIELD ANVANDARE AS CHARACTER  
   FIELD ALLAMA AS LOGICAL
   FIELD VALVARD AS CHARACTER
   FIELD FORSTA AS LOGICAL
   FIELD STAMP AS LOGICAL.
   
 {MAGA.I}  
   
   /*DEFINE TEMP-TABLE maga                       
   FIELD VARDNR AS INTEGER                   
   FIELD MARKNR AS INTEGER                   
   FIELD PERSONNUMMER AS CHARACTER           
   FIELD PNR2 AS CHARACTER                   
   FIELD MARKAGARE AS CHARACTER              
   FIELD GATUADRESS AS CHARACTER             
   FIELD POSTNUMMER AS CHARACTER             
   FIELD POSTADRESS AS CHARACTER             
   FIELD BETECKNING AS CHARACTER             
   FIELD PROCENT AS INTEGER                  
   FIELD ANAMN AS CHARACTER                  
   FIELD AADRESS AS CHARACTER                
   FIELD APONR AS CHARACTER                  
   FIELD APADRESS AS CHARACTER               
   FIELD APERNR AS CHARACTER 
   FIELD ORDNING AS INTEGER
   FIELD MARKREC AS RECID                    
   INDEX MARKNR IS PRIMARY MARKNR ASCENDING
   INDEX ORDN ORDNING ASCENDING.*/
DEFINE VARIABLE skprotapph AS HANDLE NO-UNDO.
DEFINE VARIABLE fbet AS CHARACTER NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
{EXTRADATA.I}

{WHANDLTEMP.I}
   


/* Local Variable Definitions ---                                       */
DEFINE VARIABLE bestdynh AS HANDLE NO-UNDO.
DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_OK 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 14 BY 1
     BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Btn_OK AT ROW 28.13 COL 111
     SPACE(0.99) SKIP(0.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Översikt"
         DEFAULT-BUTTON Btn_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Personaluppgifter */
DO:   
   IF VALID-HANDLE(bestdynh) THEN DO: 
      RUN avs_UI IN bestdynh.
      DELETE PROCEDURE bestdynh NO-ERROR.  
   END.
   
  APPLY "END-ERROR":U TO SELF.
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
   {muswait.i}
   {ALLSTARTDYN.I}
   DEBUGGER:SET-BREAK().   
  RUN enable_UI.
  {FRMSIZEDF.I}
  
  EMPTY TEMP-TABLE sumtemp NO-ERROR. 
   CREATE uppfoltemp.
   ASSIGN
   uppfoltemp.FORETAG = globforetag   
   uppfoltemp.ALLAMA = FALSE
   uppfoltemp.VALVARD = ""
   uppfoltemp.FORSTA = TRUE
   uppfoltemp.STAMP = FALSE.    
  FIND FIRST markval /*WHERE markval.BETECKNING = fastfortemp.BETECKNING*/ NO-LOCK NO-ERROR.  
  IF AVAILABLE markval THEN DO:                       
     RUN markupp_UI IN skprotapph
     (INPUT "" ,INPUT markval.BETECKNING,INPUT markval.MARKREC,INPUT TABLE uppfoltemp,INPUT TABLE markval,INPUT TABLE maga,OUTPUT TABLE sumtemp).           
  END.
  /*se EXEMKABLU2.P */
  fbet = "".
  EMPTY TEMP-TABLE MUAtemp  NO-ERROR. 
  FOR EACH markval BY markval.BETECKNING:
      IF markval.BETECKNING NE fbet THEN DO:
   
         FIND FIRST sumtemp WHERE sumtemp.BETECKNING = markval.BETECKNING AND sumtemp.MARKNR = markval.MARKNR NO-ERROR.
         IF AVAILABLE sumtemp THEN DO:
            
            DEBUGGER:SET-BREAK().
            CREATE MUAtemp.
            MUAtemp.BETECKNING = markval.BETECKNING.
            MUAtemp.MARKNR = markval.MARKNR.
            MUAtemp.MINIMI = MUAtemp.MINIMI + sumtemp.MINIERS.                
            MUAtemp.lufttedning = MUAtemp.lufttedning + sumtemp.OMN + sumtemp.OMB + sumtemp.SMN + sumtemp.SMB.                                     
            IF sumtemp.JKNDEC > 0 THEN MUAtemp.jordkabel = MUAtemp.jordkabel  + sumtemp.JKNDEC - sumtemp.JKB.            
            ELSE MUAtemp.jordkabel = MUAtemp.jordkabel + sumtemp.JKN - sumtemp.JKB.               
            MUAtemp.natstation = MUAtemp.natstation + sumtemp.TSF - sumtemp.TSB.
            MUAtemp.ksk = MUAtemp.ksk + sumtemp.TSKF - sumtemp.TSKB.
            
            IF varforetypval[40] = 1 THEN DO:               
               MUAtemp.lufttedninguex = MUAtemp.lufttedninguex + ((sumtemp.OMN + sumtemp.OMB)/ ( 1 + ( 0.25 * 0.66 )))  + ((sumtemp.SMN + sumtemp.SMB) / 1.25).                                      
               IF sumtemp.JKNDEC > 0 THEN MUAtemp.jordkabeluex = MUAtemp.jordkabeluex + ((sumtemp.JKNDEC - sumtemp.JKB) / 1.25).
               ELSE MUAtemp.jordkabeluex = MUAtemp.jordkabeluex + ((sumtemp.JKN - sumtemp.JKB) / 1.25).                  
               MUAtemp.natstationuex = MUAtemp.natstationuex + ((sumtemp.TSF - sumtemp.TSB) / 1.25).
               MUAtemp.kskuex = MUAtemp.kskuex + ((sumtemp.TSKF - sumtemp.TSKB) / 1.25).            
            END.    
            DEBUGGER:SET-BREAK().                   
            
            /* OM NÅGOT FAST BELOPP  och belopp 20141113*/
            IF ( sumtemp.SMF + sumtemp.JKF +  sumtemp.OMF ) > 0 AND INTEGER( sumtemp.SMF + sumtemp.JKF +  sumtemp.OMF - sumtemp.OVRIG) = 0 THEN                
            MUAtemp.ovrigt = MUAtemp.ovrigt +  sumtemp.OVRIG.
            ELSE IF INTEGER( sumtemp.OVRIG - sumtemp.TILLAGG ) > 0 THEN 
            MUAtemp.ovrigt = MUAtemp.ovrigt + sumtemp.OVRIG - sumtemp.TILLAGG.            
            /*/* OM NÅGOT FAST BELOPP  och minimibelopp 20141113*/
            IF ( sumtemp.SMF + sumtemp.JKF +  sumtemp.OMF ) > 0 AND INTEGER( sumtemp.SMF + sumtemp.JKF +  sumtemp.OMF - sumtemp.OVRIG) = 0 THEN                
            MUAtemp.ovrigt = MUAtemp.ovrigt + sumtemp.TSKF - sumtemp.TSKB + sumtemp.OVRIG.
            ELSE IF INTEGER( sumtemp.OVRIG - sumtemp.TILLAGG ) > 0 THEN 
            MUAtemp.ovrigt = MUAtemp.ovrigt + sumtemp.TSKF - sumtemp.TSKB + sumtemp.OVRIG - sumtemp.TILLAGG.
            ELSE MUAtemp.ovrigt = MUAtemp.ovrigt + sumtemp.TSKF - sumtemp.TSKB.*/            
            MUAtemp.OVERENS = MUAtemp.OVERENS + sumtemp.TILLAGG.                                    
            MUAtemp.jordkabel = MUAtemp.jordkabel + sumtemp.JKLN - sumtemp.JKLB.     /* obs! lägg till lågspänning om uppdalad*/                                
            MUAtemp.TOTALT = MUATEMP.TOTALT + sumtemp.SUMMA.     
            
            IF MUAtemp.MINIMI > 0 THEN DO:
               IF INTEGER(MUAtemp.MINIMI -  MUAtemp.lufttedning - MUAtemp.jordkabel - MUAtemp.natstation - MUAtemp.ksk - MUAtemp.ovrigt - MUAtemp.OVERENS - sumtemp.FROTPAEXP) > 0 THEN 
               MUAtemp.TLMINIMI =  MUAtemp.MINIMI -  MUAtemp.lufttedning - MUAtemp.jordkabel - MUAtemp.natstation - MUAtemp.ksk - MUAtemp.ovrigt - MUAtemp.OVERENS - sumtemp.FROTPAEXP.
            END.    
         END.
         /*MESSAGE MUAtemp.BETECKNING sumtemp.BETECKNING sumtemp.FROTPAEXP
         VIEW-AS ALERT-BOX.*/
         IF varforetypval[40] = 1 THEN DO:
            MUAtemp.EXL = (MUAtemp.lufttedning - MUAtemp.lufttedninguex) + (MUAtemp.jordkabel - MUAtemp.jordkabeluex ) + (MUAtemp.natstation - MUAtemp.natstationuex) + (MUAtemp.ksk - MUAtemp.kskuex) + sumtemp.FROTPAEXP.            
         END.
         ELSE DO:
            MUAtemp.EXL = ((sumtemp.OMN + sumtemp.OMB) * ( 0.25 * 0.66 ))  + ((sumtemp.SMN + sumtemp.SMB) * 0.25) + ((sumtemp.TSF + sumtemp.TSKF) * 0.25)  + sumtemp.FROTPAEXP.
            /*OM DECIMAL FRAMRÄKNAD - ANVÄND*/
            IF sumtemp.JKNDEC > 0 THEN MUAtemp.EXL = MUAtemp.EXL + ((sumtemp.JKNDEC - sumtemp.JKB) * 0.25)  + sumtemp.FROTPAEXP.
            ELSE MUAtemp.EXL = MUAtemp.EXL + ((sumtemp.JKN - sumtemp.JKB) * 0.25)  + sumtemp.FROTPAEXP.
         END.      
      END.
      fbet = markval.BETECKNING.
   END.
      
  FOR EACH markval BY markval.BETECKNING:
     FIND FIRST MUAtemp WHERE MUAtemp.BETECKNING = markval.BETECKNING NO-LOCK NO-ERROR.    
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
      EMPTY TEMP-TABLE extradatatemp NO-ERROR.
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "VARDFAST2"     
      inextradatatemp.HUVUDINT = markval.VARDNR
      inextradatatemp.HUVUDCH = markval.BETECKNING.
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FIND FIRST extradatatemp NO-LOCK NO-ERROR.
      IF AVAILABLE extradatatemp THEN DO:      
         ASSIGN
         /*.CH11 = extradatatemp.SOKCH[1]     löpnr markägare*/    
         MUAtemp.F = extradatatemp.SOKLOG[1]        
         MUAtemp.T = extradatatemp.SOKLOG[2]
         MUAtemp.frotnetto = extradatatemp.SOKINT[1]        
         MUAtemp.texpl = extradatatemp.SOKINT[2].
         /*MUAtemp.INT3 = extradatatemp.SOKINT[3]. sKOGMARK ANVÄND EJ*/
      END.
  END.   
  
  RUN OVERSIKTDYN.P PERSISTENT SET bestdynh (INPUT THIS-PROCEDURE, INPUT framesizeh,INPUT TABLE whandltemp).
  RUN  skapabrw_UI IN bestdynh (INPUT 1,INPUT 1, TEMP-TABLE muatemp:DEFAULT-BUFFER-HANDLE).
  

  {DIA_M_SLUT.I}
  
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI Dialog-Frame 
PROCEDURE allstartbrw_UI :
   IF Guru.Konstanter:appcon THEN DO:
      RUN SKAPPROTOU7.P PERSISTENT SET skprotapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN SKAPPROTOU7.P PERSISTENT SET skprotapph.
   END.
    IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
   END.  
   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
   CREATE whandltemp.
   ordningnr = 1.
   /*RUN whandle_UI (INPUT ordningnr, {&WINDOW-NAME}:HANDLE).*/
   RUN whandle_UI (INPUT ordningnr,FRAME Dialog-Frame:HANDLE).
   RUN whandle_UI (INPUT ordningnr,BTN_OK:HANDLE IN FRAME Dialog-Frame). 
  
  
       
   
   
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
  ENABLE Btn_OK 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE musa Dialog-Frame 
PROCEDURE musa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {musarrow.i}   
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE musw Dialog-Frame 
PROCEDURE musw :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   {muswait.i}  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE whandle_UI Dialog-Frame 
PROCEDURE whandle_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ordnr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER ordh AS HANDLE NO-UNDO.
   ASSIGN
   whandltemp.WF[ordnr] = ordh.
   ordningnr = ordningnr + 1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

=======
&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
{MUATEMP.I}
{SUMTEMP.I}
{ALLDEF.I}
{GLOBVAR2DEL1.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED 
  
{MARKVAL.I}
DEFINE TEMP-TABLE uppfoltemp   
   FIELD FORETAG AS CHARACTER
   FIELD ANVANDARE AS CHARACTER  
   FIELD ALLAMA AS LOGICAL
   FIELD VALVARD AS CHARACTER
   FIELD FORSTA AS LOGICAL
   FIELD STAMP AS LOGICAL.
   
 {MAGA.I}  
   
   /*DEFINE TEMP-TABLE maga                       
   FIELD VARDNR AS INTEGER                   
   FIELD MARKNR AS INTEGER                   
   FIELD PERSONNUMMER AS CHARACTER           
   FIELD PNR2 AS CHARACTER                   
   FIELD MARKAGARE AS CHARACTER              
   FIELD GATUADRESS AS CHARACTER             
   FIELD POSTNUMMER AS CHARACTER             
   FIELD POSTADRESS AS CHARACTER             
   FIELD BETECKNING AS CHARACTER             
   FIELD PROCENT AS INTEGER                  
   FIELD ANAMN AS CHARACTER                  
   FIELD AADRESS AS CHARACTER                
   FIELD APONR AS CHARACTER                  
   FIELD APADRESS AS CHARACTER               
   FIELD APERNR AS CHARACTER 
   FIELD ORDNING AS INTEGER
   FIELD MARKREC AS RECID                    
   INDEX MARKNR IS PRIMARY MARKNR ASCENDING
   INDEX ORDN ORDNING ASCENDING.*/
DEFINE VARIABLE skprotapph AS HANDLE NO-UNDO.
DEFINE VARIABLE fbet AS CHARACTER NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
{EXTRADATA.I}

{WHANDLTEMP.I}
   


/* Local Variable Definitions ---                                       */
DEFINE VARIABLE bestdynh AS HANDLE NO-UNDO.
DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_OK 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 14 BY 1
     BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Btn_OK AT ROW 28.13 COL 111
     SPACE(0.99) SKIP(0.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Översikt"
         DEFAULT-BUTTON Btn_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Personaluppgifter */
DO:   
   IF VALID-HANDLE(bestdynh) THEN DO: 
      RUN avs_UI IN bestdynh.
      DELETE PROCEDURE bestdynh NO-ERROR.  
   END.
   
  APPLY "END-ERROR":U TO SELF.
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
   {muswait.i}
   {ALLSTARTDYN.I}
   DEBUGGER:SET-BREAK().   
  RUN enable_UI.
  {FRMSIZEDF.I}
  
  EMPTY TEMP-TABLE sumtemp NO-ERROR. 
   CREATE uppfoltemp.
   ASSIGN
   uppfoltemp.FORETAG = globforetag   
   uppfoltemp.ALLAMA = FALSE
   uppfoltemp.VALVARD = ""
   uppfoltemp.FORSTA = TRUE
   uppfoltemp.STAMP = FALSE.    
  FIND FIRST markval /*WHERE markval.BETECKNING = fastfortemp.BETECKNING*/ NO-LOCK NO-ERROR.  
  IF AVAILABLE markval THEN DO:                       
     RUN markupp_UI IN skprotapph
     (INPUT "" ,INPUT markval.BETECKNING,INPUT markval.MARKREC,INPUT TABLE uppfoltemp,INPUT TABLE markval,INPUT TABLE maga,OUTPUT TABLE sumtemp).           
  END.
  /*se EXEMKABLU2.P */
  fbet = "".
  EMPTY TEMP-TABLE MUAtemp  NO-ERROR. 
  FOR EACH markval BY markval.BETECKNING:
      IF markval.BETECKNING NE fbet THEN DO:
   
         FIND FIRST sumtemp WHERE sumtemp.BETECKNING = markval.BETECKNING AND sumtemp.MARKNR = markval.MARKNR NO-ERROR.
         IF AVAILABLE sumtemp THEN DO:
            
            DEBUGGER:SET-BREAK().
            CREATE MUAtemp.
            MUAtemp.BETECKNING = markval.BETECKNING.
            MUAtemp.MARKNR = markval.MARKNR.
            MUAtemp.MINIMI = MUAtemp.MINIMI + sumtemp.MINIERS.                
            MUAtemp.lufttedning = MUAtemp.lufttedning + sumtemp.OMN + sumtemp.OMB + sumtemp.SMN + sumtemp.SMB.                                     
            IF sumtemp.JKNDEC > 0 THEN MUAtemp.jordkabel = MUAtemp.jordkabel  + sumtemp.JKNDEC - sumtemp.JKB.            
            ELSE MUAtemp.jordkabel = MUAtemp.jordkabel + sumtemp.JKN - sumtemp.JKB.               
            MUAtemp.natstation = MUAtemp.natstation + sumtemp.TSF - sumtemp.TSB.
            MUAtemp.ksk = MUAtemp.ksk + sumtemp.TSKF - sumtemp.TSKB.
            
            IF varforetypval[40] = 1 THEN DO:               
               MUAtemp.lufttedninguex = MUAtemp.lufttedninguex + ((sumtemp.OMN + sumtemp.OMB)/ ( 1 + ( 0.25 * 0.66 )))  + ((sumtemp.SMN + sumtemp.SMB) / 1.25).                                      
               IF sumtemp.JKNDEC > 0 THEN MUAtemp.jordkabeluex = MUAtemp.jordkabeluex + ((sumtemp.JKNDEC - sumtemp.JKB) / 1.25).
               ELSE MUAtemp.jordkabeluex = MUAtemp.jordkabeluex + ((sumtemp.JKN - sumtemp.JKB) / 1.25).                  
               MUAtemp.natstationuex = MUAtemp.natstationuex + ((sumtemp.TSF - sumtemp.TSB) / 1.25).
               MUAtemp.kskuex = MUAtemp.kskuex + ((sumtemp.TSKF - sumtemp.TSKB) / 1.25).            
            END.    
            DEBUGGER:SET-BREAK().                   
            
            /* OM NÅGOT FAST BELOPP  och belopp 20141113*/
            IF ( sumtemp.SMF + sumtemp.JKF +  sumtemp.OMF ) > 0 AND INTEGER( sumtemp.SMF + sumtemp.JKF +  sumtemp.OMF - sumtemp.OVRIG) = 0 THEN                
            MUAtemp.ovrigt = MUAtemp.ovrigt +  sumtemp.OVRIG.
            ELSE IF INTEGER( sumtemp.OVRIG - sumtemp.TILLAGG ) > 0 THEN 
            MUAtemp.ovrigt = MUAtemp.ovrigt + sumtemp.OVRIG - sumtemp.TILLAGG.            
            /*/* OM NÅGOT FAST BELOPP  och minimibelopp 20141113*/
            IF ( sumtemp.SMF + sumtemp.JKF +  sumtemp.OMF ) > 0 AND INTEGER( sumtemp.SMF + sumtemp.JKF +  sumtemp.OMF - sumtemp.OVRIG) = 0 THEN                
            MUAtemp.ovrigt = MUAtemp.ovrigt + sumtemp.TSKF - sumtemp.TSKB + sumtemp.OVRIG.
            ELSE IF INTEGER( sumtemp.OVRIG - sumtemp.TILLAGG ) > 0 THEN 
            MUAtemp.ovrigt = MUAtemp.ovrigt + sumtemp.TSKF - sumtemp.TSKB + sumtemp.OVRIG - sumtemp.TILLAGG.
            ELSE MUAtemp.ovrigt = MUAtemp.ovrigt + sumtemp.TSKF - sumtemp.TSKB.*/            
            MUAtemp.OVERENS = MUAtemp.OVERENS + sumtemp.TILLAGG.                                    
            MUAtemp.jordkabel = MUAtemp.jordkabel + sumtemp.JKLN - sumtemp.JKLB.     /* obs! lägg till lågspänning om uppdalad*/                                
            MUAtemp.TOTALT = MUATEMP.TOTALT + sumtemp.SUMMA.     
            
            IF MUAtemp.MINIMI > 0 THEN DO:
               IF INTEGER(MUAtemp.MINIMI -  MUAtemp.lufttedning - MUAtemp.jordkabel - MUAtemp.natstation - MUAtemp.ksk - MUAtemp.ovrigt - MUAtemp.OVERENS - sumtemp.FROTPAEXP) > 0 THEN 
               MUAtemp.TLMINIMI =  MUAtemp.MINIMI -  MUAtemp.lufttedning - MUAtemp.jordkabel - MUAtemp.natstation - MUAtemp.ksk - MUAtemp.ovrigt - MUAtemp.OVERENS - sumtemp.FROTPAEXP.
            END.    
         END.
         /*MESSAGE MUAtemp.BETECKNING sumtemp.BETECKNING sumtemp.FROTPAEXP
         VIEW-AS ALERT-BOX.*/
         IF varforetypval[40] = 1 THEN DO:
            MUAtemp.EXL = (MUAtemp.lufttedning - MUAtemp.lufttedninguex) + (MUAtemp.jordkabel - MUAtemp.jordkabeluex ) + (MUAtemp.natstation - MUAtemp.natstationuex) + (MUAtemp.ksk - MUAtemp.kskuex) + sumtemp.FROTPAEXP.            
         END.
         ELSE DO:
            MUAtemp.EXL = ((sumtemp.OMN + sumtemp.OMB) * ( 0.25 * 0.66 ))  + ((sumtemp.SMN + sumtemp.SMB) * 0.25) + ((sumtemp.TSF + sumtemp.TSKF) * 0.25)  + sumtemp.FROTPAEXP.
            /*OM DECIMAL FRAMRÄKNAD - ANVÄND*/
            IF sumtemp.JKNDEC > 0 THEN MUAtemp.EXL = MUAtemp.EXL + ((sumtemp.JKNDEC - sumtemp.JKB) * 0.25)  + sumtemp.FROTPAEXP.
            ELSE MUAtemp.EXL = MUAtemp.EXL + ((sumtemp.JKN - sumtemp.JKB) * 0.25)  + sumtemp.FROTPAEXP.
         END.      
      END.
      fbet = markval.BETECKNING.
   END.
      
  FOR EACH markval BY markval.BETECKNING:
     FIND FIRST MUAtemp WHERE MUAtemp.BETECKNING = markval.BETECKNING NO-LOCK NO-ERROR.    
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
      EMPTY TEMP-TABLE extradatatemp NO-ERROR.
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "VARDFAST2"     
      inextradatatemp.HUVUDINT = markval.VARDNR
      inextradatatemp.HUVUDCH = markval.BETECKNING.
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FIND FIRST extradatatemp NO-LOCK NO-ERROR.
      IF AVAILABLE extradatatemp THEN DO:      
         ASSIGN
         /*.CH11 = extradatatemp.SOKCH[1]     löpnr markägare*/    
         MUAtemp.F = extradatatemp.SOKLOG[1]        
         MUAtemp.T = extradatatemp.SOKLOG[2]
         MUAtemp.frotnetto = extradatatemp.SOKINT[1]        
         MUAtemp.texpl = extradatatemp.SOKINT[2].
         /*MUAtemp.INT3 = extradatatemp.SOKINT[3]. sKOGMARK ANVÄND EJ*/
      END.
  END.   
  
  RUN OVERSIKTDYN.P PERSISTENT SET bestdynh (INPUT THIS-PROCEDURE, INPUT framesizeh,INPUT TABLE whandltemp).
  RUN  skapabrw_UI IN bestdynh (INPUT 1,INPUT 1, TEMP-TABLE muatemp:DEFAULT-BUFFER-HANDLE).
  

  {DIA_M_SLUT.I}
  
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI Dialog-Frame 
PROCEDURE allstartbrw_UI :
   IF Guru.Konstanter:appcon THEN DO:
      RUN SKAPPROTOU7.P PERSISTENT SET skprotapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN SKAPPROTOU7.P PERSISTENT SET skprotapph.
   END.
    IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
   END.  
   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
   CREATE whandltemp.
   ordningnr = 1.
   /*RUN whandle_UI (INPUT ordningnr, {&WINDOW-NAME}:HANDLE).*/
   RUN whandle_UI (INPUT ordningnr,FRAME Dialog-Frame:HANDLE).
   RUN whandle_UI (INPUT ordningnr,BTN_OK:HANDLE IN FRAME Dialog-Frame). 
  
  
       
   
   
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
  ENABLE Btn_OK 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE musa Dialog-Frame 
PROCEDURE musa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {musarrow.i}   
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE musw Dialog-Frame 
PROCEDURE musw :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   {muswait.i}  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE whandle_UI Dialog-Frame 
PROCEDURE whandle_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ordnr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER ordh AS HANDLE NO-UNDO.
   ASSIGN
   whandltemp.WF[ordnr] = ordh.
   ordningnr = ordningnr + 1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

>>>>>>> branch 'master' of file:///\\server05\delad\REMOTEGURU\GuruRemote.git
