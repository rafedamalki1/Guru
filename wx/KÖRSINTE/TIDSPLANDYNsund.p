/*TIDSPLANDYNSUND.P*/
{GLOBVAR2DEL1.I}

{REGVAR.I}
&Scoped-define SHARED SHARED 
{AVTAONRTEMP.I}
{TIDSPLAN.I}
DEFINE SHARED VARIABLE brwproch AS HANDLE NO-UNDO.
DEFINE VARIABLE dynbrwh AS HANDLE NO-UNDO.
DEFINE VARIABLE dynfrmh AS HANDLE NO-UNDO.
DEFINE VARIABLE dynbuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE fillsth AS HANDLE NO-UNDO.
DEFINE VARIABLE fillslh AS HANDLE NO-UNDO.
DEFINE VARIABLE dynqueh AS HANDLE NO-UNDO.
DEFINE VARIABLE openquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE tmpcolh AS HANDLE NO-UNDO.
DEFINE VARIABLE tempcolh AS HANDLE NO-UNDO.
DEFINE VARIABLE fieldh AS HANDLE NO-UNDO.
DEFINE VARIABLE fieldh2 AS HANDLE NO-UNDO.
DEFINE VARIABLE loopraknare AS INTEGER NO-UNDO.
DEFINE VARIABLE raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE hLast AS HANDLE NO-UNDO.
DEFINE VARIABLE startdatum AS DATE NO-UNDO.
DEFINE VARIABLE slutdatum AS DATE NO-UNDO.
DEFINE VARIABLE stdatum AS DATE NO-UNDO.
DEFINE VARIABLE sldatum AS DATE NO-UNDO.
DEFINE VARIABLE antalveckor AS INTEGER NO-UNDO.
DEFINE VARIABLE startpos AS INTEGER NO-UNDO.
DEFINE VARIABLE antalcol AS INTEGER NO-UNDO.
DEFINE VARIABLE antalvec AS INTEGER NO-UNDO.
DEFINE VARIABLE startvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE VARIABLE slutvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE VARIABLE tempvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE VARIABLE tempnum AS INTEGER NO-UNDO.
DEFINE VARIABLE colraknare AS INTEGER NO-UNDO.
DEFINE VARIABLE tabort AS LOGICAL NO-UNDO.
DEFINE VARIABLE colvalue AS INTEGER NO-UNDO.
DEFINE VARIABLE labelvar AS CHARACTER NO-UNDO.

DEFINE VARIABLE brwradnr AS INTEGER NO-UNDO.
DEFINE VARIABLE brwrowid AS ROWID NO-UNDO.
DEFINE VARIABLE lastbrwrow AS ROWID NO-UNDO.

DEFINE TEMP-TABLE coltemp
   FIELD PROCH AS HANDLE
   FIELD CH AS HANDLE
   FIELD PROCNAME AS CHARACTER
   FIELD CNAME AS CHARACTER
   FIELD DTYP AS CHARACTER
   FIELD NUM AS INTEGER
   INDEX NUM AS PRIMARY NUM.

DEFINE TEMP-TABLE celltemp
   FIELD PROCH AS HANDLE
   FIELD CH AS HANDLE
   FIELD PROCNAME AS CHARACTER
   FIELD CNAME AS CHARACTER
   FIELD DTYP AS CHARACTER
   FIELD NUM AS INTEGER
   FIELD CROWID AS ROWID
   INDEX NUM AS PRIMARY NUM CNAME.

DEFINE TEMP-TABLE menutemp
   FIELD MENUH AS HANDLE
   FIELD MENUITEMH AS HANDLE
   FIELD BRWH AS HANDLE.

DEFINE INPUT PARAMETER brwh AS HANDLE.
DEFINE INPUT PARAMETER frm AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER fillinsth AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER fillinslh AS HANDLE NO-UNDO.
ASSIGN
dynfrmh = frm
dynbrwh = brwh
fillsth = fillinsth
fillslh = fillinslh
tabort = FALSE.
dynbrwh:NO-EMPTY-SPACE = FALSE NO-ERROR.
ON 'LEAVE':U OF fillsth PERSISTENT RUN leavefillst_UI IN THIS-PROCEDURE.
ON 'LEAVE':U OF fillslh PERSISTENT RUN leavefillsl_UI IN THIS-PROCEDURE.
ON 'VALUE-CHANGED' OF dynbrwh PERSISTENT RUN vbrw_UI IN THIS-PROCEDURE.

RETURN.

PROCEDURE vbrw_UI :
   brwrowid = dynbuffh:ROWID.
   /*
   dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
   dynbrwh:DESELECT-ROWS() NO-ERROR.
   */
END PROCEDURE.
PROCEDURE stalman_UI :
   DEFINE VARIABLE hjdate AS DATE NO-UNDO.
   FIND FIRST celltemp WHERE celltemp.CNAME = "SLUTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
   IF AVAILABLE celltemp THEN DO:
      fieldh = dynbuffh:BUFFER-FIELD(celltemp.CNAME).
      hjdate = fieldh:BUFFER-VALUE.
   END.
   dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
   brwrowid = dynbuffh:ROWID.
   FIND FIRST celltemp WHERE celltemp.CNAME = "STARTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
   IF AVAILABLE celltemp THEN DO:
      fieldh = dynbuffh:BUFFER-FIELD(celltemp.CNAME).
      IF fieldh:BUFFER-VALUE = ? THEN Guru.GlobalaVariabler:regdatum = TODAY.                  
      ELSE Guru.GlobalaVariabler:regdatum = fieldh:BUFFER-VALUE.      
      RUN AlmanBtn.w.
      IF fieldh:BUFFER-VALUE = Guru.GlobalaVariabler:regdatum THEN RETURN.
      IF hjdate NE ? THEN DO:
         IF regdatum > hjdate THEN DO:
            MESSAGE "Startdatum kan inte vara större än slutdatum" VIEW-AS ALERT-BOX.
            RETURN.
         END.
      END.      
      IF regdatum = fieldh:BUFFER-VALUE THEN RETURN.
      fieldh:BUFFER-VALUE = regdatum.      
      celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.
      RUN checkcolumnsm_UI (INPUT 1).      
   END.   
END PROCEDURE.
PROCEDURE checkcolumnsm_UI :
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   IF dynbuffh:AVAILABLE THEN DO:
      dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
      brwrowid = dynbuffh:ROWID.
      FIND FIRST celltemp WHERE celltemp.CNAME = "STARTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
      IF AVAILABLE celltemp THEN DO:
         fieldh = dynbuffh:BUFFER-FIELD(celltemp.CNAME).
         celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.
         stdatum = fieldh:BUFFER-VALUE.      
      END.
      FIND FIRST celltemp WHERE celltemp.CNAME = "SLUTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
      IF AVAILABLE celltemp THEN DO:
         fieldh = dynbuffh:BUFFER-FIELD(celltemp.CNAME).
         celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.
         sldatum = fieldh:BUFFER-VALUE.
      END.
      fieldh = dynbuffh:BUFFER-FIELD("ANDRAD").
      fieldh:BUFFER-VALUE = TRUE. 
      IF vad = 1 THEN      fieldh = dynbuffh:BUFFER-FIELD("STARTDAT").
      ELSE IF vad = 2 THEN fieldh = dynbuffh:BUFFER-FIELD("SLUTDAT").
      RUN setcolorleave_UI.      
   END.
END PROCEDURE.
PROCEDURE setcolorleave_UI :
   IF fieldh = dynbuffh:BUFFER-FIELD("STARTDAT") THEN DO:
      dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
      brwrowid = dynbuffh:ROWID.
      FIND FIRST celltemp WHERE celltemp.CNAME = "MON" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
      IF AVAILABLE celltemp THEN celltemp.CH:BGCOLOR = ?.
      REPEAT:
         FIND NEXT celltemp WHERE celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
         IF AVAILABLE celltemp THEN DO:
            celltemp.CH:BGCOLOR = ?.
         END.
         ELSE LEAVE.
      END.
      RUN setcolor_UI.
   END.
   ELSE IF fieldh = dynbuffh:BUFFER-FIELD("SLUTDAT") THEN DO:
      FIND FIRST celltemp WHERE celltemp.CNAME = "MON" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
      IF AVAILABLE celltemp THEN celltemp.CH:BGCOLOR = ?.
      REPEAT:
         FIND NEXT celltemp WHERE celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
         IF AVAILABLE celltemp THEN DO:
            celltemp.CH:BGCOLOR = ?.
         END.
         ELSE LEAVE.
      END.      
      RUN setcolor_UI.
   END.
   APPLY "LEAVE" TO dynbrwh.   
END PROCEDURE.

PROCEDURE slalman_UI :
   DEFINE VARIABLE hjdate AS DATE NO-UNDO.
   FIND FIRST celltemp WHERE celltemp.CNAME = "STARTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
   IF AVAILABLE celltemp THEN DO:
      fieldh = dynbuffh:BUFFER-FIELD(celltemp.CNAME).
      hjdate = fieldh:BUFFER-VALUE.
   END.
   dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
   brwrowid = dynbuffh:ROWID.
   FIND FIRST celltemp WHERE celltemp.CNAME = "SLUTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
   IF AVAILABLE celltemp THEN DO:
      fieldh = dynbuffh:BUFFER-FIELD(celltemp.CNAME).
      IF fieldh:BUFFER-VALUE = ? THEN Guru.GlobalaVariabler:regdatum = TODAY.                  
      ELSE Guru.GlobalaVariabler:regdatum = fieldh:BUFFER-VALUE.      
      RUN AlmanBtn.w.
      IF fieldh:BUFFER-VALUE = Guru.GlobalaVariabler:regdatum THEN RETURN.
      IF hjdate NE ? THEN DO:
         IF regdatum < hjdate THEN DO:
            MESSAGE "Slutdatum kan inte vara mindre än startdatum" VIEW-AS ALERT-BOX.
            RETURN.
         END.
      END.
      fieldh:BUFFER-VALUE = regdatum.      
      celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.
      RUN checkcolumnsm_UI (INPUT 2).      
   END.
END PROCEDURE.

PROCEDURE createbrw_UI :
   FIND LAST tidsplantemp WHERE tidsplantemp.SLUTDAT NE ? AND tidsplantemp.SLUTDAT < 01/01/2099 USE-INDEX DATUM NO-LOCK NO-ERROR.
   IF AVAILABLE tidsplantemp THEN DO:
      ASSIGN
      regdatum = tidsplantemp.SLUTDAT.      
   END. 
   ELSE DO:
      regdatum = TODAY + 14.
   END.
   slutdatum = regdatum.
   FIND FIRST tidsplantemp WHERE tidsplantemp.STARTDAT NE ? USE-INDEX DATUM NO-LOCK NO-ERROR.
   IF AVAILABLE tidsplantemp THEN DO:
      ASSIGN
      regdatum = tidsplantemp.STARTDAT.     
   END.
   ELSE DO:
      regdatum = TODAY.
   END.
   startdatum = regdatum. 
   RUN brwegenskap_UI.
   /*Skapa fast fält*/
   RUN createfields_UI.
   /*Lägg till veckor*/
   RUN vecka_UI.
   RUN colvalue_UI.
   /*Öppna queryn*/
   RUN dynbrw_UI.
   dynbrwh:NUM-LOCKED-COLUMNS = 9.
   dynqueh:QUERY-OPEN().   
   ENABLE ALL WITH FRAME dynfrmh.
   
   APPLY "LEAVE" TO dynbrwh.
   APPLY "ENTRY" TO fillslh.
   dynbrwh:DESELECT-ROWS() NO-ERROR.
   
END PROCEDURE.       

PROCEDURE brwegenskap_UI :
   /*Ge egenskaper till browser*/
   ASSIGN 
   dynbrwh:NAME                     = "BRW_TIDSPLAN"
   dynbrwh:WIDTH                    = 124
   dynbrwh:HEIGHT                   = 22.8
   dynbrwh:EXPANDABLE               = YES 
   dynbrwh:COLUMN                   = 1.5
   dynbrwh:ROW                      = 2.4
   dynbrwh:FONT                     = 4
   dynbrwh:FRAME                    = dynfrmh
   dynbrwh:MULTIPLE                 = FALSE
   dynbrwh:READ-ONLY                = FALSE
   dynbrwh:ROW-MARKERS              = TRUE
   dynbrwh:SEPARATORS               = TRUE
   dynbrwh:SENSITIVE                = TRUE
   dynbrwh:ALLOW-COLUMN-SEARCHING   = TRUE
   dynbrwh:EXPANDABLE               = FALSE
   dynbrwh:VISIBLE                  = TRUE
   dynbrwh:COLUMN-SCROLLING         = TRUE
   dynbrwh:COLUMN-RESIZABLE         = FALSE
   dynbrwh:MAX-DATA-GUESS           = 100
   dynbrwh:TITLE                    = "Tidsplanering".
   /*Skapa popup menu*/
   /*
   CREATE MENU popmenu
   ASSIGN POPUP-ONLY = TRUE
   TITLE = "Menu".
   ASSIGN dynbrwh:POPUP-MENU = popmenu.
   */
   ON 'LEAVE':U OF dynbrwh PERSISTENT RUN leavebrw_UI IN THIS-PROCEDURE.
   ON 'ENTRY':U OF dynbrwh PERSISTENT RUN entrybrw_UI IN THIS-PROCEDURE.
   /* se rowdispextra_ui
   ON "ROW-DISPLAY" OF dynbrwh PERSISTENT RUN checkcolumns_UI IN THIS-PROCEDURE.
   */
   /*Skapa buffer för temptabell */
   dynbuffh = TEMP-TABLE tidsplantemp:DEFAULT-BUFFER-HANDLE.

   /*Skapa queryn för browsern och förbered den*/
   CREATE QUERY dynqueh.
   dynqueh:SET-BUFFERS(dynbuffh).
   dynbuffh = dynqueh:GET-BUFFER-HANDLE. 
   openquery = "FOR EACH " + dynbuffh:TABLE + " NO-LOCK BY ARBART".
   dynqueh:QUERY-PREPARE(openquery).
   /*Sätt queryn*/
   dynbrwh:QUERY = dynqueh.
   
END PROCEDURE.

PROCEDURE createfields_UI :
   /*Skapa och lägg till de fält som ska finnas med*/
   fieldh = dynbuffh:BUFFER-FIELD("ARBART").
   fieldh:FORMAT = "X(256)".
   tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
   ASSIGN
   tmpcolh:WIDTH-CHARS = 10
   tmpcolh:LABEL = Guru.Konstanter:gartk 
   tmpcolh:READ-ONLY = TRUE 
   tmpcolh:VISIBLE = TRUE.  
   fieldh = dynbuffh:BUFFER-FIELD("AONR").
   
   tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
   ASSIGN
   tmpcolh:WIDTH-CHARS = 6
   tmpcolh:LABEL = Guru.Konstanter:gaok 
   tmpcolh:READ-ONLY = TRUE
   tmpcolh:VISIBLE = TRUE.
   
   fieldh = dynbuffh:BUFFER-FIELD("DELNR").
  
   tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
   ASSIGN
   tmpcolh:WIDTH-CHARS = 3
   tmpcolh:LABEL = "Delnr"
   tmpcolh:READ-ONLY = TRUE
   tmpcolh:VISIBLE = TRUE.
   
   fieldh = dynbuffh:BUFFER-FIELD("ORT").
   fieldh:FORMAT = "X(256)".
   tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
   ASSIGN
   tmpcolh:WIDTH-CHARS = 15
   tmpcolh:LABEL = Guru.Konstanter:gaonamnk
   tmpcolh:READ-ONLY = TRUE
   tmpcolh:VISIBLE = TRUE
   tmpcolh:RESIZABLE = TRUE.      

   fieldh = dynbuffh:BUFFER-FIELD("PERSONAL").
   fieldh:FORMAT = "X(256)".
   tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
   ON 'ENTRY':U OF tmpcolh PERSISTENT RUN entry_UI IN THIS-PROCEDURE.
   ON 'LEAVE':U OF tmpcolh PERSISTENT RUN leave_UI IN THIS-PROCEDURE.
   ASSIGN
   tmpcolh:WIDTH-CHARS = 8
   tmpcolh:LABEL = "Personal" 
   tmpcolh:READ-ONLY = FALSE
   tmpcolh:VISIBLE = TRUE
   tmpcolh:RESIZABLE = TRUE.
   
   fieldh = dynbuffh:BUFFER-FIELD("ENTREPRENOR").
   fieldh:FORMAT = "X(256)".
   tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
   ON 'ENTRY':U OF tmpcolh PERSISTENT RUN entry_UI IN THIS-PROCEDURE.
   ON 'LEAVE':U OF tmpcolh PERSISTENT RUN leave_UI IN THIS-PROCEDURE.
   ASSIGN
   tmpcolh:WIDTH-CHARS = 8
   tmpcolh:LABEL = "Entreprenör" 
   tmpcolh:READ-ONLY = FALSE
   tmpcolh:VISIBLE = TRUE
   tmpcolh:RESIZABLE = TRUE.   

   fieldh = dynbuffh:BUFFER-FIELD("PROJEKTLEDARE").
   fieldh:FORMAT = "X(256)".
   tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
   ON 'ENTRY':U OF tmpcolh PERSISTENT RUN entry_UI IN THIS-PROCEDURE.
   ON 'LEAVE':U OF tmpcolh PERSISTENT RUN leave_UI IN THIS-PROCEDURE.
   ASSIGN
   tmpcolh:WIDTH-CHARS = 8
   tmpcolh:LABEL = "Projektledare" 
   tmpcolh:READ-ONLY = FALSE
   tmpcolh:VISIBLE = TRUE
   tmpcolh:RESIZABLE = TRUE.   

   fieldh = dynbuffh:BUFFER-FIELD("STARTDAT").
   tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
   ON 'LEAVE':U OF tmpcolh PERSISTENT RUN leave_UI IN THIS-PROCEDURE.
   ON 'ENTRY':U OF tmpcolh PERSISTENT RUN entry_UI IN THIS-PROCEDURE.
   ON 'MOUSE-MENU-CLICK':U OF tmpcolh PERSISTENT RUN stalman_UI IN THIS-PROCEDURE.
   
   ASSIGN
   tmpcolh:WIDTH-CHARS = 6
   tmpcolh:LABEL = "Start" 
   tmpcolh:READ-ONLY = FALSE
   tmpcolh:VISIBLE = TRUE.         

   fieldh = dynbuffh:BUFFER-FIELD("SLUTDAT").
   tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
   ON 'LEAVE':U OF tmpcolh PERSISTENT RUN leave_UI IN THIS-PROCEDURE.
   ON 'ENTRY':U OF tmpcolh PERSISTENT RUN entry_UI IN THIS-PROCEDURE.
   ON 'MOUSE-MENU-CLICK':U OF tmpcolh PERSISTENT RUN slalman_UI IN THIS-PROCEDURE.
   ASSIGN
   tmpcolh:WIDTH-CHARS = 6
   tmpcolh:LABEL = "Slut" 
   tmpcolh:READ-ONLY = FALSE
   tmpcolh:VISIBLE = TRUE. 
   tmpcolh = dynbrwh:GET-BROWSE-COLUMN(1).
   
END PROCEDURE.

PROCEDURE vecka_UI :

   IF WEEKDAY(startdatum) = 1 THEN regdatum = startdatum - 6.
   ELSE IF WEEKDAY(startdatum) = 2 THEN regdatum = startdatum.
   ELSE IF WEEKDAY(startdatum) = 3 THEN regdatum = startdatum - 1.
   ELSE IF WEEKDAY(startdatum) = 4 THEN regdatum = startdatum - 2.
   ELSE IF WEEKDAY(startdatum) = 5 THEN regdatum = startdatum - 3.
   ELSE IF WEEKDAY(startdatum) = 6 THEN regdatum = startdatum - 4.
   ELSE IF WEEKDAY(startdatum) = 7 THEN regdatum = startdatum - 5.
   RUN REGVEC.P.
   tempvnr = regvnr.
   REPEAT:
      fieldh = dynbuffh:BUFFER-FIELD("MON").
      tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).    
      ON 'ENTRY':U OF tmpcolh PERSISTENT RUN setcolcell_UI IN THIS-PROCEDURE.
      /* Om första måndag*/
      /*Om skapa ny browser och första veckan*/
      tmpcolh:LABEL = STRING(tempvnr) + "!m". 
      ASSIGN
      tmpcolh:WIDTH-CHARS = 2.2
      tmpcolh:READ-ONLY = FALSE
      tmpcolh:VISIBLE = TRUE.         
      
      fieldh = dynbuffh:BUFFER-FIELD("TIS").
      tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
      ON 'ENTRY':U OF tmpcolh PERSISTENT RUN setcolcell_UI IN THIS-PROCEDURE.
      ASSIGN
      tmpcolh:WIDTH-CHARS = 1.5
      tmpcolh:LABEL = "ti" 
      tmpcolh:READ-ONLY = FALSE
      tmpcolh:VISIBLE = TRUE.         
      
      fieldh = dynbuffh:BUFFER-FIELD("ONS").
      tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
      ON 'ENTRY':U OF tmpcolh PERSISTENT RUN setcolcell_UI IN THIS-PROCEDURE.
      ASSIGN
      tmpcolh:WIDTH-CHARS = 1.5
      tmpcolh:LABEL = "o"
      tmpcolh:READ-ONLY = FALSE
      tmpcolh:VISIBLE = TRUE.
      
      fieldh = dynbuffh:BUFFER-FIELD("TOR").
      tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
      ON 'ENTRY':U OF tmpcolh PERSISTENT RUN setcolcell_UI IN THIS-PROCEDURE.
      ASSIGN
      tmpcolh:WIDTH-CHARS = 1.5
      tmpcolh:LABEL = "to"
      tmpcolh:READ-ONLY = FALSE
      tmpcolh:VISIBLE = TRUE.         
      
      fieldh = dynbuffh:BUFFER-FIELD("FRE").
      tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
      ON 'ENTRY':U OF tmpcolh PERSISTENT RUN setcolcell_UI IN THIS-PROCEDURE.
      ASSIGN
      tmpcolh:WIDTH-CHARS = 1.5
      tmpcolh:LABEL = "f" 
      tmpcolh:READ-ONLY = FALSE
      tmpcolh:VISIBLE = TRUE.         
      
      fieldh = dynbuffh:BUFFER-FIELD("LOR").
      tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
      ON 'ENTRY':U OF tmpcolh PERSISTENT RUN setcolcell_UI IN THIS-PROCEDURE.
      ASSIGN
      tmpcolh:WIDTH-CHARS = 1.5
      tmpcolh:LABEL = "l" 
      tmpcolh:READ-ONLY = FALSE
      tmpcolh:VISIBLE = TRUE.         
      
      fieldh = dynbuffh:BUFFER-FIELD("SON").
      tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
      ON 'ENTRY':U OF tmpcolh PERSISTENT RUN setcolcell_UI IN THIS-PROCEDURE.
      ASSIGN
      tmpcolh:WIDTH-CHARS = 1.5
      tmpcolh:LABEL = "s" 
      tmpcolh:READ-ONLY = FALSE
      tmpcolh:VISIBLE = TRUE.              
      regdatum = regdatum + 7.
      IF regdatum > slutdatum THEN LEAVE.
      RUN REGVEC.P.
      tempvnr = regvnr.
   END.    
   IF tabort = TRUE THEN DO:
      EMPTY TEMP-TABLE celltemp NO-ERROR.  
      tabort = FALSE.
   END.
   FIND LAST celltemp USE-INDEX NUM NO-LOCK NO-ERROR.
   IF AVAILABLE celltemp THEN tempnum = celltemp.NUM + 1.
   ELSE tempnum = 1. 
   RUN columnstotemp_UI.
   dynbrwh:DESELECT-ROWS() NO-ERROR.    
END PROCEDURE.

PROCEDURE colvalue_UI :
   dynqueh:GET-FIRST(NO-LOCK).
   REPEAT:
      IF dynqueh:QUERY-OFF-END THEN LEAVE.  
      fieldh = dynbuffh:BUFFER-FIELD("PLAN").
      IF fieldh:BUFFER-VALUE = TRUE THEN DO: /*Planerad aktivitet*/
         IF globforetag = "SUND" OR globforetag = "SNAT" OR globforetag = "MISV" OR globforetag = "ELPA" THEN DO:
            fieldh = dynbuffh:BUFFER-FIELD("ARBARTKOD").
             /*Arbartskoder för El*/
            IF fieldh:BUFFER-VALUE = 11 OR fieldh:BUFFER-VALUE = 12 OR 
            fieldh:BUFFER-VALUE = 13 OR fieldh:BUFFER-VALUE = 14 OR fieldh:BUFFER-VALUE = 15 THEN DO:
               fieldh = dynbuffh:BUFFER-FIELD("FARGNR").
               fieldh:BUFFER-VALUE = 9.         
            END.
            ELSE DO:
               fieldh = dynbuffh:BUFFER-FIELD("FARGNR").
               fieldh:BUFFER-VALUE = 12.         
            END.
         END.         
      END.
      ELSE DO: /*Tillfällig aktivitet*/
         IF globforetag = "SUND" OR globforetag = "SNAT" OR globforetag = "MISV" OR globforetag = "ELPA" THEN DO:
            fieldh = dynbuffh:BUFFER-FIELD("ARBARTKOD").
            /*Arbartskoder för El*/
            IF fieldh:BUFFER-VALUE = 11 OR fieldh:BUFFER-VALUE = 12 OR 
            fieldh:BUFFER-VALUE = 13 OR fieldh:BUFFER-VALUE = 14 OR fieldh:BUFFER-VALUE = 15 THEN DO:
               fieldh = dynbuffh:BUFFER-FIELD("FARGNR").
               fieldh:BUFFER-VALUE = 14.         
            END.
            ELSE DO:
               fieldh = dynbuffh:BUFFER-FIELD("FARGNR").
               fieldh:BUFFER-VALUE = 2.         
            END.
         END.                 
      END.
      dynqueh:GET-NEXT(NO-LOCK).
   END.

END PROCEDURE.

/* IF SELF:HANDLE <> BROWSE BROWSE-1:HANDLE THEN DO: */
/*    ASSIGN hLast = LAST-EVENT:WIDGET-LEAVE.        */
/*    IF VALID-HANDLE(hLast) THEN DO:                */
/*       SELF:READ-ONLY = TRUE.                      */
/*       APPLY "ENTRY" TO hLast.                     */
/*       SELF:READ-ONLY = FALSE.                     */
/*       SELF:FONT = IF SELF:FONT = ? THEN 6 ELSE ?. */
/*       RETURN NO-APPLY.                            */
/*    END.                                           */
/* END.                                              */

PROCEDURE setcolcell_UI :
   IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
      RETURN NO-APPLY.
   END.
   ELSE DO:
      IF SELF:HANDLE <> dynbrwh:HANDLE THEN DO:
         dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
         brwrowid = dynbuffh:ROWID.
         hLast = LAST-EVENT:WIDGET-LEAVE.                                                                                                  
         IF SELF:NAME = "MON" OR SELF:NAME = "TIS" OR SELF:NAME = "ONS" OR SELF:NAME = "TOR" OR
         SELF:NAME = "FRE" OR SELF:NAME = "LOR" OR SELF:NAME = "SON" THEN DO:
            SELF:READ-ONLY = TRUE.
            IF VALID-HANDLE(hLast) THEN APPLY "ENTRY" TO hLast.
            ELSE APPLY "ENTRY" TO fillsth.
            SELF:READ-ONLY = FALSE.
            FIND FIRST celltemp WHERE celltemp.CH = SELF AND celltemp.CROWID = brwrowid NO-LOCK NO-ERROR.
            IF AVAILABLE celltemp THEN DO:
               fieldh = dynbuffh:BUFFER-FIELD("FARGNR").
               celltemp.CH:BGCOLOR = IF SELF:BGCOLOR = ? THEN fieldh:BUFFER-VALUE ELSE ?.
               RUN checkdate_UI.                                   
            END.
            RETURN NO-APPLY.
         END.      
      END.   
   END.
   
END PROCEDURE.

PROCEDURE checkdate_UI :
   DEFINE VARIABLE step AS INTEGER NO-UNDO.
   DEFINE VARIABLE colname AS CHARACTER NO-UNDO.
   ASSIGN
   colname = ""
   step = 0.
   IF celltemp.CNAME = "SON" THEN DO: 
      ASSIGN
      step = 6.
      colname = "sön".
   END.
   ELSE IF celltemp.CNAME = "LOR" THEN DO:
      ASSIGN
      step = 5
      colname = "lör".
   END.
   ELSE IF celltemp.CNAME = "FRE" THEN DO:
      ASSIGN
      step = 4
      colname = "fre".
   END.
   ELSE IF celltemp.CNAME = "TOR" THEN DO:
      ASSIGN
      step = 3
      colname = "tor".                    
   END.
   ELSE IF celltemp.CNAME = "ONS" THEN DO:
      ASSIGN
      step = 2
      colname = "ons".
   END.
   ELSE IF celltemp.CNAME = "TIS" THEN DO:
      ASSIGN
      step = 1
      colname = "tis".
   END.
   ELSE IF celltemp.CNAME = "MON" THEN DO:
      ASSIGN
      step = 0
      colname = "mån".
   END.
   DO WHILE step > 0:
      FIND PREV celltemp WHERE celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
      step = step - 1.  
   END.
   IF AVAILABLE celltemp THEN DO:
      IF celltemp.CNAME = "MON" THEN DO:
         regvnr = INTEGER(SUBSTRING(celltemp.CH:LABEL,1,3)).
         regdagnamn = colname.
         RUN VECODAT.P.
         IF dynbuffh:AVAILABLE = TRUE THEN DO:
            dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
            FIND FIRST celltemp WHERE celltemp.CNAME = "STARTDAT" AND celltemp.CROWID = brwrowid NO-LOCK NO-ERROR.
            IF AVAILABLE celltemp THEN DO:
               fieldh = dynbuffh:BUFFER-FIELD(celltemp.CNAME).
               IF regdatum <= fieldh:BUFFER-VALUE THEN DO:                  
                  fieldh:BUFFER-VALUE = regdatum.                     
                  celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.
               END.   
               ELSE IF regdatum = fieldh:BUFFER-VALUE THEN DO: 
                  fieldh:BUFFER-VALUE = regdatum + 1.
                  celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.
               END.
               ELSE IF fieldh:BUFFER-VALUE = ? THEN DO:
                  fieldh:BUFFER-VALUE = regdatum.
                  celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.
               END.
            END.   
            FIND FIRST celltemp WHERE celltemp.CNAME = "SLUTDAT" AND celltemp.CROWID = brwrowid NO-LOCK NO-ERROR.
            IF AVAILABLE celltemp THEN DO:
               fieldh = dynbuffh:BUFFER-FIELD(celltemp.CNAME).
               IF regdatum > fieldh:BUFFER-VALUE THEN DO:
                  fieldh:BUFFER-VALUE = regdatum.           
                  celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.                 
               END.
               ELSE IF regdatum = fieldh:BUFFER-VALUE  THEN DO:
                  fieldh:BUFFER-VALUE = regdatum - 1.       
                  celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.                 
               END.                  
               ELSE IF fieldh:BUFFER-VALUE = ? THEN DO:
                  fieldh:BUFFER-VALUE = regdatum.
               END.
            END.
            fieldh = dynbuffh:BUFFER-FIELD("ANDRAD").
            fieldh:BUFFER-VALUE = TRUE. 
            dynbrwh:DESELECT-ROWS() NO-ERROR.
            RUN checkcolumns_UI.
         END.
      END.         
   END.         
END PROCEDURE.

PROCEDURE columnstotemp_UI.
   DEFINE VARIABLE spartempnum AS INTEGER NO-UNDO.
   IF dynqueh:QUERY-OPEN() = ? THEN dynqueh:QUERY-OPEN().
   spartempnum = tempnum.
   dynqueh:GET-FIRST(NO-LOCK).
   IF dynqueh:QUERY-OFF-END THEN LEAVE.
   REPEAT:
      tempcolh = dynbrwh:GET-BROWSE-COLUMN(tempnum).
      brwrowid = dynbuffh:ROWID.
      DO WHILE VALID-HANDLE(tempcolh) AND dynbuffh:ROWID = brwrowid: 
         fieldh = dynbuffh:BUFFER-FIELD(tempcolh:NAME).
         CREATE celltemp.
         ASSIGN
         celltemp.DTYP = tempcolh:DATA-TYP
         celltemp.CH = tempcolh
         celltemp.CNAME = tempcolh:NAME
         celltemp.NUM = tempnum
         celltemp.CROWID = dynbuffh:ROWID
         tempcolh = tempcolh:NEXT-COLUMN.
         tempnum = tempnum + 1.              
      END.
      dynqueh:GET-NEXT(NO-LOCK).
      tempnum = spartempnum.
      IF dynqueh:QUERY-OFF-END THEN LEAVE.
   END.
     
END PROCEDURE.

PROCEDURE rensa_UI :
   dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
   brwrowid = dynbuffh:ROWID.
   FIND FIRST celltemp WHERE celltemp.CNAME = "MON" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
   IF AVAILABLE celltemp THEN celltemp.CH:BGCOLOR = ?.
   REPEAT:
      FIND NEXT celltemp WHERE celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
      IF AVAILABLE celltemp THEN DO:
         celltemp.CH:BGCOLOR = ?.
      END.
      ELSE LEAVE.
   END.
   FIND FIRST celltemp WHERE celltemp.CNAME = "STARTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
   IF AVAILABLE celltemp THEN DO:
      fieldh = dynbuffh:BUFFER-FIELD(celltemp.CNAME).
      fieldh:BUFFER-VALUE = "".
      celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE. 
   END.
   FIND FIRST celltemp WHERE celltemp.CNAME = "SLUTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
   IF AVAILABLE celltemp THEN DO:
      fieldh = dynbuffh:BUFFER-FIELD(celltemp.CNAME).
      fieldh:BUFFER-VALUE = "".
      celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE. 
   END.
   dynbrwh:DESELECT-ROWS() NO-ERROR.
   
END PROCEDURE.

PROCEDURE leavebrw_UI : 
   dynbrwh:DESELECT-ROWS() NO-ERROR.
END PROCEDURE.

PROCEDURE entrybrw_UI : 
   dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
   RUN getrownr_UI.
   dynbrwh:DESELECT-ROWS() NO-ERROR.
END PROCEDURE.

PROCEDURE rowdispextra_UI:
   DEFINE INPUT PARAMETER TABLE FOR coltemp.
   DEFINE INPUT PARAMETER brwh AS HANDLE NO-UNDO.
   dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR. 
   ASSIGN
   stdatum = ?
   sldatum = ?.
   RUN checkcolumns_UI.  
   dynbrwh:DESELECT-ROWS() NO-ERROR. 
END PROCEDURE.

PROCEDURE leave_UI :
   DEFINE VARIABLE hjdate AS DATE NO-UNDO.
   DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
   fieldh = dynbuffh:BUFFER-FIELD(FOCUS:NAME).
   dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
   brwrowid = dynbuffh:ROWID.
   musz = FALSE.
   IF fieldh:DATA-TYPE = "DATE" THEN DO:
      IF fieldh:BUFFER-VALUE = DATE(SELF:SCREEN-VALUE) THEN.
      ELSE musz = TRUE.
   END.
   ELSE IF fieldh:BUFFER-VALUE = SELF:SCREEN-VALUE THEN.
   ELSE musz = TRUE.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      IF fieldh = dynbuffh:BUFFER-FIELD("STARTDAT") THEN DO:
         dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
         brwrowid = dynbuffh:ROWID.
         FIND FIRST celltemp WHERE celltemp.CNAME = "SLUTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
         IF AVAILABLE celltemp THEN DO:
            fieldh = dynbuffh:BUFFER-FIELD(celltemp.CNAME).
            hjdate = fieldh:BUFFER-VALUE.
            IF hjdate = ? THEN.
            ELSE DO:
               IF DATE(SELF:SCREEN-VALUE) > hjdate THEN DO:
                  MESSAGE "Startdatum kan inte vara större än slutdatum" VIEW-AS ALERT-BOX.
                  RETURN NO-APPLY.
               END.
            END.
         END.
         fieldh = dynbuffh:BUFFER-FIELD("STARTDAT").
         FIND FIRST celltemp WHERE celltemp.CNAME = "MON" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
         IF AVAILABLE celltemp THEN celltemp.CH:BGCOLOR = ?.
         REPEAT:
            FIND NEXT celltemp WHERE celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
            IF AVAILABLE celltemp THEN DO:
               celltemp.CH:BGCOLOR = ?.
            END.
            ELSE LEAVE.
         END.
         FIND FIRST celltemp WHERE celltemp.CNAME = "STARTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
         IF AVAILABLE celltemp THEN DO:
            fieldh:BUFFER-VALUE = DATE(SELF:SCREEN-VALUE).
            celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.
            FIND FIRST celltemp WHERE celltemp.CNAME = "SLUTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
            IF AVAILABLE celltemp THEN DO:
               fieldh2 = dynbuffh:BUFFER-FIELD("SLUTDAT").
               celltemp.CH:SCREEN-VALUE = fieldh2:BUFFER-VALUE.
               IF fieldh:BUFFER-VALUE > fieldh2:BUFFER-VALUE THEN DO:
                  fieldh2:BUFFER-VALUE = fieldh:BUFFER-VALUE.
                  celltemp.CH:SCREEN-VALUE = fieldh2:BUFFER-VALUE.
               END.
            END.
         END.
      END.
      ELSE IF fieldh = dynbuffh:BUFFER-FIELD("SLUTDAT") THEN DO:
         dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
         brwrowid = dynbuffh:ROWID.
         FIND FIRST celltemp WHERE celltemp.CNAME = "STARTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
         IF AVAILABLE celltemp THEN DO:
            fieldh = dynbuffh:BUFFER-FIELD(celltemp.CNAME).
            hjdate = fieldh:BUFFER-VALUE.
            IF hjdate = ? THEN.
            ELSE DO:
               IF DATE(SELF:SCREEN-VALUE) < hjdate THEN DO:
                  MESSAGE "Slutdatum kan inte vara mindre än startdatum" VIEW-AS ALERT-BOX.
                  RETURN NO-APPLY.                  
               END.
            END.
         END.
         fieldh = dynbuffh:BUFFER-FIELD("SLUTDAT").
         FIND FIRST celltemp WHERE celltemp.CNAME = "MON" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
         IF AVAILABLE celltemp THEN celltemp.CH:BGCOLOR = ?.
         REPEAT:
            FIND NEXT celltemp WHERE celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
            IF AVAILABLE celltemp THEN DO:
               celltemp.CH:BGCOLOR = ?.
            END.
            ELSE LEAVE.
         END.
         FIND FIRST celltemp WHERE celltemp.CNAME = "SLUTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
         IF AVAILABLE celltemp THEN DO:
            fieldh:BUFFER-VALUE = DATE(SELF:SCREEN-VALUE).
            celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.

            FIND FIRST celltemp WHERE celltemp.CNAME = "STARTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
            IF AVAILABLE celltemp THEN DO:
               fieldh2 = dynbuffh:BUFFER-FIELD("STARTDAT").
               IF fieldh:BUFFER-VALUE < fieldh2:BUFFER-VALUE THEN DO:
                  fieldh2:BUFFER-VALUE = fieldh:BUFFER-VALUE.
                  celltemp.CH:SCREEN-VALUE = fieldh2:BUFFER-VALUE.
               END.
            END.
         END.
         APPLY "LEAVE" TO dynbrwh.
      END. 
      ELSE IF fieldh = dynbuffh:BUFFER-FIELD("PERSONAL") THEN DO:
         FIND FIRST celltemp WHERE celltemp.CNAME = "PERSONAL" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
         IF AVAILABLE celltemp THEN DO:
            fieldh:BUFFER-VALUE = SELF:SCREEN-VALUE.
            celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.             
         END.
         
      END. 
      ELSE IF fieldh = dynbuffh:BUFFER-FIELD("ENTREPRENOR") THEN DO:
         FIND FIRST celltemp WHERE celltemp.CNAME = "ENTREPRENOR" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
         IF AVAILABLE celltemp THEN DO:
            fieldh:BUFFER-VALUE = SELF:SCREEN-VALUE.
            celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.                         
         END.
         
      END. 
      ELSE IF fieldh = dynbuffh:BUFFER-FIELD("PROJEKTLEDARE") THEN DO:
         FIND FIRST celltemp WHERE celltemp.CNAME = "PROJEKTLEDARE" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
         IF AVAILABLE celltemp THEN DO:
            fieldh:BUFFER-VALUE = SELF:SCREEN-VALUE.
            celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.                        
         END.
         
      END. 
      fieldh = dynbuffh:BUFFER-FIELD("ANDRAD").
      fieldh:BUFFER-VALUE = TRUE.
      RUN checkcolumns_UI.
   END.
   dynbrwh:DESELECT-ROWS() NO-ERROR.
   musz = FALSE.
END PROCEDURE.
       
PROCEDURE entry_UI : 
   dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
END PROCEDURE.

PROCEDURE leavefillst_UI :
   regdatum = DATE(fillsth:SCREEN-VALUE).
   IF startdatum = regdatum THEN RETURN.
   startdatum = regdatum.
   regdatum = DATE(fillslh:SCREEN-VALUE).
   slutdatum = regdatum.
   
   RUN newbrw_UI.
   
END PROCEDURE.

PROCEDURE leavefillsl_UI :
   regdatum = DATE(fillslh:SCREEN-VALUE).
   IF slutdatum = regdatum THEN RETURN.
   slutdatum = regdatum.
   regdatum = DATE(fillsth:SCREEN-VALUE).
   startdatum = regdatum.
   
   RUN newbrw_UI.
   
END.


PROCEDURE checkcolumns_UI :
   IF dynbuffh:AVAILABLE THEN DO:
      dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
      brwrowid = dynbuffh:ROWID.
      FIND FIRST celltemp WHERE celltemp.CNAME = "STARTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
      IF AVAILABLE celltemp THEN DO:
         fieldh = dynbuffh:BUFFER-FIELD(celltemp.CNAME).
         celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.
         stdatum = fieldh:BUFFER-VALUE.      
      END.
      FIND FIRST celltemp WHERE celltemp.CNAME = "SLUTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
      IF AVAILABLE celltemp THEN DO:
         fieldh = dynbuffh:BUFFER-FIELD(celltemp.CNAME).
         celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.
         sldatum = fieldh:BUFFER-VALUE.
      END.
      IF stdatum NE ? AND sldatum NE ? THEN DO:
         RUN setcolor_UI.
      END.
      dynbrwh:DESELECT-ROWS() NO-ERROR.      
   END.
END PROCEDURE.

PROCEDURE getrownr_UI :
   brwradnr = dynbrwh:FOCUSED-ROW.   
   brwrowid = dynbuffh:ROWID.  
END PROCEDURE.

PROCEDURE setcolor_UI :
   brwrowid = dynbuffh:ROWID.
   /*Stega fram till startpos*/
   ASSIGN 
   regdatum = startdatum.
   RUN REGVEC.P.
   regdagnamn = "mån".
   RUN VECODAT.P.
   ASSIGN
   startpos = stdatum - regdatum
   loopraknare = 1.
   FIND FIRST celltemp WHERE celltemp.CNAME = "MON" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
   IF AVAILABLE celltemp THEN celltemp.CH:BGCOLOR = ?.
   DO WHILE loopraknare LE startpos:
      FIND NEXT celltemp WHERE celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
      IF AVAILABLE celltemp THEN DO:
         celltemp.CH:BGCOLOR = IF SELF:BGCOLOR = ? THEN fieldh:BUFFER-VALUE ELSE ?.          
         loopraknare = loopraknare + 1.
      END.
      ELSE loopraknare = startpos + 1.
   END.
   /*Stega och färga hela intervallet*/
   IF sldatum = stdatum OR stdatum > sldatum THEN antalcol = 1.
   ELSE IF sldatum > stdatum THEN antalcol = (sldatum + 1) - stdatum.
   loopraknare = 0.
   fieldh = dynbuffh:BUFFER-FIELD("FARGNR").
   DO WHILE loopraknare < antalcol:
      IF AVAILABLE celltemp THEN DO: 
         IF celltemp.CNAME = "MON" OR celltemp.CNAME = "TIS" OR celltemp.CNAME = "ONS" OR celltemp.CNAME = "TOR" OR
         celltemp.CNAME = "FRE" OR celltemp.CNAME = "LOR" OR celltemp.CNAME = "SON" THEN DO:           
            celltemp.CH:BGCOLOR = fieldh:BUFFER-VALUE.                                   
         END.
         loopraknare = loopraknare + 1.
      END.
      ELSE loopraknare = antalcol + 1.
      FIND NEXT celltemp WHERE celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
   END.
   ASSIGN
   stdatum = ?
   sldatum = ?.
   
END PROCEDURE.

PROCEDURE newbrw_UI :
   /*Ge egenskaper till browser*/
   IF VALID-HANDLE(brwproch) THEN DELETE PROCEDURE brwproch NO-ERROR.
   IF VALID-HANDLE(dynbrwh) THEN DELETE OBJECT dynbrwh.
   IF VALID-HANDLE(dynqueh) THEN DELETE OBJECT dynqueh.
   IF VALID-HANDLE(dynbuffh) THEN DELETE OBJECT dynbuffh NO-ERROR.
   IF VALID-HANDLE(tmpcolh) THEN DELETE OBJECT tmpcolh.
   IF VALID-HANDLE(fieldh) THEN DELETE OBJECT fieldh.
   ASSIGN
   brwproch = ?
   dynbrwh = ?
   dynqueh = ?
   dynbuffh = ?
   tmpcolh = ?
   fieldh = ?. 

   EMPTY TEMP-TABLE celltemp NO-ERROR. 
   CREATE BROWSE dynbrwh.
   dynbrwh:HIDDEN = TRUE.
   RUN brwegenskap_UI.
   dynbrwh:HIDDEN = TRUE.
   /*Skapa fast fält*/
   RUN createfields_UI.  
   /*Lägg till veckor*/
   tabort = TRUE.
   RUN vecka_UI.
   tabort = FALSE.    
   RUN colvalue_UI.
   /*Öppna queryn*/
   RUN dynbrw_UI.
   dynbrwh:NUM-LOCKED-COLUMNS = 9.
   IF dynqueh:QUERY-OPEN() = ? THEN dynqueh:QUERY-OPEN().   
   ENABLE ALL.
   dynbrwh:DESELECT-ROWS() NO-ERROR.   
   APPLY "LEAVE" TO dynbrwh.      
   dynbrwh:HIDDEN = FALSE.

END PROCEDURE.

PROCEDURE dynbrw_UI :
   RUN DYNBRW.P PERSISTENT SET brwproch (INPUT dynbrwh).
   RUN dynprogextra IN brwproch (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN brwproch (INPUT TRUE).
   RUN brwsetupstop_UI IN brwproch (INPUT 1).
   RUN addmenuitem2_UI IN brwproch (INPUT dynbrwh, INPUT "Sätt startdatum", INPUT "stalman_UI").
   RUN addmenuitem2_UI IN brwproch (INPUT dynbrwh, INPUT "Sätt slutdatum", INPUT "slalman_UI"). 
   
END PROCEDURE.


PROCEDURE releaseh_UI :
   dynqueh:QUERY-CLOSE NO-ERROR.
   DELETE OBJECT dynqueh NO-ERROR.  
   DELETE OBJECT dynbrwh NO-ERROR.
   
END PROCEDURE.                   

PROCEDURE pers_UI :
   DEFINE INPUT PARAMETER persh AS HANDLE NO-UNDO.
   FIND FIRST celltemp WHERE celltemp.CROWID = brwrowid NO-LOCK NO-ERROR.
   IF AVAILABLE celltemp THEN DO:
      dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
      fieldh = dynbuffh:BUFFER-FIELD("PERSONAL").
      IF persh:SCREEN-VALUE NE "" THEN do:
         fieldh:BUFFER-VALUE = persh:SCREEN-VALUE.
         celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.
         fieldh = dynbuffh:BUFFER-FIELD("ANDRAD").
         fieldh:BUFFER-VALUE = TRUE. 
         dynbrwh:DESELECT-ROWS() NO-ERROR.
      END.
   END.
END PROCEDURE.

PROCEDURE entre_UI :
   DEFINE INPUT PARAMETER entreh AS HANDLE NO-UNDO.
   FIND FIRST celltemp WHERE celltemp.CROWID = brwrowid NO-LOCK NO-ERROR.
   IF AVAILABLE celltemp THEN DO:
      dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
      fieldh = dynbuffh:BUFFER-FIELD("ENTREPRENOR").
      IF entreh:SCREEN-VALUE NE "" THEN DO:
         fieldh:BUFFER-VALUE = entreh:SCREEN-VALUE.
         celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.
         fieldh = dynbuffh:BUFFER-FIELD("ANDRAD").
         fieldh:BUFFER-VALUE = TRUE. 
         dynbrwh:DESELECT-ROWS() NO-ERROR.
      END.
   END.
END PROCEDURE.

PROCEDURE proj_UI :
   DEFINE INPUT PARAMETER projh AS HANDLE NO-UNDO.
   FIND FIRST celltemp WHERE celltemp.CROWID = brwrowid NO-LOCK NO-ERROR.
   IF AVAILABLE celltemp THEN DO:
      dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
      fieldh = dynbuffh:BUFFER-FIELD("PROJEKTLEDARE").
      IF projh:SCREEN-VALUE NE "" THEN DO: 
         fieldh:BUFFER-VALUE = projh:SCREEN-VALUE.
         celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.
         fieldh = dynbuffh:BUFFER-FIELD("ANDRAD").
         fieldh:BUFFER-VALUE = TRUE. 
         dynbrwh:DESELECT-ROWS() NO-ERROR.
      END.
   END.
END PROCEDURE.
