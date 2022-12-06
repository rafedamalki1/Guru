
/*xTIDSPLANDYN.P*/
{GLOBVAR2DEL1.I}
/*{EGENBEN.I}*/
{REGVAR.I}
DEFINE VARIABLE dynbrwh AS HANDLE NO-UNDO.
DEFINE VARIABLE dynfrmh AS HANDLE NO-UNDO.
DEFINE VARIABLE dynbuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE dynqueh AS HANDLE NO-UNDO.
DEFINE VARIABLE openquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE tmpcolh AS HANDLE NO-UNDO.
DEFINE VARIABLE tempcolh AS HANDLE NO-UNDO.
DEFINE VARIABLE fieldh AS HANDLE NO-UNDO.
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
DEFINE VARIABLE popmenu AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE coltemp
   FIELD PROCH AS HANDLE
   FIELD CH AS HANDLE
   FIELD PROCNAME AS CHARACTER
   FIELD CNAME AS CHARACTER
   FIELD DTYP AS CHARACTER
   FIELD NUM AS INTEGER
   INDEX NUM AS PRIMARY NUM CNAME.

DEFINE TEMP-TABLE menutemp
   FIELD MENUH AS HANDLE
   FIELD MENUITEMH AS HANDLE
   FIELD BRWH AS HANDLE.

DEFINE TEMP-TABLE tidsplantemp NO-UNDO 
   FIELD AONR           AS CHARACTER
   FIELD DELNR          AS INTEGER
   FIELD ARBART          AS CHARACTER
   FIELD AKTIVITET      AS CHARACTER FORMAT "x(256)"
   FIELD TIDLAGE        AS CHARACTER
   FIELD IDTIDLAG       AS CHARACTER
   FIELD TIDSPERIOD     AS CHARACTER
   FIELD STARTDAT       AS DATE 
   FIELD SLUTDAT        AS DATE 
   FIELD PERSONAL       AS CHARACTER
   FIELD ANDRAD         AS LOGICAL
   FIELD MON            AS CHARACTER
   FIELD TIS            AS CHARACTER
   FIELD ONS            AS CHARACTER
   FIELD TOR            AS CHARACTER
   FIELD FRE            AS CHARACTER
   FIELD LOR            AS CHARACTER
   FIELD SON            AS CHARACTER
   INDEX AONR IS PRIMARY AONR DELNR STARTDAT
   INDEX DATUM STARTDAT SLUTDAT AONR DELNR.

DEFINE INPUT PARAMETER brwh AS HANDLE.
DEFINE INPUT PARAMETER frm AS HANDLE NO-UNDO.
ASSIGN
dynfrmh = frm
dynbrwh = brwh
tabort = FALSE.

RETURN.

PROCEDURE brwegenskap_UI :
   /*Ge egenskaper till browser*/
   ASSIGN 
   dynbrwh:NAME                     = "BRW_TIDSPLAN"
   dynbrwh:WIDTH                    = 124
   dynbrwh:HEIGHT                   = 23.25
   dynbrwh:EXPANDABLE               = YES 
   dynbrwh:COLUMN                   = 1.5
   dynbrwh:ROW                      = 2.8
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
   CREATE MENU popmenu
   ASSIGN POPUP-ONLY = TRUE
   TITLE = "Menu".
   ASSIGN dynbrwh:POPUP-MENU = popmenu.

   /*Skapa buffer för temptabell */
   dynbuffh = TEMP-TABLE tidsplantemp:DEFAULT-BUFFER-HANDLE.

   /*Skapa queryn för browsern och förbered den*/
   CREATE QUERY dynqueh.
   dynqueh:SET-BUFFERS(dynbuffh).
   dynbuffh = dynqueh:GET-BUFFER-HANDLE. 
   openquery = "FOR EACH " + dynbuffh:TABLE + " NO-LOCK.".
   dynqueh:QUERY-PREPARE(openquery).
   /*Sätt queryn*/
   dynbrwh:QUERY = dynqueh.

   /*Skapa fast fält*/
   RUN createfields_UI. 

END PROCEDURE.


PROCEDURE createbrw_UI :
   DEFINE INPUT PARAMETER TABLE FOR tidsplantemp.
   FIND FIRST tidsplantemp WHERE tidsplantemp.STARTDAT NE ? USE-INDEX DATUM NO-LOCK NO-ERROR.
   IF AVAILABLE tidsplantemp THEN DO:
      ASSIGN
      regdatum = tidsplantemp.STARTDAT.
      RUN REGVEC.P.
      ASSIGN
      startvnr = regvnr
      startdatum = regdatum.
   END.
   FIND LAST tidsplantemp WHERE tidsplantemp.SLUTDAT NE ? AND tidsplantemp.SLUTDAT < 01/01/2099 USE-INDEX DATUM NO-LOCK NO-ERROR.
   IF AVAILABLE tidsplantemp THEN DO:
      ASSIGN
      regdatum = tidsplantemp.SLUTDAT.
      RUN REGVEC.P.
      ASSIGN
      slutvnr = regvnr
      slutdatum = regdatum.
   END.  
   IF startvnr NE 0 AND slutvnr NE 0 THEN DO:
      antalveckor = (slutvnr + 1) - startvnr.
      IF antalveckor > 52 THEN antalveckor = 52.
      ELSE IF antalveckor = 0 THEN antalveckor = 1.
   END.
   ELSE antalveckor = 1.
   RUN brwegenskap_UI.
   /*Lägg till veckor*/
   RUN vecka_UI (INPUT antalveckor).
   /*Öppna queryn*/
   dynqueh:QUERY-OPEN().  
   
   ENABLE ALL WITH FRAME dynfrmh.

   APPLY "ENTRY" TO dynbrwh.
   
END PROCEDURE.       

PROCEDURE colvalue_UI :
   DEFINE INPUT PARAMETER widgeth AS HANDLE NO-UNDO.
   IF VALID-HANDLE(widgeth) THEN DO:
      IF widgeth:BGCOLOR = 12 THEN DO:
         colvalue = 12.
      END.
      ELSE IF widgeth:BGCOLOR = 2 THEN DO:
         colvalue = 2.
      END.
      ELSE IF widgeth:BGCOLOR = 9 THEN DO:
         colvalue = 9.         
      END.
      ELSE IF widgeth:BGCOLOR = 14 THEN DO:
         colvalue = 14.         
      END.      
   END.
END PROCEDURE.

PROCEDURE setcol_UI :
   IF SELF:HANDLE <> dynbrwh:HANDLE THEN DO:
      ASSIGN hLast = LAST-EVENT:WIDGET-LEAVE.
      IF VALID-HANDLE(hLast) THEN DO:
         IF SELF:NAME = "MON" OR SELF:NAME = "TIS" OR SELF:NAME = "ONS" OR SELF:NAME = "TOR" OR
         SELF:NAME = "FRE" OR SELF:NAME = "LOR" OR SELF:NAME = "SON" THEN DO:
            SELF:READ-ONLY = TRUE.
            APPLY "ENTRY" TO hLast.
            SELF:READ-ONLY = FALSE.
            SELF:BGCOLOR = IF SELF:BGCOLOR = ? THEN colvalue ELSE ?.
            RUN checkdate_UI.
            APPLY "ENTRY" TO hLast.
            RETURN NO-APPLY.
         END.
      END.      
   END.
   
END PROCEDURE.

PROCEDURE checkdate_UI :
   DEFINE VARIABLE step AS INTEGER NO-UNDO.
   DEFINE VARIABLE colname AS CHARACTER NO-UNDO.
   FIND FIRST coltemp WHERE coltemp.CH = SELF NO-LOCK NO-ERROR.
   IF AVAILABLE coltemp THEN DO:
      IF labelvar NE "" THEN DO:
         MESSAGE labelvar VIEW-AS ALERT-BOX.
      END.
      IF coltemp.CNAME = "SON" THEN DO: 
         ASSIGN
         step = 6.
         colname = "sön".
      END.
      ELSE IF coltemp.CNAME = "LOR" THEN DO:
         ASSIGN
         step = 5
         colname = "lör".
      END.
      ELSE IF coltemp.CNAME = "FRE" THEN DO:
         ASSIGN
         step = 4
         colname = "fre".
      END.
      ELSE IF coltemp.CNAME = "TOR" THEN DO:
         ASSIGN
         step = 3
         colname = "tor".                    
      END.
      ELSE IF coltemp.CNAME = "ONS" THEN DO:
         ASSIGN
         step = 2
         colname = "ons".
      END.
      ELSE IF coltemp.CNAME = "TIS" THEN DO:
         ASSIGN
         step = 1
         colname = "tis".
      END.
      ELSE IF coltemp.CNAME = "MON" THEN DO:
         ASSIGN
         step = 0
         colname = "mån".
      END.
      DO WHILE step > 0:
         FIND PREV coltemp USE-INDEX NUM NO-LOCK NO-ERROR.
         step = step - 1.
      END.
      IF AVAILABLE coltemp THEN DO:
         IF coltemp.CNAME = "MON" THEN DO:
            regvnr = INTEGER(SUBSTRING(coltemp.CH:LABEL,1,3)).
            regdagnamn = colname.
            RUN VECODAT.P.
            IF AVAILABLE tidsplantemp THEN DO:
               IF regdatum < tidsplantemp.STARTDAT THEN DO:
                  FIND FIRST coltemp WHERE coltemp.CNAME = "STARTDAT" NO-LOCK NO-ERROR.
                  IF AVAILABLE coltemp THEN DO:
                     ASSIGN
                     tidsplantemp.STARTDAT = regdatum
                     tidsplantemp.ANDRAD = TRUE.
                     fieldh = coltemp.CH:BUFFER-FIELD("STARTDAT").
                     fieldh:BUFFER-VALUE = tidsplantemp.STARTDAT.
                  END.
               END.                              
               ELSE IF regdatum > tidsplantemp.SLUTDAT THEN DO:
                  FIND FIRST coltemp WHERE coltemp.CNAME = "SLUTDAT" NO-LOCK NO-ERROR.
                  IF AVAILABLE coltemp THEN DO:
                     ASSIGN
                     tidsplantemp.SLUTDAT = regdatum
                     tidsplantemp.ANDRAD = TRUE.
                     fieldh = coltemp.CH:BUFFER-FIELD("SLUTDAT").
                     fieldh:BUFFER-VALUE = tidsplantemp.SLUTDAT.                     
                  END.
               END.
               ELSE IF regdatum = tidsplantemp.STARTDAT THEN DO:
                  FIND FIRST coltemp WHERE coltemp.CNAME = "STARTDAT" NO-LOCK NO-ERROR.
                  IF AVAILABLE coltemp THEN DO:
                     ASSIGN
                     tidsplantemp.STARTDAT = regdatum + 1
                     tidsplantemp.ANDRAD = TRUE.
                     fieldh = coltemp.CH:BUFFER-FIELD("STARTDAT").
                     fieldh:BUFFER-VALUE = tidsplantemp.STARTDAT.
                  END.
               END.
               ELSE IF regdatum = tidsplantemp.SLUTDAT  THEN DO:
                  FIND FIRST coltemp WHERE coltemp.CNAME = "SLUTDAT" NO-LOCK NO-ERROR.
                  IF AVAILABLE coltemp THEN DO:
                     ASSIGN
                     tidsplantemp.SLUTDAT = regdatum - 1
                     tidsplantemp.ANDRAD = TRUE.
                     fieldh = coltemp.CH:BUFFER-FIELD("SLUTDAT").
                     fieldh:BUFFER-VALUE = tidsplantemp.SLUTDAT. 
                  END.
                  
               END.
               dynqueh:QUERY-OPEN().
            END.
         END.         
      END.          
   END.   
   
END PROCEDURE.

PROCEDURE columnstotemp_UI.
   tempcolh = dynbrwh:GET-BROWSE-COLUMN(tempnum).   
   DO WHILE VALID-HANDLE(tempcolh): 
      CREATE coltemp.
      ASSIGN
      coltemp.DTYP = tempcolh:DATA-TYP
      coltemp.CH = tempcolh
      coltemp.CNAME = tempcolh:NAME
      coltemp.NUM = tempnum.
      tempcolh = tempcolh:NEXT-COLUMN.
      tempnum = tempnum + 1.
   END.
   
END PROCEDURE.

PROCEDURE rowdispextra_UI :    
   dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR. 
   ASSIGN
   stdatum = ?
   sldatum = ?.
   RUN checkcolumns_UI.  
   dynbrwh:DESELECT-ROWS() NO-ERROR. 
END PROCEDURE.


PROCEDURE leavedat_UI : 
   IF fieldh = dynbuffh:BUFFER-FIELD("STARTDAT") THEN DO:
      fieldh:BUFFER-VALUE = SELF:SCREEN-VALUE.
   END.
   ELSE IF fieldh = dynbuffh:BUFFER-FIELD("SLUTDAT")  THEN DO:
      fieldh:BUFFER-VALUE = SELF:SCREEN-VALUE.
   END.                                           
   APPLY "ROW-DISPLAY" TO dynbrwh.
END PROCEDURE.

PROCEDURE entrydat_UI : 
   dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.   
END PROCEDURE.

PROCEDURE checkcolumns_UI :
   FIND FIRST coltemp USE-INDEX NUM NO-LOCK NO-ERROR.
   IF AVAILABLE coltemp THEN DO:
      DO WHILE coltemp.CNAME NE "STARTDAT":
         FIND NEXT coltemp USE-INDEX NUM NO-LOCK NO-ERROR.
         IF NOT AVAILABLE coltemp THEN LEAVE.
      END.
      IF AVAILABLE coltemp THEN DO:
         IF coltemp.CNAME = "STARTDAT" THEN DO:
            fieldh = dynbuffh:BUFFER-FIELD(coltemp.CNAME).
            stdatum = fieldh:BUFFER-VALUE.      
         END.
         FIND NEXT coltemp USE-INDEX NUM NO-LOCK NO-ERROR.
         IF AVAILABLE coltemp THEN DO:
            IF coltemp.CNAME = "SLUTDAT" THEN DO:
               fieldh = dynbuffh:BUFFER-FIELD(coltemp.CNAME).
               sldatum = fieldh:BUFFER-VALUE.
            END.
         END.
         IF stdatum NE ? AND sldatum NE ? THEN RUN setcolor_UI. 
      END.      
   END.
      
END PROCEDURE.

PROCEDURE setcolor_UI :
   /*Stega fram till startpos*/
   ASSIGN 
   regdatum = stdatum.
   RUN REGVEC.P.
   regdagnamn = "mån".
   RUN VECODAT.P.
   ASSIGN
   startpos = stdatum - regdatum
   loopraknare = 1.
   FIND FIRST coltemp WHERE coltemp.CNAME = "MON" USE-INDEX NUM NO-LOCK NO-ERROR.
   DO WHILE loopraknare LE startpos:
      FIND NEXT coltemp NO-LOCK NO-ERROR.
      IF AVAILABLE coltemp THEN DO:
         loopraknare = loopraknare + 1.
      END.
      ELSE loopraknare = startpos + 1.
   END.
   /*Stega och färga hela intervallet*/
   ASSIGN
   antalcol = sldatum - stdatum + 1.
   loopraknare = 1.
   DO WHILE loopraknare LE antalcol:
      IF AVAILABLE coltemp THEN DO: 
         IF coltemp.CNAME = "MON" OR coltemp.CNAME = "TIS" OR coltemp.CNAME = "ONS" OR coltemp.CNAME = "TOR" OR
         coltemp.CNAME = "FRE" OR coltemp.CNAME = "LOR" OR coltemp.CNAME = "SON" THEN DO:
            coltemp.CH:BGCOLOR = colvalue.            
         END.
/*          ELSE IF coltemp.CNAME = "LOR" OR coltemp.CNAME = "SON" THEN DO: */
/*             coltemp.CH:BGCOLOR = ?.                                      */
/*          END.                                                            */
         loopraknare = loopraknare + 1.
      END.
      ELSE loopraknare = antalcol + 1.
      FIND NEXT coltemp NO-LOCK NO-ERROR.
   END.
   ASSIGN
   stdatum = ?
   sldatum = ?.

END PROCEDURE.

PROCEDURE vecka_UI :
   DEFINE INPUT PARAMETER antal AS INTEGER NO-UNDO.
   /*Lägg antal veckor beroende på intervall*/
   DEBUGGER:SET-BREAK().
   antalveckor = antal.
   ASSIGN
   colraknare = 0
   labelvar = "".
   DO WHILE colraknare < antalveckor:
      fieldh = dynbuffh:BUFFER-FIELD("MON").
      tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
      ON 'ENTRY':U OF tmpcolh PERSISTENT RUN setcol_UI IN THIS-PROCEDURE.
      /* Om första måndag*/
      IF colraknare = 0 THEN DO:
         /*Om skapa ny browser och första veckan*/
         IF tabort = TRUE THEN DO:
            tmpcolh:LABEL = STRING(startvnr) + "!m". 
            regvnr = startvnr.
            tempvnr = regvnr.
         END.
         ELSE DO:
            IF tempvnr = 0 THEN DO:
               tempvnr = regvnr.
            END.
            ELSE DO:
               tempvnr = tempvnr + 1.
            END.
            tmpcolh:LABEL = STRING(tempvnr) + "!m".                                   
         END.
      END.
      ELSE DO:
         tempvnr = tempvnr + 1.
         tmpcolh:LABEL = STRING(tempvnr) + "!m".
      END.
      ASSIGN
      tmpcolh:WIDTH-CHARS = 3
      tmpcolh:READ-ONLY = FALSE
      tmpcolh:VISIBLE = TRUE.         
      
      fieldh = dynbuffh:BUFFER-FIELD("TIS").
      tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
      ON 'ENTRY':U OF tmpcolh PERSISTENT RUN setcol_UI IN THIS-PROCEDURE.
      ASSIGN
      tmpcolh:WIDTH-CHARS = 2
      tmpcolh:LABEL = "ti" 
      tmpcolh:READ-ONLY = FALSE
      tmpcolh:VISIBLE = TRUE.         
      
      fieldh = dynbuffh:BUFFER-FIELD("ONS").
      tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
      ON 'ENTRY':U OF tmpcolh PERSISTENT RUN setcol_UI IN THIS-PROCEDURE.
      ASSIGN
      tmpcolh:WIDTH-CHARS = 2
      tmpcolh:LABEL = "o" 
      tmpcolh:READ-ONLY = FALSE
      tmpcolh:VISIBLE = TRUE.
      
      fieldh = dynbuffh:BUFFER-FIELD("TOR").
      tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
      ON 'ENTRY':U OF tmpcolh PERSISTENT RUN setcol_UI IN THIS-PROCEDURE.
      ASSIGN
      tmpcolh:WIDTH-CHARS = 2
      tmpcolh:LABEL = "to" 
      tmpcolh:READ-ONLY = FALSE
      tmpcolh:VISIBLE = TRUE.         
      
      fieldh = dynbuffh:BUFFER-FIELD("FRE").
      tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
      ON 'ENTRY':U OF tmpcolh PERSISTENT RUN setcol_UI IN THIS-PROCEDURE.
      ASSIGN
      tmpcolh:WIDTH-CHARS = 2
      tmpcolh:LABEL = "f" 
      tmpcolh:READ-ONLY = FALSE
      tmpcolh:VISIBLE = TRUE.         
      
      fieldh = dynbuffh:BUFFER-FIELD("LOR").
      tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
      ON 'ENTRY':U OF tmpcolh PERSISTENT RUN setcol_UI IN THIS-PROCEDURE.
      ASSIGN
      tmpcolh:WIDTH-CHARS = 2
      tmpcolh:LABEL = "l" 
      tmpcolh:READ-ONLY = FALSE
      tmpcolh:VISIBLE = TRUE.         
      
      fieldh = dynbuffh:BUFFER-FIELD("SON").
      tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
      ON 'ENTRY':U OF tmpcolh PERSISTENT RUN setcol_UI IN THIS-PROCEDURE.
      ASSIGN
      tmpcolh:WIDTH-CHARS = 2
      tmpcolh:LABEL = "s" 
      tmpcolh:READ-ONLY = FALSE
      tmpcolh:VISIBLE = TRUE.         
      
      colraknare = colraknare + 1.      
   END.    
   IF tabort = TRUE THEN DO:
      EMPTY TEMP-TABLE coltemp NO-ERROR.  
      tabort = FALSE.
   END.
   FIND LAST coltemp USE-INDEX NUM NO-LOCK NO-ERROR.
   IF AVAILABLE coltemp THEN tempnum = coltemp.NUM + 1.
   ELSE tempnum = 1.  
   RUN columnstotemp_UI.
   dynbrwh:DESELECT-ROWS() NO-ERROR.   
   
END PROCEDURE.

PROCEDURE bortvecka_UI :
   RUN skapanybrw_UI.
      
END PROCEDURE.

PROCEDURE skapanybrw_UI :
   FIND LAST coltemp WHERE coltemp.CNAME = "MON" USE-INDEX NUM  NO-LOCK NO-ERROR.
   IF AVAILABLE coltemp THEN DO:
      regvnr = INTEGER(SUBSTRING(coltemp.CH:LABEL,1,3)).
      IF startvnr NE 0 AND regvnr NE 0 THEN DO:
         antalveckor = regvnr - startvnr.         
      END.
      ELSE antalveckor = 1.
   
      /*Ge egenskaper till browser*/
      IF VALID-HANDLE(dynbrwh) THEN DELETE OBJECT dynbrwh.
      IF VALID-HANDLE(dynqueh) THEN DELETE OBJECT dynqueh.
      IF VALID-HANDLE(dynbuffh) THEN DELETE OBJECT dynbuffh NO-ERROR.
      IF VALID-HANDLE(tmpcolh) THEN DELETE OBJECT tmpcolh.
      IF VALID-HANDLE(fieldh) THEN DELETE OBJECT fieldh.
      
      CREATE BROWSE dynbrwh.
      RUN brwegenskap_UI.
      
      /*Lägg till veckor*/
      tabort = TRUE.
      RUN vecka_UI (INPUT antalveckor).
      tabort = FALSE.           
      /*Öppna queryn*/
      dynqueh:QUERY-OPEN().   
      ENABLE ALL.
      APPLY "ENTRY" TO dynbrwh.
   END.
   
END PROCEDURE.

PROCEDURE releaseh_UI :
   dynqueh:QUERY-CLOSE NO-ERROR.
   DELETE OBJECT dynqueh NO-ERROR.  
   DELETE OBJECT dynbrwh NO-ERROR.  

END PROCEDURE.

PROCEDURE createfields_UI :
   /*Skapa och lägg till de fält som ska finnas med*/
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
   tmpcolh:WIDTH-CHARS = 4
   tmpcolh:LABEL = "Delnr"
   tmpcolh:READ-ONLY = TRUE
   tmpcolh:VISIBLE = TRUE.

   fieldh = dynbuffh:BUFFER-FIELD("ARBART").
   tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
   ASSIGN
   tmpcolh:WIDTH-CHARS = 10
   tmpcolh:LABEL = "Arbetsart" 
   tmpcolh:READ-ONLY = FALSE
   tmpcolh:VISIBLE = TRUE.  

   fieldh = dynbuffh:BUFFER-FIELD("AKTIVITET").
   tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
   ASSIGN
   tmpcolh:WIDTH-CHARS = 20
   tmpcolh:LABEL = "Aktivitet" 
   tmpcolh:READ-ONLY = FALSE
   tmpcolh:VISIBLE = TRUE.      

   fieldh = dynbuffh:BUFFER-FIELD("TIDLAGE").
   tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
   ASSIGN
   tmpcolh:WIDTH-CHARS = 10
   tmpcolh:LABEL = "Tidläge" 
   tmpcolh:READ-ONLY = TRUE
   tmpcolh:VISIBLE = TRUE.         

   fieldh = dynbuffh:BUFFER-FIELD("TIDSPERIOD").
   tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
   ASSIGN
   tmpcolh:WIDTH-CHARS = 8
   tmpcolh:LABEL = "Tidsperiod" 
   tmpcolh:READ-ONLY = FALSE
   tmpcolh:VISIBLE = TRUE.         

   fieldh = dynbuffh:BUFFER-FIELD("STARTDAT").
   tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
   ON 'LEAVE':U OF tmpcolh PERSISTENT RUN leavedat_UI IN THIS-PROCEDURE.
   ON 'ENTRY':U OF tmpcolh PERSISTENT RUN entrydat_UI IN THIS-PROCEDURE.
   ASSIGN
   tmpcolh:WIDTH-CHARS = 6
   tmpcolh:LABEL = "Start" 
   tmpcolh:READ-ONLY = FALSE
   tmpcolh:VISIBLE = TRUE.         

   fieldh = dynbuffh:BUFFER-FIELD("SLUTDAT").
   tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
   ON 'LEAVE':U OF tmpcolh PERSISTENT RUN leavedat_UI IN THIS-PROCEDURE.
   ON 'ENTRY':U OF tmpcolh PERSISTENT RUN entrydat_UI IN THIS-PROCEDURE.
   ASSIGN
   tmpcolh:WIDTH-CHARS = 6
   tmpcolh:LABEL = "Slut" 
   tmpcolh:READ-ONLY = FALSE
   tmpcolh:VISIBLE = TRUE.         

   fieldh = dynbuffh:BUFFER-FIELD("PERSONAL").
   tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
   ASSIGN
   tmpcolh:WIDTH-CHARS = 14
   tmpcolh:LABEL = "Personal" 
   tmpcolh:READ-ONLY = FALSE
   tmpcolh:VISIBLE = TRUE. 
END PROCEDURE.

