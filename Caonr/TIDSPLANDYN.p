/*TIDSPLANDYN.P*/
{GLOBVAR2DEL1.I}
{EXECLIN.I}
{REGVAR.I}
{TIDPLANPWID.I}
&Scoped-define SHARED SHARED 
{AVTAONRTEMP.I}
{TIDSPLAN.I}
{ARBART.I}   
DEFINE SHARED VARIABLE skrivskydd AS LOGICAL NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE brwproch AS HANDLE NO-UNDO.
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
DEFINE VARIABLE bg AS INTEGER NO-UNDO.
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
DEFINE VARIABLE andrastart AS LOGICAL NO-UNDO.
DEFINE VARIABLE temph AS HANDLE NO-UNDO. 
DEFINE VARIABLE tempstart AS DATE NO-UNDO.
DEFINE VARIABLE tempslut AS DATE NO-UNDO.
DEFINE VARIABLE goringet AS LOGICAL NO-UNDO.
DEFINE VARIABLE invar AS CHARACTER NO-UNDO.
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
   FIELD FARG AS INTEGER
   FIELD CROWID AS ROWID
   INDEX NUM AS PRIMARY NUM CNAME.
DEFINE TEMP-TABLE menutemp
   FIELD MENUH AS HANDLE
   FIELD MENUITEMH AS HANDLE
   FIELD BRWH AS HANDLE.
DEFINE VARIABLE antalbrw AS INTEGER NO-UNDO.
DEFINE VARIABLE vilkenbrw AS INTEGER NO-UNDO.
ASSIGN
dynfrmh = frm
fillsth = fillinsth
fillslh = fillinslh
tabort = FALSE.
vilkenbrw = 1.
ON 'LEAVE':U OF fillsth PERSISTENT RUN leavefillst_UI IN THIS-PROCEDURE.
ON 'LEAVE':U OF fillslh PERSISTENT RUN leavefillsl_UI IN THIS-PROCEDURE.
ON 'VALUE-CHANGED':U OF RADVAL PERSISTENT RUN vcradval_UI IN THIS-PROCEDURE.
ON 'CHOOSE':U OF BTN_EXCEL PERSISTENT RUN btnexcel_UI IN THIS-PROCEDURE.
ON 'CHOOSE':U OF BTN_SKRIV PERSISTENT RUN btnskriv_UI IN THIS-PROCEDURE.
ON 'MOUSE-MENU-CLICK':U OF BTN_SKRIV PERSISTENT RUN btnskrivm_UI IN THIS-PROCEDURE.
ON 'VALUE-CHANGED':U OF TOG_KALKYL PERSISTENT RUN vctogkalk_UI IN THIS-PROCEDURE.
ON 'VALUE-CHANGED':U OF TOG_OVRIGA PERSISTENT RUN vctogovriga_UI IN THIS-PROCEDURE.

RUN extratrigg_UI.
RETURN.

PROCEDURE vctogkalk_UI :   
   IF skrivskydd = TRUE THEN.
   ELSE DO:
      MESSAGE "OBS! Vill du spara dina ändringar?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL TITLE "Spara ändringar?" UPDATE svar AS LOGICAL.
      CASE svar:
         WHEN TRUE THEN DO:
            RUN sparplan_UI IN huvprogh (INPUT FALSE).
         END.
         WHEN FALSE THEN DO:
               
         END.  
         OTHERWISE DO: 
            IF TOG_KALKYL:CHECKED = TRUE THEN TOG_KALKYL:CHECKED = FALSE.
            ELSE TOG_KALKYL:CHECKED = TRUE. 
            RETURN NO-APPLY. 
         END.       
      END CASE.     
   END.   
   IF TOG_OVRIGA:CHECKED = TRUE THEN TOG_OVRIGA:CHECKED = FALSE.
   RUN kalkylstart_UI IN huvprogh.
   RUN kalkyldel2_UI IN huvprogh.
   RUN vcradval_UI.
END PROCEDURE.
PROCEDURE vctogovriga_UI :
   IF skrivskydd = TRUE THEN.
   ELSE DO:
      MESSAGE "OBS! Vill du spara dina ändringar?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL TITLE "Spara ändringar?" UPDATE svar AS LOGICAL.         
      CASE svar:
         WHEN TRUE THEN DO:
            RUN sparplan_UI IN huvprogh (INPUT FALSE).
         END.
         WHEN FALSE THEN DO:
               
         END.  
         OTHERWISE DO: 
            IF TOG_OVRIGA:CHECKED = TRUE THEN TOG_OVRIGA:CHECKED = FALSE.
            ELSE TOG_OVRIGA:CHECKED = TRUE.      
            RETURN NO-APPLY. 
         END.       
      END CASE.
   END.   
   IF TOG_KALKYL:CHECKED = TRUE THEN TOG_KALKYL:CHECKED = FALSE.      
   RUN ovrigastart_UI IN huvprogh.
   RUN kalkyldel2_UI IN huvprogh.
   RUN vcradval_UI.
END PROCEDURE.

PROCEDURE btnskrivm_UI :
   RUN SIDLANGD.W.
END PROCEDURE.
PROCEDURE btnskriv_UI :
   RUN SKRIVVAL.W (INPUT TRUE).       
   IF musz = TRUE THEN DO:
      musz = FALSE. 
      RETURN NO-APPLY.
   END.
   RUN excellkoll_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN.
   END.
   RUN visaexel_UI (FALSE).
   RUN excel_UI (INPUT TRUE).
   RUN musa IN huvprogh.             
   
END PROCEDURE.


PROCEDURE excellkoll_UI :
   DEFINE VARIABLE antalcolums AS INTEGER NO-UNDO.
   musz = FALSE.
   antalcolums = dynbrwh:NUM-COLUMNS.
   IF vilkenbrw = 1 THEN DO:
      IF antalcolums + 9 > 100 THEN DO:                   
         MESSAGE "Du kan max visa 91 dagar i Excel" VIEW-AS ALERT-BOX.
         musz = TRUE.
      END.  
   END.
   ELSE IF vilkenbrw = 2 THEN DO:
      IF antalcolums + 9 > 100 THEN DO:                   
         MESSAGE "Du kan max visa 91 veckor i Excel" VIEW-AS ALERT-BOX.
         musz = TRUE.
      END.  
   END.
   ELSE IF vilkenbrw = 3 THEN DO:
      IF antalcolums + 9 > 100 THEN DO:                   
         MESSAGE "Du kan max visa 91 månader i Excel" VIEW-AS ALERT-BOX.
         musz = TRUE.
      END.
   END.

END PROCEDURE.
PROCEDURE fixastorlek_UI :
   
   FRAME-WEEK:ROW = TOG_OVRIGA:ROW + TOG_OVRIGA:HEIGHT + 0.2.
   FRAME-MONTH:ROW = FRAME-WEEK:ROW.
   FRAME-DAY:ROW  = FRAME-WEEK:ROW.
END PROCEDURE.
PROCEDURE vcradval_UI : 
   
   DEFINE VARIABLE movetotopp AS LOGICAL NO-UNDO.
   vilkenbrw = INTEGER(RADVAL:SCREEN-VALUE).
   RUN musw IN huvprogh.
   IF vilkenbrw = 1 THEN DO:
      RUN newbrw_UI.
      ASSIGN
      FRAME-WEEK:HIDDEN = TRUE
      FRAME-MONTH:HIDDEN = TRUE
      FRAME-DAY:HIDDEN = FALSE.      
   END.
   ELSE IF vilkenbrw = 2 THEN DO:     
      RUN newbrw_UI.               
      ASSIGN
      FRAME-WEEK:HIDDEN = FALSE
      FRAME-MONTH:HIDDEN = TRUE
      FRAME-DAY:HIDDEN = TRUE.      
   END.
   ELSE IF vilkenbrw = 3 THEN DO:
      RUN newbrw_UI.               
      ASSIGN
      FRAME-MONTH:HIDDEN = FALSE
      FRAME-WEEK:HIDDEN = TRUE
      FRAME-DAY:HIDDEN = TRUE.     
   END.
   RUN musa IN huvprogh.
   
END PROCEDURE.

PROCEDURE sistabredd_UI :
   RUN musw IN huvprogh.
   FIND LAST celltemp USE-INDEX NUM NO-LOCK NO-ERROR.
   tempcolh = dynbrwh:GET-BROWSE-COLUMN(celltemp.NUM).
   tmpcolh:WIDTH-CHARS = 1.5.
END PROCEDURE.

PROCEDURE btnexcel_UI :
   RUN musw IN huvprogh.
   RUN excellkoll_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN.
   END.
   RUN excel_UI (INPUT FALSE).
   RUN musa IN huvprogh.          
END PROCEDURE.

PROCEDURE excel_UI :
   DEFINE INPUT PARAMETER saveas AS LOGICAL NO-UNDO.
   DEFINE VARIABLE tempcounter AS INTEGER NO-UNDO.
   RUN allac_UI.
   tempcolh = dynbrwh:GET-BROWSE-COLUMN(1).  
   labelvar = "".
   tempcounter = 0.
   DO WHILE VALID-HANDLE(tempcolh):
      tempcounter = tempcounter + 1. 
      IF tempcounter > maxantalcolexcel THEN LEAVE.
      IF tempcolh:DATA-TYPE = "DATE" THEN bredd[tempcounter] = 11.
      ELSE IF tempcolh:NAME = "mon" THEN bredd[tempcounter] = tempcolh:WIDTH + 1. 
      ELSE IF tempcolh:NAME = "tis" THEN bredd[tempcounter] = 1.
      ELSE IF tempcolh:NAME = "ons" THEN bredd[tempcounter] = 1.
      ELSE IF tempcolh:NAME = "tor" THEN bredd[tempcounter] = 1.
      ELSE IF tempcolh:NAME = "fre" THEN bredd[tempcounter] = 1.
      ELSE IF tempcolh:NAME = "lor" THEN bredd[tempcounter] = 1.
      ELSE IF tempcolh:NAME = "son" THEN bredd[tempcounter] = 1.      
      ELSE IF tempcolh:NAME = "Jan" THEN bredd[tempcounter] = tempcolh:WIDTH + 2. 
      ELSE IF tempcolh:NAME = "Feb" THEN bredd[tempcounter] = 1.
      ELSE IF tempcolh:NAME = "Mar" THEN bredd[tempcounter] = 1.
      ELSE IF tempcolh:NAME = "Apr" THEN bredd[tempcounter] = 1.
      ELSE IF tempcolh:NAME = "Maj" THEN bredd[tempcounter] = 1.
      ELSE IF tempcolh:NAME = "Jun" THEN bredd[tempcounter] = 1.
      ELSE IF tempcolh:NAME = "Jul" THEN bredd[tempcounter] = 1.      
      ELSE IF tempcolh:NAME = "Aug" THEN bredd[tempcounter] = 1.   
      ELSE IF tempcolh:NAME = "Sep" THEN bredd[tempcounter] = 1.   
      ELSE IF tempcolh:NAME = "Okt" THEN bredd[tempcounter] = 1.   
      ELSE IF tempcolh:NAME = "Nov" THEN bredd[tempcounter] = 1.   
      ELSE IF tempcolh:NAME = "Decm" THEN bredd[tempcounter] = 1.   
      ELSE bredd[tempcounter] = tempcolh:WIDTH.      
      tempcolh = tempcolh:NEXT-COLUMN.         
   END.   
   slutbredd = 3.
   breddantal = tempcounter.
   RUN colbredd_UI.  
   IF saveas = TRUE THEN RUN startexcelval_UI (INPUT FALSE).
   ELSE RUN startexcelval_UI (INPUT TRUE).
   RUN gridlines_UI (INPUT TRUE).
   raknare = 1.
   RUN kolumnexcel_UI.
   tempcolh = dynbrwh:FIRST-COLUMN. 
   tempcounter = 0.
   labelvar = "".  
   DO WHILE VALID-HANDLE(tempcolh):    
      tempcounter = tempcounter + 1. 
      IF tempcounter > maxantalcolexcel THEN LEAVE.
      cRange = allac[tempcounter] + "1".
      chWorkSheet:Range(cRange):VALUE = REPLACE(tempcolh:LABEL,"!"," ") NO-ERROR.
      IF tempcolh:COLUMN-FGCOLOR = ? THEN.
      ELSE IF tempcolh:COLUMN-FGCOLOR = 0 THEN.
      ELSE DO:
         iRad = 1.
         RUN profexcelfarg_UI (INPUT tempcolh:COLUMN-FGCOLOR,OUTPUT bg). 
         RUN bgcell3_UI (INPUT allac[tempcounter], INPUT bg).                      
      END.       
      
      tempcolh = tempcolh:NEXT-COLUMN.         
   END.  
   tempcounter = 1.
   dynqueh:GET-FIRST(NO-LOCK).
   
   REPEAT:
      IF dynqueh:QUERY-OFF-END THEN LEAVE. 
      brwrowid = dynbuffh:ROWID.
      tempcounter = tempcounter + 1. 
      irad = tempcounter.
      RUN excelut_UI (INPUT tempcounter).      
      dynqueh:GET-NEXT(NO-LOCK).
   END.  
   RUN sidbrytbredd_UI (INPUT 2).
   RUN namnbladexcel_UI (INPUT dynbrwh:TITLE).
   IF saveas = TRUE THEN RUN slutmedprint_UI (INPUT 1).
   ELSE RUN slutexcel_UI.
 END PROCEDURE.

PROCEDURE excelut_UI :
   DEFINE INPUT PARAMETER radervar AS INTEGER NO-UNDO.
   DEFINE VARIABLE tempbufh AS HANDLE NO-UNDO.
   DEFINE VARIABLE decichar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE tempcounter AS INTEGER NO-UNDO.
   tempcolh = dynbrwh:GET-BROWSE-COLUMN(1).  
   tempcounter = 0.
   DO WHILE VALID-HANDLE(tempcolh):
      tempcounter = tempcounter + 1. 
      IF tempcounter > maxantalcolexcel THEN LEAVE.
      IF tempcolh:NAME = ? THEN.
      ELSE DO:
         tempbufh = dynbuffh:BUFFER-FIELD(tempcolh:NAME).
         IF tempbufh:BUFFER-VALUE = ? THEN.
         ELSE DO:
            cRange = allac[tempcounter] + STRING(radervar).
            IF tempcolh:DATA-TYPE = "DECIMAL" THEN DO:                                    
               decichar = REPLACE(STRING(ROUND(tempbufh:BUFFER-VALUE,2)),".",",").               
               chWorkSheet:Range(cRange):VALUE = decichar NO-ERROR.
            END.
            ELSE IF tempcolh:DATA-TYPE = "DATE" THEN DO:                                    
               decichar = STRING(tempbufh:BUFFER-VALUE,"9999/99/99").               
               chWorkSheet:Range(cRange):VALUE = decichar NO-ERROR.
            END.
            ELSE chWorkSheet:Range(cRange):VALUE = STRING(tempbufh:BUFFER-VALUE) NO-ERROR. 
            IF tempcolh:COLUMN-FGCOLOR = ? THEN.
            ELSE IF tempcolh:COLUMN-FGCOLOR = 0 THEN.
            ELSE DO:
               RUN profexcelfarg_UI (INPUT tempcolh:COLUMN-FGCOLOR,OUTPUT bg). 
               RUN bgcell3_UI (INPUT allac[tempcounter], INPUT bg).               
            END.    
                                                  
            FIND FIRST celltemp WHERE celltemp.NUM = tempcounter AND celltemp.CROWID = brwrowid NO-LOCK NO-ERROR.
            IF AVAILABLE celltemp THEN DO: 
               IF celltemp.CNAME = "MON" OR celltemp.CNAME = "TIS" OR celltemp.CNAME = "ONS" OR celltemp.CNAME = "TOR" OR
               celltemp.CNAME = "FRE" OR celltemp.CNAME = "LOR" OR celltemp.CNAME = "SON" OR
               celltemp.CNAME = "JAN" OR
               celltemp.CNAME = "FEB" OR
               celltemp.CNAME = "MAR" OR
               celltemp.CNAME = "APR" OR
               celltemp.CNAME = "MAJ" OR
               celltemp.CNAME = "JUN" OR
               celltemp.CNAME = "JUL" OR
               celltemp.CNAME = "AUG" OR
               celltemp.CNAME = "SEP" OR
               celltemp.CNAME = "OKT" OR
               celltemp.CNAME = "NOV" OR
               celltemp.CNAME = "DECM"                  
               THEN DO:
                  IF celltemp.FARG NE ? THEN DO:
                     IF celltemp.FARG = 0 OR celltemp.FARG = 8 OR celltemp.FARG = 15 THEN.
                     ELSE DO:
                        RUN profexcelfarg_UI (INPUT celltemp.FARG,OUTPUT bg). 
                        RUN bgcellc_UI (INPUT allac[tempcounter],INPUT allac[tempcounter], INPUT bg).               
                     END.
                  END.              
               END.
            END.                                                 
         END.
      END.
      tempcolh = tempcolh:NEXT-COLUMN.         
   END.
END PROCEDURE.
PROCEDURE vbrw_UI :
   dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
   dynbrwh:DESELECT-ROWS() NO-ERROR.
   brwrowid = dynbuffh:ROWID.      
END PROCEDURE.
PROCEDURE stalman_UI :   
   DEFINE VARIABLE hjdate AS DATE NO-UNDO.
   DEFINE VARIABLE fieldh2 AS HANDLE NO-UNDO.
   FIND FIRST celltemp WHERE celltemp.CNAME = "SLUTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
   IF AVAILABLE celltemp THEN DO:
      fieldh2 = dynbuffh:BUFFER-FIELD(celltemp.CNAME).
      hjdate = fieldh2:BUFFER-VALUE.
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
            MESSAGE "Startdatum kan inte vara större än slutdatum!" Skip
            "Vill du sätta slutdatum lika med startdatum?"
            VIEW-AS ALERT-BOX
            QUESTION BUTTONS YES-NO-CANCEL TITLE "Datum ändringar?" UPDATE svar AS LOGICAL.         
            IF svar THEN DO:
               fieldh2:BUFFER-VALUE =  regdatum.    
            END.
            ELSE IF NOT svar THEN DO:
               RETURN NO-APPLY.          
            END.
         END.
      END.      
      IF regdatum = fieldh:BUFFER-VALUE THEN RETURN.
      fieldh:BUFFER-VALUE = regdatum.      
      IF fieldh:BUFFER-VALUE NE ? THEN DO:
         celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.
      END.
      RUN checkcolumnsm_UI (INPUT 1).      
   END.   
   RUN musa IN huvprogh.
END PROCEDURE.
PROCEDURE checkcolumnsm_UI :
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   IF dynbuffh:AVAILABLE THEN DO:
      dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
      brwrowid = dynbuffh:ROWID.
      FIND FIRST celltemp WHERE celltemp.CNAME = "STARTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
      IF AVAILABLE celltemp THEN DO:
         fieldh = dynbuffh:BUFFER-FIELD(celltemp.CNAME).
         IF fieldh:BUFFER-VALUE NE ? THEN DO:
            celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.
         END.
         stdatum = fieldh:BUFFER-VALUE.      
      END.
      FIND FIRST celltemp WHERE celltemp.CNAME = "SLUTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
      IF AVAILABLE celltemp THEN DO:
         fieldh = dynbuffh:BUFFER-FIELD(celltemp.CNAME).
         IF fieldh:BUFFER-VALUE NE ? THEN DO:
            celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.
         END.
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
      RUN vilkencelltemp_UI.
      IF AVAILABLE celltemp THEN DO:
         celltemp.CH:BGCOLOR = ?.
         celltemp.FARG = ?.
      END.
      REPEAT:
         FIND NEXT celltemp WHERE celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
         IF AVAILABLE celltemp THEN DO:
            celltemp.CH:BGCOLOR = ?.
            celltemp.FARG = ?.
         END.
         ELSE LEAVE.
      END.
      RUN setcolor_UI.
   END.
   ELSE IF fieldh = dynbuffh:BUFFER-FIELD("SLUTDAT") THEN DO:
      FIND FIRST celltemp WHERE celltemp.CNAME = "MON" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
      IF AVAILABLE celltemp THEN DO:
         celltemp.CH:BGCOLOR = ?.
         celltemp.FARG = ?.
      END.
      REPEAT:
         FIND NEXT celltemp WHERE celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
         IF AVAILABLE celltemp THEN DO:
            celltemp.CH:BGCOLOR = ?.
            celltemp.FARG = ?.
         END.
         ELSE LEAVE.
      END.      
      RUN setcolor_UI.
   END.
   APPLY "LEAVE" TO dynbrwh.   
END PROCEDURE.

PROCEDURE slalman_UI :
   DEFINE VARIABLE hjdate AS DATE NO-UNDO.
   DEFINE VARIABLE fieldh2 AS HANDLE NO-UNDO.
   FIND FIRST celltemp WHERE celltemp.CNAME = "STARTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
   IF AVAILABLE celltemp THEN DO:
      fieldh2 = dynbuffh:BUFFER-FIELD(celltemp.CNAME).
      hjdate = fieldh:BUFFER-VALUE.
   END.
   dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
   brwrowid = dynbuffh:ROWID.
   FIND FIRST celltemp WHERE celltemp.CNAME = "SLUTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
   IF AVAILABLE celltemp THEN DO:
      fieldh = dynbuffh:BUFFER-FIELD(celltemp.CNAME).
      IF fieldh:BUFFER-VALUE = ? THEN regdatum = TODAY.                  
      ELSE regdatum = fieldh:BUFFER-VALUE.      
      RUN AlmanBtn.w.
      IF fieldh:BUFFER-VALUE = regdatum THEN RETURN.
      IF hjdate NE ? THEN DO:
         IF regdatum < hjdate THEN DO:
            MESSAGE "Slutdatum kan inte vara mindre än startdatum!" Skip
            "Vill du sätta startdatum lika med slutdatum?"
            VIEW-AS ALERT-BOX
            QUESTION BUTTONS YES-NO-CANCEL TITLE "Datum ändringar?" UPDATE svar AS LOGICAL.         
            IF svar THEN DO:
               fieldh2:BUFFER-VALUE =  regdatum.    
            END.
            ELSE IF NOT svar THEN DO:
               RETURN NO-APPLY.          
            END.            
         END.
      END.
      fieldh:BUFFER-VALUE = regdatum.      
      IF fieldh:BUFFER-VALUE NE ? THEN DO:
         celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.
      END.
      RUN checkcolumnsm_UI (INPUT 2).
   END.
   RUN musa IN huvprogh.
END PROCEDURE.

PROCEDURE createbrw_UI :   
   /*första gången*/
   FIND LAST tidsplantemp WHERE tidsplantemp.SLUTDAT NE ? AND tidsplantemp.SLUTDAT < 01/01/2099 USE-INDEX SDATUM NO-LOCK NO-ERROR.
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
   IF slutdatum -  startdatum > 365 THEN slutdatum = startdatum + 365. 
   RUN newbrw_UI.    
   dynbrwh:DESELECT-ROWS() NO-ERROR.   
END PROCEDURE.       

PROCEDURE brwegenskap_UI :
   /*Ge egenskaper till browser*/
   RUN musw IN huvprogh.
   ASSIGN 
   dynbrwh:NAME                     = "BRW_TIDSPLAN"
   dynbrwh:WIDTH                    = 124
   dynbrwh:HEIGHT                   = 24
   dynbrwh:EXPANDABLE               = YES 
   dynbrwh:COLUMN                   = 1.5
   dynbrwh:ROW                      = 2.4
   dynbrwh:FONT                     = 4
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
   IF vilkenbrw = 1 THEN DO:
      ASSIGN
      dynbrwh:FRAME      = FRAME-DAY
      dynbrwh:ROW                      = 1
      dynbrwh:WIDTH                    = dynbrwh:FRAME:WIDTH - 1.75
      dynbrwh:HEIGHT                   = dynbrwh:FRAME:HEIGHT - 0.95.
   END.
   ELSE IF vilkenbrw = 2 THEN DO: 
      ASSIGN
      dynbrwh:FRAME = FRAME-WEEK
      dynbrwh:ROW                      = 1
      dynbrwh:WIDTH                    = dynbrwh:FRAME:WIDTH - 1.75 
      dynbrwh:HEIGHT                   = dynbrwh:FRAME:HEIGHT - 0.95.
      dynbrwh:ROW-MARKERS              = FALSE. 
   END.
   ELSE IF vilkenbrw = 3 THEN DO:
      ASSIGN
      dynbrwh:FRAME = FRAME-MONTH
      dynbrwh:ROW                      = 1
      dynbrwh:WIDTH                    = dynbrwh:FRAME:WIDTH - 1.75 
      dynbrwh:HEIGHT                   = dynbrwh:FRAME:HEIGHT - 0.95.
      dynbrwh:ROW-MARKERS              = FALSE.
   END.
   
   ON 'LEAVE':U OF dynbrwh PERSISTENT RUN leavebrw_UI IN THIS-PROCEDURE.
   ON 'ENTRY':U OF dynbrwh PERSISTENT RUN entrybrw_UI IN THIS-PROCEDURE.   
   /*Skapa buffer för temptabell */
   dynbuffh = TEMP-TABLE tidsplantemp:DEFAULT-BUFFER-HANDLE.
   /*Skapa queryn för browsern och förbered den*/
   CREATE QUERY dynqueh.
   dynqueh:SET-BUFFERS(dynbuffh).
   dynbuffh = dynqueh:GET-BUFFER-HANDLE. 
   openquery = "FOR EACH " + dynbuffh:TABLE + " NO-LOCK BY ARBART BY AONR BY PERSONAL".
   dynqueh:QUERY-PREPARE(openquery).
   /*Sätt queryn*/
   dynbrwh:QUERY = dynqueh.
END PROCEDURE.

PROCEDURE createfields_UI :
   RUN musw IN huvprogh.
   /*Skapa och lägg till de fält som ska finnas med*/
   DEFINE VARIABLE frmh AS HANDLE NO-UNDO.
   frmh = dynbrwh:FRAME.
   frmh:HIDDEN = FALSE.
   dynbrwh:HIDDEN = FALSE.
   fieldh = dynbuffh:BUFFER-FIELD("ARBART").
   fieldh:FORMAT = "X(256)".
   tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
   
   ASSIGN
   tmpcolh:WIDTH-CHARS = 10
   tmpcolh:LABEL = "!" + Guru.Konstanter:gartk 
   tmpcolh:READ-ONLY = TRUE 
   tmpcolh:VISIBLE = TRUE.  
   frmh:HIDDEN = TRUE.   
   IF typ = "PROJEKT" THEN fieldh = dynbuffh:BUFFER-FIELD("AONR").
   ELSE IF typ = "PLAN" THEN fieldh = dynbuffh:BUFFER-FIELD("PLANNR").
   tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
   ASSIGN
   tmpcolh:WIDTH-CHARS = 6
   tmpcolh:LABEL = Guru.Konstanter:gaok 
   tmpcolh:READ-ONLY = TRUE
   tmpcolh:VISIBLE = TRUE.
   
    &Scoped-define FORMATNAMN fieldh   
      {AOFORMAT4.I}
   IF typ = "PLAN" THEN DO:
      ASSIGN 
      tmpcolh:LABEL = Guru.Konstanter:gplk.
   END.
   IF typ = "PROJEKT" THEN fieldh = dynbuffh:BUFFER-FIELD("DELNR").
   ELSE IF typ = "PLAN" THEN DO:
      fieldh = dynbuffh:BUFFER-FIELD("ARTAL").
      fieldh:FORMAT = "9999".     
   END.
   tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
   ASSIGN
   tmpcolh:WIDTH-CHARS = 4
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
   tmpcolh:LABEL = "Aktivitet" 
   tmpcolh:READ-ONLY = FALSE
   tmpcolh:VISIBLE = TRUE
   tmpcolh:RESIZABLE = TRUE.   
   IF vilkenbrw > 1  THEN tmpcolh:READ-ONLY = TRUE.
   IF TOG_KALKYL:CHECKED OR TOG_OVRIGA:CHECKED THEN DO:
      fieldh = dynbuffh:BUFFER-FIELD("TIMMAR").
      tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
      ON 'ENTRY':U OF tmpcolh PERSISTENT RUN entry_UI IN THIS-PROCEDURE.
      ON 'LEAVE':U OF tmpcolh PERSISTENT RUN leave_UI IN THIS-PROCEDURE.
      ASSIGN
      tmpcolh:WIDTH-CHARS = 8.
      tmpcolh:LABEL = "Timmar kalkyl".
      IF TOG_OVRIGA:CHECKED THEN tmpcolh:LABEL = "Timmar".
      ASSIGN
      tmpcolh:READ-ONLY = FALSE
      tmpcolh:VISIBLE = TRUE
      tmpcolh:RESIZABLE = TRUE.   
      IF vilkenbrw > 1  THEN tmpcolh:READ-ONLY = TRUE.
       fieldh = dynbuffh:BUFFER-FIELD("ANTAL").
      tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
      ON 'ENTRY':U OF tmpcolh PERSISTENT RUN entry_UI IN THIS-PROCEDURE.
      ON 'LEAVE':U OF tmpcolh PERSISTENT RUN leave_UI IN THIS-PROCEDURE.
      ASSIGN
      tmpcolh:WIDTH-CHARS = 10
      tmpcolh:LABEL = "Antal personer" 
      tmpcolh:READ-ONLY = FALSE
      tmpcolh:VISIBLE = TRUE
      tmpcolh:RESIZABLE = TRUE.  
      IF vilkenbrw > 1  THEN tmpcolh:READ-ONLY = TRUE. 
   END.   
   ELSE DO:
      fieldh = dynbuffh:BUFFER-FIELD("ENTREPRENOR").
      fieldh:FORMAT = "X(256)".
      tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
      ON 'ENTRY':U OF tmpcolh PERSISTENT RUN entry_UI IN THIS-PROCEDURE.
      ON 'LEAVE':U OF tmpcolh PERSISTENT RUN leave_UI IN THIS-PROCEDURE.
      ASSIGN
      tmpcolh:WIDTH-CHARS = 8.
      tmpcolh:LABEL = "Entreprenör".
      ASSIGN
      tmpcolh:READ-ONLY = FALSE
      tmpcolh:VISIBLE = TRUE
      tmpcolh:RESIZABLE = TRUE.   
      IF vilkenbrw > 1  THEN tmpcolh:READ-ONLY = TRUE. 
   END.   
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
   IF vilkenbrw > 1  THEN tmpcolh:READ-ONLY = TRUE. 
   fieldh = dynbuffh:BUFFER-FIELD("STARTDAT").
   tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
   ON 'LEAVE':U OF tmpcolh PERSISTENT RUN leave_UI IN THIS-PROCEDURE.
   ON 'ENTRY':U OF tmpcolh PERSISTENT RUN entry_UI IN THIS-PROCEDURE.
   IF vilkenbrw = 1  THEN DO:
      ON 'MOUSE-MENU-CLICK':U OF tmpcolh PERSISTENT RUN stalman_UI IN THIS-PROCEDURE.
   END.
   ASSIGN
   tmpcolh:WIDTH-CHARS = 6
   tmpcolh:LABEL = "Start" 
   tmpcolh:READ-ONLY = FALSE
   tmpcolh:VISIBLE = TRUE.         
   IF vilkenbrw > 1  THEN tmpcolh:READ-ONLY = TRUE. 

   fieldh = dynbuffh:BUFFER-FIELD("SLUTDAT").
   tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
   ON 'LEAVE':U OF tmpcolh PERSISTENT RUN leave_UI IN THIS-PROCEDURE.
   ON 'ENTRY':U OF tmpcolh PERSISTENT RUN entry_UI IN THIS-PROCEDURE.
   IF vilkenbrw = 1  THEN DO:
      ON 'MOUSE-MENU-CLICK':U OF tmpcolh PERSISTENT RUN slalman_UI IN THIS-PROCEDURE.
   END.
   ASSIGN
   tmpcolh:WIDTH-CHARS = 6
   tmpcolh:LABEL = "Slut" 
   tmpcolh:READ-ONLY = FALSE
   tmpcolh:VISIBLE = TRUE. 
   tmpcolh = dynbrwh:GET-BROWSE-COLUMN(1).
   IF vilkenbrw > 1  THEN tmpcolh:READ-ONLY = TRUE. 
END PROCEDURE.

PROCEDURE manad_UI :
   RUN musw IN huvprogh.
   
   DEFINE VARIABLE manadlop AS INTEGER NO-UNDO.
   DEFINE VARIABLE manadslut AS INTEGER NO-UNDO.
   DEFINE VARIABLE arlop AS INTEGER NO-UNDO.
   DEFINE VARIABLE manadvar AS CHARACTER NO-UNDO.
   manadlop = MONTH(startdatum).
   arlop = YEAR(startdatum).  
   REPEAT:
      IF manadlop = 1 THEN manadvar =      "Jan".
      ELSE IF manadlop = 2 THEN manadvar = "Feb".
      ELSE IF manadlop = 3 THEN manadvar = "Mar".
      ELSE IF manadlop = 4 THEN manadvar = "Apr".
      ELSE IF manadlop = 5 THEN manadvar = "Maj".
      ELSE IF manadlop = 6 THEN manadvar = "Jun".
      ELSE IF manadlop = 7 THEN manadvar = "Jul".
      ELSE IF manadlop = 8 THEN manadvar = "Aug".
      ELSE IF manadlop = 9 THEN manadvar = "Sep".
      ELSE IF manadlop = 10 THEN manadvar = "Okt".
      ELSE IF manadlop = 11 THEN manadvar = "Nov".
      ELSE IF manadlop = 12 THEN manadvar = "Decm".
      fieldh = dynbuffh:BUFFER-FIELD(manadvar).
      tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).    
      ON 'ENTRY':U OF tmpcolh PERSISTENT    RUN setcolcell_UI IN THIS-PROCEDURE.        
      /* Om första måndag*/
      /*Om skapa ny browser och första veckan*/
      IF manadlop = 1 THEN DO: 
         ASSIGN
         tmpcolh:WIDTH-CHARS = 3        
         tmpcolh:LABEL = STRING(arlop) + "!" + SUBSTRING(manadvar,1,3). 
      END.
      ELSE DO:  
         ASSIGN
         tmpcolh:WIDTH-CHARS = 2.5
         tmpcolh:LABEL = SUBSTRING(manadvar,1,3).       
      END.
      ASSIGN
      tmpcolh:READ-ONLY = TRUE
      tmpcolh:VISIBLE = TRUE.                    
      manadlop = manadlop + 1.
      IF manadlop > 12 THEN DO:
         ASSIGN
         manadlop = 1
         arlop = arlop + 1.
      END.
      IF arlop = YEAR(slutdatum) THEN DO:
         IF manadlop > MONTH(slutdatum) THEN LEAVE.
      END. 
      IF arlop > YEAR(slutdatum) THEN DO:         
         LEAVE.
      END.         
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


PROCEDURE vecka_UI :
   RUN musw IN huvprogh.
   DEFINE VARIABLE temp AS DATE NO-UNDO.

   IF WEEKDAY(startdatum) = 1 THEN regdatum = startdatum - 6.
   ELSE IF WEEKDAY(startdatum) = 2 THEN regdatum = startdatum.
   ELSE IF WEEKDAY(startdatum) = 3 THEN regdatum = startdatum - 1.
   ELSE IF WEEKDAY(startdatum) = 4 THEN regdatum = startdatum - 2.
   ELSE IF WEEKDAY(startdatum) = 5 THEN regdatum = startdatum - 3.
   ELSE IF WEEKDAY(startdatum) = 6 THEN regdatum = startdatum - 4.
   ELSE IF WEEKDAY(startdatum) = 7 THEN regdatum = startdatum - 5.
   /* minus värdet för varje kolumn*/     
   dynbrwh:HIDDEN = FALSE.
   RUN REGVEC.P.
   tempvnr = regvnr.
   
   REPEAT:
      fieldh = dynbuffh:BUFFER-FIELD("MON").
      tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).    
      ON 'ENTRY':U OF tmpcolh PERSISTENT    RUN setcolcell_UI IN THIS-PROCEDURE.        
      /* Om första måndag*/
      /*Om skapa ny browser och första veckan*/
      IF vilkenbrw = 1 THEN tmpcolh:LABEL = STRING(tempvnr,"999") + "!m". 
      ELSE IF vilkenbrw = 2 THEN tmpcolh:LABEL = "v" + STRING(tempvnr,"999"). 
      ASSIGN 
      tmpcolh:WIDTH-CHARS = 2.2
      tmpcolh:READ-ONLY = FALSE
      tmpcolh:VISIBLE = TRUE.         
      IF vilkenbrw = 2 THEN DO:
         ASSIGN
         tmpcolh:WIDTH-CHARS = 3
         tmpcolh:READ-ONLY = TRUE.
      END.
      IF vilkenbrw = 1 THEN DO:
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
         tmpcolh:LABEL-FGCOLOR = 12
         tmpcolh:WIDTH-CHARS = 1.5
         tmpcolh:LABEL = "l" 
         tmpcolh:READ-ONLY = FALSE
         tmpcolh:VISIBLE = TRUE.         
         
         fieldh = dynbuffh:BUFFER-FIELD("SON").
         tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
         ON 'ENTRY':U OF tmpcolh PERSISTENT RUN setcolcell_UI IN THIS-PROCEDURE.
         ASSIGN
         tmpcolh:LABEL-FGCOLOR = 12
         tmpcolh:WIDTH-CHARS = 1.5
         tmpcolh:LABEL = "s" 
         tmpcolh:READ-ONLY = FALSE
         tmpcolh:VISIBLE = TRUE.              
      END.
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
         SELF:NAME = "FRE" OR SELF:NAME = "LOR" OR SELF:NAME = "SON"             
            THEN DO:
            SELF:READ-ONLY = TRUE.
            IF VALID-HANDLE(hLast) THEN APPLY "ENTRY" TO hLast.
            ELSE APPLY "ENTRY" TO fillsth.
            SELF:READ-ONLY = FALSE.
            FIND FIRST celltemp WHERE celltemp.CH = SELF AND celltemp.CROWID = brwrowid NO-LOCK NO-ERROR.
            IF AVAILABLE celltemp THEN DO:
               fieldh = dynbuffh:BUFFER-FIELD("FARGNR").
               celltemp.CH:BGCOLOR = IF SELF:BGCOLOR = ? THEN fieldh:BUFFER-VALUE ELSE ?.
               celltemp.FARG = IF SELF:BGCOLOR = ? THEN fieldh:BUFFER-VALUE ELSE ?.
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
            FIND FIRST celltemp WHERE celltemp.CNAME = "STARTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
               IF AVAILABLE celltemp THEN DO:
                  temph = dynbuffh:BUFFER-FIELD("STARTDAT").
                  tempstart = temph:BUFFER-VALUE. 
                  temph = dynbuffh:BUFFER-FIELD("SLUTDAT").
                  tempslut = temph:BUFFER-VALUE.                   
                  IF tempstart = tempslut AND tempstart = regdatum THEN DO:
                     dynbrwh:DESELECT-ROWS() NO-ERROR.
                     goringet = TRUE.
                     FIND FIRST celltemp WHERE celltemp.CH = SELF AND celltemp.CROWID = brwrowid NO-LOCK NO-ERROR.
                     IF AVAILABLE celltemp THEN DO:                     
                        fieldh = dynbuffh:BUFFER-FIELD("FARGNR").                        
                        celltemp.CH:BGCOLOR = IF SELF:BGCOLOR = ? THEN fieldh:BUFFER-VALUE ELSE ?.
                        celltemp.FARG = IF SELF:BGCOLOR = ? THEN fieldh:BUFFER-VALUE ELSE ?.
                     END.
                  END.

                  ELSE goringet = FALSE.                  
               END.            
            IF goringet = FALSE THEN DO:
               FIND FIRST celltemp WHERE celltemp.CNAME = "STARTDAT" AND celltemp.CROWID = brwrowid NO-LOCK NO-ERROR.
               IF AVAILABLE celltemp THEN DO:
                  fieldh = dynbuffh:BUFFER-FIELD(celltemp.CNAME).                  
                  IF regdatum < fieldh:BUFFER-VALUE THEN DO:                  /*IF regdatum <= fieldh:BUFFER-VALUE THEN DO:                  */
                     fieldh:BUFFER-VALUE = regdatum.                     
                     IF fieldh:BUFFER-VALUE NE ? THEN DO:
                        celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.
                     END.
                  END.
                  ELSE IF regdatum = fieldh:BUFFER-VALUE THEN DO: 
                     fieldh:BUFFER-VALUE = regdatum + 1.
                     IF fieldh:BUFFER-VALUE NE ? THEN DO:
                        celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.
                     END.
                  END.
                  ELSE IF fieldh:BUFFER-VALUE = ? THEN DO:
                     fieldh:BUFFER-VALUE = regdatum.
                     IF fieldh:BUFFER-VALUE NE ? THEN DO:
                        celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.
                     END.
                  END.
               END.   
               FIND FIRST celltemp WHERE celltemp.CNAME = "SLUTDAT" AND celltemp.CROWID = brwrowid NO-LOCK NO-ERROR.
               IF AVAILABLE celltemp THEN DO:
                  fieldh = dynbuffh:BUFFER-FIELD(celltemp.CNAME).                  
                  IF regdatum > fieldh:BUFFER-VALUE THEN DO:
                     fieldh:BUFFER-VALUE = regdatum.           
                     IF fieldh:BUFFER-VALUE NE ? THEN DO:
                        celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.                 
                     END.
                  END.
                  ELSE IF regdatum = fieldh:BUFFER-VALUE  THEN DO:
                     fieldh:BUFFER-VALUE = regdatum - 1.       
                     IF fieldh:BUFFER-VALUE NE ? THEN DO:
                        celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.                 
                     END.
                  END.                  
                  ELSE IF fieldh:BUFFER-VALUE = ? THEN DO:
                     IF fieldh:BUFFER-VALUE NE ? THEN DO:
                        fieldh:BUFFER-VALUE = regdatum.
                     END.
                  END.
               END.
               fieldh = dynbuffh:BUFFER-FIELD("ANDRAD").
               fieldh:BUFFER-VALUE = TRUE. 
               dynbrwh:DESELECT-ROWS() NO-ERROR.               
               RUN checkcolumns_UI.
            END.            
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
         celltemp.CROWID = dynbuffh:ROWID.         
         tempcolh = tempcolh:NEXT-COLUMN.
         tempnum = tempnum + 1.              
      END.
      dynqueh:GET-NEXT(NO-LOCK).
      tempnum = spartempnum.
      IF dynqueh:QUERY-OFF-END THEN LEAVE.
   END.
     
END PROCEDURE.

PROCEDURE rensa_UI :
   IF vilkenbrw > 1 THEN RETURN.
   status-ok = dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
   IF status-ok = FALSE THEN DO:
      MESSAGE "Du måste välja någon rad. Klicka på 'Pucken' längs till vänster." VIEW-AS ALERT-BOX.
      RETURN.
   END.
   brwrowid = dynbuffh:ROWID.   
   FIND FIRST celltemp WHERE celltemp.CNAME = "MON" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
   IF AVAILABLE celltemp THEN DO:
      celltemp.CH:BGCOLOR = ?.
      celltemp.FARG = ?.      
   END.
   REPEAT:
      FIND NEXT celltemp WHERE celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
      IF AVAILABLE celltemp THEN DO:
         celltemp.CH:BGCOLOR = ?.
         celltemp.FARG = ?.
      END.
      ELSE LEAVE.
   END.

   FIND FIRST celltemp WHERE celltemp.CNAME = "STARTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
   IF AVAILABLE celltemp THEN DO:
      fieldh = dynbuffh:BUFFER-FIELD(celltemp.CNAME).
      IF fieldh:BUFFER-VALUE NE ? THEN DO:
         celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE. 
      END.      
   END.
   FIND FIRST celltemp WHERE celltemp.CNAME = "SLUTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
   IF AVAILABLE celltemp THEN DO:
      fieldh = dynbuffh:BUFFER-FIELD(celltemp.CNAME).
      IF fieldh:BUFFER-VALUE NE ? THEN DO:
         celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE. 
      END.      
   END.          
   fieldh = dynbuffh:BUFFER-FIELD("ANDRAD").
   fieldh:BUFFER-VALUE = TRUE.
   RUN checkcolumns_UI.   
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



PROCEDURE vilkencelltemp_UI :
   DEFINE VARIABLE manadvar AS CHARACTER NO-UNDO.
   IF MONTH(DATE(fillsth:SCREEN-VALUE)) = 1 THEN manadvar =      "Jan".
   ELSE IF MONTH(DATE(fillsth:SCREEN-VALUE)) = 2 THEN manadvar = "Feb".
   ELSE IF MONTH(DATE(fillsth:SCREEN-VALUE)) = 3 THEN manadvar = "Mar".
   ELSE IF MONTH(DATE(fillsth:SCREEN-VALUE)) = 4 THEN manadvar = "Apr".
   ELSE IF MONTH(DATE(fillsth:SCREEN-VALUE)) = 5 THEN manadvar = "Maj".
   ELSE IF MONTH(DATE(fillsth:SCREEN-VALUE)) = 6 THEN manadvar = "Jun".
   ELSE IF MONTH(DATE(fillsth:SCREEN-VALUE)) = 7 THEN manadvar = "Jul".
   ELSE IF MONTH(DATE(fillsth:SCREEN-VALUE)) = 8 THEN manadvar = "Aug".
   ELSE IF MONTH(DATE(fillsth:SCREEN-VALUE)) = 9 THEN manadvar = "Sep".
   ELSE IF MONTH(DATE(fillsth:SCREEN-VALUE)) = 10 THEN manadvar = "Okt".
   ELSE IF MONTH(DATE(fillsth:SCREEN-VALUE)) = 11 THEN manadvar = "Nov".
   ELSE IF MONTH(DATE(fillsth:SCREEN-VALUE)) = 12 THEN manadvar = "Decm".
   IF vilkenbrw = 1 OR vilkenbrw = 2 THEN DO:
      FIND FIRST celltemp WHERE celltemp.CNAME = "MON" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
   END.
   ELSE IF vilkenbrw = 3 THEN DO:      
      FIND FIRST celltemp WHERE celltemp.CNAME = manadvar AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.      
   END.
END PROCEDURE.

PROCEDURE leave_UI :
   DEFINE VARIABLE hjdate AS DATE NO-UNDO.
   DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
   invar = SELF:SCREEN-VALUE.
   dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
   fieldh = dynbuffh:BUFFER-FIELD(FOCUS:NAME).
   IF fieldh = ? THEN RETURN.   
   brwrowid = dynbuffh:ROWID.
   musz = FALSE.
   IF fieldh:DATA-TYPE = "DATE" THEN DO:
      IF fieldh:BUFFER-VALUE = DATE(invar) THEN.
      ELSE musz = TRUE.
   END.
   ELSE IF fieldh:BUFFER-VALUE = invar THEN. /*jämför screenvalue med sparat och gör ingenting om det är samma*/
   ELSE musz = TRUE.
   IF musz = FALSE THEN DO:      
   END.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      IF fieldh = dynbuffh:BUFFER-FIELD("STARTDAT") THEN DO:
         RUN leavestart_UI (INPUT hjdate).
      END.
      ELSE IF fieldh = dynbuffh:BUFFER-FIELD("SLUTDAT") THEN DO:
         RUN leaveslut_UI (INPUT hjdate).
      END. 
      ELSE IF fieldh = dynbuffh:BUFFER-FIELD("PERSONAL") THEN DO:
         RUN leavepersonal_UI.
      END. 
      ELSE IF fieldh = dynbuffh:BUFFER-FIELD("ENTREPRENOR") THEN DO:
         RUN leaveentrepenor_UI.
      END. 
      ELSE IF fieldh = dynbuffh:BUFFER-FIELD("PROJEKTLEDARE") THEN DO:
         RUN leaveprojektledare_UI.
      END. 
      ELSE IF fieldh = dynbuffh:BUFFER-FIELD("TIMMAR") THEN DO:
         fieldh:BUFFER-VALUE = invar.
         FIND FIRST celltemp WHERE celltemp.CNAME = "TIMMAR" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
         IF AVAILABLE celltemp THEN DO:
            IF fieldh:BUFFER-VALUE NE ? THEN DO:
               IF fieldh:BUFFER-VALUE > 0 THEN DO:
                  celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.
                  RUN leaveantal_UI.
               END.
            END.
         END.
         
      END.
      ELSE IF fieldh = dynbuffh:BUFFER-FIELD("ANTAL") THEN DO:
         fieldh:BUFFER-VALUE = invar.
         FIND FIRST celltemp WHERE celltemp.CNAME = "ANTAL" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
         IF AVAILABLE celltemp THEN DO:
            IF fieldh:BUFFER-VALUE NE ? THEN DO:
               IF fieldh:BUFFER-VALUE > 0 THEN DO:
                  celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.
                  RUN leaveantal_UI.
               END.
            END.
         END.         
      END.
      fieldh = dynbuffh:BUFFER-FIELD("ANDRAD").
      fieldh:BUFFER-VALUE = TRUE.
      RUN checkcolumns_UI.
   END.
   dynbrwh:DESELECT-ROWS() NO-ERROR.
   musz = FALSE.
END PROCEDURE.
/* START under-procedurer till leave_UI */
PROCEDURE leaveslut_UI :
   DEFINE INPUT PARAMETER hjdate AS DATE NO-UNDO.
   dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
   brwrowid = dynbuffh:ROWID.
   FIND FIRST celltemp WHERE celltemp.CNAME = "STARTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
   IF AVAILABLE celltemp THEN DO:
      fieldh = dynbuffh:BUFFER-FIELD(celltemp.CNAME).
      hjdate = fieldh:BUFFER-VALUE.
      IF hjdate = ? THEN.
      ELSE DO:         
         IF DATE(invar) < hjdate THEN DO:
            MESSAGE "Slutdatum kan inte vara mindre än startdatum!" Skip
            "Vill du sätta startdatum lika med slutdatum?"
            VIEW-AS ALERT-BOX
            QUESTION BUTTONS YES-NO-CANCEL TITLE "Datum ändringar?" UPDATE svar AS LOGICAL.         
            IF svar THEN DO:
               fieldh:BUFFER-VALUE =  DATE(invar).    
            END.
            ELSE IF NOT svar THEN DO:
               RETURN NO-APPLY.          
            END.                             
         END.         
      END.
   END.
   fieldh = dynbuffh:BUFFER-FIELD("SLUTDAT").
   RUN vilkencelltemp_UI.
   IF AVAILABLE celltemp THEN DO:
      celltemp.CH:BGCOLOR = ?.
      celltemp.FARG = ?.
   END.
   REPEAT:
      FIND NEXT celltemp WHERE celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
      IF AVAILABLE celltemp THEN DO:
         celltemp.CH:BGCOLOR = ?.
         celltemp.FARG = ?.
      END.
      ELSE LEAVE.
   END.
   FIND FIRST celltemp WHERE celltemp.CNAME = "SLUTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
   IF AVAILABLE celltemp THEN DO:
      fieldh:BUFFER-VALUE = DATE(invar).
      IF fieldh:BUFFER-VALUE NE ? THEN DO:
         celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.
      END.
      FIND FIRST celltemp WHERE celltemp.CNAME = "STARTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
      IF AVAILABLE celltemp THEN DO:
         fieldh2 = dynbuffh:BUFFER-FIELD("STARTDAT").
         IF fieldh:BUFFER-VALUE < fieldh2:BUFFER-VALUE THEN DO:
            fieldh2:BUFFER-VALUE = fieldh:BUFFER-VALUE.
            IF fieldh2:BUFFER-VALUE NE ? THEN DO:
               celltemp.CH:SCREEN-VALUE = fieldh2:BUFFER-VALUE.
            END.
         END.
      END.
   END.
   APPLY "LEAVE" TO dynbrwh.   
END PROCEDURE.
              
PROCEDURE leavestartspec_UI :
   DEFINE INPUT PARAMETER hjdate AS DATE NO-UNDO.
   dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
   brwrowid = dynbuffh:ROWID.
   hjdate = hjdate + 1.
   fieldh = dynbuffh:BUFFER-FIELD("STARTDAT").
   RUN vilkencelltemp_UI.
   IF AVAILABLE celltemp THEN DO:
      celltemp.CH:BGCOLOR = ?.
      celltemp.FARG = ?.
   END.
   REPEAT:
      FIND NEXT celltemp WHERE celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
      IF AVAILABLE celltemp THEN DO:
         celltemp.CH:BGCOLOR = ?.
         celltemp.FARG = ?.
      END.
      ELSE LEAVE.
   END.
   FIND FIRST celltemp WHERE celltemp.CNAME = "STARTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
   IF AVAILABLE celltemp THEN DO:
      fieldh:BUFFER-VALUE = DATE(SELF:SCREEN-VALUE).
      IF fieldh:BUFFER-VALUE NE ? THEN DO:
         celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.
      END.
      FIND FIRST celltemp WHERE celltemp.CNAME = "SLUTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
      IF AVAILABLE celltemp THEN DO:
         fieldh2 = dynbuffh:BUFFER-FIELD("SLUTDAT").
         IF fieldh2:BUFFER-VALUE NE ? THEN DO:
            celltemp.CH:SCREEN-VALUE = fieldh2:BUFFER-VALUE.
         END.
         IF fieldh:BUFFER-VALUE > fieldh2:BUFFER-VALUE THEN DO:
            fieldh2:BUFFER-VALUE = fieldh:BUFFER-VALUE.
            IF fieldh2:BUFFER-VALUE NE ? THEN DO:
               celltemp.CH:SCREEN-VALUE = fieldh2:BUFFER-VALUE.
            END.
         END.
      END.
   END.   
END PROCEDURE.

PROCEDURE leavestart_UI :
   DEFINE INPUT PARAMETER hjdate AS DATE NO-UNDO.
   dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
   brwrowid = dynbuffh:ROWID.
   FIND FIRST celltemp WHERE celltemp.CNAME = "SLUTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
   
   IF AVAILABLE celltemp THEN DO:
      fieldh = dynbuffh:BUFFER-FIELD(celltemp.CNAME).
      hjdate = fieldh:BUFFER-VALUE.
      IF hjdate = ? THEN.
      ELSE DO:
         IF DATE(invar) > hjdate THEN DO:            
            MESSAGE "Startdatum kan inte vara större än slutdatum!" Skip
            "Vill du sätta slutdatum lika med startdatum?"
            VIEW-AS ALERT-BOX
            QUESTION BUTTONS YES-NO-CANCEL TITLE "Datum ändringar?" UPDATE svar AS LOGICAL.         
            IF svar THEN DO:
               fieldh:BUFFER-VALUE =  DATE(invar).    
            END.
            ELSE IF NOT svar THEN DO:
               RETURN NO-APPLY.          
            END.
         END.
      END.
   END.
   fieldh = dynbuffh:BUFFER-FIELD("STARTDAT").
   RUN vilkencelltemp_UI.
   IF AVAILABLE celltemp THEN DO:
      celltemp.CH:BGCOLOR = ?.
      celltemp.FARG = ?.
   END.
   REPEAT:
      FIND NEXT celltemp WHERE celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
      IF AVAILABLE celltemp THEN DO:
         celltemp.CH:BGCOLOR = ?.
         celltemp.FARG = ?.
      END.
      ELSE LEAVE.
   END.
   FIND FIRST celltemp WHERE celltemp.CNAME = "STARTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
   IF AVAILABLE celltemp THEN DO:
      fieldh:BUFFER-VALUE = DATE(invar).
      IF fieldh:BUFFER-VALUE NE ? THEN DO:
         celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.
      END.
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
END PROCEDURE.

PROCEDURE leavepersonal_UI :
   FIND FIRST celltemp WHERE celltemp.CNAME = "PERSONAL" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
   IF AVAILABLE celltemp THEN DO:
      fieldh:BUFFER-VALUE = invar.
      IF fieldh:BUFFER-VALUE NE ? THEN DO:
         celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.             
      END.
   END.
END PROCEDURE.

PROCEDURE leaveentrepenor_UI :
   FIND FIRST celltemp WHERE celltemp.CNAME = "ENTREPRENOR" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
   IF AVAILABLE celltemp THEN DO:
      fieldh:BUFFER-VALUE = invar.
      IF fieldh:BUFFER-VALUE NE ? THEN DO:
         celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.                         
      END.
   END.
END PROCEDURE.

PROCEDURE leaveprojektledare_UI :
   FIND FIRST celltemp WHERE celltemp.CNAME = "PROJEKTLEDARE" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
   IF AVAILABLE celltemp THEN DO:
      fieldh:BUFFER-VALUE = invar.
      IF fieldh:BUFFER-VALUE NE ? THEN DO:
         celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.                        
      END.
   END.
END PROCEDURE.

PROCEDURE leaveantal_UI :
   DEFINE VARIABLE stdat AS DATE NO-UNDO.
   DEFINE VARIABLE sldat AS DATE NO-UNDO.
   DEFINE VARIABLE antalvar AS DECIMAL NO-UNDO.
   DEFINE VARIABLE timmarvar AS DECIMAL NO-UNDO.
   FIND FIRST celltemp WHERE celltemp.CNAME = "ANTAL" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
   IF AVAILABLE celltemp THEN DO:        
      fieldh = dynbuffh:BUFFER-FIELD(celltemp.CNAME).      
      IF fieldh:BUFFER-VALUE NE ? THEN DO:
         IF fieldh:BUFFER-VALUE > 0 THEN DO:
            ASSIGN            
            antalvar = fieldh:BUFFER-VALUE.
            FIND FIRST celltemp WHERE celltemp.CNAME = "TIMMAR" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
            IF AVAILABLE celltemp THEN DO:
               fieldh = dynbuffh:BUFFER-FIELD(celltemp.CNAME).
               IF fieldh:BUFFER-VALUE NE ? THEN DO:
                  IF DECIMAL(fieldh:BUFFER-VALUE) > 0 THEN DO:
                     timmarvar = DECIMAL(fieldh:BUFFER-VALUE) NO-ERROR.
                     IF timmarvar NE ? THEN DO:
                        FIND FIRST celltemp WHERE celltemp.CNAME = "STARTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
                        IF AVAILABLE celltemp THEN DO:
                           fieldh = dynbuffh:BUFFER-FIELD(celltemp.CNAME).
                           IF fieldh:BUFFER-VALUE NE ? THEN DO:
                              stdat = fieldh:BUFFER-VALUE.
                              sldat = stdat + ROUND(timmarvar / (8 * antalvar),0).
                              IF sldat NE ? THEN DO:
                                 RUN helg_UI (INPUT stdat,INPUT-OUTPUT sldat).
                                 FIND FIRST celltemp WHERE celltemp.CNAME = "SLUTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
                                 IF AVAILABLE celltemp THEN DO:
                                    fieldh = dynbuffh:BUFFER-FIELD(celltemp.CNAME).
                                    fieldh:BUFFER-VALUE = sldat.     
                                    celltemp.CH:SCREEN-VALUE =  fieldh:BUFFER-VALUE.
                                    invar = STRING(sldat).
                                    RUN leaveslut_UI (INPUT sldat).                                                                      
                                 END.
                              END.
                           END.
                        END.
                     END.
                  END.
               END.
            END.
         END.
      END.     
   END.
END PROCEDURE.
PROCEDURE entry_UI : 
   dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
END PROCEDURE.

PROCEDURE helg_UI :
   DEFINE INPUT PARAMETER dt1 AS DATE NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER dt2 AS DATE NO-UNDO.
   DEFINE VARIABLE antalextra AS INTEGER NO-UNDO.
   DEFINE VARIABLE skilnad AS INTEGER NO-UNDO.
   skilnad = dt2 - dt1.
   antalextra = 1.
   dt2 = dt1. 
   REPEAT:
      IF WEEKDAY(dt2) = 1 THEN.
      ELSE IF WEEKDAY(dt2) = 7 THEN.
      ELSE antalextra  = antalextra + 1.
      IF antalextra > skilnad THEN LEAVE.
      dt2 = dt2 + 1.
      
   END.   
END PROCEDURE.
PROCEDURE leavefillst_UI :  
   regdatum = DATE(fillsth:SCREEN-VALUE).
   IF startdatum = regdatum THEN RETURN.
   startdatum = regdatum.
   slutdatum = DATE(fillslh:SCREEN-VALUE).
   
   IF DATE(fillslh:SCREEN-VALUE) <= startdatum THEN DO:
      fillslh:SCREEN-VALUE = STRING(startdatum + 14).
      IF fieldh:BUFFER-VALUE NE ? THEN DO:
         slutdatum = DATE(fillslh:SCREEN-VALUE).
      END.
   END.
   RUN vcradval_UI.
   
   RUN musa IN huvprogh.    
END PROCEDURE.

PROCEDURE leavefillsl_UI :
   regdatum = DATE(fillslh:SCREEN-VALUE).
   IF slutdatum = regdatum THEN RETURN.
   slutdatum = regdatum.
   startdatum = DATE(fillsth:SCREEN-VALUE).   
   IF DATE(fillsth:SCREEN-VALUE) >= slutdatum THEN DO: 
      fillsth:SCREEN-VALUE = string(slutdatum - 14).
      startdatum = DATE(fillsth:SCREEN-VALUE).
   END.
   RUN vcradval_UI.   
   RUN musa IN huvprogh.
END.

PROCEDURE checkcolumns_UI :   
   IF dynbuffh:AVAILABLE THEN DO:
      dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
      brwrowid = dynbuffh:ROWID.
      FIND FIRST celltemp WHERE celltemp.CNAME = "STARTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
      IF AVAILABLE celltemp THEN DO:         
         fieldh = dynbuffh:BUFFER-FIELD(celltemp.CNAME).
         IF fieldh:BUFFER-VALUE NE ? THEN DO:
            celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.
         END.
         stdatum = fieldh:BUFFER-VALUE.      
      END.
      FIND FIRST celltemp WHERE celltemp.CNAME = "SLUTDAT" AND celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
      IF AVAILABLE celltemp THEN DO:
         fieldh = dynbuffh:BUFFER-FIELD(celltemp.CNAME).
         IF fieldh:BUFFER-VALUE NE ? THEN DO:
            celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.
         END.
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
   DEFINE VARIABLE temp AS INTEGER NO-UNDO.   
   DEFINE VARIABLE startvnr AS INTEGER NO-UNDO.
   DEFINE VARIABLE lopar AS INTEGER NO-UNDO.
   DEFINE VARIABLE instart AS DATE NO-UNDO.
   DEFINE VARIABLE aostart AS DATE NO-UNDO.
   brwrowid = dynbuffh:ROWID.
   RUN musw IN huvprogh.
   IF vilkenbrw = 1 THEN DO:
      ASSIGN 
      regdatum = startdatum.
      RUN REGVEC.P.
      regdagnamn = "mån".
      RUN VECODAT.P.
      ASSIGN
      startpos = stdatum - regdatum.
         /*Stega och färga hela intervallet*/
      IF sldatum = stdatum OR stdatum > sldatum THEN antalcol = 1.
      ELSE IF sldatum > stdatum THEN antalcol = (sldatum + 1) - stdatum.
   
      /*bugfix antal färgfyrkanter...*/
      IF stdatum < regdatum THEN DO:
         temp = regdatum - stdatum.
         IF temp < 0 THEN temp = temp * (-1).
         antalcol = antalcol - temp.   
      END.
   END.
   ELSE IF vilkenbrw = 2 THEN DO:
      IF stdatum < startdatum THEN startpos = 0.
      ELSE DO:     
         ASSIGN 
         regdatum = startdatum.
         RUN REGVEC.P.
         regdagnamn = "mån".
         RUN VECODAT.P.
         instart = regdatum.
         ASSIGN 
         regdatum = stdatum.
         RUN REGVEC.P.
         regdagnamn = "mån".
         RUN VECODAT.P.
         aostart = regdatum.
         startpos = (aostart - instart) / 7.                    
      END.
                                   
      IF sldatum <= slutdatum THEN DO:                     
         ASSIGN 
         regdatum = stdatum.
         RUN REGVEC.P.
         regdagnamn = "mån".
         RUN VECODAT.P.
         instart = regdatum.
         ASSIGN 
         regdatum = sldatum.
         RUN REGVEC.P.
         regdagnamn = "mån".
         RUN VECODAT.P.
         aostart = regdatum.
         antalcol = 1 + (aostart - instart) / 7.
      END.
      ELSE DO:
         ASSIGN 
         regdatum = stdatum.
         RUN REGVEC.P.
         regdagnamn = "mån".
         RUN VECODAT.P.
         instart = regdatum.
         ASSIGN 
         regdatum = slutdatum.
         RUN REGVEC.P.
         regdagnamn = "mån".
         RUN VECODAT.P.
         aostart = regdatum.
         antalcol = 1 + (aostart - instart) / 7.
      END.   
      
   END.
   ELSE IF vilkenbrw = 3 THEN DO:
      /*startdatum = visningsdatum stdatum = tilägestart*/
      IF stdatum < startdatum THEN startpos = 0.
      ELSE DO:
         IF YEAR(startdatum) = YEAR(stdatum) THEN DO:
            startpos = MONTH(stdatum) - MONTH(startdatum).            
         END.
         ELSE DO:
            startpos = 12 - MONTH(startdatum).
            startpos = startpos.
            startpos = startpos + MONTH(stdatum).
            IF YEAR(sldatum) - YEAR(stdatum) > 1  THEN DO:
               startpos = startpos + 12 * (YEAR(sldatum) - YEAR(stdatum) - 1).            
            END.
         END.
      END.     
      IF YEAR(sldatum) = YEAR(stdatum) THEN DO: 
         antalcol = MONTH(sldatum) - MONTH(stdatum).
         antalcol = antalcol + 1.
      END.
      ELSE DO:         
         antalcol = 12 - MONTH(stdatum).
         antalcol = antalcol + 1.
         antalcol = antalcol + MONTH(sldatum).
         IF YEAR(sldatum) - YEAR(stdatum) > 1  THEN DO:
            antalcol = antalcol + 12 * (YEAR(sldatum) - YEAR(stdatum) - 1).
         END.
      END.     
   END.
   /*Stega fram till startpos*/   
   loopraknare = 1.
   RUN vilkencelltemp_UI.
   IF AVAILABLE celltemp THEN DO:
      celltemp.CH:BGCOLOR = ?.
      celltemp.FARG = ?.
   END.
   DO WHILE loopraknare LE startpos:
      FIND NEXT celltemp WHERE celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
      IF AVAILABLE celltemp THEN DO:
         celltemp.CH:BGCOLOR = IF SELF:BGCOLOR = ? THEN fieldh:BUFFER-VALUE ELSE ?.          
         celltemp.FARG = IF SELF:BGCOLOR = ? THEN fieldh:BUFFER-VALUE ELSE ?.          
         loopraknare = loopraknare + 1.
      END.
      ELSE loopraknare = startpos + 1.
   END.   
   loopraknare = 0.
   fieldh = dynbuffh:BUFFER-FIELD("FARGNR").
   DO WHILE loopraknare < antalcol:
      IF AVAILABLE celltemp THEN DO: 
         IF celltemp.CNAME = "MON" OR celltemp.CNAME = "TIS" OR celltemp.CNAME = "ONS" OR celltemp.CNAME = "TOR" OR
         celltemp.CNAME = "FRE" OR celltemp.CNAME = "LOR" OR celltemp.CNAME = "SON" OR
         celltemp.CNAME = "JAN" OR
         celltemp.CNAME = "FEB" OR
         celltemp.CNAME = "MAR" OR
         celltemp.CNAME = "APR" OR
         celltemp.CNAME = "MAJ" OR
         celltemp.CNAME = "JUN" OR
         celltemp.CNAME = "JUL" OR
         celltemp.CNAME = "AUG" OR
         celltemp.CNAME = "SEP" OR
         celltemp.CNAME = "OKT" OR
         celltemp.CNAME = "NOV" OR
         celltemp.CNAME = "DECM"                  
         THEN DO:           
            celltemp.CH:BGCOLOR = fieldh:BUFFER-VALUE.                                   
            celltemp.FARG = fieldh:BUFFER-VALUE. 
         END.
         loopraknare = loopraknare + 1.
      END.
      ELSE loopraknare = antalcol + 1.
      FIND NEXT celltemp WHERE celltemp.CROWID = brwrowid USE-INDEX NUM NO-LOCK NO-ERROR.
   END.
   ASSIGN
   stdatum = ?
   sldatum = ?.     
   RUN musa IN huvprogh.
END PROCEDURE.

PROCEDURE newbrw_UI :  
   DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
   /*Ge egenskaper till browser*/
   RUN musw IN huvprogh.
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
   RUN extratrigg_UI.
   
   dynbrwh:NO-EMPTY-SPACE = FALSE NO-ERROR.
   dynbrwh:HIDDEN = TRUE.
   RUN brwegenskap_UI.
   dynbrwh:HIDDEN = TRUE.
   /*Skapa fast fält*/
   RUN createfields_UI.  
   /*Lägg till veckor*/
   tabort = TRUE.   
   IF vilkenbrw = 1 OR vilkenbrw = 2 THEN RUN vecka_UI.
   ELSE IF vilkenbrw = 3 THEN RUN manad_UI.   
   tabort = FALSE.        
   /*Öppna queryn*/
   dynbrwh:HIDDEN = FALSE.   
   RUN dynbrw_UI.
   dynbrwh:NO-EMPTY-SPACE = FALSE NO-ERROR.
   dynbrwh:NUM-LOCKED-COLUMNS = 9.        
   RUN setcolindex_UI IN brwproch (INPUT "ARBART BY AONR BY DELNR BY PERSONAL").
   kommando = " tidsplantemp.TIDPLAN = TRUE ".
   IF TOG_KALKYL:CHECKED THEN kommando = kommando + " OR " + "tidsplantemp.KALKYL = TRUE ".
   IF TOG_OVRIGA:CHECKED THEN kommando = kommando + " OR " + "tidsplantemp.OVRIGA = TRUE ".    
   RUN setcolsortvar_UI IN brwproch (INPUT kommando).
   RUN openbdynspec_UI IN brwproch.    
   dynbrwh:SENSITIVE  = TRUE.  
   dynbrwh:HIDDEN = FALSE.
   dynbrwh:DESELECT-ROWS() NO-ERROR.
END PROCEDURE.

PROCEDURE extratrigg_UI :
   IF VALID-HANDLE(dynbrwh) THEN DO:
      ON 'VALUE-CHANGED' OF dynbrwh PERSISTENT RUN vbrw_UI IN THIS-PROCEDURE.
   END.
END PROCEDURE.

PROCEDURE dynbrw_UI :
   
   RUN musw IN huvprogh.
   RUN DYNBRW.P PERSISTENT SET brwproch (INPUT dynbrwh).
   RUN dynprogextra IN brwproch (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN brwproch (INPUT TRUE).
   RUN brwsetupstop_UI IN brwproch (INPUT 1).
   IF vilkenbrw = 1 THEN DO:
      RUN addmenuitem2_UI IN brwproch (INPUT dynbrwh, INPUT "Sätt startdatum", INPUT "stalman_UI",INPUT THIS-PROCEDURE).
      RUN addmenuitem2_UI IN brwproch (INPUT dynbrwh, INPUT "Sätt slutdatum", INPUT "slalman_UI",INPUT THIS-PROCEDURE).   
   END.  
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
         IF fieldh:BUFFER-VALUE NE ? THEN DO:
            celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.
         END.
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
         IF fieldh:BUFFER-VALUE NE ? THEN DO:
            celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.
         END.
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
         IF fieldh:BUFFER-VALUE NE ? THEN DO:
            celltemp.CH:SCREEN-VALUE = fieldh:BUFFER-VALUE.
         END.
         fieldh = dynbuffh:BUFFER-FIELD("ANDRAD").
         fieldh:BUFFER-VALUE = TRUE. 
         dynbrwh:DESELECT-ROWS() NO-ERROR.
      END.
   END.
END PROCEDURE.
PROCEDURE avs_UI :
   IF VALID-HANDLE(brwproch) THEN DELETE PROCEDURE brwproch NO-ERROR.   
END PROCEDURE.
