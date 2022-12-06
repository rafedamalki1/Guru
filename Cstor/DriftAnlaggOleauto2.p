/*DriftAnlaggOleauto2.p 
  DriftAnlaggOleauto2.p*/
&SCOPED-DEFINE NEW
&SCOPED-DEFINE SHARED SHARED 
/*{ALLDEF.I}*/
{GLOBVAR2DEL1.I}

{STORTEMP.I}

/*
{AVDELNINGTEMP.I}
*/
DEFINE VARIABLE titelvar AS CHARACTER NO-UNDO.
{DRIFTTEMP.I}
DEFINE TEMP-TABLE spann_temp2    
   FIELD SPANID AS INTEGER
   FIELD NAMN AS CHARACTER
   INDEX SPAN SPANID. 
DEFINE TEMP-TABLE diagram NO-UNDO
   FIELD SPANID AS INTEGER
   FIELD SERCH AS CHARACTER
   FIELD NAMN AS CHARACTER
   INDEX SPAN SPANID. 
DEFINE VARIABLE seriestrang AS CHARACTER NO-UNDO.
titelvar = "Driftstörning per Felorsak".
{EXECLIN.I}
DEFINE INPUT PARAMETER TABLE FOR spann_temp2.
DEFINE INPUT PARAMETER TABLE FOR anlaggningsdeltemp.
DEFINE INPUT PARAMETER TABLE FOR felorsaktt.
DEFINE INPUT PARAMETER TABLE FOR drift_tempE2.
DEFINE INPUT PARAMETER TABLE FOR drift_tempE3.
DEFINE VARIABLE bladnr AS INTEGER NO-UNDO.




/* create a new Excel Application object */
CREATE "Excel.Application" chExcelApplication.

/* launch Excel so it is visible to the user */
chExcelApplication:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApplication:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApplication:Sheets:Item(1).

cRange = "B3".
irad = 3.
cColName = "B".
cRange = cColName + STRING(irad).

RUN rubrik_UI.


DEBUGGER:SET-BREAK().

irad = 4.
cColName = "A".
cRange = cColName + STRING(irad).
DEFINE VARIABLE felid AS INTEGER NO-UNDO.

FOR EACH spann_temp2 WHERE NO-LOCK:
   IF spann_temp2.NAMN BEGINS "UTGÅTT" THEN DELETE spann_temp2.
   ELSE DO:
      FOR EACH felorsaktt  WHERE NO-LOCK:
         FIND FIRST drift_tempE2 WHERE drift_tempE2.FELOID = felorsaktt.FELOID AND drift_tempE2.SPANID = spann_temp2.SPANID NO-LOCK NO-ERROR.
         IF NOT AVAILABLE drift_tempE2 THEN DO:
            CREATE drift_tempE2.
            drift_tempE2.SPANID = spann_temp2.SPANID.
            drift_tempE2.FELOID = felorsaktt.FELOID.
         END.
         drift_tempE2.NAMN = spann_temp2.NAMN.
      END.
      CREATE diagram.
      BUFFER-COPY spann_temp2 TO diagram.
   END.
END.
/*
=SERIE(Blad1!$A$132;Blad1!$B$14:$J$14;Blad1!$B$156:$J$156;1)
*/

FOR EACH spann_temp2:
   FOR EACH drift_tempE2 WHERE drift_tempE2.SPANID = spann_temp2.SPANID  BREAK BY drift_tempE2.FELOID:
      ACCUMULATE drift_tempE2.NAMN (COUNT BY drift_tempE2.FELOID).      
      IF LAST-OF(drift_tempE2.FELOID) THEN DO:
         RUN valueDataOut("A" + STRING(irad),spann_temp2.NAMN).
         FIND FIRST diagram WHERE diagram.SPANID = spann_temp2.SPANID NO-LOCK NO-ERROR.
         /*       
          diagram.SERCH = "B" + STRING(irad - 1) + ":J" + STRING(irad - 1) + ";$B$" + STRING(irad) + ":$J$" + STRING(irad). 
         diagram.SERCH = "A" + STRING(irad - 1) + ":J" + STRING(irad).
         diagram.SERCH = "B3:J3;" + "$B$" + STRING(irad) + ":$J$" + STRING(irad).
         */
         diagram.SERCH = "B3:J3;" + "B" + STRING(irad) + ":J" + STRING(irad).
         IF drift_tempE2.FELOID = 1 THEN RUN valueDataOut("B" + STRING(irad),STRING(drift_tempE2.ANTAL,">>>>>>9")).
         ELSE IF drift_tempE2.FELOID = 2 THEN RUN valueDataOut("C" + STRING(irad),STRING(drift_tempE2.ANTAL,">>>>>>9")).
         ELSE IF drift_tempE2.FELOID = 3 THEN RUN valueDataOut("D" + STRING(irad),STRING(drift_tempE2.ANTAL,">>>>>>9")). 
         ELSE IF drift_tempE2.FELOID = 4 THEN RUN valueDataOut("E" + STRING(irad),STRING(drift_tempE2.ANTAL,">>>>>>9")).
         ELSE IF drift_tempE2.FELOID = 5 THEN RUN valueDataOut("F" + STRING(irad),STRING(drift_tempE2.ANTAL,">>>>>>9")).
         ELSE IF drift_tempE2.FELOID = 6 THEN RUN valueDataOut("G" + STRING(irad),STRING(drift_tempE2.ANTAL,">>>>>>9")). 
         ELSE IF drift_tempE2.FELOID = 7 THEN RUN valueDataOut("H" + STRING(irad),STRING(drift_tempE2.ANTAL,">>>>>>9")).
         ELSE IF drift_tempE2.FELOID = 8 THEN RUN valueDataOut("I" + STRING(irad),STRING(drift_tempE2.ANTAL,">>>>>>9")).
         ELSE IF drift_tempE2.FELOID = 9 THEN RUN valueDataOut("J" + STRING(irad),STRING(drift_tempE2.ANTAL,">>>>>>9")).
         
      END.   
   END.
   
   
   RUN Rowdown.
   /*
   RUN rubrik_UI.
   RUN Rowdown.
   */
END.

FIND FIRST diagram WHERE NO-LOCK NO-ERROR.
RUN diagram_UI.
                     

RELEASE OBJECT chExcelApplication NO-ERROR .      
RELEASE OBJECT chWorkbook NO-ERROR .
RELEASE OBJECT chWorksheet NO-ERROR .
RELEASE OBJECT chChart NO-ERROR .
RELEASE OBJECT chWorksheetRange NO-ERROR . 
PROCEDURE rubrik_UI :
  
   cColName = "B".
   cRange = cColName + STRING(irad).
   FOR EACH felorsaktt WHERE NO-LOCK:
      RUN valueDataOut(cRange,felorsaktt.NAMN).
      RUN ColRight.
   END.
      
END PROCEDURE.
PROCEDURE diagram_UI :
   FOR EACH diagram WHERE NO-LOCK:
      chWorkSheet:SELECT.
      chWorkSheet:Range(diagram.SERCH):Select().
      chChart=chExcelApplication:Charts:Add().
      chChart:SeriesCollection(1):NAME = diagram.NAMN.
      chChart:Name = diagram.NAMN.
      chChart:Type = 5.
   END.
   
   
END PROCEDURE.

/*
 ActiveChart.ChartType = xlPie
    Range("E20").Select
    ActiveSheet.ChartObjects("Diagram 3").Activate
    ActiveSheet.ChartObjects("Diagram 3").Activate
    ActiveSheet.ChartObjects("Diagram 3").Activate
    ActiveChart.SeriesCollection.NewSeries
    ActiveChart.SeriesCollection(1).Name = "=Blad1!$A$8"
    ActiveChart.SeriesCollection(1).Values = "=Blad1!$B$8:$J$8"
    ActiveChart.SeriesCollection(1).XValues = "=Blad1!$B$3:$J$3"

 SetSourceData ( 
     Com-Handle-Source BY-POINTER,
     <anytype>-PlotBy ).
chWorkSheet:Range(diagram.SERCH):Select().
bladnr = 0.
FIND FIRST diagram WHERE NO-LOCK NO-ERROR.
chWorkSheet:Range("Blad1!$B$3:$J$3;Blad1!$B$4:$J$4"):Select().
chChart=chExcelApplication:Charts:Add().
   chChart:Name = diagram.NAMN.
   chChart:Type = 5.
  
RELEASE OBJECT chWorksheet NO-ERROR .
   chWorksheet = ?.
chWorkSheet = chExcelApplication:Sheets:Item(2).   
chWorkSheet:Range("Blad2!$B$3:$J$3;Blad2!$B$5:$J$5"):Select().   
chChart=chExcelApplication:Charts:Add().
   chChart:Name = "KALLE".
   chChart:Type = 5.   
*/
/*
FOR EACH diagram WHERE NO-LOCK:
   /*
   bladnr = bladnr + 1.
   chWorkSheet = chExcelApplication:Sheets:ITEM(bladnr).
   MESSAGE chWorkSheet:NAME
   VIEW-AS ALERT-BOX.
   chWorkSheet:Range(diagram.SERCH):Select().
   */
   MESSAGE "1" chWorkSheet
   VIEW-AS ALERT-BOX.
   chWorkSheet:Range("Blad1!$B$3:$J$3;Blad1!$B$4:$J$4"):Select().
 
   chChart=chExcelApplication:Charts:Add().
   chChart:Name = diagram.NAMN.
   chChart:Type = 5.
    MESSAGE "2" chWorkSheet
   VIEW-AS ALERT-BOX.
END.
*/
/*
OPEN QUERY aq FOR EACH anlaggningsdeltemp USE-INDEX ADELID NO-LOCK.
GET FIRST aq NO-LOCK.
DO WHILE AVAILABLE(anlaggningsdeltemp):
   FIND FIRST drift_tempE2 WHERE drift_tempE2.ADELID = anlaggningsdeltemp.ADELID
   USE-INDEX ADELID NO-LOCK NO-ERROR.
   IF AVAILABLE drift_tempE2 THEN DO:
      FIND FIRST drift_tempE3 WHERE drift_tempE3.ADELID = anlaggningsdeltemp.ADELID
      USE-INDEX ADELID NO-LOCK NO-ERROR.
      RUN valueDataOut(cRange,SUBSTRING(anlaggningsdeltemp.NAMN,1,20)).
      FOR EACH drift_tempE2 WHERE drift_tempE2.ADELID = anlaggningsdeltemp.ADELID
      USE-INDEX ADELID NO-LOCK:
         
         IF drift_tempE2.FELOID = 1 THEN RUN valueDataOut("B" + STRING(irad),STRING(drift_tempE2.ANTAL,">>>>>>9")).
         ELSE IF drift_tempE2.FELOID = 2 THEN RUN valueDataOut("C" + STRING(irad),STRING(drift_tempE2.ANTAL,">>>>>>9")).
         ELSE IF drift_tempE2.FELOID = 3 THEN RUN valueDataOut("D" + STRING(irad),STRING(drift_tempE2.ANTAL,">>>>>>9")). 
         ELSE IF drift_tempE2.FELOID = 4 THEN RUN valueDataOut("E" + STRING(irad),STRING(drift_tempE2.ANTAL,">>>>>>9")).
         ELSE IF drift_tempE2.FELOID = 5 THEN RUN valueDataOut("F" + STRING(irad),STRING(drift_tempE2.ANTAL,">>>>>>9")).
         ELSE IF drift_tempE2.FELOID = 7 THEN RUN valueDataOut("G" + STRING(irad),STRING(drift_tempE2.ANTAL,">>>>>>9")). 
         ELSE IF drift_tempE2.FELOID = 9 THEN RUN valueDataOut("H" + STRING(irad),STRING(drift_tempE2.ANTAL,">>>>>>9")). 
         ELSE IF drift_tempE2.FELOID = 8 THEN RUN valueDataOut("I" + STRING(irad),STRING(drift_tempE2.ANTAL,">>>>>>9")). 
                                              RUN valueDataOut("J" + STRING(irad),STRING(drift_tempE3.ANTAL,">>>>>>9")).
                                              RUN valueDataOut("K" + STRING(irad),drift_tempE3.NAMN).
          
      END.
      RUN Rowdown. 
   END.
   RUN Rowdown.
   GET NEXT aq NO-LOCK.
END.
CLOSE QUERY aq.
*/
