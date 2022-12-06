/* DYNBRWPROC.I */
DEFINE VARIABLE extenthelp AS INTEGER NO-UNDO.
PROCEDURE triggs_UI :
   ON ANY-PRINTABLE OF dynbrwh PERSISTENT RUN hittadyn_UI IN THIS-PROCEDURE.
   ON START-SEARCH OF dynbrwh PERSISTENT RUN startsearchproc IN THIS-PROCEDURE.
   ON CTRL-A OF dynbrwh PERSISTENT RUN ctrlaproc IN THIS-PROCEDURE.
   ON CTRL-C OF dynbrwh PERSISTENT RUN ctrlcproc IN THIS-PROCEDURE.
 /* ON CTRL-Z OF dynbrwh PERSISTENT RUN multiselect_UI IN THIS-PROCEDURE.*/
   ON ROW-DISPLAY OF dynbrwh PERSISTENT RUN rowdisplayproc IN THIS-PROCEDURE.
   
   ON MOUSE-SELECT-CLICK OF dynbrwh PERSISTENT RUN mouseselclick IN THIS-PROCEDURE.
END PROCEDURE.
DEFINE TEMP-TABLE ostemp NO-UNDO
FIELD TEMPFIL AS CHARACTER.


PROCEDURE lagerSaldo_UI :
   DEFINE VARIABLE Enrtabfalth AS HANDLE NO-UNDO.
   DEFINE VARIABLE Antaltabfalth AS HANDLE NO-UNDO.
   DEFINE VARIABLE Levkodtabfalth AS HANDLE NO-UNDO.
   /*
   {ANNPASSALEVTEMP1.I}
   */
    
   Levkodtabfalth = dynbuffh:BUFFER-FIELD("LEVKOD") NO-ERROR.
   IF Levkodtabfalth = ? THEN RETURN.
   
   Enrtabfalth = dynbuffh:BUFFER-FIELD("ENR") NO-ERROR.
   IF Enrtabfalth = ? THEN RETURN.
   Antaltabfalth = dynbuffh:BUFFER-FIELD("INKANTAL") NO-ERROR.
   IF Antaltabfalth = ? THEN  Antaltabfalth = dynbuffh:BUFFER-FIELD("ANTAL") NO-ERROR.
  
   Guru.GlobalaVariabler:LevkodtabInkfalth = dynbuffh:BUFFER-FIELD("LEVKODINK") NO-ERROR.
   
   Guru.GlobalaVariabler:jmfPrisEnrTTh:EMPTY-TEMP-TABLE() NO-ERROR.
   /*
   dynqueh:GET-FIRST().
   DO WHILE dynqueh:QUERY-OFF-END = FALSE:
      MESSAGE     dynbuffh:BUFFER-FIELD("LEVKODINK"):BUFFER-VALUE dynbuffh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE    dynbuffh:BUFFER-FIELD("INKANTAL"):BUFFER-VALUE  
            VIEW-AS ALERT-BOX.
      dynqueh:GET-NEXT(NO-LOCK).
   END.
   */
   dynqueh:GET-FIRST().
   DO WHILE dynqueh:QUERY-OFF-END = FALSE:
     
      IF Guru.GlobalaVariabler:LevkodtabInkfalth NE ? THEN DO:
        /* {ANNPASSAMTRLPEJMED.I}*/
         IF dynbuffh:BUFFER-FIELD("LEVKODINK"):BUFFER-VALUE = "0" THEN.       
         ELSE Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-CREATE().
      END.
      ELSE DO:
         /*{ANNPASSAMTRLPEJMED2.I}*/
         Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-CREATE().
      END.   
      IF Guru.GlobalaVariabler:jmfPrisEnrTTh:AVAILABLE THEN DO:
         Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE = dynbuffh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE NO-ERROR.
         IF Guru.GlobalaVariabler:LevkodtabInkfalth NE ? THEN DO:
            Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE = dynbuffh:BUFFER-FIELD("LEVKODINK"):BUFFER-VALUE NO-ERROR.
            Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("InputLEVKOD"):BUFFER-VALUE = dynbuffh:BUFFER-FIELD("LEVKODINK"):BUFFER-VALUE NO-ERROR.
            Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("InputBENAMNING"):BUFFER-VALUE = dynbuffh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE NO-ERROR.
         END.
         IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:            
            IF SUBSTRING(dynbuffh:BUFFER-FIELD("ENR"):BUFFER-VALUE ,1,1) NE "E" THEN Guru.GlobalaVariabler:jmfPrisEnrTTh::BUFFER-FIELD("ENR"):BUFFER-VALUE = "E" + dynbuffh:BUFFER-FIELD("ENR"):BUFFER-VALUE.
            Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE = dynbuffh:BUFFER-FIELD("ENR"):BUFFER-VALUE.                                       
         END.
         ELSE Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE = dynbuffh:BUFFER-FIELD("ENR"):BUFFER-VALUE.   
         IF Antaltabfalth NE ? THEN Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = Antaltabfalth:BUFFER-VALUE .
      END.  
    
      Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-RELEASE() NO-ERROR.  
      dynqueh:GET-NEXT(NO-LOCK).
   END.
   
   
   Guru.Konstanter:MtrlHelpStart().
   RUN jmfmtrpris_UI IN Guru.Konstanter:MtrlClasserStart.
  
   IF Guru.GlobalaVariabler:LevkodtabInkfalth = ? THEN Guru.GlobalaVariabler:jmfPrisEnrTTh:EMPTY-TEMP-TABLE().
   Guru.GlobalaVariabler:LevkodtabInkfalth = ?.
  /* {ANNPASSALEVTEMP2.I}*/
   
END PROCEDURE.

PROCEDURE SetBrwFont_UI :
   DEFINE INPUT  PARAMETER fnr AS INTEGER NO-UNDO.
   dynbrwh:FONT = fnr.
END PROCEDURE.
PROCEDURE ostempdel_UI :
   FOR EACH ostemp WHERE NO-LOCK:
      OS-DELETE VALUE(ostemp.TEMPFIL) NO-ERROR.
   END.
   EMPTY TEMP-TABLE ostemp NO-ERROR. 
END PROCEDURE.
PROCEDURE wversion_UI :
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
         
   IF vad = 1 THEN DO: 
      IF lastcolh NE ? THEN DO:
         IF dynbrwh:NAME = "BRW_TIDSPLAN" THEN .
         ELSE IF dynbrwh:NAME = "BRW_UT" THEN RUN WIDGETDEFVERSION.P (INPUT 2, INPUT dynbrwh).
         ELSE RUN WIDGETDEFVERSION.P (INPUT 1, INPUT dynbrwh).
      END.
      ELSE RUN WIDGETDEFVERSION.P (INPUT 1, INPUT dynbrwh).      
   END.
   ELSE IF vad = 2 THEN DO:
      RUN WIDGETDEFVERSION.P (INPUT 2, INPUT dynbrwh).   
   END.
END.
/********************************** Triggers *********************************************/

/* Följande rader kopplar ihop 'händelser', för browsern, med procedurer. 
*  Detta för att alla browsrar som använder sig av DYNBRW.P har gemensamma 
*  funktioner för olika 'händelser' som t.ex. ANY-PRINTABLE kopplas till proceduren
*  'hittadyn_UI' som har till uppgift att söka igenom innehållet i browsern utifrån
*  den tangent man har tryckt och sedan markera den rad som först stämmer överens. 
*/




/******************************* Trigger Procedures *************************************/

/*------------------------------------------------------------------------------
   Namn:       frmhset_UI                  
   Syfte:      font .
   In-data:    -
   Ut-data:    -
   Kommentar:  
------------------------------------------------------------------------------*/
PROCEDURE frmhset_UI :
   DEFINE INPUT PARAMETER frmh AS HANDLE NO-UNDO.
   frameh = frmh.  
   IF  VALID-HANDLE(aocolh) THEN DO:
      IF frameh:FONT = 6 THEN DO:
         IF Guru.Konstanter:varforetypval[8] >= 7 THEN DO:
            aocolh:WIDTH-CHARS = Guru.Konstanter:varforetypval[8] - 0.5.         
         END.
         ELSE DO:
            IF aocolh:WIDTH-CHARS <= 6 OR aocolh:WIDTH-CHARS = ? THEN aocolh:WIDTH-CHARS = 6 - 0.5.                     
         END.
      END.
   END.
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       setshiftcol_UI                  
   Syfte:      Ändra färger på rader i browser.
   In-data:    -
   Ut-data:    -
   Kommentar:  
------------------------------------------------------------------------------*/
PROCEDURE setshiftcol_UI :
   DEFINE INPUT PARAMETER radval AS LOGICAL NO-UNDO.
   colshiftlog = radval.
   RUN columnstotemp_UI.
   
END PROCEDURE.


/*------------------------------------------------------------------------------
   Namn:       startsearchproc                  
   Syfte:      Lagra den senast markerade posten, sortera om browsern och 
               markera sedan den senast valda posten.
   In-data:    -
   Ut-data:    -
   Kommentar:  Event procedure, anropas vid START-SEARCH händelse.
------------------------------------------------------------------------------*/
PROCEDURE startsearchproc :
   RUN fetchselrowid_UI.
   RUN selmulti_UI.
   RUN colsortdynbrw_UI (INPUT colsortcharinput).    
   RUN multiselect_UI.
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       ctrlaproc
   Syfte:      Markerar alla poster i browsern.
   In-data:    -
   Ut-data:    -
   Kommentar:  Event procedure, anropas vid CTRL-A händelse.
------------------------------------------------------------------------------*/
PROCEDURE ctrlaproc :
   IF multvar = TRUE THEN dynbrwh:SELECT-ALL() NO-ERROR.   
END PROCEDURE.

   /*------------------------------------------------------------------------------
      Namn:       excelproc
      Syfte:      Kopiera markerade poster i browsern till klippbordet.
      In-data:    -
      Ut-data:    -
      Kommentar:  Event procedure, anropas vid CTRL-C händelse.
   ------------------------------------------------------------------------------*/

PROCEDURE excelproc :
   DEFINE VARIABLE franfil AS CHARACTER NO-UNDO.
   DEFINE VARIABLE sparatxtxlsx AS CHARACTER NO-UNDO.
   /*dynbuffh = dynqueh:GET-BUFFER-HANDLE.*/
   tempcolh = dynbrwh:GET-BROWSE-COLUMN(1).  
   labelvar = "".
   DO WHILE VALID-HANDLE(tempcolh):
      IF tempcolh:VISIBLE = TRUE THEN labelvar = labelvar + tempcolh:LABEL + CHR(9).
      tempcolh = tempcolh:NEXT-COLUMN.         
   END.
   franfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt". 
  
   tempcounter = 1.
   labelvar = REPLACE(labelvar,"!"," ").
   OUTPUT TO VALUE(franfil).
   IF labelvar NE ? THEN PUT UNFORMATTED  labelvar SKIP.
   IF multvar = TRUE THEN DO:
      DO WHILE tempcounter LE dynbrwh:NUM-SELECTED-ROWS: /* För varje markerad rad */
         dynbrwh:FETCH-SELECTED-ROW(tempcounter).  /* 'Fånga' raden utifrån ett nummer */
         RUN excelut_UI.
         PUT SKIP.
         tempcounter = tempcounter + 1.
      END.
   END.
   ELSE DO:
      dynqueh:GET-FIRST(NO-LOCK).
      REPEAT:
         IF dynqueh:QUERY-OFF-END THEN LEAVE.  
         RUN excelut_UI.
         PUT SKIP.
         dynqueh:GET-NEXT(NO-LOCK).
      END.
   END.
   OUTPUT CLOSE.
   /*Anders Olsson Elpool i Umeå AB  9 nov 2017 18:20:06 
   OM DU INTE HAR EXCEL 
   */
   /*
    MESSAGE "E18" franfil
   VIEW-AS ALERT-BOX.
   */
   
   RUN open_UI (INPUT franfil).   
  
   RUN slutexcel_UI.
   
   IF INDEX(franfil,".txt") > 0 THEN  sparatxtxlsx = REPLACE(franfil,".txt",".xls").     
   CREATE ostemp.
   ostemp.TEMPFIL = franfil.
   CREATE ostemp.
   ostemp.TEMPFIL = sparatxtxlsx.
END PROCEDURE.


PROCEDURE excelut_UI :
   DEFINE VARIABLE tempbufh AS HANDLE NO-UNDO.
   DEFINE VARIABLE decichar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE datumh AS CHARACTER NO-UNDO.
   DEFINE VARIABLE arraynamn AS CHARACTER NO-UNDO.
   DEFINE VARIABLE arraycounterMax AS INTEGER NO-UNDO.
   DEFINE VARIABLE arraycounter AS INTEGER NO-UNDO.
   DEFINE VARIABLE arraycounterName AS CHARACTER NO-UNDO.
   tempcolh = dynbrwh:GET-BROWSE-COLUMN(1).    
   LOOP:
   DO WHILE VALID-HANDLE(tempcolh):
      IF tempcolh:NAME = ? THEN PUT UNFORMATTED CHR(9).
      ELSE IF tempcolh:VISIBLE = FALSE THEN "".
      ELSE DO:
         tempbufh = dynbuffh:BUFFER-FIELD(tempcolh:NAME) NO-ERROR.
         IF tempbufh:EXTENT > 0 THEN DO:
            IF arraynamn = tempbufh:NAME THEN arraycounter = arraycounter + 1.
            ELSE arraycounter = 1. 
            arraynamn = tempbufh:NAME.
            IF tempbufh:BUFFER-VALUE(arraycounter) = ? THEN DO:
               PUT UNFORMATTED CHR(9).
               tempcolh = tempcolh:NEXT-COLUMN.
               NEXT LOOP.
            END.
         END.
         ELSE DO:
            arraynamn = "".
            arraycounter = 0.
                
            IF tempbufh:BUFFER-VALUE = ? THEN DO:
               PUT UNFORMATTED CHR(9).
               tempcolh = tempcolh:NEXT-COLUMN.
               NEXT LOOP. 
            END.
         END.
         
         IF tempcolh:DATA-TYPE = "DECIMAL" THEN DO: 
            IF arraynamn = tempbufh:NAME THEN DO:
               IF tempbufh:BUFFER-VALUE(arraycounter) = "?" THEN tempbufh:BUFFER-VALUE(arraycounter) = "".
               decichar = REPLACE(STRING(ROUND(tempbufh:BUFFER-VALUE(arraycounter),2)),".",",").
            END.
            ELSE decichar = REPLACE(STRING(ROUND(tempbufh:BUFFER-VALUE,2)),".",",").               
            PUT UNFORMATTED STRING(decichar) + CHR(9).
         END.
         ELSE IF tempcolh:DATA-TYPE = "Date" THEN DO:
             IF arraynamn = tempbufh:NAME THEN DO:
               IF tempbufh:BUFFER-VALUE(arraycounter) = "?" THEN tempbufh:BUFFER-VALUE(arraycounter) = "".
             /*  PUT UNFORMATTED STRING(tempbufh:BUFFER-VALUE,"99/99/9999") + CHR(9).*/
               PUT UNFORMATTED STRING(tempbufh:BUFFER-VALUE(arraycounter),"99/99/9999") + CHR(9).
            END.
            ELSE  PUT UNFORMATTED STRING(tempbufh:BUFFER-VALUE,"99/99/9999") + CHR(9).
           
         END.
         ELSE IF tempcolh:NAME  = "ENR" OR tempcolh:LABEL = Guru.Konstanter:genk OR tempcolh:LABEL  = "Till Enr" OR tempcolh:LABEL  = "Från Enr"
         OR tempcolh:NAME  = "Till " + Guru.Konstanter:genk OR tempcolh:NAME  = "Från " + Guru.Konstanter:genk OR tempcolh:NAME  = "ENRERS" THEN DO:
            
            IF SUBSTRING(tempbufh:BUFFER-VALUE,1,1) = "E" THEN PUT UNFORMATTED STRING(tempbufh:BUFFER-VALUE) + CHR(9).
            ELSE IF STRING(tempbufh:BUFFER-VALUE) = "" THEN PUT UNFORMATTED STRING(tempbufh:BUFFER-VALUE) + CHR(9).
            ELSE PUT UNFORMATTED "E" + STRING(tempbufh:BUFFER-VALUE) + CHR(9).   
         END.
         ELSE IF tempcolh:DATA-TYPE = "LOGICAL" THEN DO:
            IF arraynamn = tempbufh:NAME THEN DO:
               IF tempbufh:BUFFER-VALUE(arraycounter) = "?" THEN tempbufh:BUFFER-VALUE(arraycounter) = "".
            /*   PUT UNFORMATTED STRING(tempbufh:BUFFER-VALUE,tempcolh:FORMAT) + CHR(9).*/
               PUT UNFORMATTED STRING(tempbufh:BUFFER-VALUE(arraycounter),tempcolh:FORMAT) + CHR(9).
            END.
            ELSE PUT UNFORMATTED STRING(tempbufh:BUFFER-VALUE(arraycounter),tempcolh:FORMAT) + CHR(9).
         END.   
         ELSE DO:
            IF arraynamn = tempbufh:NAME THEN DO:
               IF tempbufh:BUFFER-VALUE(arraycounter) = "?" THEN tempbufh:BUFFER-VALUE(arraycounter) = "".
               PUT UNFORMATTED STRING(tempbufh:BUFFER-VALUE(arraycounter)) + CHR(9).
            END.
            ELSE DO:   
               IF tempbufh:BUFFER-VALUE = "?" THEN tempbufh:BUFFER-VALUE = "".
               PUT UNFORMATTED STRING(tempbufh:BUFFER-VALUE) + CHR(9).
             END.  
         END.   
      END.
      
      tempcolh = tempcolh:NEXT-COLUMN.         
   END.
   
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       ctrlcproc
   Syfte:      Kopiera markerade poster i browsern till klippbordet.
   In-data:    -
   Ut-data:    -
   Kommentar:  Event procedure, anropas vid CTRL-C händelse.
------------------------------------------------------------------------------*/

PROCEDURE ctrlcproc :
   DEFINE VARIABLE arraycounterMax AS INTEGER NO-UNDO.
   DEFINE VARIABLE arraycounter AS INTEGER NO-UNDO.
   DEFINE VARIABLE arraycounterName AS CHARACTER NO-UNDO.
   dynbuffh = dynqueh:GET-BUFFER-HANDLE(1).
   DEFINE VARIABLE tempbufh AS HANDLE NO-UNDO.
   CLIPBOARD:MULTIPLE = TRUE.
   CLIPBOARD:ITEMS-PER-ROW = dynbrwh:NUM-COLUMNS.
   tempcounter = 1.
   tempcolh = dynbrwh:GET-BROWSE-COLUMN(1).     
   DO WHILE VALID-HANDLE(tempcolh):
      IF Guru.Konstanter:mtrlsekvar[6] = TRUE AND tempcolh:LABEL MATCHES "*PRIS*" THEN CLIPBOARD:ITEMS-PER-ROW = CLIPBOARD:ITEMS-PER-ROW - 1.
      ELSE CLIPBOARD:VALUE = tempcolh:LABEL.
      tempcolh = tempcolh:NEXT-COLUMN.         
   END.
   DO WHILE tempcounter LE dynbrwh:NUM-SELECTED-ROWS: /* För varje markerad rad */
      dynbrwh:FETCH-SELECTED-ROW(tempcounter).  /* 'Fånga' raden utifrån ett nummer */
      tempcolh = dynbrwh:GET-BROWSE-COLUMN(1).     
      DO WHILE VALID-HANDLE(tempcolh):
         IF arraycounterName = tempcolh:NAME THEN.
         ELSE DO:
            arraycounterName = tempcolh:NAME.
            IF Guru.Konstanter:mtrlsekvar[6] = TRUE AND tempcolh:LABEL MATCHES "*PRIS*" THEN.
            ELSE IF tempcolh:NAME = ? THEN DO:
               CLIPBOARD:VALUE = "?".
            END.
            ELSE DO:
               tempbufh = dynbuffh:BUFFER-FIELD(tempcolh:NAME).
               IF tempbufh:EXTENT = 0 THEN DO:
                  IF tempbufh:BUFFER-VALUE = ? THEN CLIPBOARD:VALUE = "?".
                  ELSE CLIPBOARD:VALUE = STRING(tempbufh:BUFFER-VALUE).
               END.
               ELSE DO:
                  arraycounter = 1.
                  arraycounterMax = tempbufh:EXTENT.
                  DO WHILE arraycounter <= arraycounterMax:
                     CLIPBOARD:VALUE = STRING(tempbufh:BUFFER-VALUE(arraycounter)).
                     arraycounter = arraycounter + 1.
                  END.   
                  
               END.        
            END.
         END.           
         tempcolh = tempcolh:NEXT-COLUMN.         
      END.
      tempcounter = tempcounter + 1.
   END.
   CLIPBOARD:MULTIPLE = FALSE.
END PROCEDURE.


PROCEDURE enrproc :
   dynbuffh = dynqueh:GET-BUFFER-HANDLE(1).
   DEFINE VARIABLE tempbufh AS HANDLE NO-UNDO.
   CLIPBOARD:ITEMS-PER-ROW = 1.
   tempbufh = dynbuffh:BUFFER-FIELD(dynenrh:NAME).
   IF tempbufh:BUFFER-VALUE = ? THEN CLIPBOARD:VALUE = "?".
   ELSE CLIPBOARD:VALUE = STRING(tempbufh:BUFFER-VALUE).
  
END PROCEDURE.
/*------------------------------------------------------------------------------
   Namn:       mouseselclick
   Syfte:      Ska fungera som VALUE-CHANGED.
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE mouseselclick :   
   IF Guru.GlobalaVariabler:LastTimeDepa NE 0 THEN Guru.GlobalaVariabler:LastTimeDepa = TIME.
   IF mselclick = TRUE THEN DO:
      APPLY "VALUE-CHANGED" TO dynbrwh.      
      RUN rowcolsel_UI (INPUT ?,INPUT 0).
   END.
END PROCEDURE.



/*------------------------------------------------------------------------------
   Namn:       enablebrw                 
   Syfte:      
   In-data:    
   Ut-data:    
   Kommentar:  
------------------------------------------------------------------------------*/
PROCEDURE enablebrw_UI.
   DEFINE INPUT PARAMETER elog AS LOGICAL NO-UNDO.
   dynbrwh:SENSITIVE = elog.
   IF elog = FALSE THEN dynbrwh:BGCOLOR = disbrwcol.
   ELSE dynbrwh:BGCOLOR = ?.   
END PROCEDURE.


/*------------------------------------------------------------------------------
   Namn:       columnstotemp_UI                 
   Syfte:      Skapar coltemp post, en för varje column.
   In-data:    -
   Ut-data:    -
   Kommentar:  Används av setrowproc_UI
------------------------------------------------------------------------------*/
PROCEDURE columnstotemp_UI.
   DEFINE VARIABLE tempvar AS INTEGER NO-UNDO.
   FIND FIRST coltemp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE coltemp THEN DO:
      tempvar = 1.
      tempcolh = dynbrwh:GET-BROWSE-COLUMN(tempvar).     
      DO WHILE VALID-HANDLE(tempcolh): 
         IF tempcolh = ? THEN RETURN.
         CREATE coltemp.
         ASSIGN
         coltemp.DTYP = tempcolh:DATA-TYP
         coltemp.CH = tempcolh
         coltemp.CNAME = tempcolh:NAME
         coltemp.NUM = tempvar.
         IF tempcolh:NAME = ? THEN coltemp.CNAME = tempcolh:LABEL.
         tempcolh = tempcolh:NEXT-COLUMN.
         tempvar = tempvar + 1.
         
      END.
   END.
   
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       setrowproc_UI
   Syfte:      Kopplar en column till en procedure som .
   In-data:    hproc -     Handle till den procedure som innehåller den interna proceduren 'chproc'
               chproc -    Namnet på den procedure som kommer att köras från 'rowdisplayproc'
               colname -   Namnet på den kolumn som ska manipuleras
   Ut-data:    -
   Kommentar:  Kopplingarna används i rowdisplayproc.
------------------------------------------------------------------------------*/
PROCEDURE setrowproc_UI.
/*ARRAY*/
   DEFINE INPUT PARAMETER hproc AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER chproc AS CHARACTER NO-UNDO.    
   DEFINE INPUT PARAMETER colname AS CHARACTER NO-UNDO.
   RUN columnstotemp_UI.
   tempcolh = dynbrwh:GET-BROWSE-COLUMN(1).   
   DO WHILE VALID-HANDLE(tempcolh): 
     
      IF tempcolh:NAME = ? THEN DO:
         IF tempcolh:LABEL = colname THEN DO:
            FIND FIRST coltemp WHERE coltemp.CH = tempcolh NO-LOCK NO-ERROR.
            IF AVAILABLE coltemp THEN DO:
               ASSIGN
               colproclog = TRUE
               coltemp.PROCH = hproc
               coltemp.PROCNAME = chproc.            
            END.
         END.   
      END.   
      ELSE IF tempcolh:NAME = colname THEN DO:
         
         FIND FIRST coltemp WHERE coltemp.CH = tempcolh NO-LOCK NO-ERROR.
         IF AVAILABLE coltemp THEN DO:
           
            ASSIGN
            colproclog = TRUE
            coltemp.PROCH = hproc
            coltemp.PROCNAME = chproc.   
                     
         END.
         LEAVE.
      END.
      tempcolh = tempcolh:NEXT-COLUMN.
      
   END.
   
END PROCEDURE.

PROCEDURE setrowprocA_UI.
/*ARRAY*/
   DEFINE INPUT PARAMETER bcnr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER hproc AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER chproc AS CHARACTER NO-UNDO.    
   DEFINE INPUT PARAMETER colname AS CHARACTER NO-UNDO.
   RUN columnstotemp_UI.
   tempcolh = dynbrwh:GET-BROWSE-COLUMN(bcnr).   
  
   FIND FIRST coltemp WHERE coltemp.CH = tempcolh NO-LOCK NO-ERROR.
   IF AVAILABLE coltemp THEN DO:
      ASSIGN
      colproclog = TRUE
      coltemp.PROCH = hproc
      coltemp.PROCNAME = chproc.
   END.
     
END PROCEDURE.


/*------------------------------------------------------------------------------
   Namn:       rowdisplayproc                  
   Syfte:      Ändrar formatet på en columns innehåll.
   In-data:    -
   Ut-data:    -
   Kommentar:  I vårat fall gäller detta endast för ao-nummer.
               Event procedure, anropas vid ROW-DISPLAY händelse.
------------------------------------------------------------------------------*/

PROCEDURE rowex_UI :
   /*extra*/

END PROCEDURE.
/*skall någt extra göras vid rowdisp*/ 
PROCEDURE rowdispextrakor :  
   DEFINE INPUT PARAMETER rowdispextra AS LOGICAL NO-UNDO.
   ASSIGN
   rowdispextravar = rowdispextra.  
END PROCEDURE.
/*vart finns det som skall köras extra vid en rowdisp*/
PROCEDURE dynprogextra :  
   DEFINE INPUT PARAMETER procexrowdis AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER rowdispkorh AS HANDLE NO-UNDO.
   procexrowdisvar = procexrowdis.
   dynprogextrah = rowdispkorh.
   
END PROCEDURE.
PROCEDURE cellbrwrow_UI :
   DEFINE INPUT  PARAMETER incellbrwrow AS ROWID NO-UNDO.
   cellbrwrow = incellbrwrow.
END PROCEDURE.   
PROCEDURE rowdisplayproc :  
  
   /*
   IF brwh:NAME = "brw_val" THEN 
      MESSAGE "1" brwcolh[1] brwcolh[2] brwcolh[3] VIEW-AS ALERT-BOX.
      */
   cellbrwrow = dynbuffh:ROWID.
   
   IF VALID-HANDLE(delnrcolh) THEN DO:
      delnrcolh:FORMAT = Guru.Konstanter:varforetypchar[1].
    
        
   END.
   IF VALID-HANDLE(kalklopnrh) THEN DO:
      kalklopnrh:FORMAT = Guru.Konstanter:varforetypchar[6].
   END.
   
   IF VALID-HANDLE(aocolh) THEN DO:
      aocolh:FORMAT = "X(" + STRING(Guru.Konstanter:varforetypval[8]) + ")".
      /*
      IF Guru.Konstanter:varforetypval[8] = 8 THEN aocolh:FORMAT = "X(8)".
      ELSE IF Guru.Konstanter:varforetypval[8] = 5 THEN aocolh:FORMAT = "X(5)".
      ELSE IF Guru.Konstanter:varforetypval[8] = 9 THEN aocolh:FORMAT = "X(9)".
      ELSE IF Guru.Konstanter:varforetypval[8] = 10 THEN aocolh:FORMAT = "X(10)".
      ELSE IF Guru.Konstanter:varforetypval[8] = 11 THEN aocolh:FORMAT = "X(11)".
      ELSE IF Guru.Konstanter:varforetypval[8] = 12 THEN aocolh:FORMAT = "X(12)".
      ELSE IF Guru.Konstanter:varforetypval[8] = 13 THEN aocolh:FORMAT = "X(13)".
      ELSE IF Guru.Konstanter:varforetypval[8] = 7 THEN aocolh:FORMAT = "X(7)".
      ELSE IF Guru.Konstanter:varforetypval[8] = 0 THEN aocolh:FORMAT = "X(6)".
      */
   END.
   /*funkar ej i 9.1e utan patch 02 skog och sund*/
   IF VALID-HANDLE(brwcolh[1]) THEN DO:
      brwcolh[1]:FORMAT = "X(256)".      
   END.
   IF VALID-HANDLE(brwcolh[2]) THEN DO:
      brwcolh[2]:FORMAT = "X(256)".      
   END.
   IF VALID-HANDLE(brwcolh[3]) THEN DO:
      brwcolh[3]:FORMAT = "X(256)".      
   END.
   IF VALID-HANDLE(brwcolh[4]) THEN DO:
      brwcolh[4]:FORMAT = "X(256)".      
   END.
   IF VALID-HANDLE(brwcolh[5]) THEN DO:
      brwcolh[5]:FORMAT = "X(256)".      
   END.
   
   IF colshiftlog = TRUE THEN DO:
      IF (dynqueh:CURRENT-RESULT-ROW MODULO 2) = 0 THEN DO:
         FOR EACH coltemp NO-LOCK:
            coltemp.CH:BGCOLOR = colvalnumber.
         END.
      END.      
   END.
   /*kör spec vid rowdisp*/
   
   /*
   tabfalth = table-name:BUFFER-FIELD(cnam).
      IF tabfalth:EXTENT = 0 THEN.
   
   tabfalth = table-name:BUFFER-FIELD(cnam).
      IF tabfalth:EXTENT = 0 THEN.
     
      ELSE DO: 
        
         
      END.
   */
   
   IF colproclog = TRUE THEN DO:     
      FOR EACH coltemp WHERE coltemp.PROCH NE ? NO-LOCK:
        
         IF coltemp.DTYP = "CHARACTER" THEN DO:
            RUN VALUE(coltemp.PROCNAME) IN coltemp.PROCH 
            (INPUT coltemp.CNAME, INPUT coltemp.CH, INPUT dynbuffh:BUFFER-FIELD(coltemp.CNAME),INPUT coltemp.DTYP).
         END.
         IF coltemp.DTYP = "INTEGER" THEN DO:
            RUN VALUE(coltemp.PROCNAME) IN coltemp.PROCH 
            (INPUT coltemp.CNAME, INPUT coltemp.CH, INPUT dynbuffh:BUFFER-FIELD(coltemp.CNAME),INPUT coltemp.DTYP).
         END.
         IF coltemp.DTYP = "DECIMAL" THEN DO:
            RUN VALUE(coltemp.PROCNAME) IN coltemp.PROCH 
            (INPUT coltemp.CNAME, INPUT coltemp.CH, INPUT dynbuffh:BUFFER-FIELD(coltemp.CNAME),INPUT coltemp.DTYP).
         END.
      END.
   END.
   IF rowdispextravar = TRUE THEN DO:   
      
      RUN VALUE(procexrowdisvar) IN dynprogextrah (INPUT TABLE coltemp,INPUT dynbrwh).
   END.
END PROCEDURE.
PROCEDURE coltemp_UI :
  
   FOR EACH coltemp :
   MESSAGE 
    PROCH 
    CH 
    
   CNAME 
    DTYP 
    NUM 
    
   VIEW-AS ALERT-BOX.
   END. 
   
END PROCEDURE.
/*------------------------------------------------------------------------------
   Namn:       rowcolsel_UI                   
   Syfte:      Vid '' sätta raden till en annan färg               
   In-data:    -
   Ut-data:    -
   Kommentar:  
------------------------------------------------------------------------------*/
PROCEDURE rowcolsel_UI :
   DEFINE INPUT PARAMETER colvalnumber AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER kompvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE tempvar AS INTEGER NO-UNDO.
   RUN fetchselrowid_UI.
   IF colshiftlog = FALSE THEN DO:
      IF kompvar = 0 THEN DO:

      END.
      /*sätter valda rader till vit*/ 
      ELSE IF kompvar = 1 OR kompvar = 3 THEN DO:        
         FOR EACH colcoltemp WHERE colcoltemp.CH:BGCOLOR = colvalnumber:
            colcoltemp.CH:BGCOLOR = 15.            
         END.
         dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.                               
      END.
      /*sätter röda rader till vita och vald rad till röd*/
      ELSE IF kompvar = 2 THEN DO:
         FOR EACH colcoltemp WHERE colcoltemp.CH:BGCOLOR = colvalnumber:
            colcoltemp.CH:BGCOLOR = 15.            
         END. 
         IF colvalnumber NE ? THEN DO:
            tempvar = 1.
            tempcolh = dynbrwh:GET-BROWSE-COLUMN(tempvar).     
            DO WHILE VALID-HANDLE(tempcolh): 
               FIND FIRST colcoltemp WHERE colcoltemp.CH = tempcolh NO-LOCK NO-ERROR.
               IF NOT AVAILABLE colcoltemp THEN DO:
                  CREATE colcoltemp.
               END.
               ASSIGN
               colcoltemp.CH = tempcolh
               colcoltemp.CNAME = tempcolh:NAME
               colcoltemp.NUM = tempvar.
               colcoltemp.CH:BGCOLOR = colvalnumber.
               tempvar = tempvar + 1.
               tempcolh = tempcolh:NEXT-COLUMN.
            END.     
            dynbrwh:DESELECT-ROWS() NO-ERROR.
         END. 
         ELSE DO:
            RUN repodyn_UI (INPUT lastbrwrow).
            dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.            
         END.         
      END.      
      /*sätter rader till vit*/
      ELSE IF kompvar = 3 THEN DO:
         FOR EACH colcoltemp WHERE colcoltemp.CH:BGCOLOR = colvalnumber:
            colcoltemp.CH:BGCOLOR = 15.                       
         END.      
         dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.                  
      END.
      
   END.  
END PROCEDURE.


/*------------------------------------------------------------------------------
   Namn:       anykey_UI                   
   Syfte:      Vid 'RETURN' görs en sökning i browsern utifrån
               sökfältets innehåll (FILL-IN kopplat till browser).
   In-data:    -
   Ut-data:    -
   Kommentar:  Event procedure, anropas vid ANY-KEY för ett angivet fill-in-fält.
------------------------------------------------------------------------------*/
PROCEDURE anykey_UI :
   /*find v9.1d*/   
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   IF KEYFUNCTION(LASTKEY) = ("CURSOR-RIGHT") OR KEYFUNCTION(LASTKEY) = ("CURSOR-LEFT") OR
      KEYFUNCTION(LASTKEY) = ("CURSOR-DOWN") OR KEYFUNCTION(LASTKEY) = ("CURSOR-UP") THEN DO :
         APPLY KEYFUNCTION(LASTKEY).
         RETURN NO-APPLY.
   END.
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:           
      RUN fillincheck_UI.   
   END.
   
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       mouseseldbclic_UI                 
   Syfte:      Se 'anykey_UI', gäller för MOUSE-SELECT-DBCLICK istället för 'RETURN'.
   In-data:    -
   Ut-data:    -
   Kommentar:  Event procedure, anropas vid MOUSE-SELECT-DBCLICK händelse.
------------------------------------------------------------------------------*/
PROCEDURE mouseseldbclic_UI :   
   RUN fillincheck_UI.      
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       addfillinchar_UI                  
   Syfte:      Kopplar samman sökfält(FILL-IN) med en column i browser, detta sker
               med hjälp av två triggrar ANY-KEY och MOUSE-SELECT-DBLCLICK.
   In-data:    fhandle - En handle till en FILL-IN widget.
               colname - Kolumn namnet för den kolumn som ska kopplas till sökfältet.
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE addfillin_UI.
   DEFINE INPUT PARAMETER fhandle AS HANDLE NO-UNDO. 
   DEFINE INPUT PARAMETER colname AS CHARACTER NO-UNDO. 
   
   IF VALID-HANDLE(fhandle) THEN DO:
      fhandle:PRIVATE-DATA = colname.
   END.                                                    
   ON ANY-KEY OF fhandle PERSISTENT RUN anykey_UI IN THIS-PROCEDURE.
   ON MOUSE-SELECT-DBLCLICK OF fhandle PERSISTENT RUN mouseseldbclic_UI IN THIS-PROCEDURE.
END PROCEDURE.

PROCEDURE addfillindes_UI.
   DEFINE INPUT PARAMETER desel AS LOGICAL NO-UNDO.     
   deselvar = desel. 
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       connectfillin                 
   Syfte:      
   In-data:    
   Ut-data:    
   Kommentar:  
------------------------------------------------------------------------------*/
PROCEDURE connectfillin_UI.
   DEFINE INPUT PARAMETER fhandle AS HANDLE NO-UNDO. 
   DEFINE INPUT PARAMETER shandle AS HANDLE NO-UNDO. 
   CREATE fillintemp.
   ASSIGN
   fillintemp.FH = fhandle
   fillintemp.SH = shandle.
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       setfillinproc                 
   Syfte:      
   In-data:    
   Ut-data:    
   Kommentar:  
------------------------------------------------------------------------------*/
PROCEDURE setfillinproc_UI.
   DEFINE INPUT PARAMETER hproc AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER chproc AS CHARACTER NO-UNDO.    
   ASSIGN
   hfillinproc = hproc
   chfillinproc = chproc.
END PROCEDURE.


/*------------------------------------------------------------------------------
   Namn:       fillincheck_UI                  
   Syfte:      
   In-data:    
   Ut-data:    
   Kommentar:  
------------------------------------------------------------------------------*/
PROCEDURE fillincheck_UI.

   IF deselvar = TRUE THEN dynbrwh:DESELECT-ROWS() NO-ERROR.
   IF SELF:READ-ONLY = FALSE THEN DO:
      FIND FIRST fillintemp WHERE fillintemp.FH = SELF OR fillintemp.SH = SELF NO-LOCK NO-ERROR.
      IF AVAILABLE fillintemp THEN DO:
         IF fillintemp.FH = SELF THEN DO: 
            ASSIGN
            selfh = fillintemp.FH
            otherh = fillintemp.SH.
         END.
         ELSE DO:
            ASSIGN
            selfh = fillintemp.SH
            otherh = fillintemp.FH.
         END.
         IF selfh:SCREEN-VALUE = "" OR otherh:SCREEN-VALUE = "" THEN DO: 
            RUN sokurvaldyn_UI (INPUT SELF:PRIVATE-DATA, INPUT SELF:SCREEN-VALUE).
         END.
         ELSE DO:
            IF NUM-ENTRIES(selfh:SCREEN-VALUE,"*") = 1 THEN DO:
               ssok = "*" + selfh:SCREEN-VALUE + "*".
            END.
            ELSE ssok = selfh:SCREEN-VALUE.
            IF NUM-ENTRIES(otherh:SCREEN-VALUE,"*") = 1 THEN DO:
               osok = "*" + otherh:SCREEN-VALUE + "*".
            END.  
            ELSE osok = otherh:SCREEN-VALUE.
            ASSIGN
            dynbuffh = dynqueh:GET-BUFFER-HANDLE(1) 
            sfalth = ?      
            ofalth = ?.
            sfalth = dynbuffh:BUFFER-FIELD(selfh:PRIVATE-DATA) NO-ERROR.
            ofalth = dynbuffh:BUFFER-FIELD(otherh:PRIVATE-DATA) NO-ERROR.
            IF sfalth = ? OR ofalth = ? THEN DO:                                               
               MESSAGE "Felaktigt kolumn namn: " ofalth:PRIVATE-DATA sfalth:PRIVATE-DATA VIEW-AS ALERT-BOX.
               RETURN.                                                             
            END.                                                                   
            IF dynqueh:IS-OPEN = FALSE THEN RETURN.
            REPEAT:
               dynqueh:GET-NEXT(NO-LOCK).
               IF dynqueh:QUERY-OFF-END THEN LEAVE.            
               RUN getfaltvalue_UI(INPUT sfalth, OUTPUT sfaltvalue).
               RUN getfaltvalue_UI(INPUT ofalth, OUTPUT ofaltvalue).
               IF sfaltvalue MATCHES ssok AND ofaltvalue MATCHES osok THEN DO:
                  brwrow = dynbuffh:ROWID.
                  lastbrwrow = brwrow.
                  RUN repodyn_UI (INPUT brwrow).
                  dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.          
                  RETURN.
               END.            
            END.
            dynqueh:GET-FIRST(NO-LOCK).
            IF dynqueh:QUERY-OFF-END THEN LEAVE.
            REPEAT:
               IF dynqueh:QUERY-OFF-END THEN LEAVE.            
               RUN getfaltvalue_UI(INPUT sfalth, OUTPUT sfaltvalue).
               RUN getfaltvalue_UI(INPUT ofalth, OUTPUT ofaltvalue).
               IF sfaltvalue MATCHES ssok AND ofaltvalue MATCHES osok THEN DO:
                  brwrow = dynbuffh:ROWID.
                  lastbrwrow = brwrow.
                  RUN repodyn_UI (INPUT brwrow).
                  dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.          
                  RETURN.
               END.            
               dynqueh:GET-NEXT(NO-LOCK).
               IF dynqueh:QUERY-OFF-END THEN LEAVE.
            END.
         END.
      END. 
      ELSE RUN sokurvaldyn_UI (INPUT SELF:PRIVATE-DATA, INPUT SELF:SCREEN-VALUE).
      IF hfillinproc NE ? THEN DO:       
         RUN VALUE(chfillinproc) IN hfillinproc.
      END.
   END.
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       getfaltvalue_UI                 
   Syfte:      
   In-data:    
   Ut-data:    
   Kommentar:  
------------------------------------------------------------------------------*/
PROCEDURE getfaltvalue_UI.
   DEFINE INPUT PARAMETER tempfh AS HANDLE NO-UNDO. 
   DEFINE OUTPUT PARAMETER tempfaltvalue AS CHARACTER NO-UNDO. 
   
   IF tempfh:DATA-TYPE = "LOGICAL" THEN DO:
      colformat = tempfh:FORMAT.
      logtrue = ENTRY (1,  colformat, "/").
      logfalse = ENTRY (2,  colformat, "/").
      IF tempfh:BUFFER-VALUE = TRUE THEN DO:
         tempfaltvalue = logtrue.
      END.
      ELSE DO:
         tempfaltvalue = logfalse.
      END.
   END.                            
   ELSE IF tempfh:DATA-TYPE = "INTEGER"  THEN DO:     
      sokord = REPLACE(sokord,",","").      
      tempfaltvalue = STRING(tempfh:BUFFER-VALUE).
   END.
   ELSE DO:
      tempfaltvalue = STRING(tempfh:BUFFER-VALUE).
   END.  
END PROCEDURE.

PROCEDURE ReadOnlyCol_UI :
   DEFINE INPUT  PARAMETER colnumnr AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER reado AS LOGICAL NO-UNDO.
   DEFINE VARIABLE dynReadOh AS HANDLE NO-UNDO.
   dynReadOh = brwh:GET-BROWSE-COLUMN(colnumnr). 
   dynReadOh:COLUMN-READ-ONLY = reado.
    
END PROCEDURE.

PROCEDURE ReadOnlyColKol_UI :
   DEFINE INPUT  PARAMETER colnumnr AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER reado AS LOGICAL NO-UNDO.
   DEFINE VARIABLE dynReadOh AS HANDLE NO-UNDO.
   dynReadOh = brwh:GET-BROWSE-COLUMN(colnumnr). 
   reado = dynReadOh:COLUMN-READ-ONLY.
    
END PROCEDURE.

PROCEDURE sattindex_UI :
   DEFINE INPUT PARAMETER varindx AS CHARACTER NO-UNDO.
   IF varindx = "" THEN sattindexvar = "".
   ELSE sattindexvar = " USE-INDEX " + varindx.
END PROCEDURE.

PROCEDURE openbdynutanbrw_UI :
   DEFINE INPUT PARAMETER tabvar AS CHARACTER NO-UNDO.  
   kommandosortquery = "FOR EACH " + dynbuffh:TABLE + tabvar + ".".    
   dynok = dynqueh:QUERY-PREPARE(kommandosortquery).
   IF dynok = TRUE THEN DO: 
      dynok = dynqueh:QUERY-OPEN() NO-ERROR.
   END.   
END PROCEDURE.

PROCEDURE openbdyn_UI :
   DEFINE INPUT PARAMETER tabvar AS CHARACTER NO-UNDO. 
   
   IF Guru.GlobalaVariabler:LastTimeDepa NE 0 THEN Guru.GlobalaVariabler:LastTimeDepa = TIME.
   IF dyncurcolh = ? THEN RETURN. 
   IF dyncurcolh:NAME = ? THEN RETURN.
   RUN getrownr_UI. 
   
   colrowshift = 0.
   dynbuffh = dynqueh:GET-BUFFER-HANDLE(1).
   
   
   
   IF varjoin = "" THEN DO:
      IF descvarcol = FALSE THEN DO:
         IF colindex = ? THEN DO:
            IF extenthelp  = 0 OR extenthelp = ? THEN  kommandosortquery = "FOR EACH " + dynbuffh:TABLE + tabvar + sattindexvar + " NO-LOCK BY " + dyncurcolh:NAME + colsortby .
            ELSE kommandosortquery = "FOR EACH " + dynbuffh:TABLE + tabvar + sattindexvar + " NO-LOCK " + colsortby .
            /* IF dyncurcolh:EXTENT = 0 THEN     
            kommandosortquery = "FOR EACH " + dynbuffh:TABLE + tabvar + sattindexvar + " NO-LOCK BY " + dyncurcolh:NAME + colsortby .
            ELSE kommandosortquery = "FOR EACH " + dynbuffh:TABLE + tabvar + sattindexvar + " NO-LOCK " + colsortby .
            */
         END.
         ELSE DO:
            kommandosortquery = "FOR EACH " + dynbuffh:TABLE + tabvar + sattindexvar + " NO-LOCK BY " + colindex + colsortby.
         END.      
      END.
      ELSE DO:
         IF colindex = ? THEN DO:
            IF extenthelp  = 0 OR extenthelp = ? THEN kommandosortquery = "FOR EACH " + dynbuffh:TABLE + tabvar + sattindexvar + " NO-LOCK BY " + dyncurcolh:NAME + " DESCENDING " + colsortby.  
            ELSE  kommandosortquery = "FOR EACH " + dynbuffh:TABLE + tabvar + sattindexvar + " NO-LOCK " + colsortby + " DESCENDING ". 
                 
         END.
         ELSE DO:
            kommandosortquery = "FOR EACH " + dynbuffh:TABLE + tabvar + sattindexvar + " NO-LOCK BY " + colindex + colsortby + " DESCENDING ".         
         END.      
      END.    
   END.
   ELSE DO:
      IF colindex = ? THEN kommandosortquery = "FOR EACH " + dynbuffh:TABLE + tabvar + sattindexvar + " NO-LOCK " + ", EACH " + varjoin + " NO-LOCK ".
      ELSE kommandosortquery = "FOR EACH " + dynbuffh:TABLE + tabvar + sattindexvar + " NO-LOCK " + ", EACH " + varjoin + " NO-LOCK BY " + colindex.  
   END.   
  
   dynok = dynqueh:QUERY-PREPARE(kommandosortquery).
   IF dynok = TRUE THEN DO: 
      dynok = dynqueh:QUERY-OPEN() NO-ERROR.
      IF dynok = FALSE THEN DO:
         IF descvarcol = FALSE THEN DO:
            IF colindex = ? THEN DO:
               kommandosortquery = "FOR EACH " + dynbuffh:TABLE + tabvar + sattindexvar + " NO-LOCK BY SUBSTRING( " + dyncurcolh:NAME + ", 1, 127)" + colsortby + ".".
            END.
            ELSE DO:
               kommandosortquery = "FOR EACH " + dynbuffh:TABLE + tabvar + sattindexvar + " NO-LOCK BY " + colindex + colsortby + ".".
            END.      
         END.
         ELSE DO:
            IF colindex = ? THEN DO:
               kommandosortquery = "FOR EACH " + dynbuffh:TABLE + tabvar + sattindexvar + " NO-LOCK BY SUBSTRING( " + dyncurcolh:NAME + ", 1, 127) DESCENDING " + colsortby + ".".
            END.
            ELSE DO:
               kommandosortquery = "FOR EACH " + dynbuffh:TABLE + tabvar + sattindexvar + " NO-LOCK BY " + colindex + " DESCENDING " + colsortby + ".".
            END.      
         END.
         dynok = dynqueh:QUERY-PREPARE(kommandosortquery).
         dynok = dynqueh:QUERY-OPEN().
      END.
      /*
      
      IF rowdispextravar = TRUE THEN DO:
         RUN VALUE(procexrowdisvar) IN dynprogextrah (INPUT TABLE coltemp,INPUT dynbrwh).
      END.
      */
     
      IF titlelog = TRUE THEN DO:
         RUN title_UI.
      END.
      ELSE RUN maxdata_UI.
   END.
   ELSE DO:
      MESSAGE "Kunde inte ladda Browser-Query!" VIEW-AS ALERT-BOX. 
      RETURN.
   END. 
END PROCEDURE.
PROCEDURE hursortq_UI:
   DEFINE OUTPUT PARAMETER hursort AS CHARACTER NO-UNDO.
   hursort = kommandosortquery.
END PROCEDURE.

           
PROCEDURE labelater_UI :
   IF dynlastcolh NE ? THEN DO:
      ASSIGN
      dynlastcolh:LABEL-BGCOLOR = bgcolvar
      dynlastcolh:LABEL-FONT   =   fontvar
      dynlastcolh:LABEL   = labelvar. 
   END.
END PROCEDURE.
PROCEDURE setsortvar_UI :
   DEFINE INPUT PARAMETER incolsortby AS CHARACTER NO-UNDO.
   colsortby = incolsortby.
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       colsortdynbrw_UI                  
   Syfte:      Öppnar en query sorterad utifrån vald kolumn. 
               Om samma kolumn väljs två gånger sorteras posterna i omvänd ordning. 
   In-data:    tabvar [CHAR] - Kommandosträng för öppnadet av en query.
               Lämnas tom ifall man vill erhålla allt innehåll.
               ex. "WHERE temptable.FIELD = vtemptable.FIELD", "".
   Ut-data:    -
   Kommentar:  Kräver att initdynbrw_UI har körts!
------------------------------------------------------------------------------*/
PROCEDURE colsortdynbrw_UI :
   DEFINE INPUT PARAMETER tabvar AS CHARACTER NO-UNDO.
   
   
   dynbuffh = dynqueh:GET-BUFFER-HANDLE(1).
   dyncurcolh = dynbrwh:CURRENT-COLUMN.
   IF VALID-HANDLE(dyncurcolh) = FALSE THEN DO:
      dyncurcolh = dynbrwh:GET-BROWSE-COLUMN(defaultcol).
   END.
   IF dyncurcolh NE ? THEN DO:
      IF dyncurcolh:NAME NE ? THEN DO:
         IF dyncurcolh:PRIVATE-DATA NE ? THEN DO:
            IF ENTRY(1,dyncurcolh:PRIVATE-DATA,"$") NE "?" THEN DO: 
               tabvar = ENTRY(1,dyncurcolh:PRIVATE-DATA,"$").
            END.
            IF ENTRY(2,dyncurcolh:PRIVATE-DATA,"$") NE "?" THEN DO: 
               colsortby = " BY " + ENTRY(2,dyncurcolh:PRIVATE-DATA,"$").
            END.      
            ELSE colsortby = "".
              
         END.
         
         ELSE DO:          
            dynbuffhexten = dyncurcolh:BUFFER-FIELD().
            extenthelp = dynbuffhexten:EXTENT NO-ERROR.                               
            IF extenthelp = 0 OR extenthelp = ? THEN DO:
               IF colindex NE ? THEN colsortby = " BY " + dyncurcolh:NAME.
               colsortby = TRIM(colsortby).
               colsortby = " " + colsortby.
            END.
            ELSE DO:
               colsortby = " BY " + dyncurcolh:LABEL.
               colsortby = REPLACE(colsortby,"^","").
               colsortby = REPLACE(colsortby,"v",""). 
               colsortby = " " + colsortby.
               /*
               IF SUBSTRING(colsortby,LENGTH(colsortby),1) = "]" THEN.
               ELSE colsortby = SUBSTRING(colsortby,1, LENGTH(colsortby) - 1).
               */
            END.
         END.
         
      END.
      ELSE DO: 
         dyncurcolh = dynlastcolh.
         RETURN.
      END.
      /*Om samma kolumn har blivit tryckt på sortera i annan ordning.*/
      IF dynlastcolh = dyncurcolh THEN DO:
         IF descvarcol = TRUE THEN descvarcol = FALSE.
         ELSE descvarcol = TRUE.           
      END.
      ELSE DO:
         RUN labelater_UI.
         
         ASSIGN
         bgcolvar = dyncurcolh:LABEL-BGCOLOR
         fontvar  = dyncurcolh:LABEL-FONT
         labelvar = dyncurcolh:LABEL
         descvarcol = FALSE.
         ASSIGN     
         dyncurcolh:LABEL-FONT = 4
         dyncurcolh:LABEL-BGCOLOR = 15. 
         
      END.
      /*Spara undan senast intryckta kolumn för en specifik browser(brwvar[ex. värdet 1])*/
      IF descvarcol = FALSE THEN dyncurcolh:LABEL   = labelvar + " ^".     
      ELSE dyncurcolh:LABEL   = labelvar + " v".
      dynlastcolh = dyncurcolh.               
   END.
   ELSE dyncurcolh = dynlastcolh.
   /* Ifall det inte finns någon kolumn markerad, ex. anrop till openbdyn_UI från
      'btn_hamt' triggern.*/
   RUN openbdyn_UI (INPUT tabvar).
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       comparedyn_UI                  
   Syfte:      
   In-data:    val [INT] - (1,2) används för att jämföra innehållet i ett fält
                  1 - Jämför med BEGINS
                  2 - Jämför med MATCHES
   Ut-data:    retval [BOOL] - Returnerar ett värde på hur jämförandet gick.
                  TRUE - Har hittat ett sökt värde och markerat denna rad.
                  FALSE - Hittade inget värde, ingen rad markerad i browsern.
   Kommentar:  Måste känna till en hel del variabler, anropas från sokurvaldyn_UI 
               och hittadyn_UI. Bör ej anropas separat från huvudprogram.
------------------------------------------------------------------------------*/
PROCEDURE comparedyn_UI :
   DEFINE INPUT PARAMETER val AS INTEGER.
   DEFINE OUTPUT PARAMETER retval AS LOGICAL.
   retval = FALSE.
   /*Hämtar en 'fält-handle' från den aktiva buffern*/
/*    dynfalth = dynbuffh:BUFFER-FIELD(dyncurcolh:NAME). */
   /* "colh" är en handle till en kolumn, blir satt i hittadyn_UI.*/
   IF dynfalth:DATA-TYPE = "LOGICAL" THEN DO:
      /*"colformat" kan exempelvis få värdet "Ja/Nej" */
      colformat = dynfalth:FORMAT.
      /* Plockar ut 'Ja' och 'Nej' för att kunna jämför kanpptryckningar mot
         detta värde.*/
      logtrue = ENTRY (1,  colformat, "/").
      logfalse = ENTRY (2,  colformat, "/").
      IF dynfalth:BUFFER-VALUE = TRUE THEN DO:
         faltvalue = logtrue.
      END.
      ELSE DO:
         faltvalue = logfalse.
      END.
   END.
   ELSE IF dynfalth:DATA-TYPE = "DATE" AND val = 1 THEN DO:
      trimnr = 1.
      faltvalue = "".
      logdate = string(dynfalth:BUFFER-VALUE).
      DO WHILE trimnr NE 0:
         trimnr = INDEX(logdate, "/").
         faltvalue = faltvalue + SUBSTRING(logdate, 1, trimnr - 1).
         logdate = TRIM(SUBSTRING(logdate, trimnr + 1)).
      END.
   END.                            
   ELSE IF dynfalth:DATA-TYPE = "INTEGER"  THEN DO:     
      sokord = REPLACE(sokord,",","").      
      faltvalue = STRING(dynfalth:BUFFER-VALUE).
   END.
   ELSE DO:
      faltvalue = string(dynfalth:BUFFER-VALUE).
   END.
   IF val = 1 THEN DO:
      IF faltvalue BEGINS multibrwsok THEN DO:
         brwrow = dynbuffh:ROWID.
         lastbrwrow = brwrow.
         RUN selectdyn_UI (INPUT brwrow).
         retval = TRUE.
         RETURN.
      END.
   END.
   ELSE IF val = 2 THEN DO:
      IF faltvalue MATCHES sokord THEN DO:
         brwrow = dynbuffh:ROWID.
         lastbrwrow = brwrow.
         RUN repodyn_UI (INPUT brwrow).
         dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR. 
         retval = TRUE.
         RETURN.
      END.
   END.
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       hittadyn_UI                  
   Syfte:      Används för att söka i en browser-column med knapptryckningar.
   In-data:    -
   Ut-data:    -
   Kommentar:  Kräver en "HANDLE" till den aktuella browsern.
               Erhålles enligt följande kommandorad.     
               dynbrwh = BROWSE {&BROWSE-NAME}:HANDLE. 
------------------------------------------------------------------------------*/
PROCEDURE hittadyn_UI :                    
   /*    {muswait.i} */
   
   brwsok = LAST-EVENT:LABEL.
   IF MTIME - multitid <= 500 THEN DO:
      multi = TRUE.
      multibrwsok = multibrwsok + brwsok.
   END.
   ELSE DO:
      saverowid = ?.
      multi = FALSE.
      multibrwsok = brwsok.
   END.
   
   multitid = MTIME.
   dynbuffh = dynqueh:GET-BUFFER-HANDLE(1).
   dynfalth = dynbuffh:BUFFER-FIELD(dyncurcolh:NAME).
   /*find v9.1d*/
   REPEAT:
      IF multi = FALSE THEN DO:
         dynqueh:GET-NEXT(NO-LOCK).
         IF dynqueh:QUERY-OFF-END THEN LEAVE.
      END.
      IF dynqueh:QUERY-OFF-END THEN DO:
         dynqueh:GET-FIRST(NO-LOCK).
         IF dynqueh:QUERY-OFF-END THEN LEAVE.
      END.
      RUN comparedyn_UI (INPUT 1, OUTPUT retvalue).
      IF retvalue = TRUE THEN RETURN.
      IF multi = TRUE THEN DO:
         dynqueh:GET-NEXT(NO-LOCK).
         IF dynqueh:QUERY-OFF-END THEN LEAVE.
      END.
   END.
   dynqueh:GET-FIRST(NO-LOCK).
   IF dynqueh:QUERY-OFF-END THEN LEAVE.
   REPEAT:
      RUN comparedyn_UI (INPUT 1, OUTPUT retvalue).
      IF retvalue = TRUE THEN RETURN.
      dynqueh:GET-NEXT(NO-LOCK).
      IF dynqueh:QUERY-OFF-END THEN LEAVE.
   END.
   IF lastbrwrow NE ? THEN DO:
      RUN repodyn_UI (INPUT lastbrwrow).
   END.
END PROCEDURE.

PROCEDURE getrownr_UI :
   brwradnr = dynbrwh:FOCUSED-ROW. 
   
END PROCEDURE.
/*------------------------------------------------------------------------------
   Namn:       repodyn_UI                  
   Syfte:      Sätter browserns fokus till angiven rad.
   In-data:    brwrow [ROWID] - Måste vara ett ROWID som finns i queryn.
               brwhandle [HD] - En handle till den aktuella browsern.  
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE repodyn_UI :
   DEFINE INPUT PARAMETER brwrow AS ROWID NO-UNDO.
   IF brwradnr <= 0 THEN brwradnr = 35.
   ELSE IF brwradnr = ? THEN brwradnr = 35.
   IF brwradnr < 35 THEN brwradnr = 35. /*anders 20131009*/
   dynbrwh:REFRESHABLE = FALSE.
   dynbrwh:SET-REPOSITIONED-ROW(brwradnr,"ALWAYS"). 
   dynqueh:REPOSITION-TO-ROWID(brwrow) NO-ERROR.
   dynbrwh:REFRESHABLE = TRUE.    
   APPLY "VALUE-CHANGED" TO dynbrwh. 
   IF brwradnr = ? THEN brwradnr = 35.
END PROCEDURE.

PROCEDURE repodynnv_UI :
   DEFINE INPUT PARAMETER brwrow AS ROWID NO-UNDO.
   IF brwradnr <= 0 THEN brwradnr = 35.
   ELSE IF brwradnr = ? THEN brwradnr = 35.
   dynbrwh:REFRESHABLE = FALSE.
   dynbrwh:SET-REPOSITIONED-ROW(brwradnr,"ALWAYS"). 
   dynqueh:REPOSITION-TO-ROWID(brwrow) NO-ERROR.
   dynbrwh:REFRESHABLE = TRUE.    
   IF brwradnr = ? THEN brwradnr = 35.
END PROCEDURE.


/*------------------------------------------------------------------------------
   Namn:       repodynEJV_UI                  
   Syfte:      Sätter browserns fokus till angiven rad. OCH EJ VALUE-CHANGED
   In-data:    brwrow [ROWID] - Måste vara ett ROWID som finns i queryn.
               brwhandle [HD] - En handle till den aktuella browsern.  
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE repodynejv_UI :
   DEFINE INPUT PARAMETER brwrow AS ROWID NO-UNDO.
   IF brwradnr <= 0 THEN brwradnr = 35.
   ELSE IF brwradnr = ? THEN brwradnr = 35.
   dynbrwh:REFRESHABLE = FALSE.
   dynbrwh:SET-REPOSITIONED-ROW(brwradnr,"ALWAYS"). 
   dynqueh:REPOSITION-TO-ROWID(brwrow) NO-ERROR.
   dynbrwh:REFRESHABLE = TRUE.    
END PROCEDURE.


/*------------------------------------------------------------------------------
   Namn:       selectbyrowid_UI                  
   Syfte:      Markerar den rad med passande ROWID 
   In-data:    browrow [ROWID] - ROWID för raden som ska markeras.
   Ut-data:    -
   Kommentar:  Måste känna till "dynbrwh" för att proceduren ska fungera.
------------------------------------------------------------------------------*/
PROCEDURE selectbyrowid_UI :
   DEFINE INPUT PARAMETER selid AS ROWID NO-UNDO.
   lastbrwrow = selid.
   RUN lastselectdyn_UI.
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       selectdyn_UI                  
   Syfte:      Markerar och avmarkerar rader i browsern. Avmarkerar rader som har
               markerats vid multiselect-knapptryckning.
   In-data:    browrow [ROWID] - ROWID för raden som ska markeras.
   Ut-data:    -
   Kommentar:  Måste känna till "dynbrwh" för att proceduren ska fungera.
------------------------------------------------------------------------------*/
PROCEDURE selectdyn_UI :
   DEFINE INPUT PARAMETER browrow AS ROWID NO-UNDO.
   DEFINE VARIABLE rowsel AS LOGICAL NO-UNDO.
   RUN repodyn_UI (INPUT browrow).
   rowsel = dynbrwh:FOCUSED-ROW-SELECTED.
   IF rowsel = FALSE AND multi = FALSE THEN DO: 
      saverowid = browrow.
      dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
   END.
   ELSE IF rowsel = TRUE AND multi = FALSE THEN DO:
      saverowid = ?.  
      dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
   END.
   ELSE IF multi = TRUE THEN DO:
      IF saverowid NE ? THEN DO:
         RUN repodyn_UI (INPUT saverowid).
         dynbrwh:DESELECT-FOCUSED-ROW() NO-ERROR.
      END.
      RUN repodyn_UI (INPUT browrow).
      dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
      IF rowsel = FALSE THEN DO:
         saverowid = browrow.
      END.
   END.
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       sokurvaldyn_UI                
   Syfte:      Söker efter en cell i en kolumn som matchar angivet värde.  
   In-data:    colnamn [CHAR] - Kolumnens namn, ex. "AONR", "HDATUM", "OMRADE" m.m.
               sok [CHAR] - Den text man ska söka efter i den angivna kolumnen. 
   Ut-data:    -
   Kommentar:  Hittar procedurer en matchningn markeras denna rad, annars händer
               ingenting.
------------------------------------------------------------------------------*/
PROCEDURE sokurvaldyn_UI :
   DEFINE INPUT PARAMETER colnamn AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER sok AS CHARACTER NO-UNDO.
   temph = dyncurcolh.
   IF NUM-ENTRIES(sok,"*") = 1 THEN DO:
      sok = "*" + sok + "*".
   END.
   sokord = sok.          
   IF VALID-HANDLE(dynbrwh) = FALSE THEN DO: 
      MESSAGE "Det finns ingen Urvalstabell att söka i!" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   IF sok = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   /* egen procedure kanske?? */
   ASSIGN
   dynbuffh = dynqueh:GET-BUFFER-HANDLE(1) 
   dynfalth = ?      
   dyncurcolh = dynbrwh:FIRST-COLUMN.
   REPEAT:
      IF dyncurcolh:NAME = colnamn THEN DO:
         dynfalth = dynbuffh:BUFFER-FIELD(dyncurcolh:NAME).
      END.
      IF dynfalth NE ? THEN LEAVE.
      dyncurcolh = dyncurcolh:NEXT-COLUMN.
      IF NOT VALID-HANDLE(dyncurcolh) THEN LEAVE.
   END.
   dyncurcolh = temph.
   IF dynfalth = ? THEN DO: 
      tempcounter = 1.
      DO WHILE tempcounter LE dynbuffh:NUM-FIELDS:
         temph = dynbuffh:BUFFER-FIELD(tempcounter).
         IF temph:NAME = colnamn THEN DO:
            dynfalth = temph.
            LEAVE.
         END.
         tempcounter = tempcounter + 1.
      END.
      IF dynfalth = ? THEN DO:
         MESSAGE "Felaktigt kolumn namn: " colnamn dynfalth VIEW-AS ALERT-BOX.
         RETURN.
      END.      
   END.
   IF dynqueh:IS-OPEN = FALSE THEN RETURN.
   REPEAT:
      dynqueh:GET-NEXT(NO-LOCK).
      IF dynqueh:QUERY-OFF-END THEN LEAVE.
      RUN comparedyn_UI (INPUT 2, OUTPUT retvalue).
      IF retvalue = TRUE THEN RETURN.
   END.
   dynqueh:GET-FIRST(NO-LOCK).
   IF dynqueh:QUERY-OFF-END THEN LEAVE.
   REPEAT:
      RUN comparedyn_UI (INPUT 2, OUTPUT retvalue).
      IF retvalue = TRUE THEN RETURN.
      dynqueh:GET-NEXT(NO-LOCK).
      IF dynqueh:QUERY-OFF-END THEN LEAVE.
   END.
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       lastselectdyn_UI                
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE lastselectdyn_UI :
   IF lastbrwrow NE ? THEN DO:
      RUN repodyn_UI (INPUT lastbrwrow).
      dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
/*       dynqueh:GET-CURRENT(NO-LOCK) NO-ERROR. */
   END.
END PROCEDURE.
PROCEDURE lastselectdynnv_UI :
   IF lastbrwrow NE ? THEN DO:
      RUN repodynnv_UI (INPUT lastbrwrow).
      dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
/*       dynqueh:GET-CURRENT(NO-LOCK) NO-ERROR. */
   END.
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       lastselectdyn_UI                
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE lastselectdynejv_UI :
   IF lastbrwrow NE ? THEN DO:
      RUN repodynejv_UI (INPUT lastbrwrow).
      dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
/*       dynqueh:GET-CURRENT(NO-LOCK) NO-ERROR. */
   END.
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       mouseselclick_UI                
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE mouseselclick_UI :
   DEFINE INPUT PARAMETER mclick AS LOGICAL NO-UNDO.
   mselclick = mclick.
END PROCEDURE.



/*------------------------------------------------------------------------------
   Namn:       setlastrowid_UI                
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE setlastrowid_UI :
   DEFINE INPUT PARAMETER inrow AS ROWID.
   lastbrwrow = inrow.
END PROCEDURE.


/*------------------------------------------------------------------------------
   Namn:       setdefaultcol_UI                
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE setdefaultcol_UI :
   DEFINE INPUT PARAMETER incol AS INTEGER.
   IF incol <= dynbrwh:NUM-COLUMNS AND incol > 0 THEN defaultcol = incol.   
   ELSE MESSAGE "setdefaultcol_UI - Ange ett värde mellan 1 och " dynbrwh:NUM-COLUMNS
      VIEW-AS ALERT-BOX TITLE "Elpool Error".
END PROCEDURE.


/*------------------------------------------------------------------------------
   Namn:       setdefaultcolbyname_UI                
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE setdefaultcolbyname_UI :
   DEFINE INPUT PARAMETER colname AS CHARACTER.
   DEFINE VARIABLE tempint AS INTEGER.
   
   ASSIGN
   colfinns = FALSE
   tempint = 1
   dyncurcolh = dynbrwh:FIRST-COLUMN.
   DO WHILE VALID-HANDLE(dyncurcolh):
      IF dyncurcolh:NAME = colname THEN DO:
         colfinns = TRUE. 
         defaultcol = tempint.
      END.
      IF colfinns = TRUE THEN LEAVE.
      dyncurcolh = dyncurcolh:NEXT-COLUMN.
      tempint = tempint + 1.
   END.
   IF colfinns = FALSE THEN DO: 
      MESSAGE "Felaktigt kolumn namn: " colname VIEW-AS ALERT-BOX.
      RETURN.
   END.
   /*
   dyncurcolh = dynbrwh:FIRST-COLUMN.
   */
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       setcolsortvar_UI                
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE setcolsortvar_UI :
   DEFINE INPUT PARAMETER sortvar AS CHARACTER NO-UNDO.
   IF INDEX(sortvar,"WHERE") = 0 THEN colsortcharinput = " WHERE " + sortvar.
   ELSE colsortcharinput = sortvar.   
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       settriggerproc_UI               
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE settriggerproc_UI :
   DEFINE INPUT PARAMETER val AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER procname AS CHARACTER NO-UNDO.
   IF val = 1 THEN DO:
      ON START-SEARCH OF dynbrwh PERSISTENT RUN VALUE(procname) IN SOURCE-PROCEDURE.
   END.
   ELSE IF val = 2 THEN DO:
      ON ANY-PRINTABLE OF dynbrwh PERSISTENT RUN VALUE(procname) IN SOURCE-PROCEDURE.
   END.   
   ELSE IF val = 3 THEN DO:
      ON ROW-DISPLAY OF dynbrwh PERSISTENT RUN VALUE(procname) IN SOURCE-PROCEDURE.
   END.
   ELSE IF val = 4 THEN DO:
      ON ROW-DISPLAY OF dynbrwh PERSISTENT RUN VALUE(procname) IN SOURCE-PROCEDURE.
   END.  
    
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       setcolindex_UI                
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE setcolindex_UI :
   DEFINE INPUT PARAMETER indname AS CHARACTER NO-UNDO.
   IF indname = "" THEN indname = ?.
   colindex = indname.
END PROCEDURE.

PROCEDURE resetcolindex_UI :
   colindex = ?.
END PROCEDURE.

PROCEDURE setcolindexkoll_UI :
   DEFINE OUTPUT PARAMETER indname AS CHARACTER NO-UNDO.
   indname = colindex.   
   IF indname = ? THEN DO:
      IF VALID-HANDLE(dynlastcolh) THEN indname = dynlastcolh:NAME. 
   END.
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       setpdatatabvar_UI                
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE setpdatatabvar_UI :
   DEFINE INPUT PARAMETER colname AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER prepstring AS CHARACTER NO-UNDO.
   tempcounter = 1.
   DO WHILE tempcounter LE dynbrwh:NUM-COLUMNS:
      tempcolh = dynbrwh:GET-BROWSE-COLUMN(tempcounter).      
      IF tempcolh:NAME = colname THEN DO:
         IF tempcolh:PRIVATE-DATA = ? THEN DO:
            tempcolh:PRIVATE-DATA = " WHERE " + prepstring + "$?".
         END.         
         ELSE DO:
            tempchar = tempcolh:PRIVATE-DATA.
            tempcolh:PRIVATE-DATA = " WHERE " + prepstring + "$" + ENTRY(2, tempchar, "$").
         END.
      END.
      tempcounter = tempcounter + 1.
   END.
END PROCEDURE.


/*------------------------------------------------------------------------------
   Namn:       setpdatatacolsort_UI                
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE setpdatacolsort_UI :
   DEFINE INPUT PARAMETER colname AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER prepstring AS CHARACTER NO-UNDO.
   tempcounter = 1.
   DO WHILE tempcounter LE dynbrwh:NUM-COLUMNS:
      tempcolh = dynbrwh:GET-BROWSE-COLUMN(tempcounter).      
      IF tempcolh:NAME = colname THEN DO:
         IF tempcolh:PRIVATE-DATA = ? THEN DO:
            tempcolh:PRIVATE-DATA = "?$" + prepstring.
         END.         
         ELSE DO:
            tempchar = tempcolh:PRIVATE-DATA.
            tempcolh:PRIVATE-DATA = ENTRY(1, tempchar, "$") + "$" + prepstring.
         END.
      END.
      tempcounter = tempcounter + 1.      
   END.
END PROCEDURE.


/*------------------------------------------------------------------------------
   Namn:       openbdynspec_UI                
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE openbdynspec_UI :
   RUN openbdyn_UI (INPUT colsortcharinput).
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       fetchselrowid_UI                
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE fetchselrowid_UI :
   IF dynbrwh:NUM-SELECTED-ROWS = 0 THEN DO:
      lastbrwrow = ?.
   END.
   ELSE DO: 
      dynbrwh:FETCH-SELECTED-ROW(1).
      dynbuffh = dynqueh:GET-BUFFER-HANDLE(1).
      lastbrwrow = dynbuffh:ROWID.
   END.
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       tooltip_UI                
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE tooltip_UI :
   DEFINE INPUT PARAMETER vartip AS CHARACTER NO-UNDO.
   dynbrwh:TOOLTIP = vartip.
END PROCEDURE.


/*------------------------------------------------------------------------------
   Namn:       title_UI                
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE notitle_UI :
   DEFINE INPUT  PARAMETER nott AS LOGICAL NO-UNDO.
   notitlelog = nott.
   /*
   dynbrwh:TITLE:HIDDEN = notitlelog.
   */
END PROCEDURE.
PROCEDURE title_UI :
   IF notitlelog = TRUE THEN RETURN.
   DEFINE VARIABLE antanurval AS INTEGER NO-UNDO.
   IF dynqueh:IS-OPEN = FALSE THEN RETURN.
   RUN getnumresult_UI (OUTPUT antanurval).
   IF dynbrwh:TITLE NE ? THEN dynbrwh:TITLE = orgtitle + " Antal " + STRING(antanurval).
   dynbrwh:MAX-DATA-GUESS = antanurval.
   dynqueh:GET-FIRST(NO-LOCK).
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       maxdata_UI                
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE maxdata_UI :
   DEFINE VARIABLE antanurval AS INTEGER NO-UNDO.
   RUN getnumresult_UI (OUTPUT antanurval).
   dynbrwh:MAX-DATA-GUESS = antanurval.
   dynqueh:GET-FIRST(NO-LOCK).
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       setorgtitle_UI                
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE setorgtitle_UI :
   DEFINE INPUT PARAMETER titlevar AS CHARACTER NO-UNDO.
   orgtitle = titlevar.   
   IF dynbrwh:TITLE NE ? THEN dynbrwh:TITLE = orgtitle.      
END PROCEDURE.


/*------------------------------------------------------------------------------
   Namn:       title_UI                
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE settitlenum_UI :
   DEFINE INPUT PARAMETER templog AS LOGICAL NO-UNDO.
   titlelog = templog.
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       setmultvar_UI                
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE setmultvar_UI :
   DEFINE INPUT PARAMETER multi AS LOGICAL NO-UNDO.
   multvar = multi.
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       ejtmultibr_UI                
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE ejtmultibr_UI :
   DEFINE INPUT PARAMETER brwrow AS ROWID NO-UNDO.
   lastbrwrow = brwrow.
   dynbrwh:DESELECT-ROWS().      
   RUN lastselectdyn_UI. 
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       sethidden_UI
   Syfte:      Möjlighet att sätta browserns HIDDEN attribut  
   In-data:    TRUE - Gömmer browsern
               FALSE - Visar browsern
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE sethidden_UI :
   DEFINE INPUT PARAMETER hiddlog AS LOGICAL NO-UNDO.
   dynbrwh:HIDDEN = hiddlog.
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       selfocusrow_UI
   Syfte:      Möjlighet att marker den rad som har fokus  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE selfocusrow_UI :
   dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       getnumresult_UI                
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE getnumresult_UI :
   DEFINE OUTPUT PARAMETER numresult AS INTEGER NO-UNDO.
   dynqueh:GET-LAST(NO-LOCK) NO-ERROR.
   numresult = dynqueh:NUM-RESULTS NO-ERROR.
   IF numresult = ? THEN numresult = 0.
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       getcolname_UI
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE getcolname_UI :
   DEFINE OUTPUT PARAMETER colnamevar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER ascdesclog AS LOGICAL NO-UNDO. /* FALSE = Ascending, TRUE = Descending */
   IF VALID-HANDLE(dyncurcolh) THEN DO:
      ASSIGN
      colnamevar = dyncurcolh:NAME
      ascdesclog = descvarcol.
   END.
END PROCEDURE.    


/*------------------------------------------------------------------------------
   Namn:       selnextprevrow_UI                
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE selnextprevrow_UI :
/*    MESSAGE STRING(dynbuffh:ROWID) dynqueh:CURRENT-RESULT-ROW. */
    /*
   lastbrwrow = ?.
   dynqueh:GET-NEXT(NO-LOCK).
   IF dynqueh:QUERY-OFF-END THEN DO:
      dynqueh:GET-PREV(NO-LOCK). 
      dynqueh:GET-PREV(NO-LOCK).
      IF dynqueh:QUERY-OFF-END THEN lastbrwrow = ?.
      ELSE lastbrwrow = dynbuffh:ROWID.       
      dynqueh:GET-NEXT(NO-LOCK).
   END.
   ELSE DO: 
      lastbrwrow = dynbuffh:ROWID.
      IF lastbrwrow = ? THEN DO:
         dynqueh:GET-LAST(NO-LOCK).
         lastbrwrow = dynbuffh:ROWID.
      END.
      ELSE dynqueh:GET-PREV(NO-LOCK). 
   END.   
   IF lastbrwrow = ? THEN DO:
      RUN openbdynspec_UI.
      dynqueh:GET-LAST(NO-LOCK).
      lastbrwrow = dynbuffh:ROWID.
   END.
   */
   
   
   lastbrwrow = ?.
   dynqueh:GET-NEXT(NO-LOCK).
   REPEAT:
      IF dynbuffh:AVAILABLE = TRUE THEN LEAVE.
      IF dynqueh:QUERY-OFF-END THEN DO:
        /* upp = TRUE.*/
         LEAVE.
      END.
      dynqueh:GET-NEXT(NO-LOCK). 
   END.
   IF dynbuffh:AVAILABLE = FALSE THEN DO:         
      REPEAT:
         IF dynbuffh:AVAILABLE = TRUE THEN LEAVE.
         IF dynqueh:QUERY-OFF-END THEN DO:
            LEAVE.
         END.
         dynqueh:GET-PREV(NO-LOCK). 
      END.
   END.
   IF dynbuffh:AVAILABLE = TRUE THEN DO:         
      lastbrwrow = dynbuffh:ROWID.
      dynqueh:GET-PREV(NO-LOCK). 
   END.
   IF lastbrwrow = ? THEN DO:
      RUN openbdynspec_UI.
      dynqueh:GET-LAST(NO-LOCK).
      lastbrwrow = dynbuffh:ROWID.
   END.
   
END PROCEDURE.


/*------------------------------------------------------------------------------
   Namn:       setdescvarcol_UI
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE setdescvarcol_UI :
   DEFINE INPUT PARAMETER ascdesclog AS LOGICAL NO-UNDO. /* FALSE = Ascending, TRUE = Descending */
   IF VALID-HANDLE(dyncurcolh) THEN DO:
       descvarcol = ascdesclog.
   END.
END PROCEDURE.    



/*------------------------------------------------------------------------------
   Namn:       refreshbrw_UI                
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE refreshbrw_UI :
   IF dynqueh:PREPARE-STRING = ? THEN DO:
      RUN openbdyn_UI (INPUT "").
   END.
   ELSE DO:
      kommandosortquery = dynqueh:PREPARE-STRING.
      dynqueh:QUERY-PREPARE(kommandosortquery).
      dynqueh:QUERY-OPEN().
      dynbuffh = dynqueh:GET-BUFFER-HANDLE(1).
   END.
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       brwsetupstop_UI                
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE brwsetupstop_UI :
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   IF vad = 1 THEN dynbrwh:ALLOW-COLUMN-SEARCHING = FALSE.
   IF vad = 2 THEN dynbrwh:COLUMN-RESIZABLE       = FALSE.
   IF vad = 3 THEN DO:
      ASSIGN
      dynbrwh:ALLOW-COLUMN-SEARCHING = FALSE
      dynbrwh:COLUMN-RESIZABLE       = FALSE. 
   END.
END PROCEDURE.


/*------------------------------------------------------------------------------
   Namn:       brwsetup_UI                
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE brwsetup_UI :
   ASSIGN
   
   /*dynbrwh:NO-EMPTY-SPACE = TRUE*/

   dynbrwh:ALLOW-COLUMN-SEARCHING = TRUE
   dynbrwh:COLUMN-RESIZABLE       = TRUE
   dyncurcolh = dynbrwh:FIRST-COLUMN.
   IF dyncurcolh = ? THEN RETURN.
   REPEAT:
      lastcolh = dyncurcolh.
      IF dyncurcolh:NAME = "KALKNR" THEN DO:
         ASSIGN
         dyncurcolh:COLUMN-FGCOLOR = 1
         dyncurcolh:LABEL-FGCOLOR = 1.
      END.
      ELSE IF dyncurcolh:NAME = "PLANNR" THEN DO:
         ASSIGN
         dyncurcolh:LABEL = Guru.Konstanter:gplk
         dyncurcolh:COLUMN-FGCOLOR = 4
         dyncurcolh:LABEL-FGCOLOR = 4.         
      END.
      ELSE IF dyncurcolh:NAME = "ARTAL" THEN DO:     
         ASSIGN
         dyncurcolh:LABEL = "Årtal"
         dyncurcolh:COLUMN-FGCOLOR = 4
         dyncurcolh:LABEL-FGCOLOR = 4.      
      END.
      ELSE IF dyncurcolh:NAME = "BERNR" THEN DO:
         ASSIGN
         dyncurcolh:COLUMN-FGCOLOR = 5
         dyncurcolh:LABEL-FGCOLOR = 5.
      END.
      ELSE IF dyncurcolh:NAME = "DELNR" OR dyncurcolh:NAME = "AONRDELNR"THEN DO:
         ASSIGN
         dyncurcolh:LABEL = Guru.Konstanter:gdelnrk
         dyncurcolh:COLUMN-FGCOLOR = 2
         dyncurcolh:LABEL-FGCOLOR = 2
         delnrcolh = dyncurcolh.
         dyncurcolh:WIDTH-CHARS =  LENGTH(Guru.Konstanter:varforetypchar[1]).
         IF dyncurcolh:WIDTH-CHARS < LENGTH(Guru.Konstanter:gdelnrk) THEN dyncurcolh:WIDTH-CHARS = LENGTH(Guru.Konstanter:gdelnrk) + 1.
      
      END.
      ELSE IF dyncurcolh:NAME = "LOPNR" THEN DO:
         ASSIGN
         
         kalklopnrh = dyncurcolh.
/*          dyncurcolh:FORMAT = Guru.Konstanter:varforetypchar[1]. */
      END.
       
      ELSE IF dyncurcolh:NAME = "AONR" OR dyncurcolh:NAME = "AONRAONR" THEN DO:
         ASSIGN
         dyncurcolh:LABEL = Guru.Konstanter:gaok
         dyncurcolh:COLUMN-FGCOLOR = 2
         dyncurcolh:LABEL-FGCOLOR = 2.
         IF Guru.Konstanter:varforetypval[8] >= 7 THEN DO:
            dyncurcolh:WIDTH-CHARS = Guru.Konstanter:varforetypval[8].
            aocolh = dyncurcolh.
         END.
         ELSE DO:
            IF dyncurcolh:WIDTH-CHARS <= 6 OR dyncurcolh:WIDTH-CHARS = ? THEN dyncurcolh:WIDTH-CHARS = 6.
            /*dyncurcolh:WIDTH-CHARS = 6.*/
            aocolh = dyncurcolh.
         END.
        
         
      END.
      ELSE IF dyncurcolh:NAME = "ORT" THEN DO:
         ASSIGN
         dyncurcolh:LABEL = Guru.Konstanter:gaonamnk.                
      END.
      ELSE IF dyncurcolh:NAME = "ENR" THEN DO:
         dynenrh = dyncurcolh.
         ASSIGN
         dyncurcolh:LABEL = Guru.Konstanter:genk.         
      END.
      /*brwcolh ANVÄNDS FÖR ATT SÄTT FORMAT SENARE*/
      ELSE IF dyncurcolh:NAME = "LEVKOD" OR dyncurcolh:NAME = "LEVKODINK" THEN DO:
         IF dyncurcolh:FORMAT NE "X(256)" THEN brwcolh[1] = dyncurcolh.                  
      END.
      ELSE IF dyncurcolh:NAME = "BENAMNING" THEN DO:
         IF dyncurcolh:FORMAT NE "X(256)" THEN brwcolh[1] = dyncurcolh.                  
      END.
      ELSE IF dyncurcolh:NAME = "FORNAMN" THEN DO:
         IF dyncurcolh:FORMAT NE "X(256)" THEN brwcolh[2] = dyncurcolh.
      END.
      ELSE IF dyncurcolh:NAME = "EFTERNAMN" THEN DO:
         IF dyncurcolh:FORMAT NE "X(256)" THEN brwcolh[3] = dyncurcolh.
      END.
     
      ELSE IF dyncurcolh:NAME = "PERSONALKOD" THEN DO:
         IF dyncurcolh:FORMAT NE "X(256)" THEN brwcolh[5] = dyncurcolh.
      END.
      ELSE IF dyncurcolh:NAME = "OMRADE" THEN DO:
         dyncurcolh:LABEL = Guru.Konstanter:gomrk.
         brwcolh[4] = dyncurcolh.
         IF Guru.Konstanter:globforetag = "FORS" THEN DO:
            dyncurcolh:WIDTH-CHARS = 5.
         END.    
      END.
      ELSE IF dyncurcolh:NAME = "AVDELNINGNR" THEN DO:
         dyncurcolh:LABEL= Guru.Konstanter:gavdk.
      END.
      dyncurcolh = dyncurcolh:NEXT-COLUMN.
      IF NOT VALID-HANDLE(dyncurcolh) THEN LEAVE.
   END.
   dyncurcolh = dynbrwh:FIRST-COLUMN.
   COLOR-TABLE:NUM-ENTRIES = 50.
   colvalnumber = 20.
   disbrwcol = 21.
   DEFINE VARIABLE hjv AS LOGICAL NO-UNDO.
   IF dynbrwh:HIDDEN = FALSE THEN DO:
      hjv = TRUE.
      dynbrwh:HIDDEN = TRUE.
   END.
   COLOR-TABLE:SET-DYNAMIC(colvalnumber, YES).
   COLOR-TABLE:SET-RGB-VALUE(colvalnumber, RGB-VALUE(242,240,238)).
   COLOR-TABLE:SET-DYNAMIC(disbrwcol, YES).
   COLOR-TABLE:SET-RGB-VALUE(disbrwcol, RGB-VALUE(212,208,200)).   
   IF hjv = TRUE THEN DO:
      hjv = FALSE.
      dynbrwh:HIDDEN = FALSE.
   END.
   
END PROCEDURE.


PROCEDURE fgbgcol_UI :
   DEFINE INPUT PARAMETER fgbg AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER colorvar AS INTEGER NO-UNDO.
   IF fgbg = FALSE THEN DO:
      FOR EACH coltemp:
         coltemp.CH:BGCOLOR = colorvar.                       
      END.      
   END.
   IF fgbg = TRUE THEN DO:
      FOR EACH coltemp:
         coltemp.CH:FGCOLOR = colorvar.                       
      END.      
   END.
   

END PROCEDURE.


/*tar bort markerade poster ur brw*/
PROCEDURE sel_back_UI :
   DEFINE VARIABLE antvalda AS INTEGER NO-UNDO.
   DEFINE VARIABLE  antraknare AS INTEGER NO-UNDO.
   RUN getrownr_UI. 
   ASSIGN               
   lastbrwrow = ?
   /*
   dynbrwh = rightqueh:GET-BUFFER-HANDLE.
   */
   antvalda = dynbrwh:NUM-SELECTED-ROWS. 
   IF antvalda = 0 THEN DO:      
      MESSAGE "Inget är markerat" VIEW-AS ALERT-BOX
         TITLE "Meddelande".
      RETURN.                
   END.
   ELSE DO: 
      antraknare = 1.
      DO WHILE antraknare LE antvalda:
         dynbrwh:FETCH-SELECTED-ROW(antraknare).
         IF dynbuffh:AVAILABLE THEN DO:        
            dynbuffh:BUFFER-DELETE() NO-ERROR.            
            IF antraknare = antvalda THEN DO:
               RUN selnextprevrow_UI.
            END.
            
         END.
         antraknare = antraknare + 1.
      END.
      
      RUN openbdynspec_UI.
      IF lastbrwrow NE ? THEN DO:
         RUN repodyn_UI (INPUT lastbrwrow).
         dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.                  
      END.
      ELSE DO:
         dynqueh:GET-LAST(NO-LOCK).
         IF dynbuffh:AVAILABLE THEN DO:
            RUN repodyn_UI (INPUT dynbuffh:ROWID).
            dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.         
         END.           
      END.
     
   END.
   IF titlelog = TRUE THEN DO:
      dynqueh:GET-LAST(NO-LOCK).
      dynbrwh:TITLE = orgtitle + " Antal " + STRING(dynqueh:NUM-RESULTS).
   END.
   
END PROCEDURE.



/*------------------------------------------------------------------------------
   Namn:       sparavaldbrw_UI                
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE sparabrw_UI :
   DEFINE INPUT PARAMETER TABLE-HANDLE temphandle. 
   DEFINE INPUT PARAMETER TABLE FOR sparaladdatemp.
   
   FIND FIRST sparaladdatemp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE sparaladdatemp THEN RETURN.
   sparaladdatemp.SLVAL = 1.
   IF Guru.Konstanter:appcon THEN DO:
      RUN SLVALDBRW.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE temphandle, INPUT TABLE sparaladdatemp).
   END.
   ELSE DO:
      RUN SLVALDBRW.P 
         (INPUT-OUTPUT TABLE-HANDLE temphandle, INPUT TABLE sparaladdatemp).
   END.      
   DELETE OBJECT temphandle.
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       laddavaldbrw_UI                
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE laddabrw_UI :
   DEFINE INPUT PARAMETER TABLE-HANDLE temphandle. 
   DEFINE INPUT PARAMETER TABLE FOR sparaladdatemp.
   DEFINE VARIABLE ttbuffh AS HANDLE NO-UNDO.
   FIND FIRST sparaladdatemp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE sparaladdatemp THEN RETURN.
   sparaladdatemp.SLVAL = 2.
    
   IF Guru.Konstanter:appcon THEN DO:
      RUN SLVALDBRW.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE temphandle, INPUT TABLE sparaladdatemp).
   END.
   ELSE DO:
      RUN SLVALDBRW.P 
         (INPUT-OUTPUT TABLE-HANDLE temphandle, INPUT TABLE sparaladdatemp).
   END.    
   ttbuffh = temphandle:DEFAULT-BUFFER-HANDLE.
   kommandosortquery = "FOR EACH " + ttbuffh:TABLE + " NO-LOCK.".
   CREATE QUERY ttqh.
   ttqh:SET-BUFFERS(ttbuffh).
   ttqh:QUERY-PREPARE(kommandosortquery).
   ttqh:QUERY-OPEN().
   ttqh:GET-FIRST(NO-LOCK).
   DO WHILE ttqh:QUERY-OFF-END = FALSE:
      dynbuffh:BUFFER-CREATE().
      dynbuffh:BUFFER-COPY(ttbuffh).
      ttqh:GET-NEXT(NO-LOCK).
   END.
   ttqh:QUERY-CLOSE.
   DELETE OBJECT ttqh NO-ERROR.
   DELETE OBJECT temphandle NO-ERROR.
   RUN openbdyn_UI (INPUT "").
END PROCEDURE.
PROCEDURE brwjoin_UI :
   DEFINE INPUT  PARAMETER injoin AS CHARACTER NO-UNDO.
   varjoin = injoin.
END PROCEDURE.
PROCEDURE laddabrw2_UI :
   /*med epost utan att lagra i databasen förutom ANVAOTAB DVS UTAN JOIN*/
   DEFINE INPUT PARAMETER TABLE-HANDLE temphandle. 
   DEFINE INPUT PARAMETER TABLE FOR sparaladdatemp.
   DEFINE VARIABLE ttbuffh AS HANDLE NO-UNDO.
   FIND FIRST sparaladdatemp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE sparaladdatemp THEN RETURN.
   sparaladdatemp.SLVAL = 3.
   IF Guru.Konstanter:appcon THEN DO:
      RUN SLVALDBRW.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE temphandle, INPUT TABLE sparaladdatemp).
   END.
   ELSE DO:
      RUN SLVALDBRW.P 
         (INPUT-OUTPUT TABLE-HANDLE temphandle, INPUT TABLE sparaladdatemp).
   END.    
   
   ttbuffh = temphandle:DEFAULT-BUFFER-HANDLE.
   kommandosortquery = "FOR EACH " + ttbuffh:TABLE + " NO-LOCK.".
   CREATE QUERY ttqh.
   ttqh:SET-BUFFERS(ttbuffh).
   ttqh:QUERY-PREPARE(kommandosortquery).
   ttqh:QUERY-OPEN().
   ttqh:GET-FIRST(NO-LOCK).
   DO WHILE ttqh:QUERY-OFF-END = FALSE:
      dynbuffh:BUFFER-CREATE().
      dynbuffh:BUFFER-COPY(ttbuffh).
      ttqh:GET-NEXT(NO-LOCK).
   END.
   ttqh:QUERY-CLOSE.
   DELETE OBJECT ttqh.
   DELETE OBJECT temphandle.
   RUN openbdyn_UI (INPUT "").
END PROCEDURE.
   

/******************************** Tillägg 2004-03-19 ********************************/

/*------------------------------------------------------------------------------
   Namn:       multiselect_UI                
   Syfte:      Att markera de poster som finns i multiseltemp tabellen.
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE multitemp_UI :
   DEFINE INPUT PARAMETER TABLE FOR multiseltemp.
END PROCEDURE.
PROCEDURE multiselect_UI :
   dynbrwh:REFRESHABLE = FALSE.  /* Ingen uppdatering av browser */
   FOR EACH multiseltemp BY NUMSEL DESCENDING:
      dynok = dynqueh:REPOSITION-TO-ROWID(multiseltemp.SELROW) NO-ERROR.  /* Positionerar queryn */
      IF dynok = TRUE THEN dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.    /* Markerar postitionerad post */
   END.
   dynbrwh:REFRESHABLE = TRUE.   /* Först nu uppdateras förändringarna */
   RUN fetchselrowid_UI.
   RUN lastselectdyn_UI.
END PROCEDURE.

PROCEDURE multiselectin_UI :
   DEFINE INPUT  PARAMETER inrw AS ROWID NO-UNDO.
   dynbrwh:REFRESHABLE = FALSE.  /* Ingen uppdatering av browser */
   dynok = dynqueh:REPOSITION-TO-ROWID(inrw) NO-ERROR.  /* Positionerar queryn */
   IF dynok = TRUE THEN dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.    /* Markerar postitionerad post */   
   dynbrwh:REFRESHABLE = TRUE.   /* Först nu uppdateras förändringarna */
   
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       selmulti_UI                
   Syfte:      Skapa en ny post för varje markerad rad i en browser.
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE selmulti_UI :
   EMPTY TEMP-TABLE multiseltemp NO-ERROR.   /* Tömmer temp-tabell */
   tempcounter = 1.
   DO WHILE tempcounter LE dynbrwh:NUM-SELECTED-ROWS: /* För varje markerad rad */
      dynbrwh:FETCH-SELECTED-ROW(tempcounter).  /* 'Fånga' raden utifrån ett nummer */
      CREATE multiseltemp.
      ASSIGN
      multiseltemp.NUMSEL = tempcounter   /* Sparar undan markeringsordningen */
      multiseltemp.SELROW = dynbuffh:ROWID.  /* Sparar Rowid för posten */
      tempcounter = tempcounter + 1.
   END.
END PROCEDURE.


/*------------------------------------------------------------------------------
   Namn:       setselectvalue_UI                
   Syfte:      Ändrar en kolumns värde, alla markerade rader påverkas.
   In-data:    colname  - namnet på kolumnen, kontroll görs.
               newvalue - det nya värdet som ska sättas.
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE setselectvalue_UI :
   DEFINE INPUT PARAMETER colname AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER newvalue AS CHARACTER NO-UNDO.
   
   IF dynbrwh:NUM-SELECTED-ROWS = 0 THEN RETURN.
   EMPTY TEMP-TABLE multiseltemp NO-ERROR.         /* Tömmer temp-tabellen */
   ASSIGN
   tempcounter = 1
   tempfieldh = ?
   dynbuffh = dynqueh:GET-BUFFER-HANDLE(1)
   tempcolh = dynbrwh:GET-BROWSE-COLUMN(1).   
   /* Loopar igenom temp-tabellen för att kolla att kolumnen finns*/
   DO WHILE VALID-HANDLE(tempcolh): 
      IF tempcolh:NAME = colname THEN DO:
         tempfieldh = tempcolh:BUFFER-FIELD(tempcolh:NAME).
         LEAVE.
      END.
      tempcolh = tempcolh:NEXT-COLUMN.
   END.
   IF tempfieldh NE ? THEN DO:
      RUN selmulti_UI.  /* Skapar en multiseltemp post för varje markerad rad */
      FOR EACH multiseltemp:  /* Loopar igenom alla markerade */
         dynbuffh:FIND-BY-ROWID(multiseltemp.SELROW) NO-ERROR. /* Sätter buffer-"pekare" till rätt post*/
         /* Kontroll av datatyp, lagrar sedan de nya värdet i de markerade posternas valda kolumn */
         IF tempfieldh:DATA-TYPE = "INTEGER" THEN tempfieldh:BUFFER-VALUE = INTEGER(newvalue).
         ELSE IF tempfieldh:DATA-TYPE = "DATE" THEN tempfieldh:BUFFER-VALUE = DATE(newvalue).
         ELSE tempfieldh:BUFFER-VALUE = newvalue.
      END.
   END.
   RUN refreshbrw_UI.   /* Öppnar om queryn */
   RUN multiselect_UI.  /* Markerar alla poster på nytt */
END PROCEDURE.


/*------------------------------------------------------------------------------
   Namn:       setmenucopy_UI                
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE setmenucopy_UI :
   
   IF dynbrwh:POPUP-MENU = ? THEN DO:
      CREATE MENU temph.
      temph:POPUP-ONLY = TRUE.
      dynbrwh:POPUP-MENU = temph.
   END.             
   ELSE DO:
      CREATE MENU-ITEM temph.
      temph:SUBTYPE = "RULE".
      temph:PARENT = dynbrwh:POPUP-MENU.
   END.
   CREATE MENU-ITEM temph.
   temph:LABEL = "Kopiera markerad text".
   temph:PARENT = dynbrwh:POPUP-MENU.
   CREATE MENU-ITEM tempexh.
   IF multvar = TRUE THEN tempexh:LABEL = "De markerade till Excel".
   ELSE  tempexh:LABEL = "Alla poster till Excel".
   tempexh:PARENT = dynbrwh:POPUP-MENU.
   ON CHOOSE OF temph PERSISTENT RUN ctrlcproc IN THIS-PROCEDURE.
   ON CHOOSE OF tempexh PERSISTENT RUN excelproc IN THIS-PROCEDURE.
   IF dynenrh NE ? THEN DO: 
      CREATE MENU-ITEM tempenrh.
      tempenrh:LABEL = "Kopiera " + Guru.Konstanter:genk.
      tempenrh:PARENT = dynbrwh:POPUP-MENU.
      ON CHOOSE OF tempenrh PERSISTENT RUN enrproc IN THIS-PROCEDURE.
   END.    
END PROCEDURE.


/*------------------------------------------------------------------------------
   Namn:       createmenu_UI                
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE createmenu_UI :
   DEFINE INPUT PARAMETER brwh AS HANDLE NO-UNDO.   
   FIND FIRST menutemp WHERE menutemp.BRWH = brwh NO-LOCK NO-ERROR.
   IF NOT AVAILABLE menutemp THEN DO:
      CREATE menutemp.
      ASSIGN
      menutemp.BRWH = brwh.
      menutemp.MENUH = brwh:POPUP-MENU.
      CREATE MENU-ITEM menutemp.MENUITEMH.      
   END.      
END PROCEDURE.

PROCEDURE getmenuitem_UI :
   DEFINE INPUT PARAMETER brwh AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER labelvar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER menyih AS HANDLE NO-UNDO.
   FIND FIRST menuitemtemp WHERE menuitemtemp.BRWH = brwh AND menuitemtemp.MENY = labelvar NO-LOCK NO-ERROR.  
   IF AVAILABLE menuitemtemp THEN menyih = menuitemtemp.MENYH.   
END PROCEDURE.

PROCEDURE sensitivem_UI :
   DEFINE INPUT PARAMETER menyih AS HANDLE NO-UNDO.
   DEFINE INPUT  PARAMETER senvar AS LOGICAL NO-UNDO.
   IF menyih = ? THEN RETURN.   
   menyih:SENSITIVE = senvar NO-ERROR.
END PROCEDURE.


/*------------------------------------------------------------------------------
   Namn:       addmenuitem_UI               
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE addmenuitemDyn_UI :
  
   DEFINE INPUT PARAMETER labelvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER procname AS CHARACTER NO-UNDO.
   RUN addmenuitem_UI (INPUT brwh, INPUT labelvar, INPUT procname).
END PROCEDURE.
PROCEDURE addmenuitem_UI :
   DEFINE INPUT PARAMETER brwh AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER labelvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER procname AS CHARACTER NO-UNDO.
   DEFINE VARIABLE temphandle AS HANDLE NO-UNDO.    
   FIND FIRST menutemp WHERE menutemp.BRWH = brwh NO-LOCK NO-ERROR.
   IF NOT AVAILABLE menutemp THEN RUN createmenu_UI (INPUT brwh).
   IF AVAILABLE menutemp THEN DO:
      /*
      CREATE MENU-ITEM temphandle.
      temphandle:LABEL = labelvar.
      temphandle:PARENT = menutemp.MENUH.
      ON CHOOSE OF temphandle PERSISTENT RUN VALUE(procname) IN SOURCE-PROCEDURE.
      CREATE menuitemtemp.
      ASSIGN
      menuitemtemp.BRWH = menutemp.BRWH
      menuitemtemp.MENY = temphandle:LABEL
      menuitemtemp.MENYH = temphandle.
      */            
      
      CREATE menuitemtemp.
      ASSIGN
      menuitemtemp.BRWH = menutemp.BRWH
      menuitemtemp.MENY = labelvar.
      CREATE MENU-ITEM menuitemtemp.MENYH.
      menuitemtemp.MENYH:LABEL = labelvar.
      menuitemtemp.MENYH:PARENT = menutemp.MENUH.
      IF huvprogh = ? THEN ON CHOOSE OF menuitemtemp.MENYH PERSISTENT RUN VALUE(procname) IN SOURCE-PROCEDURE.
      ELSE IF SOURCE-PROCEDURE = huvprogh THEN ON CHOOSE OF menuitemtemp.MENYH PERSISTENT RUN VALUE(procname) IN SOURCE-PROCEDURE.
      ELSE ON CHOOSE OF menuitemtemp.MENYH PERSISTENT RUN VALUE(procname) IN huvprogh.
      
      /*ON CHOOSE OF menuitemtemp.MENYH PERSISTENT RUN VALUE(procname) IN SOURCE-PROCEDURE.
      */
   END.   
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       addmenuitem2_UI               
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/
PROCEDURE addmenuitem2_UI :
   DEFINE INPUT PARAMETER brwh AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER labelvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER procname AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER varth AS HANDLE NO-UNDO.
   IF varth = ? THEN varth = THIS-PROCEDURE.
   DEFINE VARIABLE temphandle AS HANDLE NO-UNDO.
   FIND FIRST menutemp WHERE menutemp.BRWH = brwh NO-LOCK NO-ERROR.
   IF NOT AVAILABLE menutemp THEN RUN createmenu_UI (INPUT brwh).
   IF AVAILABLE menutemp THEN DO:
      
      CREATE menuitemtemp.
      ASSIGN
      menuitemtemp.BRWH = menutemp.BRWH
      menuitemtemp.MENY = labelvar.
      CREATE MENU-ITEM menuitemtemp.MENYH.
      menuitemtemp.MENYH:LABEL = labelvar.
      menuitemtemp.MENYH:PARENT = menutemp.MENUH.
      ON CHOOSE OF menuitemtemp.MENYH PERSISTENT RUN VALUE(procname) IN varth.                 
   END.   
END PROCEDURE.

PROCEDURE disablemenuitem_UI :
   DEFINE INPUT PARAMETER brwh AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER labelvar AS CHARACTER NO-UNDO.
   
   FOR EACH menuitemtemp WHERE menuitemtemp.BRWH = brwh 
                       AND menuitemtemp.MENY = labelvar.
      menuitemtemp.MENYH:SENSITIVE = FALSE.
   END.  
   
END PROCEDURE.

PROCEDURE enablemenuitem_UI :
   DEFINE INPUT PARAMETER brwh AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER labelvar AS CHARACTER NO-UNDO.
   
   FOR EACH menuitemtemp WHERE menuitemtemp.BRWH = brwh 
                       AND menuitemtemp.MENY = labelvar.                       
      menuitemtemp.MENYH:SENSITIVE = TRUE.
   END.
   
END PROCEDURE.


PROCEDURE avmark_UI :
   dynbrwh:DESELECT-ROWS() NO-ERROR.
END PROCEDURE.

PROCEDURE selnextprevrowright_UI :
   lastbrwrow = ?.
   dynqueh:GET-NEXT(NO-LOCK). 
   REPEAT:
      IF dynbuffh:AVAILABLE = TRUE THEN LEAVE.
      IF dynqueh:QUERY-OFF-END THEN DO:
         LEAVE.
      END.
      dynqueh:GET-NEXT(NO-LOCK). 
   END.
   IF dynbuffh:AVAILABLE = FALSE THEN DO:         
      REPEAT:
         IF dynbuffh:AVAILABLE = TRUE THEN LEAVE.
         IF dynqueh:QUERY-OFF-END THEN DO:
            LEAVE.
         END.
         dynqueh:GET-PREV(NO-LOCK). 
      END.
   END.
   IF dynbuffh:AVAILABLE = TRUE THEN DO:         
      lastbrwrow = dynbuffh:ROWID.
      dynqueh:GET-PREV(NO-LOCK). 
   END.
   IF lastbrwrow = ? THEN DO:
      RUN openbdynspec_UI.
      dynqueh:GET-LAST(NO-LOCK).
      lastbrwrow = dynbuffh:ROWID.
   END.
END PROCEDURE.
PROCEDURE valrattright_UI :
   DEFINE INPUT PARAMETER raknare AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER svar AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER inrow AS ROWID NO-UNDO.
   IF raknare = dynbrwh:NUM-SELECTED-ROWS THEN DO:
      IF svar = TRUE THEN RUN selnextprevrowright_UI.         
      ELSE RUN setlastrowid_UI (INPUT inrow).
   END.   
END PROCEDURE.
PROCEDURE initbrw_UI :
   DEFINE INPUT PARAMETER TABLE FOR valsoktemp.  
   FIND FIRST valsoktemp NO-LOCK NO-ERROR.
   IF AVAILABLE valsoktemp THEN DO:
      ASSIGN
      colsortcharinput = valsoktemp.SOKCHAR[2].      /*Öppningsquery*/
      /*temptabvar = valsoktemp.SOKCHAR[3].*/      
      IF dynbrwh:TITLE NE ? THEN dynbrwh:TITLE = orgtitle.      
  END.
  RETURN.
END PROCEDURE.
PROCEDURE tomaTT_UI :
   dynbuffh:EMPTY-TEMP-TABLE NO-ERROR.   
END PROCEDURE.
PROCEDURE sokhmt_UI :
   DEFINE INPUT PARAMETER TABLE FOR valsoktemp.
   DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
   DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
   DEFINE VARIABLE brwantal AS INTEGER NO-UNDO.
   /*
   ASSIGN                                                                
   valsoktemp.SOKCHAR[1] = "MTRL"     /*Skarp tabell*/                   
   valsoktemp.SOKCHAR[2] = orgfraga   /*Öppningsquery*/                  
   valsoktemp.SOKCHAR[3] = "BENAMNING" /*sökfält*/                       
   valsoktemp.SOKCHAR[4] = "MTRLROW"  /*temptabells faltnamn för rowid*/ 
   valsoktemp.SOKCHAR[5] = aosok.      /*sök på*/                        
   */
   ASSIGN
   felmedd = "".
   FIND FIRST valsoktemp NO-LOCK NO-ERROR.
   IF AVAILABLE valsoktemp THEN DO:
      colsortcharinput = valsoktemp.SOKCHAR[2].
      kommandosortquery = "FOR EACH " + dynbrwh:TABLE + valsoktemp.SOKCHAR[2].
      RUN setcolsortvar_UI (INPUT colsortcharinput).
      svar = TRUE.
      IF valsoktemp.SOKCHAR[5] = "" THEN.
      ELSE DO:
         IF INDEX(valsoktemp.SOKCHAR[5],"*",1) = 0 THEN DO:
            IF LENGTH(valsoktemp.SOKCHAR[5]) <= 2 THEN DO:
               svar = FALSE.
               MESSAGE "Den här sökningen kan ta lite tid att uföra. Vill du fortsätta?" VIEW-AS ALERT-BOX 
                  QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar. 
            END.
         END.
         ELSE DO:
            IF SUBSTRING(valsoktemp.SOKCHAR[5],1,1) NE "*" AND SUBSTRING(valsoktemp.SOKCHAR[5],3,1) = "*" THEN DO:
               svar = FALSE.
               MESSAGE "Den här sökningen kan ta lite tid att uföra. Vill du fortsätta?" VIEW-AS ALERT-BOX 
                  QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar. 
            END.
            ELSE IF LENGTH(valsoktemp.SOKCHAR[5]) <= 3 OR SUBSTRING(valsoktemp.SOKCHAR[5],4,1) = "*" THEN DO:
               svar = FALSE.
               MESSAGE "Den här sökningen kan ta lite tid att uföra. Vill du fortsätta?" VIEW-AS ALERT-BOX 
                  QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar. 
            END.
         END.
      END.
      IF svar THEN DO:
         IF dynbrwh:TITLE NE ? THEN dynbrwh:TITLE = orgtitle + "   Vänta, sökning pågår... ".
         IF INDEX(valsoktemp.SOKCHAR[5],"#",1) NE 0 THEN DO:
            valsoktemp.SOKCHAR[5] = "".
         END.
         felmedd = "". 
         
         IF ejtomabrw = FALSE THEN dynbuffh:EMPTY-TEMP-TABLE NO-ERROR. 
         
         IF Guru.Konstanter:appcon THEN DO:
            RUN DYNBRWAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT TABLE valsoktemp,INPUT-OUTPUT TABLE-HANDLE tth,OUTPUT felmedd).
         END.
         ELSE DO:
            RUN DYNBRWAPP.P (INPUT TABLE valsoktemp,INPUT-OUTPUT TABLE-HANDLE tth,OUTPUT felmedd).
         END.
         IF felmedd = "" THEN DO:
            RUN openbdynspec_UI.  
            dynqueh:GET-FIRST(NO-LOCK).
            lastbrwrow = dynbuffh:ROWID.
            RUN lastselectdyn_UI.
            RUN getnumresult_UI (OUTPUT brwantal).
            dynbrwh:MAX-DATA-GUESS = brwantal.            
            IF dynbrwh:TITLE NE ? THEN dynbrwh:TITLE = orgtitle + "   Urvalresultat " + STRING(brwantal) + "st".               
         END.
         ELSE DO:
            MESSAGE felmedd VIEW-AS ALERT-BOX.
            RUN refreshbrw_UI.  
            IF dynbrwh:TITLE NE ? THEN dynbrwh:TITLE = orgtitle.               
         END.
      END.
   END.
   RETURN.
END PROCEDURE.

PROCEDURE sokhTT_UI :
   DEFINE INPUT PARAMETER TABLE FOR valsoktemp.
   DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
   DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
   DEFINE VARIABLE brwantal AS INTEGER NO-UNDO.
   felmedd = "".
   FIND FIRST valsoktemp NO-LOCK NO-ERROR.
   IF AVAILABLE valsoktemp THEN DO:
      colsortcharinput = valsoktemp.SOKCHAR[2].
      RUN setcolsortvar_UI (INPUT colsortcharinput).
      IF dynbrwh:TITLE NE ? THEN dynbrwh:TITLE = orgtitle + "   Vänta, sökning pågår... ".
      IF INDEX(valsoktemp.SOKCHAR[5],"#",1) NE 0 THEN DO:
         valsoktemp.SOKCHAR[5] = "".
      END.
      RUN DYNBRWAPPTT.P (INPUT TABLE valsoktemp,INPUT TABLE-HANDLE tth,INPUT-OUTPUT kommandosortquery).
     
     
      dynok = dynqueh:QUERY-PREPARE(kommandosortquery).
      dynok = dynqueh:QUERY-OPEN().  
      dynqueh:GET-FIRST(NO-LOCK).
      lastbrwrow = dynbuffh:ROWID.
      RUN lastselectdyn_UI.
      RUN getnumresult_UI (OUTPUT brwantal).
      dynbrwh:MAX-DATA-GUESS = brwantal.            
      IF dynbrwh:TITLE NE ? THEN dynbrwh:TITLE = orgtitle + "   Urvalresultat " + STRING(brwantal) + "st".               
      
   END.
   RETURN.
END PROCEDURE.



PROCEDURE sokFelerahmt_UI :
   DEFINE INPUT  PARAMETER ejtomabrwin AS LOGICAL NO-UNDO.
   ejtomabrw = ejtomabrwin.
   RETURN.
END PROCEDURE.

  
PROCEDURE fetrowid_UI :
   DEFINE INPUT PARAMETER antal_raknare AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER brwrowut AS ROWID     NO-UNDO.
   dynbrwh:FETCH-SELECTED-ROW(antal_raknare) NO-ERROR.
   IF NOT VALID-HANDLE(dynbuffh) THEN DO: 
      dynbuffh = dynqueh:GET-BUFFER-HANDLE(1).
   END.
   brwrowut = dynbuffh:ROWID.
END PROCEDURE.

PROCEDURE loprowid_UI :
   DEFINE INPUT PARAMETER antal_raknare AS INTEGER NO-UNDO.
   
   dynbrwh:FETCH-SELECTED-ROW(antal_raknare) NO-ERROR.
   dynfalth = dynbuffh:BUFFER-FIELD("aonr") NO-ERROR.
   
   MESSAGE  STRING(dynfalth:BUFFER-VALUE)
   VIEW-AS ALERT-BOX.
  
END PROCEDURE.


PROCEDURE dselectfocen_UI :
   dynbrwh:DESELECT-FOCUSED-ROW() NO-ERROR.
END PROCEDURE.

PROCEDURE dselectfoc_UI :
   dynbrwh:DESELECT-ROWS() NO-ERROR.
END PROCEDURE.

PROCEDURE selectrecid_UI :
   DEFINE OUTPUT PARAMETER brwrowut AS RECID  NO-UNDO.
   dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
   IF NOT VALID-HANDLE(dynbuffh) THEN dynbuffh = dynqueh:GET-BUFFER-HANDLE(1).
   brwrowut = dynbuffh:RECID.
END PROCEDURE.
PROCEDURE nselectrowid_UI :
   DEFINE OUTPUT PARAMETER brwrowut AS ROWID  NO-UNDO.
   dynbrwh:SELECT-NEXT-ROW() NO-ERROR.
   IF NOT VALID-HANDLE(dynbuffh) THEN dynbuffh = dynqueh:GET-BUFFER-HANDLE(1).
   brwrowut = dynbuffh:ROWID.
END PROCEDURE.
PROCEDURE pselectrowid_UI :
   DEFINE OUTPUT PARAMETER brwrowut AS ROWID  NO-UNDO.
   dynbrwh:SELECT-PREV-ROW() NO-ERROR.
   IF NOT VALID-HANDLE(dynbuffh) THEN dynbuffh = dynqueh:GET-BUFFER-HANDLE(1).
   brwrowut = dynbuffh:ROWID.
END PROCEDURE.
PROCEDURE selectrowid_UI :                                        
   DEFINE OUTPUT PARAMETER brwrowut AS ROWID     NO-UNDO.
   dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
   IF NOT VALID-HANDLE(dynbuffh) THEN dynbuffh = dynqueh:GET-BUFFER-HANDLE(1).
   brwrowut = dynbuffh:ROWID.
END PROCEDURE.
PROCEDURE getrowid_UI :
   DEFINE OUTPUT PARAMETER brwrowut AS ROWID     NO-UNDO.
   dynqueh:GET-CURRENT(NO-LOCK).
   IF NOT VALID-HANDLE(dynbuffh) THEN dynbuffh = dynqueh:GET-BUFFER-HANDLE(1).
   brwrowut = dynbuffh:ROWID.
END PROCEDURE.

PROCEDURE getquery_UI :
   DEFINE OUTPUT PARAMETER komsort AS CHARACTER NO-UNDO.
   komsort = kommandosortquery.
END PROCEDURE.

PROCEDURE getrow_UI :
   DEFINE OUTPUT PARAMETER crow AS ROWID NO-UNDO.
   crow = lastbrwrow.
END PROCEDURE. 
PROCEDURE getfirst_UI :
   dynqueh:GET-FIRST(NO-LOCK) NO-ERROR.
   IF NOT VALID-HANDLE(dynbuffh) THEN dynbuffh = dynqueh:GET-BUFFER-HANDLE(1).
   lastbrwrow = dynbuffh:ROWID.
END PROCEDURE.
PROCEDURE getnext_UI :
   dynqueh:GET-NEXT(NO-LOCK) NO-ERROR.
   IF NOT VALID-HANDLE(dynbuffh) THEN dynbuffh = dynqueh:GET-BUFFER-HANDLE(1).
   lastbrwrow = dynbuffh:ROWID.
END PROCEDURE.
PROCEDURE getprev_UI :
   dynqueh:GET-PREV(NO-LOCK) NO-ERROR.
   IF NOT VALID-HANDLE(dynbuffh) THEN dynbuffh = dynqueh:GET-BUFFER-HANDLE(1).
   lastbrwrow = dynbuffh:ROWID.
END PROCEDURE.
 
PROCEDURE getlast_UI :
   dynqueh:GET-LAST(NO-LOCK) NO-ERROR.
   IF NOT VALID-HANDLE(dynbuffh) THEN dynbuffh = dynqueh:GET-BUFFER-HANDLE(1).
   lastbrwrow = dynbuffh:ROWID.
END PROCEDURE.
PROCEDURE finns_UI :
   DEFINE OUTPUT PARAMETER lastbrwrow2 AS ROWID NO-UNDO.
   lastbrwrow2 = lastbrwrow.
END PROCEDURE.
PROCEDURE getcurrent_UI :
   DEFINE OUTPUT PARAMETER crow AS ROWID NO-UNDO.
   dynqueh:GET-CURRENT(NO-LOCK) NO-ERROR.
   IF NOT VALID-HANDLE(dynbuffh) THEN dynbuffh = dynqueh:GET-BUFFER-HANDLE(1).
   crow = dynbuffh:ROWID.
END PROCEDURE.
PROCEDURE getfirstkoll_UI :
   DEFINE OUTPUT PARAMETER brwrowut AS ROWID     NO-UNDO.
   dynqueh:GET-FIRST(NO-LOCK) NO-ERROR.
   brwrowut = dynbuffh:ROWID.
END PROCEDURE.
PROCEDURE getnextkoll_UI :
   DEFINE OUTPUT PARAMETER brwrowut AS ROWID     NO-UNDO.
   dynqueh:GET-NEXT(NO-LOCK) NO-ERROR.
   brwrowut = dynbuffh:ROWID.
END PROCEDURE.
PROCEDURE getprevkoll_UI :
   DEFINE OUTPUT PARAMETER brwrowut AS ROWID     NO-UNDO.
   dynqueh:GET-PREV(NO-LOCK) NO-ERROR.
   brwrowut = dynbuffh:ROWID.
END PROCEDURE.
PROCEDURE getlastkoll_UI :
   DEFINE OUTPUT PARAMETER brwrowut AS ROWID     NO-UNDO.
   dynqueh:GET-LAST(NO-LOCK) NO-ERROR.
   brwrowut = dynbuffh:ROWID.
END PROCEDURE.


PROCEDURE setcellvalue_UI :
 /*funkar NUUUUUU!*/
   DEFINE INPUT PARAMETER colnamn AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER covarde AS CHARACTER NO-UNDO.
   dynfalth = dynbuffh:BUFFER-FIELD(colnamn).
   IF covarde = ? THEN covarde = "".
   dynfalth:BUFFER-VALUE = covarde.    
END PROCEDURE.

PROCEDURE cellvalue_UI :
   DEFINE INPUT PARAMETER colnamn AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER covarde AS CHARACTER NO-UNDO.
   dynfalth = dynbuffh:BUFFER-FIELD(colnamn) NO-ERROR.
   covarde = STRING(dynfalth:BUFFER-VALUE) NO-ERROR.
END PROCEDURE.

PROCEDURE cellbredd_UI :
   DEFINE INPUT PARAMETER colnummer AS INTEGER NO-UNDO.
   tempcolh = dynbrwh:GET-BROWSE-COLUMN(colnummer).
   colnummer = ROUND(LENGTH(tempcolh:LABEL) / 1.5,0).
   IF colnummer < 4 THEN colnummer = 4.
   tempcolh:WIDTH-CHARS = colnummer.
     
END PROCEDURE.
PROCEDURE cellbreddcol_UI :
   DEFINE INPUT PARAMETER colnummer AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER colbredd AS INTEGER NO-UNDO.
   tempcolh = dynbrwh:GET-BROWSE-COLUMN(colnummer).
   tempcolh:WIDTH-CHARS = colbredd.
     
END PROCEDURE.

PROCEDURE cellbreddejmindre_UI :
   DEFINE INPUT PARAMETER colnummer AS INTEGER NO-UNDO.
   tempcolh = dynbrwh:GET-BROWSE-COLUMN(colnummer).
   colnummer = ROUND(LENGTH(tempcolh:LABEL) / 1.5,0).
   IF colnummer < 4 THEN colnummer = 4.
   IF colnummer > tempcolh:WIDTH-CHARS THEN tempcolh:WIDTH-CHARS = colnummer.
     
END PROCEDURE.

PROCEDURE cellabel_UI :
   DEFINE INPUT PARAMETER falt AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER labelvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE tempbufh AS HANDLE NO-UNDO.
   FIND FIRST coltemp WHERE coltemp.CNAME = falt NO-LOCK NO-ERROR.
   IF AVAILABLE coltemp THEN DO:
      /*
      tempbufh = dynbrwh:GET-BROWSE-COLUMN(coltemp.NUM).
      */
      coltemp.CH:LABEL = labelvar.
   END.
END PROCEDURE.
/*------------------------------------------------------------------------------
   Namn:       setmenulev_UI                
   Syfte:      -  
   In-data:    -
   Ut-data:    -
   Kommentar:  -
------------------------------------------------------------------------------*/

PROCEDURE setmenulev_UI :
   IF dynbrwh:POPUP-MENU = ? THEN DO:
      CREATE MENU temph.
      temph:POPUP-ONLY = TRUE.
      dynbrwh:POPUP-MENU = temph.
   END.             
   ELSE DO:
      CREATE MENU-ITEM temph.
      temph:SUBTYPE = "RULE".
      temph:PARENT = dynbrwh:POPUP-MENU.
   END.
   CREATE MENU-ITEM temph.
   temph:LABEL = "Visa information".
   temph:PARENT = dynbrwh:POPUP-MENU.
   ON CHOOSE OF temph PERSISTENT RUN openlevproc_UI IN THIS-PROCEDURE.
   
END PROCEDURE.

/*------------------------------------------------------------------------------
   Namn:       openlevproc_UI
   Syfte:      Gå till leverantörs webplats och hämta upp mtrlspecifikation
   In-data:    -
   Ut-data:    -
   Kommentar:  Event procedure.
------------------------------------------------------------------------------*/
PROCEDURE openlevproc_UI :
   DEFINE VARIABLE tempbufh AS HANDLE NO-UNDO.
   DEFINE VARIABLE valenr AS CHARACTER NO-UNDO.
   DEFINE VARIABLE levkodvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE levnamnvar AS CHARACTER NO-UNDO.
   ASSIGN
   valenr = ""
   levnamnvar = "".
   dynbuffh = dynqueh:GET-BUFFER-HANDLE(1).
   IF dynbuffh:AVAILABLE = TRUE THEN DO:
      dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
      tempbufh = dynbuffh:BUFFER-FIELD("ENR").
      valenr = tempbufh:BUFFER-VALUE.
      tempbufh = dynbuffh:BUFFER-FIELD("LEVKOD").
      levkodvar = tempbufh:BUFFER-VALUE.
      IF levkodvar NE "" OR levkodvar NE ? THEN DO:
         IF Guru.Konstanter:appcon THEN DO:
            RUN DYNMTRLAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT levkodvar,OUTPUT levnamnvar). 
         END.
         ELSE DO:
            RUN DYNMTRLAPP.P (INPUT levkodvar,OUTPUT levnamnvar).
         END. 
         IF levnamnvar NE "" THEN DO:
            {LEVLANK.I}
            
         END.         
      END.
   END.
END PROCEDURE.



