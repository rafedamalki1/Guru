/*LULEFAKKONTO.I*/
PROCEDURE kundut_UI:      
   DEFINE VARIABLE ik AS INTEGER NO-UNDO.
   
   ASSIGN
   nrcolk[1] = 1
   nrcolk[2] = 2
   nrcolk[3] = 3
   nrcolk[4] = 4
   nrcolk[5] = 5
   nrcolk[6] = 6
   
    
   breddantalk = 6   
   breddk[1] = 11
   breddk[2] = 30
   breddk[3] = 35
   breddk[4] = 35
   breddk[5] = 35
   breddk[6] = 30.
   
   ASSIGN
   ik = 2.     
   utnrk[nrcolk[1]] = 1.
   DO WHILE ik <= breddantalk:             
      utnrk[ik] = utnrk[ik - 1] + breddk[ik - 1].            
      ik = ik + 1.
   END.
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   IF FORETAG.FORETAG = "ELPA" THEN DO:                                  
      prognamno = CAPS("d:\DELAD\PRO9s\EXPORT\kundguru" + STRING(TODAY,"99999999") + ".txt"). 
      prognamnkk = CAPS("d:\DELAD\PRO9s\EXKOPIA\kundguru.txt"). 
   END.
   IF FORETAG.FORETAG = "LULE" THEN DO:                                  
      prognamno = CAPS("D:\ELPOOL\DELAD\PRO9S\EXPORT\kundguru" + JURPERS.JUDID + STRING(TODAY,"99999999") + ".txt"). 
      prognamnkk = CAPS("D:\ELPOOL\DELAD\PRO9S\EXKOPIA\kundguru" + JURPERS.JUDID + ".txt"). 
   END.
   CREATE tidut.   
   ASSIGN
   SUBSTRING(tidut.UT,utnrk[nrcolk[1]]) = BESTTAB.VIBESTID
   SUBSTRING(tidut.UT,utnrk[nrcolk[2]]) = BESTTAB.BESTNAMN
   SUBSTRING(tidut.UT,utnrk[nrcolk[3]]) = BESTTAB.ADRESS
   SUBSTRING(tidut.UT,utnrk[nrcolk[4]]) = BESTTAB.FAKADRESS 
   SUBSTRING(tidut.UT,utnrk[nrcolk[5]]) = STRING(BESTTAB.PNR,"999 99") + " " + BESTTAB.ORT 
   SUBSTRING(tidut.UT,utnrk[nrcolk[6]]) = "".                
   OUTPUT STREAM eko TO VALUE(prognamno) APPEND.
   OUTPUT STREAM ekospar TO VALUE(prognamnkk) APPEND.
   FOR EACH tidut:
      PUT STREAM eko UNFORMATTED
      tidut.UT AT 1 SKIP.
      PUT STREAM ekospar UNFORMATTED
      tidut.UT AT 1 SKIP.
   END.
   OUTPUT STREAM eko CLOSE. 
   OUTPUT STREAM ekospar CLOSE.
   
END PROCEDURE.

PROCEDURE utlule_UI:
   /*faktfor*/
   DEFINE VARIABLE prognamnvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE fakthuvfil  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE fakthuvfilkopia AS CHARACTER NO-UNDO.
   DEFINE VARIABLE bokfil      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE bokfilkopia AS CHARACTER NO-UNDO.

   
   IF namndb() = "utbi" THEN RETURN.
   IF namndb() = "LULE" OR namndb() = "RT9" THEN musz = musz.
   ELSE RETURN.
   IF Guru.Konstanter:globforetag = Guru.Konstanter:globforetag THEN RETURN.
   Guru.Konstanter:globforetag = FORETAG.FORETAG.   
   regar = YEAR(TODAY).
   regdatum = TODAY.
   RUN REGVEC.P.
   IF MONTH(TODAY) = 1 THEN regmannamn = "jan".
   ELSE IF MONTH(TODAY) = 2 THEN regmannamn = "feb".
   ELSE IF MONTH(TODAY) = 3 THEN regmannamn = "mar". 
   ELSE IF MONTH(TODAY) = 4 THEN regmannamn = "apr". 
   ELSE IF MONTH(TODAY) = 5 THEN regmannamn = "maj". 
   ELSE IF MONTH(TODAY) = 6 THEN regmannamn = "jun".
   ELSE IF MONTH(TODAY) = 7 THEN regmannamn = "jul".
   ELSE IF MONTH(TODAY) = 8 THEN regmannamn = "aug". 
   ELSE IF MONTH(TODAY) = 9 THEN regmannamn = "sep".
   ELSE IF MONTH(TODAY) = 10 THEN regmannamn = "okt". 
   ELSE IF MONTH(TODAY) = 11 THEN regmannamn = "nov".
   ELSE IF MONTH(TODAY) = 12 THEN regmannamn = "dec".
   /*
   FIND FIRST AONRTAB WHERE AONRTAB.AONR = viskonttemp.AONR AND AONRTAB.DELNR = viskonttemp.DELNR NO-LOCK NO-ERROR.
   FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = AONRTAB.OMRADE NO-LOCK NO-ERROR.
   */
   FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = FAKTPLAN.OMRADE NO-LOCK NO-ERROR.
   FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
   FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.
   RUN kundut_UI.
   IF Guru.Konstanter:globforetag = "ELPA" THEN DO: 
      prognamnvar = "D:\DELAD\PRO9s\".        
   END.
   ELSE  DO: 
      prognamnvar = "D:\elpool\DELAD\PRO9s\".
   END.
   
   ASSIGN     
   fakthuvfil = CAPS(prognamnvar + "EXPORT\faktguru" + JURPERS.JUDID + STRING(TODAY,"99999999") + ".txt")
   fakthuvfilkopia = CAPS(prognamnvar + "exkopia\gurufakt" + JURPERS.JUDID + STRING(regar) + regmannamn + ".txt")    
   bokfilkopia = CAPS(prognamnvar + "exkopia\bokf" + JURPERS.JUDID + STRING(regar) + regmannamn + ".txt")    
   bokfil = CAPS(prognamnvar + "export\bokfguru" + JURPERS.JUDID + "asw" + STRING(TODAY,"99999999") + ".txt").          
   
   str = "".
   ASSIGN
   nrcol[1] = 1
   nrcol[2] = 2
   nrcol[3] = 3
   nrcol[4] = 4
   nrcol[5] = 5
   nrcol[6] = 6
   nrcol[7] = 7
   nrcol[8] = 8       
   nrcol[9] = 9
   nrcol[10] = 10
   nrcol[11] = 11
   breddantal = 11  
   bredd[1] =  7
   bredd[2] =  10
   bredd[3] =  10
   bredd[4] =  15
   bredd[5] =  15
   bredd[6] =  3
   bredd[7] =  8
   bredd[8] =  11
   bredd[9] =  11
   bredd[10] = 30
   bredd[11] = 30.
   ASSIGN
   i = 2.     
   utnr[nrcol[1]] = 1.
   DO WHILE i <= breddantal:             
      utnr[i] = utnr[i - 1] + bredd[i - 1].            
      i = i + 1.
   END.
   ASSIGN
   nrcolIBT[1] = 1
   nrcolIBT[2] = 2
   nrcolIBT[3] = 3
   nrcolIBT[4] = 4
   nrcolIBT[5] = 5
   nrcolIBT[6] = 6
   nrcolIBT[7] = 7
   nrcolIBT[8] = 8
   nrcolIBT[9] = 9
   nrcolIBT[10] = 10
   nrcolIBT[11] = 11
   breddantalIBT = 11   
   breddIBT[1] = 4
   breddIBT[2] = 3
   breddIBT[3] = 4
   breddIBT[4] = 9
   breddIBT[5] = 10
   breddIBT[6] = 17
   breddIBT[7] = 30
   breddIBT[8] = 8
   breddIBT[9] = 4
   breddIBT[10] = 3
   breddIBT[11] = 7.
   ASSIGN
   i = 2.     
   utnrIBT[nrcolIBT[1]] = 1.
   DO WHILE i <= breddantalIBT:             
      utnrIBT[i] = utnrIBT[i - 1] + breddIBT[i - 1].            
      i = i + 1.
   END.
   str = "".
   /*KUNDFAKTUROR STEG 1 FAKTURAHUVUD SROKBA*/
   ASSIGN
   SUBSTRING(str,utnr[nrcol[1]]) = STRING(skarpvar,"9999999")
   SUBSTRING(str,utnr[nrcol[2]]) = STRING(varfakturd,"99999999")
   SUBSTRING(str,utnr[nrcol[3]]) = STRING(varforfalld,"99999999")                                                          
   SUBSTRING(str,utnr[nrcol[6]]) = "SEK"
   SUBSTRING(str,utnr[nrcol[7]]) = STRING(YEAR(varbokdatum),"9999") + STRING(MONTH(varbokdatum),"99")
   SUBSTRING(str,utnr[nrcol[8]]) = SUBSTRING(BESTTAB.VIBESTID,1,11)
   SUBSTRING(str,utnr[nrcol[9]]) = SUBSTRING(BESTTAB.VIBESTID,1,11)
   SUBSTRING(str,utnr[nrcol[10]]) = FAKTPLAN.NAMN
   SUBSTRING(str,utnr[nrcol[11]]) = "".           
   IF debkred = FALSE THEN DO:     
      ASSIGN                                            
      SUBSTRING(str,utnr[nrcol[4]]) = TRIM(STRING(varbelopp + varmbelopp,"->>>>>>>>>>.999"))
      SUBSTRING(str,utnr[nrcol[5]]) = TRIM(STRING(varmbelopp,"->>>>>>>>>>.999")).
   END.
   /*kredit*/
   IF debkred = TRUE THEN DO:
      ASSIGN                                                 
      SUBSTRING(str,utnr[nrcol[4]]) = TRIM(STRING(-1 * varbelopp + varmbelopp,"->>>>>>>>>>.999"))
      SUBSTRING(str,utnr[nrcol[5]]) = TRIM(STRING(-1 * varmbelopp,"->>>>>>>>>>.999")).
   END.                     
   OUTPUT TO VALUE(fakthuvfil) APPEND.
   PUT UNFORMATTED str AT 1 SKIP.
   OUTPUT CLOSE.
   OUTPUT TO VALUE(fakthuvfilkopia) APPEND.
   PUT UNFORMATTED str AT 1 SKIP.
   OUTPUT CLOSE.
   /*KUNDFAKTUROR STEG 2 KONTERING*/
   IF debkred = FALSE THEN DO:
      FIND FIRST FAKTURERINGSTYP WHERE 
      FAKTURERINGSTYP.FAKTTYPID = FAKTURERAD.FAKTTYPID NO-LOCK NO-ERROR.
      RUN viskont_UI.      
   END.
   IF debkred = TRUE THEN DO:
      FIND FIRST FAKTURERINGSTYP WHERE 
      FAKTURERINGSTYP.FAKTTYPID = FAKTKRED.FAKTTYPID NO-LOCK NO-ERROR.
      RUN viskontK_UI.      
   END.
   FOR EACH viskonttemp:   
      IF viskonttemp.ORDNING = 99 THEN NEXT.
      /*IF viskonttemp.ORDNING = 1 THEN NEXT.*/
      IF viskonttemp.AONR = "Summa:" THEN NEXT.
      RUN bokf_UI.
      OUTPUT TO VALUE(bokfil) APPEND.
      PUT UNFORMATTED str AT 1 SKIP.
      OUTPUT CLOSE.
      OUTPUT TO VALUE(bokfilkopia) APPEND.
      PUT UNFORMATTED str AT 1 SKIP.
      OUTPUT CLOSE.
   END.
   
END PROCEDURE.
PROCEDURE bokf_UI:
   DEFINE VARIABLE hbelopp AS CHARACTER NO-UNDO.
   DEFINE VARIABLE pbelopp AS CHARACTER NO-UNDO.
   DEFINE VARIABLE hkbelopp AS CHARACTER NO-UNDO.
   DEFINE VARIABLE pkbelopp AS CHARACTER NO-UNDO.
   /*SROIBT*/    
   ASSIGN
   str = "".
   ASSIGN
   SUBSTRING(str,utnrIBT[nrcolIBT[1]]) = STRING(viskonttemp.KONTO,"x(4)")
   SUBSTRING(str,utnrIBT[nrcolIBT[2]]) = ""
   SUBSTRING(str,utnrIBT[nrcolIBT[3]]) = viskonttemp.K1.
   IF viskonttemp.AONR NE "" THEN
   SUBSTRING(str,utnrIBT[nrcolIBT[4]]) = STRING(viskonttemp.AONR,"999999") + STRING(viskonttemp.DELNR,"999").
   ASSIGN
   SUBSTRING(str,utnrIBT[nrcolIBT[5]]) = STRING(varbokdatum,"99999999") + "  "
   SUBSTRING(str,utnrIBT[nrcolIBT[7]]) = "F" + STRING(skarpvar,"999999") + " " + SUBSTRING(BESTTAB.VIBESTID,1,11)
   SUBSTRING(str,utnrIBT[nrcolIBT[8]]) = STRING(YEAR(varbokdatum),"9999") + STRING(MONTH(varbokdatum),"99")                  
   SUBSTRING(str,utnrIBT[nrcolIBT[9]]) = "GUR"                                                                                          
   SUBSTRING(str,utnrIBT[nrcolIBT[10]]) = ""                                                                                             
   SUBSTRING(str,utnrIBT[nrcolIBT[11]]) = "".
   hbelopp = TRIM(STRING( viskonttemp.debet,"->>>>>>>>>>>9.99")) + "0".      
   hkbelopp = TRIM(STRING(-1 * viskonttemp.kredit,"->>>>>>>>>>>9.99")) + "0".    
   
   IF viskonttemp.debet NE 0 THEN DO:                                       
      SUBSTRING(str,utnrIBT[nrcolIBT[6]]) = hbelopp.      
   END.
   /*kredit*/
   IF viskonttemp.kredit NE 0 THEN DO:
      ASSIGN
      SUBSTRING(str,utnrIBT[nrcolIBT[6]]) = hkbelopp.    
   END.                        
END PROCEDURE.


  

