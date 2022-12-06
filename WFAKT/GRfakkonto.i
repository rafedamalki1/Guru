/*GRFAKKONTO.I*/
PROCEDURE utgr_UI:
   /*faktfor*/
   DEFINE VARIABLE filut AS CHARACTER NO-UNDO.
   DEFINE VARIABLE filutkopia AS CHARACTER NO-UNDO.
   DEFINE VARIABLE formappvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE fakthuvfil AS CHARACTER NO-UNDO.
   IF namndb() = "GRANIT" OR namndb() = "GRANNORD" OR namndb() = "RT9" THEN musz = musz.
   ELSE RETURN.
   Guru.Konstanter:globforetag = FORETAG.FORETAG. 
   IF Guru.Konstanter:globforetag = "ELPA" THEN DO: 
      ASSIGN
      prognamn = "\\pc112\DELAD\PRO9\guru\EXPORT\"
      prognamnk = "\\pc112\DELAD\PRO9\guru\EXPORT\EXKOPIA\"                                                     
      fakthuvfil = "SROKBA.txt"
      prognamnIBT = "SROIBT.txt"
      prognamnIBTI = "SROIBTRV.txt".
   END.
   IF Guru.Konstanter:globforetag = "GRAN"  THEN DO: 
      ASSIGN     
      prognamn = "\\GRANGURU\guru_ser\server\PRO9s\EXPORT\" 
      prognamnk = "\\GRANGURU\guru_ser\server\PRO9s\EXPORT\EXKOPIA\"
      fakthuvfil = "SROKBA.txt"
      prognamnIBT =  "SROIBT.txt"
      prognamnIBTI = "SROIBTRV.txt".     
   END.   
   IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      IF BESTTAB.VIBESTID = "N1" THEN bestvarmapp = "N1\".
      ELSE IF BESTTAB.VIBESTID = "N4" THEN bestvarmapp = "N4\".
      ELSE IF BESTTAB.VIBESTID = "800" OR BESTTAB.VIBESTID = "801" OR BESTTAB.VIBESTID = "802" OR 
      BESTTAB.VIBESTID = "803" OR BESTTAB.VIBESTID = "807" OR
      BESTTAB.VIBESTID = "808" THEN bestvarmapp = "N1\".
      ELSE bestvarmapp = "N4\".
   END.
   IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
      bestvarmapp = "G1\".
   END.
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
   nrcol[12] = 12
   nrcol[13] = 13
   nrcol[14] = 14
   nrcol[15] = 15
   nrcol[16] = 16
   nrcol[17] = 17
   nrcol[18] = 18
   nrcol[19] = 19
   nrcol[20] = 20
   nrcol[21] = 21
   nrcol[22] = 22
   nrcol[23] = 23
   nrcol[24] = 24
   nrcol[25] = 25
   nrcol[26] = 26
   nrcol[27] = 27
   nrcol[28] = 28
   nrcol[29] = 29
   nrcol[30] = 30
   nrcol[31] = 31
   nrcol[32] = 32
   nrcol[33] = 33
   nrcol[34] = 34
   nrcol[35] = 35
   nrcol[36] = 36
   nrcol[37] = 37
   nrcol[38] = 38
   nrcol[39] = 39
   nrcol[40] = 40
   nrcol[41] = 41
   nrcol[42] = 42
   nrcol[43] = 43
   nrcol[44] = 44
   nrcol[45] = 45
   nrcol[46] = 46
   nrcol[47] = 47
   nrcol[48] = 48
   nrcol[49] = 49
   nrcol[50] = 50
   nrcol[51] = 51
   nrcol[52] = 52
   nrcol[53] = 53
   nrcol[54] = 54
   nrcol[55] = 55.
   ASSIGN
   /*ny kolumn*/
   breddantal = 55   /*antal kolumner*/
   bredd[1] =  2
   bredd[2] =  1
   bredd[3] =  5
   bredd[4] =  7
   bredd[5] =  4
   bredd[6] =  6
   bredd[7] =  1
   bredd[8] =  7
   bredd[9] =  7
   bredd[10] = 8
   bredd[11] = 8
   bredd[12] = 11
   bredd[13] = 17
   bredd[14] = 4
   bredd[15] = 17
   bredd[16] = 4
   bredd[17] = 17
   bredd[18] = 8
   bredd[19] = 11
   bredd[20] = 1
   bredd[21] = 17
   bredd[22] = 8
   bredd[23] = 5
   bredd[24] = 8
   bredd[25] = 5
   bredd[26] = 30
   bredd[27] = 1
   bredd[28] = 1
   bredd[29] = 8
   bredd[30] = 2
   bredd[31] = 17
   bredd[32] = 7
   bredd[33] = 3
   bredd[34] = 3
   bredd[35] = 3
   bredd[36] = 3
   bredd[37] = 3
   bredd[38] = 2
   bredd[39] = 2
   bredd[40] = 9
   bredd[41] = 1
   bredd[42] = 1
   bredd[43] = 7
   bredd[44] = 7
   bredd[45] = 9
   bredd[46] = 1
   bredd[47] = 35
   bredd[48] = 15
   bredd[49] = 3
   bredd[50] = 17
   bredd[51] = 3
   bredd[52] = 5
   bredd[53] = 2
   bredd[54] = 12
   bredd[55] = 4.
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
   nrcolIBT[12] = 12
   nrcolIBT[13] = 13
   nrcolIBT[14] = 14
   nrcolIBT[15] = 15
   nrcolIBT[16] = 16
   nrcolIBT[17] = 17
   nrcolIBT[18] = 18
   nrcolIBT[19] = 19
   nrcolIBT[20] = 20
   nrcolIBT[21] = 21
   nrcolIBT[22] = 22
   nrcolIBT[23] = 23
   nrcolIBT[24] = 24.
   ASSIGN
   /*ny kolumn*/
   breddantalIBT = 24   /*antal kolumner*/
   breddIBT[1] =  1
   breddIBT[2] =  5
   breddIBT[3] =  3
   breddIBT[4] =  4
   breddIBT[5] =  6
   breddIBT[6] =  1
   breddIBT[7] =  1
   breddIBT[8] =  1
   breddIBT[9] =  1
   breddIBT[10] = 3
   breddIBT[11] = 7
   breddIBT[12] = 4
   breddIBT[13] = 8
   breddIBT[14] = 13
   breddIBT[15] = 17
   breddIBT[16] = 17
   breddIBT[17] = 17
   breddIBT[18] = 4
   breddIBT[19] = 30
   breddIBT[20] = 4
   breddIBT[21] = 6
   breddIBT[22] = 4
   breddIBT[23] = 1
   breddIBT[24] = 9.
   ASSIGN
   i = 2.     
   utnrIBT[nrcolIBT[1]] = 1.
   DO WHILE i <= breddantalIBT:             
      utnrIBT[i] = utnrIBT[i - 1] + breddIBT[i - 1].            
      i = i + 1.
   END.
   
   /*KUNDFAKTUROR STEG 1 FAKTURAHUVUD SROKBA*/
  
   IF debkred = FALSE THEN DO:     
      FIND FIRST FAKTURERINGSTYP WHERE 
      FAKTURERINGSTYP.FAKTTYPID = FAKTURERAD.FAKTTYPID NO-LOCK NO-ERROR.
      ASSIGN         
      SUBSTRING(str,utnr[nrcol[1]]) = "GF"
      SUBSTRING(str,utnr[nrcol[6]]) = STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99").
      IF varbelopp + varmbelopp >= 0 THEN DO:
         ASSIGN
         SUBSTRING(str,utnr[nrcol[13]]) = "+" + STRING((varbelopp + varmbelopp) * 1000,"9999999999999999").
      END.
      ELSE DO:
         ASSIGN
         SUBSTRING(str,utnr[nrcol[13]]) = "-" + STRING((-1 * varbelopp + varmbelopp) * 1000,"9999999999999999").
      END.
      IF varmbelopp >= 0 THEN DO:
         SUBSTRING(str,utnr[nrcol[50]]) = "+" + STRING(varmbelopp * 1000,"9999999999999999").
      END.
      ELSE DO:
         SUBSTRING(str,utnr[nrcol[50]]) = "-" + STRING(-1 * varmbelopp * 1000,"9999999999999999").    
      END.
      SUBSTRING(str,utnr[nrcol[51]]) = "GUF".
   END.
   IF debkred = TRUE THEN DO:
      FIND FIRST FAKTURERINGSTYP WHERE 
      FAKTURERINGSTYP.FAKTTYPID = FAKTKRED.FAKTTYPID NO-LOCK NO-ERROR.
      ASSIGN
      SUBSTRING(str,utnr[nrcol[1]]) = "GF"
      SUBSTRING(str,utnr[nrcol[6]]) = STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99")      
      SUBSTRING(str,utnr[nrcol[13]]) = "-" + STRING((varbelopp + varmbelopp) * 1000,"9999999999999999")
      SUBSTRING(str,utnr[nrcol[50]]) = "-" + STRING(varmbelopp * 1000,"9999999999999999")
      SUBSTRING(str,utnr[nrcol[51]]) = "GUK".
   END.                     
   ASSIGN
   SUBSTRING(str,utnr[nrcol[8]]) = STRING(skarpvar,"9999999")
   SUBSTRING(str,utnr[nrcol[11]]) = STRING(TODAY,"99999999")
   SUBSTRING(str,utnr[nrcol[12]]) = SUBSTRING(bestvarmapp,1,2).                    
   ASSIGN
   SUBSTRING(str,utnr[nrcol[14]]) = "SEK "
   SUBSTRING(str,utnr[nrcol[15]]) = "0"
   SUBSTRING(str,utnr[nrcol[17]]) = "0"
   SUBSTRING(str,utnr[nrcol[18]]) = STRING(varforfalld,"99999999")
   SUBSTRING(str,utnr[nrcol[20]]) = "S"
   SUBSTRING(str,utnr[nrcol[27]]) = "0"
   SUBSTRING(str,utnr[nrcol[29]]) = "0".
   ASSIGN
   SUBSTRING(str,utnr[nrcol[39]]) = "0"
   SUBSTRING(str,utnr[nrcol[40]]) = "1" + STRING(skarpvar,"99999999")
   SUBSTRING(str,utnr[nrcol[41]]) = "N"
   SUBSTRING(str,utnr[nrcol[42]]) = "J".
   ASSIGN
   filut = ""
   filutkopia = "".
   IF Guru.Konstanter:globforetag = "ELPA" THEN formappvar = "N9\".
   ELSE IF Guru.Konstanter:globforetag = "GRAN" THEN formappvar = "N9\".
   ELSE IF Guru.Konstanter:globforetag = "GRIT" THEN formappvar = "G1\".
   ASSIGN
   filut = prognamn + formappvar 
   filutkopia = prognamnk + formappvar.
   IF SEARCH(filut) = ? THEN DO:
      OS-CREATE-DIR VALUE(filut).
   END.
   IF SEARCH(filutkopia) = ? THEN DO:
      OS-CREATE-DIR VALUE(filutkopia).
   END.
   ASSIGN
   filut = filut + fakthuvfil 
   filutkopia = filutkopia + fakthuvfil.
   OUTPUT TO VALUE(filut) APPEND.
   PUT UNFORMATTED str AT 1 SKIP.
   OUTPUT CLOSE.
   OUTPUT TO VALUE(filutkopia) APPEND.
   PUT UNFORMATTED str AT 1 SKIP.
   OUTPUT CLOSE.
   /*KUNDFAKTUROR STEG 2 KONTERING*/
   IF debkred = FALSE THEN DO:
      RUN viskont_UI.      
   END.
   IF debkred = TRUE THEN DO:
      RUN viskontK_UI.      
   END.      
   FOR EACH viskonttemp:   
      IF viskonttemp.ORDNING = 99 THEN NEXT.
      IF viskonttemp.ORDNING = 1 THEN NEXT.
      IF viskonttemp.AONR = "Summa:" THEN NEXT.
      /*SROIBT*/
      RUN ibt_UI.
      ASSIGN
      filut = ""
      filutkopia = "".
      IF Guru.Konstanter:globforetag = "ELPA" THEN formappvar = "N9\".
      ELSE IF Guru.Konstanter:globforetag = "GRAN" THEN formappvar = "N9\".
      ELSE IF Guru.Konstanter:globforetag = "GRIT" THEN formappvar = "G1\".
      ASSIGN
      filut = prognamn + formappvar 
      filutkopia = prognamnk + formappvar.
      IF SEARCH(filut) = ? THEN DO:
         OS-CREATE-DIR VALUE(filut).
      END.
      IF SEARCH(filutkopia) = ? THEN DO:
         OS-CREATE-DIR VALUE(filutkopia).
      END.
      ASSIGN
      filut = filut + prognamnIBT 
      filutkopia = filutkopia + prognamnIBT.
      OUTPUT TO VALUE(filut) APPEND.
      PUT UNFORMATTED str AT 1 SKIP.
      IF strIGUR1 NE "" THEN PUT UNFORMATTED strIGUR1 AT 1 SKIP.
      IF strIGUR2 NE "" THEN PUT UNFORMATTED strIGUR2 AT 1 SKIP.
      OUTPUT CLOSE.
      OUTPUT TO VALUE(filutkopia) APPEND.
      PUT UNFORMATTED str AT 1 SKIP.
      IF strIGUR1 NE "" THEN PUT UNFORMATTED strIGUR1 AT 1 SKIP.
      IF strIGUR2 NE "" THEN PUT UNFORMATTED strIGUR2 AT 1 SKIP.
      OUTPUT CLOSE.
   END.      
   /*LEVERANTÖRS FAKTUROR STEG 1 KONTERING endast interna kunder*/
   FIND FIRST FAKTREGLER WHERE FAKTREGLER.FAKTNR = FAKTPLAN.FAKTNR 
   USE-INDEX FAKTREGLER NO-LOCK NO-ERROR.

   IF FAKTREGLER.KUNDID = 2 THEN DO:            
      FOR EACH sumkont BREAK BY sumkont.K2:         
         ACCUMULATE 
         sumkont.BELOPP (TOTAL BY sumkont.K2).         
         /*SROIBTRV*/
         IF LAST-OF(sumkont.K2) THEN DO:                        
            ASSIGN
            str = ""
            str2 = "".                 
            ASSIGN                        
            SUBSTRING(str,utnrIBT[nrcolIBT[2]]) = "66140"
            SUBSTRING(str,utnrIBT[nrcolIBT[3]]) = SUBSTRING(BESTTAB.VIBESTID,1,3)
            SUBSTRING(str,utnrIBT[nrcolIBT[4]]) = STRING(sumkont.K2,"x(4)") 
            SUBSTRING(str,utnrIBT[nrcolIBT[11]]) = ""               
            SUBSTRING(str,utnrIBT[nrcolIBT[13]]) = STRING(TODAY,"99999999").
            IF debkred = FALSE THEN DO:
               ASSIGN
               SUBSTRING(str,utnrIBT[nrcolIBT[10]]) = "".
               IF (ACCUM TOTAL BY sumkont.K2 sumkont.BELOPP) > 0 THEN DO:
                  ASSIGN
                  SUBSTRING(str,utnrIBT[nrcolIBT[15]]) = "+" + 
                  STRING((ACCUM TOTAL BY sumkont.K2 sumkont.BELOPP) * 1000,"9999999999999999").         
               END.
               ELSE DO:
                  SUBSTRING(str,utnrIBT[nrcolIBT[15]]) = "-" + 
                  STRING(ABS((ACCUM TOTAL BY sumkont.K2 sumkont.BELOPP)) * 1000,"9999999999999999").         
               END.
            END.
            ELSE DO:
               ASSIGN
               SUBSTRING(str,utnrIBT[nrcolIBT[10]]) = "".
               IF (ACCUM TOTAL BY sumkont.K2 sumkont.BELOPP) > 0 THEN DO:
                  ASSIGN
                  SUBSTRING(str,utnrIBT[nrcolIBT[15]]) = "-" +  
                  STRING((ACCUM TOTAL BY sumkont.K2 sumkont.BELOPP) * 1000,"9999999999999999").         
               END.
               ELSE DO:
                  ASSIGN
                  SUBSTRING(str,utnrIBT[nrcolIBT[15]]) = "+" + 
                  STRING(ABS((ACCUM TOTAL BY sumkont.K2 sumkont.BELOPP)) * 1000,"9999999999999999").         
               END.
            END.
            ASSIGN 
            SUBSTRING(str,utnrIBT[nrcolIBT[18]]) = "SEK "
            SUBSTRING(str,utnrIBT[nrcolIBT[19]]) = SUBSTRING(bestvarmapp,1,2) + " " + SUBSTRING(BESTTAB.BESTNAMN,1,19)
            SUBSTRING(str,utnrIBT[nrcolIBT[21]]) = STRING(YEAR(varbokdatum),"9999") + STRING(MONTH(varbokdatum),"99")
            SUBSTRING(str,utnrIBT[nrcolIBT[22]]) = "RVGU"
            SUBSTRING(str,utnrIBT[nrcolIBT[23]]) = "X".      
            ASSIGN
            filut = ""
            filutkopia = "".
            ASSIGN
            filut = prognamn + bestvarmapp 
            filutkopia = prognamnk + bestvarmapp.
            IF SEARCH(filut) = ? THEN DO:
               OS-CREATE-DIR VALUE(filut).
            END.
            IF SEARCH(filutkopia) = ? THEN DO:
               OS-CREATE-DIR VALUE(filutkopia).
            END.
            ASSIGN                      
            filut = filut + prognamnIBTI 
            filutkopia = filutkopia + prognamnIBTI.
            OUTPUT TO VALUE(filut) APPEND.
            PUT UNFORMATTED str AT 1 SKIP.
            OUTPUT CLOSE.
            OUTPUT TO VALUE(filutkopia) APPEND.
            PUT UNFORMATTED str AT 1 SKIP.
            OUTPUT CLOSE.
         END.
      END.
      /*SROIBTRV*/
      ASSIGN
      str = ""
      str2 = "".                 
      ASSIGN                        
      SUBSTRING(str,utnrIBT[nrcolIBT[2]]) = "KUG"
      SUBSTRING(str,utnrIBT[nrcolIBT[3]]) = ""
      SUBSTRING(str,utnrIBT[nrcolIBT[11]]) = ""
      SUBSTRING(str,utnrIBT[nrcolIBT[13]]) = STRING(TODAY,"99999999").
      IF debkred = FALSE THEN DO:
         ASSIGN
         SUBSTRING(str,utnrIBT[nrcolIBT[10]]) = "".
         IF (ACCUM TOTAL sumkont.BELOPP) >= 0 THEN SUBSTRING(str,utnrIBT[nrcolIBT[15]]) = "-" + STRING((ACCUM TOTAL sumkont.BELOPP) * 1000,"9999999999999999").         
         ELSE SUBSTRING(str,utnrIBT[nrcolIBT[15]]) = "+" + STRING((ACCUM TOTAL sumkont.BELOPP) * -1 * 1000,"9999999999999999").         
      END.
      ELSE DO:
         ASSIGN
         SUBSTRING(str,utnrIBT[nrcolIBT[10]]) = "".
         IF (ACCUM TOTAL sumkont.BELOPP) >= 0 THEN SUBSTRING(str,utnrIBT[nrcolIBT[15]]) = "+" + STRING((ACCUM TOTAL sumkont.BELOPP) * 1000,"9999999999999999").         
         ELSE SUBSTRING(str,utnrIBT[nrcolIBT[15]]) = "-" + STRING((ACCUM TOTAL sumkont.BELOPP) * -1 * 1000,"9999999999999999").
      END.
      ASSIGN
      SUBSTRING(str,utnrIBT[nrcolIBT[18]]) = "SEK "
      SUBSTRING(str,utnrIBT[nrcolIBT[19]]) = SUBSTRING(bestvarmapp,1,2) + " " + SUBSTRING(BESTTAB.BESTNAMN,1,19)
      SUBSTRING(str,utnrIBT[nrcolIBT[21]]) = STRING(YEAR(varbokdatum),"9999") + STRING(MONTH(varbokdatum),"99")
      SUBSTRING(str,utnrIBT[nrcolIBT[22]]) = "RVGU"
      SUBSTRING(str,utnrIBT[nrcolIBT[23]]) = "X".      
      ASSIGN
      filut = ""
      filutkopia = "".
      ASSIGN
      filut = prognamn + bestvarmapp 
      filutkopia = prognamnk + bestvarmapp.
      IF SEARCH(filut) = ? THEN DO:
         OS-CREATE-DIR VALUE(filut).
      END.
      IF SEARCH(filutkopia) = ? THEN DO:
         OS-CREATE-DIR VALUE(filutkopia).
      END.
      ASSIGN                      
      filut = filut + prognamnIBTI 
      filutkopia = filutkopia + prognamnIBTI.
      OUTPUT TO VALUE(filut) APPEND.
      PUT UNFORMATTED str AT 1 SKIP.
      OUTPUT CLOSE.
      OUTPUT TO VALUE(filutkopia) APPEND.
      PUT UNFORMATTED str AT 1 SKIP.
      OUTPUT CLOSE.
   END.
END PROCEDURE.
PROCEDURE ibt_UI:
   /*SROIBT*/    
   ASSIGN
   strIGUR1 = ""
   strIGUR2 = ""
   str = ""
   str2 = "". 
   ASSIGN                        
   SUBSTRING(str,utnrIBT[nrcolIBT[2]]) = STRING(viskonttemp.KONTO,"x(5)"). 
   IF viskonttemp.ORDNING = 2 THEN musz = musz.
   ELSE DO:
      IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
         FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE =  viskonttemp.OMRADE NO-LOCK NO-ERROR.
         IF AVAILABLE OMRADETAB THEN
         SUBSTRING(str,utnrIBT[nrcolIBT[3]]) = STRING(INTEGER(OMRADETAB.ORGIDNUM),"999").    
      END.
      ELSE SUBSTRING(str,utnrIBT[nrcolIBT[3]]) = STRING(INTEGER(viskonttemp.OMRADE),"999").  
   END.        
   ASSIGN
   SUBSTRING(str,utnrIBT[nrcolIBT[4]]) = STRING(viskonttemp.MOTPART,"x(4)") 
   SUBSTRING(str,utnrIBT[nrcolIBT[5]]) = STRING(viskonttemp.AONR,"x(6)")
   SUBSTRING(str,utnrIBT[nrcolIBT[11]]) = STRING(skarpvar,"9999999")
   SUBSTRING(str,utnrIBT[nrcolIBT[13]]) = STRING(varbokdatum,"99999999")
   SUBSTRING(str,utnrIBT[nrcolIBT[21]]) = STRING(YEAR(varbokdatum),"9999") + STRING(MONTH(varbokdatum),"99").   
   IF viskonttemp.ORDNING = 2 THEN DO:
      ASSIGN 
      SUBSTRING(str,utnrIBT[nrcolIBT[13]]) = STRING(TODAY,"99999999")
      SUBSTRING(str,utnrIBT[nrcolIBT[21]]) = STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99").
      FIND FIRST MOMSTAB WHERE MOMSTAB.MOMSKOD = viskonttemp.KONTO 
      NO-LOCK NO-ERROR.
      IF AVAILABLE MOMSTAB THEN DO:
         IF MOMSTAB.MOMSEXTERNT >= 25 THEN DO:
            ASSIGN
            SUBSTRING(str,utnrIBT[nrcolIBT[2]]) = STRING("MUH","x(5)") 
            SUBSTRING(str,utnrIBT[nrcolIBT[12]]) = "UTG1".
         END.
         IF MOMSTAB.MOMSEXTERNT >= 12 AND MOMSTAB.MOMSEXTERNT < 25 THEN DO:
            ASSIGN
            SUBSTRING(str,utnrIBT[nrcolIBT[2]]) = STRING("MUM","x(5)") 
            SUBSTRING(str,utnrIBT[nrcolIBT[12]]) = "UTG2".
         END.
         IF MOMSTAB.MOMSEXTERNT < 12 THEN DO:
            ASSIGN
            SUBSTRING(str,utnrIBT[nrcolIBT[2]]) = STRING("MUL","x(5)") 
            SUBSTRING(str,utnrIBT[nrcolIBT[12]]) = "UTG3".
         END.
      END.
      IF debkred = FALSE THEN DO:
         ASSIGN
         SUBSTRING(str,utnrIBT[nrcolIBT[10]]) = "GUF".
         IF viskonttemp.DEBET NE 0 THEN DO:
            IF viskonttemp.DEBET >= 0 THEN 
            SUBSTRING(str,utnrIBT[nrcolIBT[15]]) = "+" + STRING(viskonttemp.DEBET * 1000,"9999999999999999").
            ELSE SUBSTRING(str,utnrIBT[nrcolIBT[15]]) = "-" + STRING(viskonttemp.DEBET * -1 * 1000,"9999999999999999").
         END.
         ELSE DO:
            IF viskonttemp.KREDIT >= 0 THEN 
            SUBSTRING(str,utnrIBT[nrcolIBT[15]]) = "-" + STRING(viskonttemp.KREDIT * 1000,"9999999999999999").         
            ELSE SUBSTRING(str,utnrIBT[nrcolIBT[15]]) = "+" + STRING(viskonttemp.KREDIT * -1 * 1000,"9999999999999999").         
         END.
      END.
      ELSE DO:
         ASSIGN
         SUBSTRING(str,utnrIBT[nrcolIBT[10]]) = "GUK".
         IF viskonttemp.KREDIT NE 0 THEN DO:
            IF viskonttemp.KREDIT >= 0 THEN 
            SUBSTRING(str,utnrIBT[nrcolIBT[15]]) = "+" + STRING(viskonttemp.KREDIT * 1000,"9999999999999999").         
            ELSE SUBSTRING(str,utnrIBT[nrcolIBT[15]]) = "-" + STRING(viskonttemp.KREDIT * -1 * 1000,"9999999999999999").         
         END.
         ELSE DO:
            IF viskonttemp.DEBET >= 0 THEN 
            SUBSTRING(str,utnrIBT[nrcolIBT[15]]) = "-" + STRING(viskonttemp.DEBET * 1000,"9999999999999999").
            ELSE SUBSTRING(str,utnrIBT[nrcolIBT[15]]) = "+" + STRING(viskonttemp.DEBET * -1 * 1000,"9999999999999999").
         END.
      END.
      SUBSTRING(str,utnrIBT[nrcolIBT[23]]) = "V".
   END.
   ELSE DO:
      ASSIGN
      SUBSTRING(strIGUR1,utnrIBT[nrcolIBT[21]]) = STRING(YEAR(varbokdatum),"9999") + STRING(MONTH(varbokdatum),"99")
      SUBSTRING(strIGUR2,utnrIBT[nrcolIBT[21]]) = STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99").
      IF debkred = FALSE THEN DO:
         ASSIGN
         SUBSTRING(str,utnrIBT[nrcolIBT[10]]) = "GUF".
         IF viskonttemp.DEBET NE 0 THEN DO:
            IF viskonttemp.DEBET >= 0 THEN
            SUBSTRING(str,utnrIBT[nrcolIBT[15]]) = "+" + STRING(viskonttemp.DEBET * 1000,"9999999999999999").         
            ELSE SUBSTRING(str,utnrIBT[nrcolIBT[15]]) = "-" + STRING(viskonttemp.DEBET * -1 * 1000,"9999999999999999").         
         END.
         ELSE DO:
            IF viskonttemp.KREDIT >= 0 THEN
            SUBSTRING(str,utnrIBT[nrcolIBT[15]]) = "-" + STRING(viskonttemp.KREDIT * 1000,"9999999999999999").   
            ELSE SUBSTRING(str,utnrIBT[nrcolIBT[15]]) = "+" + STRING(viskonttemp.KREDIT * -1 * 1000,"9999999999999999").   
         END.
         ASSIGN
         SUBSTRING(strIGUR1,utnrIBT[nrcolIBT[2]]) = "IUGUR".
         SUBSTRING(strIGUR1,utnrIBT[nrcolIBT[10]]) = "GUF".
         IF viskonttemp.DEBET NE 0 THEN DO:
            IF viskonttemp.DEBET >= 0 THEN
            SUBSTRING(strIGUR1,utnrIBT[nrcolIBT[15]]) = "-" + STRING(viskonttemp.DEBET * 1000,"9999999999999999").         
            ELSE SUBSTRING(strIGUR1,utnrIBT[nrcolIBT[15]]) = "+" + STRING(viskonttemp.DEBET * -1 * 1000,"9999999999999999").         
         END.        
         ELSE DO:
            IF viskonttemp.KREDIT >= 0 THEN
            SUBSTRING(strIGUR1,utnrIBT[nrcolIBT[15]]) = "+" + STRING(viskonttemp.KREDIT * 1000,"9999999999999999").   
            ELSE SUBSTRING(strIGUR1,utnrIBT[nrcolIBT[15]]) = "-" + STRING(viskonttemp.KREDIT * -1 * 1000,"9999999999999999").   
         END.
         ASSIGN
         SUBSTRING(strIGUR1,utnrIBT[nrcolIBT[11]]) = STRING(skarpvar,"9999999")
         SUBSTRING(strIGUR1,utnrIBT[nrcolIBT[13]]) = STRING(varbokdatum,"99999999").         
         ASSIGN
         SUBSTRING(strIGUR2,utnrIBT[nrcolIBT[2]]) = "IUGUR".
         SUBSTRING(strIGUR2,utnrIBT[nrcolIBT[10]]) = "GUF".
         IF viskonttemp.DEBET NE 0 THEN DO:
            IF viskonttemp.DEBET >= 0 THEN
            SUBSTRING(strIGUR2,utnrIBT[nrcolIBT[15]]) = "+" + STRING(viskonttemp.DEBET * 1000,"9999999999999999").         
            ELSE SUBSTRING(strIGUR2,utnrIBT[nrcolIBT[15]]) = "-" + STRING(viskonttemp.DEBET * -1 * 1000,"9999999999999999").         
         END.
         ELSE DO:
            IF viskonttemp.KREDIT >= 0 THEN
            SUBSTRING(strIGUR2,utnrIBT[nrcolIBT[15]]) = "-" + STRING(viskonttemp.KREDIT * 1000,"9999999999999999").   
            ELSE SUBSTRING(strIGUR2,utnrIBT[nrcolIBT[15]]) = "+" + STRING(viskonttemp.KREDIT * -1 * 1000,"9999999999999999").   
         END.
         ASSIGN
         SUBSTRING(strIGUR2,utnrIBT[nrcolIBT[11]]) = STRING(skarpvar,"9999999")
         SUBSTRING(strIGUR2,utnrIBT[nrcolIBT[13]]) = STRING(TODAY,"99999999").         
      END.
      ELSE DO:
         ASSIGN
         SUBSTRING(str,utnrIBT[nrcolIBT[10]]) = "GUK".
         IF viskonttemp.KREDIT NE 0 THEN DO:
            IF viskonttemp.KREDIT >= 0 THEN
            SUBSTRING(str,utnrIBT[nrcolIBT[15]]) = "+" + STRING(viskonttemp.KREDIT * 1000,"9999999999999999").         
            ELSE SUBSTRING(str,utnrIBT[nrcolIBT[15]]) = "-" + STRING(viskonttemp.KREDIT * -1 * 1000,"9999999999999999").      
         END.         
         ASSIGN
         SUBSTRING(strIGUR1,utnrIBT[nrcolIBT[2]]) = "IUGUR".
         SUBSTRING(strIGUR1,utnrIBT[nrcolIBT[10]]) = "GUK".
         
         IF viskonttemp.KREDIT NE 0 THEN DO:
            IF viskonttemp.KREDIT >= 0 THEN
            SUBSTRING(strIGUR1,utnrIBT[nrcolIBT[15]]) = "-" + STRING(viskonttemp.KREDIT * 1000,"9999999999999999").         
            ELSE SUBSTRING(strIGUR1,utnrIBT[nrcolIBT[15]]) = "+" + STRING(viskonttemp.KREDIT * -1 * 1000,"9999999999999999").         
         END.
         ELSE DO:
            IF viskonttemp.DEBET >= 0 THEN
            SUBSTRING(strIGUR1,utnrIBT[nrcolIBT[15]]) = "+" + STRING(viskonttemp.DEBET * 1000,"9999999999999999").   
            ELSE SUBSTRING(strIGUR1,utnrIBT[nrcolIBT[15]]) = "-" + STRING(viskonttemp.DEBET * -1 * 1000,"9999999999999999").   
         END.
         ASSIGN
         SUBSTRING(strIGUR1,utnrIBT[nrcolIBT[11]]) = STRING(skarpvar,"9999999")
         SUBSTRING(strIGUR1,utnrIBT[nrcolIBT[13]]) = STRING(varbokdatum,"99999999").         
         ASSIGN
         SUBSTRING(strIGUR2,utnrIBT[nrcolIBT[2]]) = "IUGUR".
         SUBSTRING(strIGUR2,utnrIBT[nrcolIBT[10]]) = "GUK".
         IF viskonttemp.KREDIT NE 0 THEN DO:
            IF viskonttemp.KREDIT >= 0 THEN
            SUBSTRING(strIGUR2,utnrIBT[nrcolIBT[15]]) = "+" + STRING(viskonttemp.KREDIT * 1000,"9999999999999999").         
            ELSE SUBSTRING(strIGUR2,utnrIBT[nrcolIBT[15]]) = "-" + STRING(viskonttemp.KREDIT * -1 * 1000,"9999999999999999").         
         END.
         ELSE DO:
            IF viskonttemp.DEBET >= 0 THEN
            SUBSTRING(strIGUR2,utnrIBT[nrcolIBT[15]]) = "-" + STRING(viskonttemp.DEBET * 1000,"9999999999999999").   
            ELSE SUBSTRING(strIGUR2,utnrIBT[nrcolIBT[15]]) = "+" + STRING(viskonttemp.DEBET * -1 * 1000,"9999999999999999").   
         END.
         ASSIGN
         SUBSTRING(strIGUR2,utnrIBT[nrcolIBT[11]]) = STRING(skarpvar,"9999999")
         SUBSTRING(strIGUR2,utnrIBT[nrcolIBT[13]]) = STRING(TODAY,"99999999").         
      END.
      SUBSTRING(str,utnrIBT[nrcolIBT[23]]) = "X".
      SUBSTRING(strIGUR1,utnrIBT[nrcolIBT[23]]) = "X".
      SUBSTRING(strIGUR2,utnrIBT[nrcolIBT[23]]) = "X".
      ASSIGN
      SUBSTRING(strIGUR1,utnrIBT[nrcolIBT[18]]) = "SEK "
      SUBSTRING(strIGUR1,utnrIBT[nrcolIBT[19]]) = SUBSTRING(bestvarmapp,1,2) + " " + SUBSTRING(BESTTAB.BESTNAMN,1,19)
      SUBSTRING(strIGUR1,utnrIBT[nrcolIBT[22]]) = "KFGU"
      SUBSTRING(strIGUR1,utnrIBT[nrcolIBT[24]]) = "1" + STRING(skarpvar,"99999999").
      ASSIGN
      SUBSTRING(strIGUR2,utnrIBT[nrcolIBT[18]]) = "SEK "
      SUBSTRING(strIGUR2,utnrIBT[nrcolIBT[19]]) = SUBSTRING(bestvarmapp,1,2) + " " + SUBSTRING(BESTTAB.BESTNAMN,1,19)
      SUBSTRING(strIGUR2,utnrIBT[nrcolIBT[22]]) = "KFGU"
      SUBSTRING(strIGUR2,utnrIBT[nrcolIBT[24]]) = "1" + STRING(skarpvar,"99999999").
   END.     
   
   ASSIGN
   SUBSTRING(str,utnrIBT[nrcolIBT[18]]) = "SEK "
   SUBSTRING(str,utnrIBT[nrcolIBT[19]]) = SUBSTRING(bestvarmapp,1,2) + " " + SUBSTRING(BESTTAB.BESTNAMN,1,19)
   SUBSTRING(str,utnrIBT[nrcolIBT[22]]) = "KFGU"
   SUBSTRING(str,utnrIBT[nrcolIBT[24]]) = "1" + STRING(skarpvar,"99999999").
END PROCEDURE.


  
