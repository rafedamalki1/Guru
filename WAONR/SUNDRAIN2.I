/*SUNDRAIN2.I*/ 
  
IF JURPERS.JUDID = "ELNÄT" THEN DO:
   IF LENGTH(AONRTAB.AONR) = 4 THEN aonrvar = aonrvar + "0".
   OUTPUT TO d:\delad\SERVER\pro10s\EXPORT\nyproj.txt APPEND.
   ASSIGN projrapp = ""
   SUBSTRING(projrapp,1,5) = aonrvar
   SUBSTRING(projrapp,7,30) = SUBSTRING(AONRTAB.ORT,1,30)
   SUBSTRING(projrapp,38,10) = STRING(stdatum,"9999/99/99")
   SUBSTRING(projrapp,49,10) = avdatchar.
   SUBSTRING(projrapp,60,3) = STRING(AONRTAB.DELNR,"999").
   PUT UNFORMATTED projrapp.
   PUT SKIP.         
   OUTPUT CLOSE.
   /*os appnend*/
   OUTPUT TO d:\delad\SERVER\pro10s\EXPORT\allanyproj.txt APPEND.               
   PUT UNFORMATTED projrapp.
   PUT SKIP.
   OUTPUT CLOSE.
END.
IF JURPERS.JUDID = "SEAB" THEN DO:
   IF LENGTH(AONRTAB.AONR) = 4 THEN aonrvar = aonrvar + "0".
   OUTPUT TO d:\delad\SERVER\pro10s\EXPORT\nyprojSEAB.txt APPEND.
   ASSIGN projrapp = "".
   IF aonrvar BEGINS "S" THEN DO:                  
      SUBSTRING(projrapp,1,5) = SUBSTRING(aonrvar,2,5).
   END.
   ELSE DO:
      SUBSTRING(projrapp,1,5) = aonrvar.
   END.
   ASSIGN
   SUBSTRING(projrapp,7,30) = SUBSTRING(AONRTAB.ORT,1,30)
   SUBSTRING(projrapp,38,10) = STRING(stdatum,"9999/99/99")
   SUBSTRING(projrapp,49,10) = avdatchar.
   PUT UNFORMATTED projrapp.
   PUT SKIP.         
   OUTPUT CLOSE.
   /*os appnend*/
   OUTPUT TO d:\delad\SERVER\pro10s\EXPORT\allanyprojSEAB.txt APPEND.               
   PUT UNFORMATTED projrapp.
   PUT SKIP.
   OUTPUT CLOSE.
END.     
IF JURPERS.JUDID = "ServaNet" THEN DO:
   IF LENGTH(AONRTAB.AONR) = 4 THEN aonrvar = aonrvar + "0".
   OUTPUT TO d:\delad\SERVER\pro10s\EXPORT\nyprojSN.txt APPEND.
   ASSIGN projrapp = "".
   ASSIGN
   SUBSTRING(projrapp,1,5) = aonrvar
   SUBSTRING(projrapp,7,30) = SUBSTRING(AONRTAB.ORT,1,30)
   SUBSTRING(projrapp,38,10) = STRING(stdatum,"9999/99/99")
   SUBSTRING(projrapp,49,10) = avdatchar.
   SUBSTRING(projrapp,60,3) = STRING(AONRTAB.DELNR,"999").
   PUT UNFORMATTED projrapp.
   PUT SKIP.         
   OUTPUT CLOSE.
   /*os appnend*/
   OUTPUT TO d:\delad\SERVER\pro10s\EXPORT\allanyprojSN.txt APPEND.               
   PUT UNFORMATTED projrapp.
   PUT SKIP.
   OUTPUT CLOSE.
END.
IF JURPERS.JUDID = "REKO" THEN DO:
   IF LENGTH(AONRTAB.AONR) = 4 THEN aonrvar = aonrvar + "0".
   OUTPUT TO d:\delad\SERVER\pro10s\EXPORT\nyprojREKO.txt APPEND.
   ASSIGN projrapp = "".
   ASSIGN
   SUBSTRING(projrapp,1,5) = aonrvar
   SUBSTRING(projrapp,7,30) = SUBSTRING(AONRTAB.ORT,1,30)
   SUBSTRING(projrapp,38,10) = STRING(stdatum,"9999/99/99")
   SUBSTRING(projrapp,49,10) = avdatchar.
   PUT UNFORMATTED projrapp.
   PUT SKIP.         
   OUTPUT CLOSE.
   /*os appnend*/
   OUTPUT TO d:\delad\SERVER\pro10s\EXPORT\allanyprojREKO.txt APPEND.               
   PUT UNFORMATTED projrapp.
   PUT SKIP.
   OUTPUT CLOSE.
END.
