/*FTPSFILESTART.p*/
DEFINE INPUT  PARAMETER varskicka AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER localFile AS CHARACTER NO-UNDO. 
DEFINE INPUT  PARAMETER remoteFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE curlvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE serverName AS  CHARACTER  NO-UNDO.  
DEFINE VARIABLE usrName AS CHARACTER NO-UNDO.
DEFINE VARIABLE usrPass AS CHARACTER NO-UNDO.
DEFINE VARIABLE remotdir AS CHARACTER NO-UNDO.
IF varskicka = "Tillberget" THEN DO:
   curlvar = "D:\curl-7.77.0-win64-mingw\bin\".
   serverName = CHR(51) + CHR(49) + CHR(46) + CHR(50) + CHR(49) + CHR(54) + CHR(46) + CHR(50) + CHR(50) + CHR(55) + CHR(46) + CHR(50) + CHR(57). 
   usrName = CHR(101) + CHR(108) + CHR(112) + CHR(108) + CHR(111) .  
   usrPass = CHR(74) + CHR(97) + CHR(103) + CHR(103) + CHR(105) + CHR(108) + CHR(108) + CHR(97) + CHR(114) + CHR(115) + CHR(107) + CHR(105) + CHR(100) + CHR(111) + CHR(114) + CHR(38) + CHR(53) + CHR(52) + CHR(49) + CHR(57) .
   remotdir = "/Guruzip/".   
END.

IF varskicka = "ESorder" THEN DO:
   serverName = CHR(98) + CHR(50) + CHR(98) + CHR(46) + CHR(101) + CHR(108) + CHR(101) + CHR(107) + CHR(116) + CHR(114) + CHR(111) + CHR(115) + CHR(107) + CHR(97) + CHR(110) + CHR(100) + CHR(105) + CHR(97) + CHR(46) + CHR(115) + CHR(101).     
END.   
IF varskicka = "Ahlorder" THEN DO:
   serverName = CHR(49) + CHR(54) + CHR(52) + CHR(46) + CHR(57) + CHR(46) + CHR(49) + CHR(48) + CHR(53) + CHR(46) + CHR(49) + CHR(51) + CHR(50).
END.   
IF Guru.Konstanter:AppSpringSet[2] = "VAST" THEN DO:
   curlvar = "E:\delad\curl-7.77.0-win64-mingw\bin\".
   IF varskicka = "ESorder" THEN DO:
      /*b2b.elektroskandia.se
      V95hp5nR%2021f*/
      usrName = CHR(118) + CHR(97) + CHR(116) + CHR(116) + CHR(101) + CHR(110) + CHR(102) + CHR(97) + CHR(108) + CHR(108) + CHR(103) + CHR(117) + CHR(114) + CHR(117). 
      usrPass = CHR(86) + CHR(57) + CHR(53) + CHR(104) + CHR(112) + CHR(53) + CHR(110) + CHR(82) + CHR(37) + CHR(50) + CHR(48) + CHR(50) + CHR(49) + CHR(102). 
      remotdir = "/".
      
   END.    
END.


IF Guru.Konstanter:AppSpringSet[2] = "FORS" THEN DO:
   curlvar = "C:\delad\curl-7.82.0-win64-mingw\bin\".  
   IF varskicka = "Ahlorder" THEN DO:
      
      usrName = CHR(73) + CHR(110) + CHR(102) + CHR(114) + CHR(97) + CHR(116) + CHR(101) + CHR(107). 
      usrPass = CHR(103) + CHR(111) + CHR(50) + CHR(48) + CHR(49) + CHR(48) + CHR(116) + CHR(114).  
      remotdir = "/".
      
   END.    
END.


RUN FTPSFILE.p (INPUT varskicka,INPUT curlvar, INPUT serverName, INPUT usrName, INPUT usrPass, INPUT localFile, INPUT  remoteFile, INPUT remotdir).

        
