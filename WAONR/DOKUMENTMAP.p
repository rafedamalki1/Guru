   /*DOKUMENTMAP.p*/
   DEFINE INPUT  PARAMETER globanv AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER globforetag AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER datornamn AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER soktempvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER guruvar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER grundmappvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE diranv AS CHARACTER NO-UNDO.
   {NAMNDB.I}
  
   IF namndb() = "ST" THEN DO:
       grundmappvar = "\\server05\d\salj\DOKUMENT\".
   END.    
   
  
   ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
      grundmappvar = "\\server05\d\dokument\DOKUMENT\".
      /*grundmappvar = "\\server05\d\4. Investering\".*/
      
   END.
   ELSE IF Guru.Konstanter:globforetag = "PINN" THEN DO:
      grundmappvar = SESSION:TEMP-DIR.  
   END.
   ELSE IF Guru.Konstanter:globforetag = "LULE" AND soktempvar  = "UTBI" THEN DO:
      grundmappvar = "\\" + Guru.Konstanter:InternNr(STRING(195)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(196)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(19)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(21)) + "\DOKUMENT\PROJEKT\UTBILDNING\".         
   END.
   ELSE IF Guru.Konstanter:globforetag = "LULE" THEN DO:
       /*Dokumentsökväg bytt 20151209 Lena*/
      /*grundmappvar = "N:\Planering\Projekt_GURU\".
      grundmappvar = "N:\4. Investering\Planering\Projekt\Projekt_GURU\".
      */
      /*  
      grundmappvar = "\\dppower\dpdoc\Elnat\Projekt\". 
       */
       grundmappvar = "\\cosy.local.luleaenergi.se\dpdoc\Elnat\Projekt\".           
   END.   
   ELSE IF soktempvar  = "GKAL" THEN DO:
       grundmappvar = "\\goliat\guru\DOKUMENT\".
   END.
   ELSE IF Guru.Konstanter:globforetag = "GKAL" AND soktempvar  = "UTBI" THEN DO:
       grundmappvar = "\\goliat\guru\DOKUMENT\utbi\".
   END.
   ELSE IF soktempvar  = "GRANNORD" THEN DO:
      grundmappvar = "\\SRV00486\APPL\GURU\DOKUMENT\GRAN\".
   END.
   ELSE IF (Guru.Konstanter:globforetag = "GRAN" AND soktempvar  = "UTBI") THEN DO:
      grundmappvar = "\\SRV00486\APPL\GURU\DOKUMENT\GRANUTBI\".         
   END.
   ELSE IF Guru.Konstanter:globforetag = "KRAF" THEN DO:
      diranv = Guru.Konstanter:globanv.
      { MOLNETMAPPEXTRA.I}
      grundmappvar = "C:\Users\" + diranv + "\286 - Kraftteknik\Projekt - Dokument\1 EL Distribution\Guru Projekt\".
      /*
      IF bildbertemp.FILNAMN BEGINS "c:\users\" THEN DO:
         bsok = SUBSTRING(FILNAMN,10,INDEX(FILNAMN,"\",10) - 10).
         bildbertemp.FILNAMN = REPLACE(bildbertemp.FILNAMN,bsok,globanv).  
      END.
      */  
      
   END.    
   ELSE DO:
      grundmappvar = guruvar + "DOKUMENTATION\". 
      OS-CREATE-DIR VALUE(grundmappvar) NO-ERROR.            
   END.
     
