{VALDBDEF.I}
   /*
CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "ELPA"
      valdbtemp.GFORETAG = "ELPA"
      valdbtemp.DBNAMN = "RT9"
      valdbtemp.ORDNING = 1      
      valdbtemp.DBCON = "-db RT9 -H PC012 -S 2553 -N TCP -U ELPAO -P KAGGEN"
      valdbtemp.DBPLATS = "D:\DELAD\PRO9S\DB\"
      valdbtemp.DBCACHE = ""      
         /*
      valdbtemp.APPCON = "-AppService appelpool9 -H PC012 -S 2555'
      valdbtemp.APPCON= '-URL http://pc012/aia/Aia1?AppService=appelpool9'
      */
      http://localhost/aia/appe1?GetServletStatus
      valdbtemp.APPCON= ("-URL AppServer://pc012:2555/appelpool9").
      valdbtemp.VALDB = "Guru utveckling".
      */
 DO TRANSACTION:
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "LAPP"
      valdbtemp.GFORETAG = "LAPP"
      valdbtemp.DBNAMN = "LAPP"      
      valdbtemp.DBCON = "-db LAPP -H 192.168.111.52 -S 2501 -N tcp"
      valdbtemp.DBPLATS = "G:\DELAD\PRO9S\DB\"
      valdbtemp.ORDNING = 1      
      /*valdbtemp.DBCACHE = "-cache " + guruvar + "SWECO.CSH"*/
         /*
      valdbtemp.APPCON = "-AppService APPLAPP -H 192.168.111.52 -S 2503'
      */
      valdbtemp.APPCON= "-URL AppServer://192.168.111.52:2503/APPLAPP".
      valdbtemp.VALDB = "Lapplands Eltjänst AB".
     
   END.


DEFINE VARIABLE ivar AS INTEGER NO-UNDO.
DEFINE VARIABLE utbivar AS CHARACTER NO-UNDO.
DEFINE VARIABLE conappvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE gforetag AS CHARACTER NO-UNDO.


DEFINE VARIABLE globforetag AS CHARACTER NO-UNDO.    
ASSIGN
gforetag = valdbtemp.GFORETAG
conappvar = valdbtemp.APPCON.
CREATE SERVER Guru.Konstanter:apphand.
IF SUBSTRING(PROVERSION,1,1) = "9" THEN DO:
   IF conappvar = "" THEN DO:
      MESSAGE "Kontakta Elpool tel 090/184540 för du kan inte ansluta korrekt!"
      VIEW-AS ALERT-BOX.
   END. 
   /*obs case-sensitv -AppService appguru9*/
   ELSE DO:
      Guru.Konstanter:appcon = Guru.Konstanter:apphand:CONNECT(conappvar,CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79),"KAGGEN",gforetag) NO-ERROR.  
   END.   
END.
ELSE DO:
/*   {CONAPP8.I}*/
END.
IF NOT Guru.Konstanter:appcon THEN DO:
   MESSAGE 
   ERROR-STATUS:NUM-MESSAGES 
   " fel uppkom vid anslutningen." SKIP 
   "Det går ej att ansluta appserver." SKIP 
   "Felet kan bero på för många samtidiga användare." SKIP
   "Kontakta system ansvarig." SKIP
   "Kontakta Elpool tel 090/184540." SKIP
   SKIP
   "Vill du se felmeddelandena ?" 
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Fel på anslutningen"
   UPDATE view-errs AS LOGICAL .       
   IF view-errs THEN DO ivar = 1 TO ERROR-STATUS:NUM-MESSAGES:
      MESSAGE ERROR-STATUS:GET-NUMBER(ivar)
      ERROR-STATUS:GET-MESSAGE(ivar)
      VIEW-AS ALERT-BOX.
   END.        
END.
ELSE DO:
   MESSAGE "ok" VIEW-AS ALERT-BOX.
END.
IF Guru.Konstanter:appcon THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT().
IF Guru.Konstanter:apphand NE ? THEN DELETE OBJECT Guru.Konstanter:apphand NO-ERROR.
apphand = ?.
appcon = FALSE.

