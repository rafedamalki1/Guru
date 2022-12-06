/*VALDBKALM.I*/                             
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER  VALDBALL.I +
    
   /*
   guruvarmellan = guruvar.
   IF INDEX(guruvar," ") NE 0 THEN DO :
      ASSIGN      
      guruvar = "..\".
   END.
   def VAR cc AS CHAR.
   cc = guruvar + "gkal.csh".
   IF SEARCH(cc) = ? THEN MESSAGE "nej" VIEW-AS ALERT-BOX.
   else MESSAGE "ja" VIEW-AS ALERT-BOX.
   d:\delad\klient\pro9\guru\wtid\appcon9.p
*/
   DO TRANSACTION:
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "kalm"
      valdbtemp.GFORETAG = "GKAL" 
      valdbtemp.DBNAMN = "GKAL"
      valdbtemp.DBCON = "-db GKAL -S 2512 -H goliat -N tcp"
      valdbtemp.DBCACHE = ""      
      valdbtemp.DBPLATS = "D:\DELAD\SERVER\PRO9S\DB\"
      valdbtemp.ORDNING = 1      
      valdbtemp.APPCON = "-AppService GKALAPP -H goliat -S 2535'
      valdbtemp.VALDB = "Kalmar Energi".
      valdbtemp.WWWFTP = FALSE.
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "kalm"
      valdbtemp.GFORETAG = "UTBIGKAL"
      valdbtemp.DBNAMN = "UTBI"
      valdbtemp.DBCON = "-db UTBI -S 2508 -H goliat -N tcp"
      valdbtemp.DBPLATS = "D:\DELAD\SERVER\PRO9S\DBTILLBAKA\"
      valdbtemp.DBCACHE = ""
      valdbtemp.ORDNING = 99
      valdbtemp.APPCON = "-AppService UTBI -H goliat -S 2510'
      valdbtemp.VALDB = "UTBILDNING KALMAR".  
      valdbtemp.WWWFTP = FALSE.                         
   END.
   
  
