
/*VALDBALTE.I*/
/*VID ?NDRINGA AV N?GOT KOMPILER ALLA SOM INNEH?LLER CONAPP.I +
 

   DO TRANSACTION:
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "ALTE"
      valdbtemp.GFORETAG = "ALTE"
      valdbtemp.DBNAMN = "ALTE"
      valdbtemp.DBCON = "-db ALTE -H alteco04 -S 2771 -N tcp"
      valdbtemp.DBPLATS = "C:\DELAD\PRO9S\DB\"
      valdbtemp.ORDNING = 1      
      valdbtemp.DBCACHE = ""      
      valdbtemp.APPCON = "-AppService APPALTE -H alteco04 -S 2773'
      valdbtemp.VALDB = "Alteco Elkonsult AB".
      valdbtemp.WWWFTP = FALSE.
   END.
