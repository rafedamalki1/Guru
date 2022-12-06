/*VALDBUMEA.I*/

 
   DO TRANSACTION:
      /*
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART =  {WWWGURU11.I}
      valdbtemp.WWWSTART10 = {WWWGURU10.I}
      valdbtemp.FORETAG = "UMEA"
      valdbtemp.GFORETAG = "UMEA"
      valdbtemp.DBNAMN = "UMEA"
      valdbtemp.DBCON = "-db umea -H balder -S 2721 -N tcp"
      valdbtemp.DBPLATS = "D:\DELAD\PRO9S\DB\"
      valdbtemp.DBCACHE = ""
      valdbtemp.ORDNING = 1      
      valdbtemp.APPCON = "-AppService APPUMEA -H balder -S 2723"
      valdbtemp.VALDB = "Umeå Energi".
      valdbtemp.WWWFTP = FALSE.
      */
   
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART =  {WWWGURU11.I}
      valdbtemp.WWWSTART10 = {WWWGURU10.I}
      valdbtemp.FORETAG = "UMEA"
      valdbtemp.GFORETAG = "UMEA"
      valdbtemp.DBNAMN = "UMEA"
      valdbtemp.DBCON = "-db umea -H INFRAGURU -S 2721 -N tcp"
      valdbtemp.DBPLATS = "D:\DELAD\PRO9S\DB\"
      valdbtemp.DBCACHE = ""
      valdbtemp.ORDNING = 1      
      valdbtemp.APPCON = "-AppService APPUMEA -H INFRAGURU -S 2723"
      valdbtemp.VALDB = "Umeå Energi Infra".
      valdbtemp.WWWFTP = FALSE.
   END.
  
   
