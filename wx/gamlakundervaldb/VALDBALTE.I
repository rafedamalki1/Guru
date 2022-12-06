
/*VALDBALTE.I*/

 

   DO TRANSACTION:
      
      
      
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART =  {WWWGURU11.I}
      valdbtemp.WWWSTART10 = {WWWGURU10.I}
      valdbtemp.FORETAG = "ALTE"
      valdbtemp.GFORETAG = "ALTE"
      valdbtemp.DBNAMN = "ALTE" 
      valdbtemp.DBCON = "-db ALTE -H " + {egnadb.I} + " -S 2771 -N tcp"
      valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
      valdbtemp.ORDNING = 1       
      valdbtemp.DBCACHE = ""      
      valdbtemp.APPCON = "-AppService APPSINGEL -H " + {egnaapp.I} + " -S 2665"
      valdbtemp.VALDB = "Alteco Elkonsult AB-ÅF-Industry AB".
      valdbtemp.WWWFTP = TRUE. 
      IF PROVERSION BEGINS "11.2" THEN.
      ELSE IF SESSION:CLIENT-TYPE = "xWEBCLIENT" THEN DO:
         valdbtemp.APPCON = "-AppService AppEgen -H " + {egna.I} + {guruonweb.i} + " " + {ssl.I}. 
      END.  
     
   END.
