/*VALDBOVIK.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "OVIK"
         valdbtemp.GFORETAG = "OVIK"
         valdbtemp.DBNAMN = "OVIK"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db OVIK -H " + {egnadb.I} + " -S 2783 -N TCP"
         valdbtemp.APPCON = "-AppService APPSINGEL -H " + {egnaapp.I} + " -S 2665"                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Övik Energi AB".
         valdbtemp.WWWFTP = TRUE. 
         IF PROVERSION BEGINS "11.2" THEN.
         ELSE IF SESSION:CLIENT-TYPE = "xWEBCLIENT" THEN DO:
            valdbtemp.APPCON = "-AppService AppEgen -H " + {egna.I} + {guruonweb.i} + " " + {ssl.I}. 
         END.                                               
   END.
  