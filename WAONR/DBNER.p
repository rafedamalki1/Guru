/*DBNER.P*/
MESSAGE "VILL DU TA BORT ANVÄNDARE ?" 
VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val1 AS LOGICAL.
CASE val1:
   WHEN TRUE THEN DO:
      DOS \\pc112\delad\PRO8\GURU\DBNER.BAT.
   /*   DOS \\pc112\delad\PRO8\DLC\bin\_mprshut.exe -db RT8 -H pc112 -S elpoolserver  -N tcp.              
   */
   END.            
END CASE. 
QUIT.   
     
  
