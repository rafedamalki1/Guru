
/*------------------------------------------------------------------------
    File        : GLOBFONSTER.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Oct 02 11:50:06 CEST 2015
    Notes       :
       
        ASSIGN
      inextradatatemp.SOKINT[5] = Guru.Konstanter:globDefaultstorb
      inextradatatemp.SOKINT[6] =  Guru.Konstanter:globDefaultstorh.
      RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).
      IF Guru.Konstanter:appcon THEN DO:  
          RUN ANVAPPT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT  (INPUT Guru.Konstanter:globanv,INPUT globstorh, INPUT Guru.Konstanter:globstorb).                         
      END.
      ELSE DO:
         RUN ANVAPPT.P (INPUT Guru.Konstanter:globanv,INPUT globstorH, INPUT Guru.Konstanter:globstorb).
      END. 
       
  ----------------------------------------------------------------------*/

      Guru.Konstanter:globDefaultstorb = extradatatemp.SOKINT[5].
      Guru.Konstanter:globDefaultstorh = extradatatemp.SOKINT[6].
      Guru.Konstanter:globstorb = storkollbredd.
      Guru.Konstanter:globstorh = storkollhojd.
      /*OM DU TILLFÄLLIGT KÖR PÅ MINDRE UPPLÖSNING*/
      IF Guru.Konstanter:globDefaultstorh > SESSION:WORK-AREA-HEIGHT-PIXELS - 50 THEN DO:
         Guru.Konstanter:globDefaultstorh = SESSION:WORK-AREA-HEIGHT-PIXELS - 50.
         IF Guru.Konstanter:globDefaultstorh <= 682 THEN  Guru.Konstanter:globDefaultstorh = 683.
      END.   
      IF Guru.Konstanter:globDefaultstorb > SESSION:WORK-AREA-WIDTH-PIXELS  - 30 THEN DO:
         Guru.Konstanter:globDefaultstorb = SESSION:WORK-AREA-WIDTH-PIXELS  - 30.
         IF Guru.Konstanter:globDefaultstorb <= 1000 THEN Guru.Konstanter:globDefaultstorb = 1001.
      END.    
      /*NÄR DU STARTAR FÖRSTA GÅNGEN FÅR DU NY DEFAULT*/
      IF Guru.Konstanter:globDefaultstorh <= 682 THEN DO:
         Guru.Konstanter:globDefaultstorh = SESSION:WORK-AREA-HEIGHT-PIXELS - 150.
         IF Guru.Konstanter:globDefaultstorh <= 682 THEN  Guru.Konstanter:globDefaultstorh = 683.
         extradatatemp.SOKINT[6] =  Guru.Konstanter:globDefaultstorh.
         RUN extraspar_UI IN edataapph (INPUT TABLE extradatatemp).
         /*
         Guru.Konstanter:globDefaultstorh = 682.
         */
      END.   
      IF Guru.Konstanter:globDefaultstorb <= 1000 THEN DO:
         Guru.Konstanter:globDefaultstorb = SESSION:WORK-AREA-WIDTH-PIXELS  - 130.
         IF Guru.Konstanter:globDefaultstorb <= 1000 THEN Guru.Konstanter:globDefaultstorb = 1001.
         extradatatemp.SOKINT[5] = Guru.Konstanter:globDefaultstorb.
         RUN extraspar_UI IN edataapph (INPUT TABLE extradatatemp).
         /*
         Guru.Konstanter:globDefaultstorb = 1000.
         */
       END.
      /*OM DU TILLFÄLLIGT KÖR PÅ MINDRE UPPLÖSNING*/ 
      IF Guru.Konstanter:globstorh > SESSION:WORK-AREA-HEIGHT-PIXELS - 50 THEN DO:
         Guru.Konstanter:globstorh = SESSION:WORK-AREA-HEIGHT-PIXELS - 50.
         IF Guru.Konstanter:globstorh <= 682 THEN  Guru.Konstanter:globstorh = 683.
      END.
      IF Guru.Konstanter:globstorb > SESSION:WORK-AREA-WIDTH-PIXELS  - 30 THEN DO:
         Guru.Konstanter:globstorb = SESSION:WORK-AREA-WIDTH-PIXELS  - 30.
         IF Guru.Konstanter:globstorb <= 1000 THEN Guru.Konstanter:globstorb = 1001.
      END.   
      /*NÄR DU STARTAR FÖRSTA GÅNGEN FÅR DU NY MAXVÄRDE*/
      IF Guru.Konstanter:globstorh <= 682 THEN DO:
         Guru.Konstanter:globstorh = SESSION:WORK-AREA-HEIGHT-PIXELS - 150.
         IF Guru.Konstanter:globstorh <= 682 THEN  Guru.Konstanter:globstorh = 683.
         IF Guru.Konstanter:appcon THEN DO:  
             RUN ANVAPPT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT  (INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globstorh, INPUT Guru.Konstanter:globstorb).                         
         END.
         ELSE DO:
            RUN ANVAPPT.P (INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globstorh, INPUT Guru.Konstanter:globstorb).
         END. 
      END.   
      IF Guru.Konstanter:globstorb <= 1000 THEN DO:
         Guru.Konstanter:globstorb = SESSION:WORK-AREA-WIDTH-PIXELS  - 130.
         IF Guru.Konstanter:globstorb <= 1000 THEN Guru.Konstanter:globstorb = 1001.
         IF Guru.Konstanter:appcon THEN DO:  
             RUN ANVAPPT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT  (INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globstorh, INPUT Guru.Konstanter:globstorb).                         
         END.
         ELSE DO:
            RUN ANVAPPT.P (INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globstorh, INPUT Guru.Konstanter:globstorb).
         END.
         /*
         Guru.Konstanter:globDefaultstorb = 1000.
         */
       END.
      /*
      IF Guru.Konstanter:globstorh < 682 THEN Guru.Konstanter:globstorh = 682.
      IF Guru.Konstanter:globstorb < 1000 THEN Guru.Konstanter:globstorb = 1000.
      */