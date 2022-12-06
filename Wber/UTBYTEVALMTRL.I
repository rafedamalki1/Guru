
/*------------------------------------------------------------------------
    File        : UTBYTEVALMTRL.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed Nov 03 13:08:39 CET 2021
    Notes       :
  ----------------------------------------------------------------------*/

   EMPTY TEMP-TABLE elist_mtrl NO-ERROR.
   IF Guru.Konstanter:appcon THEN DO:                    
      
      IF list_mtrl.LEVKOD =  Guru.SharedVariable:ValdmtrlLeverantor AND Guru.GlobalaVariabler:KundListId = 0 THEN DO:
         /*Lena Olsson Elpool i Umeå AB  19 nov 2021 16:30:39 
           byter  mtrl om lev = vald lev och ingen kundlista */                  
         RUN UTBYTESLISTA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT "GbNb", INPUT  Guru.GlobalaVariabler:KundListId,INPUT list_mtrl.LEVKOD,INPUT list_mtrl.ENR,INPUT Guru.SharedVariable:ValdmtrlLeverantor, OUTPUT mtrlrowID).
         RUN UTBYTESVALBER.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT mtrlrowID,OUTPUT TABLE elist_mtrl). 
         
      END.      
      ELSE DO:
         RUN UTBYTESLISTA.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT "UPPERSATT", INPUT Guru.GlobalaVariabler:KundListId,INPUT list_mtrl.LEVKOD,INPUT list_mtrl.ENR,INPUT Guru.SharedVariable:ValdmtrlLeverantor, OUTPUT mtrlrowID).
          RUN UTBYTESVALBER.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT mtrlrowID,OUTPUT TABLE elist_mtrl).  
       END.
    END.
    ELSE DO:
      IF list_mtrl.LEVKOD =  Guru.SharedVariable:ValdmtrlLeverantor AND Guru.GlobalaVariabler:KundListId = 0 THEN DO:
         /*Lena Olsson Elpool i Umeå AB  19 nov 2021 16:30:39 
           byter mtrl om lev = vald lev och ingen kundlista */                  
         RUN UTBYTESLISTA.P (INPUT "GbNb", INPUT  Guru.GlobalaVariabler:KundListId,INPUT list_mtrl.LEVKOD,INPUT list_mtrl.ENR,INPUT Guru.SharedVariable:ValdmtrlLeverantor, OUTPUT mtrlrowID).
         RUN UTBYTESVALBER.P  (INPUT mtrlrowID,OUTPUT TABLE elist_mtrl).
      END.      
      ELSE DO:
          RUN UTBYTESLISTA.P   
          (INPUT "UPPERSATT", INPUT Guru.GlobalaVariabler:KundListId,INPUT list_mtrl.LEVKOD,INPUT list_mtrl.ENR,INPUT Guru.SharedVariable:ValdmtrlLeverantor, OUTPUT mtrlrowID).
          RUN UTBYTESVALBER.P (INPUT mtrlrowID,OUTPUT TABLE elist_mtrl).  
       END.
    END. 
    FIND FIRST elist_mtrl WHERE NO-LOCK NO-ERROR.
    IF AVAILABLE elist_mtrl THEN DO:
       mtrlrowID = ROWID(list_mtrl).
       
       IF kon_val.GRUPP = 0 OR LOOKUP(STRING(kon_val.GRUPP), Guru.Konstanter:varforetypchar[5]) > 0 THEN DO:       
          FIND FIRST list_mtrl WHERE list_mtrl.NUM = kon_val.NUM AND list_mtrl.SKAPNUM = kon_val.SKAPNUM AND
          list_mtrl.ENR = elist_mtrl.ENR AND list_mtrl.LEVKOD = elist_mtrl.LEVKOD AND
          ROWID(list_mtrl) NE mtrlrowID
          NO-LOCK NO-ERROR.
       END.
       ELSE DO:   
          FIND FIRST list_mtrl WHERE list_mtrl.NUM = kon_val.NUM AND 
          list_mtrl.ENR = elist_mtrl.ENR AND list_mtrl.LEVKOD = elist_mtrl.LEVKOD AND
          ROWID(list_mtrl) NE mtrlrowID
          NO-LOCK NO-ERROR.
       END.   
       IF AVAILABLE list_mtrl THEN DO:
          mtrl_rowid = ROWID(list_mtrl).
          FIND FIRST list_mtrl WHERE ROWID(list_mtrl) = mtrlrowID NO-LOCK NO-ERROR.
          DELETE list_mtrl.
          FIND FIRST list_mtrl WHERE ROWID(list_mtrl) = mtrl_rowid NO-LOCK NO-ERROR.
       END. 
       ELSE DO:  
          FIND FIRST list_mtrl WHERE ROWID(list_mtrl) = mtrl_rowid NO-LOCK NO-ERROR.
          ASSIGN
          list_mtrl.ENR       = elist_mtrl.ENR
          list_mtrl.BENAMNING = elist_mtrl.BENAMNING
          list_mtrl.ENHET     = elist_mtrl.ENHET
          list_mtrl.PRIS      = elist_mtrl.PRIS
          list_mtrl.LEVKOD    = elist_mtrl.LEVKOD.
       END.
       EMPTY TEMP-TABLE elist_mtrl NO-ERROR. 
    END.   
        
