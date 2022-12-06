 FOR EACH kostreg WHERE KOSTREG.BOKKONTO = "68400" OR
                       KOSTREG.BOKKONTO = "65922" OR
                       KOSTREG.BOKKONTO = "68300".
    IF kostreg.aonr BEGINS "X" THEN . 
    ELSE DO:
            kostreg.aonr = "xxx" + kostreg.aonr.
             DISP        kostreg.aonr FORMAT "X(20)".
    END.
    
   
      

   END.
   
   


FOR EACH kostreg WHERE KOSTREG.INKOMST NE 0 AND KOSTREG.BETDATUM >= 09/08/2004:
   DISPLAY            KOSTREG.INKOMST FORMAT "->>>>>>>>>>>>>9".
   ASSIGN KOSTREG.INKOMST = KOSTREG.INKOMST * -1.
END.
