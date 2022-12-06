
   
 /*xonemtrlbort.p*/
  
 
 FOR EACH mtrl WHERE MTRL.KALKNR = 0 EXCLUSIVE-LOCK:
    IF MTRL.LEVKOD BEGINS "99" THEN .
    ELSE IF MTRL.LEVKOD = "24834" THEN . /*GB Gravyr*/ 
    ELSE IF MTRL.LEVKOD = "7" THEN . /*GB Gravyr innan*/ 
    ELSE DELETE MTRL.
 END.   
 
    
