
/*------------------------------------------------------------------------
    File        :HKEYLOCAMACHINE.I
   */ 

    /*Anders Olsson Elpool i Ume? AB  2 feb 2017 14:18:58 
   HKEY_LOCAL_MACHINE
   ?r progre 
   */
   
    
   IF ERROR-STATUS:NUM-MESSAGES > 0 THEN RUN HKEYADMPER10.p (OUTPUT hkeyvar).
   LOAD "SOFTWARE\PSC\WEBCLIENT\" + PROVERSION  BASE-KEY hkeyvar NO-ERROR.
    
   
     
