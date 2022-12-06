 OPEN QUERY aonrkq FOR EACH AONRKONTkod NO-LOCK.
 GET FIRST aonrkq no-LOCK.
 IF AVAILABLE AONRKONT THEN do:
    find first AONRTAB where AONRTAB.AONR = AONRKONTkod.AONR AND 
    AONRTAB.DELNR = AONRKONTkod.DELNR no-lock no-error.
    if not avAilable aonrtab THEN DO TRANSACTION:
      GET CURRENT aonrkq EXCLUSIVE-LOCK.
      DELETE AONRKONTkod. 
   END.         
END.
REPEAT:  
   GET NEXT aonrkq NO-LOCK.
   IF AVAILABLE AONRKONT THEN do:  
      find first AONRTAB where AONRTAB.AONR = AONRKONTkod.AONR AND 
       AONRTAB.DELNR = AONRKONTkod.DELNR no-lock no-error.
       if not avAilable  aonrtab THEN DO TRANSACTION:
          GET CURRENT aonrkq EXCLUSIVE-LOCK.
          DELETE AONRKONTkod. 
       END.         
    END.
   ELSE LEAVE.      
END.

