/*ANVSKAP.P*/             
&Scoped-define NEW   
&Scoped-define SHARED
{ANVPERS.I}

DEFINE INPUT PARAMETER vadgora AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER vem AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR anvandartemp.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR personaltemp.
IF vadgora = 1 OR vadgora = 3 OR vadgora = 4 OR vadgora = 5 THEN DO:
   IF vem = ""  THEN DO:
      EMPTY TEMP-TABLE anvandartemp NO-ERROR. 
      FOR EACH ANVANDARE NO-LOCK:
         CREATE anvandartemp.
         BUFFER-COPY ANVANDARE TO anvandartemp. 
         Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + ANVANDARE.PERSONALKOD.      
      END.
   END.
   ELSE DO:
      FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = vem NO-LOCK NO-ERROR.
      IF AVAILABLE ANVANDARE THEN DO:
         FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = vem NO-ERROR.
         IF NOT AVAILABLE anvandartemp THEN CREATE anvandartemp.
         BUFFER-COPY ANVANDARE TO anvandartemp.
         Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + ANVANDARE.PERSONALKOD.
      END.
      IF NOT AVAILABLE ANVANDARE THEN DO:
         FIND FIRST ANVANDARE WHERE ANVANDARE.PERSONALKOD = vem NO-LOCK NO-ERROR.
         IF AVAILABLE ANVANDARE THEN DO:
            FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = vem NO-ERROR.
            IF NOT AVAILABLE anvandartemp THEN CREATE anvandartemp.
            BUFFER-COPY ANVANDARE TO anvandartemp.
            Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + ANVANDARE.PERSONALKOD.
         END.
      END.
   END.
END.
IF vadgora = 2 OR vadgora = 3 THEN DO:
   IF vem = ""  THEN DO:
      EMPTY TEMP-TABLE personaltemp NO-ERROR. 
      
      FOR EACH PERSONALTAB WHERE PERSONALTAB.AKTIV = TRUE NO-LOCK:
         CREATE personaltemp.
         BUFFER-COPY PERSONALTAB TO personaltemp.  
         Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.       
      END.
   END.
   ELSE DO:
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = vem AND PERSONALTAB.AKTIV = TRUE
      NO-LOCK NO-ERROR.
      IF AVAILABLE PERSONALTAB THEN DO:
         FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = vem NO-ERROR.
         IF NOT AVAILABLE personaltemp THEN CREATE personaltemp.
         BUFFER-COPY PERSONALTAB TO personaltemp.
         Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
      END.
   END.
END.
IF vadgora = 4 THEN DO:
   IF vem = ""  THEN DO:
      EMPTY TEMP-TABLE personaltemp NO-ERROR. 
      FOR EACH PERSONALTAB NO-LOCK:
         CREATE personaltemp.
         BUFFER-COPY PERSONALTAB TO personaltemp.      
         Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
      END.
   END.
   ELSE DO:
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = vem 
      NO-LOCK NO-ERROR.
      IF AVAILABLE PERSONALTAB THEN DO:
         FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = vem NO-ERROR.
         IF NOT AVAILABLE personaltemp THEN CREATE personaltemp.
         BUFFER-COPY PERSONALTAB TO personaltemp.
         Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
      END.
   END.
END.

{GDPRLOGGCLIENT.I}
