DEFINE SHARED VARIABLE globforetag AS CHARACTER NO-UNDO.
 
 
DEFINE VARIABLE appfel AS LOGICAL NO-UNDO.
{SOKDEF.I}
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 1
      soktemp.SOKCHAR[1] = globforetag
      soktemp.SOKCHAR[2] = PERSONALTAB.PERSONALKOD
      soktemp.SOKCHAR[3] = 'RESTID...'
      soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL 
      soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
      {SOKANROP.I}
      soktemp.SOKDECI[1]


         {SOKSTART.I}
         ASSIGN
         soktemp.SOKVAL = 1
         soktemp.SOKCHAR[1] = globforetag
         soktemp.SOKCHAR[2] = PERSONALTAB.PERSONALKOD
         soktemp.SOKCHAR[3] = AONRTAB.PRISTYP
         soktemp.SOKCHAR[4] = PERSONALTAB.BEFATTNING 
         soktemp.SOKDATE[1] = TODAY.
         {SOKANROP.I}
            /*PRISFOR*/
      IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
      END.
      ELSE DO:
         {SOKSTART.I}
         ASSIGN
         soktemp.SOKVAL = 1
         soktemp.SOKCHAR[1] = globforetag
         soktemp.SOKCHAR[2] = PERSONALTAB.PERSONALKOD
         soktemp.SOKCHAR[3] = AONRTAB.PRISTYP
         soktemp.SOKCHAR[4] = PERSONALTAB.BEFATTNING 
         soktemp.SOKDATE[1] = TODAY.
         {SOKANROP.I}
         ASSIGN
         FILL-IN-PRIS = soktemp.SOKDECI[1].
         DISPLAY FILL-IN-PRIS WITH FRAME {&FRAME-NAME}.
      END.  
IF TIDREGITAB.OVERTIDTILL = "" THEN tidbuff.OVERTIDTILL = soktemp.SOKCHAR[4].    



           IF tidbuff.OVERTIDTILL = "" THEN tidbuff.OVERTIDTILL = PERSONALTAB.BEFATTNING. 
           tidbuff.OVERTIDTILL = TIDREGITAB.OVERTIDTILL
         FIND FIRST TIMKOSTNADSTAB WHERE TIMKOSTNADSTAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
TIMKOSTNADSTAB.PRISTYP = AONRTAB.PRISTYP NO-LOCK NO-ERROR.


           FIND FIRST TIMKOSTNADSTAB WHERE TIMKOSTNADSTAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIMKOSTNADSTAB.PRISTYP = AONRTAB.PRISTYP USE-INDEX PRISPERS NO-LOCK NO-ERROR.          


           IF soktemp.SOKDECI[1] > 0 THEN tidbuff.PRIS = soktemp.SOKDECI[1].


           /*PRISFOR*/
                  IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
                     {SOKSTART.I}
                     ASSIGN
                     soktemp.SOKVAL = 1
                     soktemp.SOKCHAR[1] = globforetag
                     soktemp.SOKCHAR[2] = TIDREGITAB.PERSONALKOD
                     soktemp.SOKCHAR[3] = FILL-IN-PRISTYP
                     soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL 
                     soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
                     {SOKANROP.I}
                     IF TIDREGITAB.OVERTIDTILL = "" THEN tidbuff.OVERTIDTILL = soktemp.SOKCHAR[4].    
                     FILL-IN-PRIS = soktemp.SOKDECI[1].
                  END.
