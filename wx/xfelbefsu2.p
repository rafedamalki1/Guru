OUTPUT TO C:\NOLLPRBEF.TXT.
    FOR EACH TIDREGITAB WHERE TIDREGITAB.DATUM GE 01/01/10 AND TIDREGITAB.TIDLOG = TRUE NO-LOCK:
    IF TIDREGITAB.PRISTYP = "FR�NVARO." THEN.
    ELSE IF TIDREGITAB.PRISTYP = "EJ.KOSTN." THEN.
    ELSE IF TIDREGITAB.PRISTYP = "RESTID..." THEN.
    ELSE IF PRIS = 0 THEN DO:
        FIND FIRST personaltab WHERE personaltab.personalkod = TIDREGITAB.PERSONALKOD NO-LOCK NO-ERROR.
        IF TIDREGITAB.OVERTIDTILL NE personaltab.befattning  THEN 
        DISP TIDREGITAB.PERSONALKOD TIDREGITAB.DATUM TIDREGITAB.OVERTIDTILL FORMAT "X(12)"  TIDREGITAB.PRIS /*TIDREGITAB.PRISTYP*/ TIDREGITAB.AONR TIDREGITAB.TOTALT personaltab.befattning .
    END.
END.