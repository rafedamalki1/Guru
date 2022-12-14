/*NRAOSKAPA.I*/
/*
TABELLER
NRSERIE.       NRSERIE PROJINT1 PROJINT2 SISTPROJ
NRPREFISERIE.  IDNRP NRTEXT PREFIX
NRSTYRSERIE.   IDNRS NRTEXT PREFIX 
NRSERIEOMRADE. NRSERIE NRTYP OMRADE
OMRADETAB.     PROJINT1 PROJINT2 SISTPROJ
PROGRAM:1.
NRVALSTYR.W -  V?LJER NRPREFISERIE OCH NRSTYRSERIE
NRAOAPP.P      SKAPAR AONR HITTAR F?RSTA LEDIGA NUMMER VIA AONRTAB.
PROGRAM:2.
NRVALPRE.W -   V?LJER NRPREFISERIE
NRAOAPP.P      SKAPAR AONR HITTAR F?RSTA LEDIGA NUMMER VIA AONRTAB.
PROGRAM:3.
NRAOSKAP.W -   V?LJ OMR?DE - NRSERIEOMRADE - NRSERIE

F?R VISSA 
V?LJ OMR?DE K?R  NRVALSTYR.W OCH V?LJ STYRNING AV NRSERIEOMRADE - NRSERIE 
DVS SAMMA OMR?DE HAR FLERA SERIER.

OM INGEN NRSERIEOMRADE FINNS:
OM OMRADETAB.SISTPROJ = OMRADETAB.PROJINT1 AND OMRADETAB.SISTPROJ = OMRADETAB.PROJINT2
TA F?RSTA NRSERIE DVS ALLA HAR SAMMA SERIE.
ANNARS TA FR?N OMRADETAB.
*/
/*H?R SELN?T LENA*/
DEBUGGER:SET-BREAK().
IF Guru.Konstanter:varforetypval[45] = 1 THEN DO:
   /*prefix och styr*/
   RUN NRVALSNAT.W (OUTPUT aonrvar,OUTPUT aonrrec).
   IF musz = TRUE THEN DO:
      RETURN.
   END.
END.
ELSE IF  Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
   /*prefix och styr*/
   RUN NRVALSTYRN.W (OUTPUT aonrvar,OUTPUT aonrrec).
   IF musz = TRUE THEN DO:
      RETURN.
   END.
END.
 
ELSE DO:
   /*r?knare*/
   IF nyttaovar = TRUE AND plannrvar NE ? THEN DO:
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 51
      soktemp.SOKCHAR[1] = plannrvar
      soktemp.SOKINT[1] = artalvar.
      {SOKANROP.I}
      extraomr[1] = soktemp.SOKCHAR[1] NO-ERROR.
   END.
   extraomr[2] = Guru.Konstanter:globomr.
   IF extraomr[1] NE "" THEN Guru.Konstanter:globomr = extraomr[1].
   RUN NRAOSKAP.W (OUTPUT aonrvar,OUTPUT aonrrec).
   Guru.Konstanter:globomr = extraomr[2].
   IF musz = TRUE THEN DO:
      RETURN.
   END.
END.
