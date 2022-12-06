
/*------------------------------------------------------------------------
    File        : Stansinput.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri May 16 12:39:44 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/
&Scoped-define NEW NEW
{SOKDEF.I}
{STANSTIDTT.i}
{REGVAR.I}
DEFINE INPUT  PARAMETER pkod AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER regdatumin AS DATE NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR stanstid.
 
 
regdatum = regdatumin.
avdatum = regdatum + 1.
regdatum = avdatum - 30.
{SOKSTART.I}
ASSIGN
soktemp.SOKVAL = 13
soktemp.SOKCHAR[1] = pkod
soktemp.SOKLOG[1] = TRUE.
{SOKANROP.I}     
REPEAT:
   RUN REGVEC.P.
   {SLUTARBW.I}
   IF regslut NE regstart THEN DO:
      CREATE stanstid.
      ASSIGN   
      stanstid.PERSONALKOD = pkod  
      stanstid.ORDNING = 0
      stanstid.UTRYCKNING = FALSE
      stanstid.VECKOVILA = FALSE
      stanstid.DATUM = regdatum
      stanstid.OVERTIDUTTAG = soktemp.SOKCHAR[3]
      stanstid.START = regstart 
      stanstid.SLUT = regslut . 
      FIND FIRST BEFATTNINGSTAB WHERE BEFATTNINGSTAB.BEFATTNING = soktemp.SOKCHAR[4]
      USE-INDEX BEF NO-LOCK NO-ERROR.  
      ASSIGN
      stanstid.BEFATTNING = BEFATTNINGSTAB.BEFATTNING
      stanstid.VIBEFATTNING = BEFATTNINGSTAB.NAMN.
      FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD = stanstid.PERSONALKOD USE-INDEX
      PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE FLEXAVT AND FLEXAVT.FLEXTID = TRUE AND stanstid.OVERTIDUTTAG = "I" THEN 
      stanstid.OVERTIDUTTAG = "F".        
   END.
   regdatum = regdatum + 1.
   IF regdatum > avdatum THEN LEAVE. 
END.