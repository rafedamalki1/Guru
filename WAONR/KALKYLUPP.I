
/*------------------------------------------------------------------------
    File        : KALKYLUPP.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : elpao
    Created     : Wed Feb 27 16:34:51 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/
{KALKYLKAT.I}
{KALKYLPRODATA.i}
{KALKSUMMTEMP.I}

PROCEDURE kalkupp_UI :
   DEFINE INPUT  PARAMETER kalknr AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER omr AS CHARACTER NO-UNDO.
   DEFINE VARIABLE KalkClasserStart AS HANDLE NO-UNDO.
   IF NOT VALID-HANDLE(KalkClasserStart) THEN RUN Modules\Kalkyl\KalkClasserStart.P PERSISTENT SET KalkClasserStart.   
   
   RUN KalkyldbApp_UI IN KalkClasserStart (INPUT kalknr, INPUT omr, OUTPUT TABLE kalkantal,OUTPUT TABLE kalkkostnad,OUTPUT TABLE kalkylprisertt, OUTPUT TABLE kalkvisningtt, OUTPUT DATASET KalkylDS).
   IF VALID-HANDLE(KalkClasserStart) THEN DELETE PROCEDURE KalkClasserStart NO-ERROR.
   FIND FIRST kalkhuvtt WHERE kalkhuvtt.KALKNR = kalknr AND kalkhuvtt.OMRADE = omr NO-LOCK NO-ERROR.
END PROCEDURE. 
