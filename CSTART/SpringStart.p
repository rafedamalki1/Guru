
/*------------------------------------------------------------------------
    File        : SpringStart.p
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : 
    Created     : Thu Oct 20 14:50:09 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/
 
DEFINE INPUT  PARAMETER Guruforetag AS CHARACTER NO-UNDO.

DEFINE VARIABLE GuruAnvandare AS CHARACTER NO-UNDO.
DEFINE VARIABLE GuruLosen AS CHARACTER NO-UNDO.

DEFINE VARIABLE meddelandevar AS CHARACTER NO-UNDO.
DEFINE VARIABLE outdatornamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE datornamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE datoruser AS CHARACTER NO-UNDO.
DEFINE VARIABLE MacAdd AS CHARACTER NO-UNDO.

/*ger dig dataor user*/
 Guru.Konstanter:AppSpringSet = "". 
RUN INLOAPI.P (OUTPUT datoruser, OUTPUT outdatornamn).
ASSIGN
datoruser = TRIM(datoruser)
datornamn = TRIM(outdatornamn).

{Computer_LanIP.I}
RUN ReadMac_UI.
RUN konstanter_UI.  



PROCEDURE ReadMac_UI :
   DEFINE VARIABLE longhelp AS LONGCHAR NO-UNDO.
   DEFINE VARIABLE st AS INTEGER NO-UNDO.
   DEFINE VARIABLE sl AS INTEGER NO-UNDO.
   DEFINE VARIABLE macfile AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mackommando AS CHARACTER NO-UNDO.
   DEFINE VARIABLE MACcheck  AS Helpers.MACcheck              NO-UNDO.
   MACcheck = NEW Helpers.MACcheck(). 
   MacAdd = MACcheck:checkMac().
  
 
END PROCEDURE.

PROCEDURE konstanter_UI :
   {AppSpringSetInfo.I} 
   ASSIGN
   Guru.Konstanter:AppSpringSet[1]  = Guruforetag    
   Guru.Konstanter:AppSpringSet[3]  = Computer_LanIP
   Guru.Konstanter:AppSpringSet[4]  = MacAdd
   Guru.Konstanter:AppSpringSet[5]  = datoruser
   Guru.Konstanter:AppSpringSet[6]  = GuruAnvandare
   Guru.Konstanter:AppSpringSet[7]  = GuruLosen.  
   Guru.Konstanter:AppSpringSet[8]  = "".    /*"nyttlösen"*/
   Guru.Konstanter:AppSpringSet[9]  = datornamn.
   
   
END PROCEDURE.
