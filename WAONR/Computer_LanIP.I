
/*------------------------------------------------------------------------
    File        : Computer_LanIP.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu May 14 12:39:40 CEST 2020
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE VARIABLE IPcheck  AS Helpers.IPcheck              NO-UNDO.
DEFINE VARIABLE Computer_LanIP AS CHARACTER NO-UNDO.

IF IPcheck = ? THEN IPcheck = NEW Helpers.IPcheck().
Computer_LanIP = Ipcheck:checkIp().
Guru.Konstanter:AppSpringSet[3]  = Computer_LanIP.
