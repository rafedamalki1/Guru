 
 /*------------------------------------------------------------------------
    File        : MACCHECK
    Purpose     : 
    Syntax      : vad har du för macadress(.net)
    Description : 
    Author(s)   : elprs
    Created     : Thu Sep 15 15:21:22 CEST 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.

CLASS Helpers.MACcheck INHERITS UserControl:    
   
CONSTRUCTOR PUBLIC MACcheck(): 

END.

METHOD PUBLIC CHARACTER checkMac():
   DEFINE VARIABLE nifs AS System.Array NO-UNDO.
   DEFINE VARIABLE nif AS System.Net.NetworkInformation.NetworkInterface NO-UNDO.
   DEFINE VARIABLE pAdress AS System.Net.NetworkInformation.PhysicalAddress NO-UNDO.
   DEFINE VARIABLE prop AS System.Net.NetworkInformation.IPInterfaceProperties NO-UNDO.
   DEFINE VARIABLE macAdress AS CHARACTER NO-UNDO.
    
   nifs = System.Net.NetworkInformation.NetworkInterface:GetAllNetworkInterfaces().
    
   {WALLMAN\Foreach.i System.Object oObject in nifs}
      macAdress = "Fel".
      nif = CAST(oObject, System.Net.NetworkInformation.NetworkInterface).
      prop = nif:GetIpProperties().
      pAdress = nif:GetPhysicalAddress().
      macAdress = pAdress:ToString().
      IF (macAdress NE "Fel") THEN RETURN macAdress.
   END.
    
   RETURN macAdress.
END METHOD.

END CLASS.