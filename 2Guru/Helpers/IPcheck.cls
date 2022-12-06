 
 /*------------------------------------------------------------------------
    File        : IPCHECK
    Purpose     : 
    Syntax      : vad har du för ip(.net)
    Description : 
    Author(s)   : elprs
    Created     : Thu Sep 10 15:21:22 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

USING Progress.Windows.UserControl.

CLASS Helpers.IPcheck INHERITS UserControl:    
   DEFINE PRIVATE VARIABLE ipEntry AS System.Net.IPHostEntry NO-UNDO.
   DEFINE PRIVATE VARIABLE iPAddress AS System.Net.IPAddress NO-UNDO.
   DEFINE PRIVATE VARIABLE strHostName AS CHARACTER NO-UNDO.
   
   CONSTRUCTOR PUBLIC IPcheck(): 

   END.

   METHOD PUBLIC CHARACTER checkIp():
     
     DEFINE VARIABLE natvarkstyp AS CHARACTER NO-UNDO.
     DEFINE VARIABLE dinIp AS CHARACTER NO-UNDO.
     
     strHostName = System.Net.Dns:GetHostName().
     ipEntry = System.Net.Dns:GetHostEntry(strHostName).
    
     {WALLMAN\Foreach.i System.Object oObject in ipEntry:AddressList}
         iPAddress = CAST(oObject, System.Net.IPAddress).
         natvarkstyp = iPAddress:AddressFamily:ToString().
         
                  
         IF natvarkstyp EQ "InterNetwork" THEN DO: 
            dinIp = iPAddress:ToString().
            RETURN dinIp.   
         END.
      END.
      RETURN dinIp.
     

   END METHOD.

   
END CLASS.