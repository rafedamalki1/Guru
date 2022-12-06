 
 /*------------------------------------------------------------------------
    File        : drivecheck
    Purpose     : 
    Syntax      : diskkoll du skickar in namn på drivern och får tillbaka enheten
    Description : 
    Author(s)   : elprs
    Created     : Thu Sep 10 15:21:22 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

USING Progress.Windows.UserControl.

CLASS Helpers.Drivecheck INHERITS UserControl:    

   DEFINE PRIVATE VARIABLE driveInfoArray AS System.Array NO-UNDO.
   
   CONSTRUCTOR PUBLIC drivecheck(): 
      THIS-OBJECT:driveInfoArray = System.IO.DriveInfo:GetDrives().
   END.

   METHOD PUBLIC CHARACTER driveLabel(driveLabel AS CHARACTER):
     DEFINE VARIABLE driveInfo AS System.IO.DriveInfo NO-UNDO.
     DEFINE VARIABLE driveName AS CHARACTER NO-UNDO.
     
     {WALLMAN\Foreach.i System.Object oObject in THIS-OBJECT:driveInfoArray}
         driveInfo = CAST(oObject, System.IO.DriveInfo).
         IF driveInfo:IsReady THEN DO:
            IF driveInfo:VolumeLabel EQ driveLabel THEN driveName = driveInfo:Name.
         END.
      END.
      RETURN driveName.
   END METHOD.

   
END CLASS.

/*
        public static bool CompareString(string a, int ct, string b)
        {
            a = a.Trim().ToLower();
            b = b.Trim().ToLower();
            switch (ct)
            {
                case CT_EQUALS:
                    if (a.Equals(b)) { return true; }
                break;
                case CT_NOTEQUALS:
                    if (!a.Equals(b)) { return true; }
                break;
                case CT_BEGINS:
                    if (a.StartsWith(b)) { return true; }
                break;
                case CT_ENDS:
                    if (a.EndsWith(b)) { return true; }
                break;
                case CT_CONTAINS:
                    if (a.IndexOf(b) != -1) { return true; }
                break;
            }
            return false;

        }*/