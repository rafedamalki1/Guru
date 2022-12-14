 
 /*------------------------------------------------------------------------
    File        : StaticMethods
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Tue Feb 28 09:56:17 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS Guru.StaticMethods: 
   METHOD STATIC PUBLIC System.Drawing.Color DotNetColor(INPUT profg AS INTEGER):
      DEFINE VARIABLE exfg AS System.Drawing.Color NO-UNDO.

     /* exfg = profg.*/
      IF profg = 0 THEN exfg = System.Drawing.Color:White.
      IF profg = 1 THEN exfg = System.Drawing.Color:DarkBlue. /*M?RKBL?*/
      IF profg = 2 THEN exfg = System.Drawing.Color:Green. /*GR?N AONR*/
      IF profg = 3 THEN exfg = System.Drawing.Color:Turquoise. /*BL?GR?N*/
      IF profg = 4 THEN exfg = System.Drawing.Color:DarkRed.  /*M?RKR?D PLANR*/
      IF profg = 5 THEN exfg = System.Drawing.Color:Purple.  /*lila*/
      IF profg = 6 THEN exfg = System.Drawing.Color:Olive.  /*gr?nbrun*/
      IF profg = 7 THEN exfg = System.Drawing.Color:DarkGray.  /*m?rkgr?*/
      IF profg = 8 THEN exfg = System.Drawing.Color:Gray. /*GR?*/
      IF profg = 9 THEN exfg = System.Drawing.Color:Blue.  /*BL?*/
      IF profg = 10 THEN exfg = System.Drawing.Color:LightGreen. /*LJUSGR?N*/
      IF profg = 11 THEN exfg = System.Drawing.Color:LightBlue. /*LJUSBL?*/
      IF profg = 12 THEN exfg = System.Drawing.Color:Red. /*R?D*/
      IF profg = 13 THEN exfg = System.Drawing.Color:Pink. /*ROSA*/
      IF profg = 14 THEN exfg = System.Drawing.Color:Yellow. /*GUL*/
      IF profg = 15 THEN exfg = System.Drawing.Color:LightYellow. /*LJUSGUL*/
      IF profg = 16 THEN exfg = System.Drawing.Color:LightCoral. /*LJUSGUL*/
      
      RETURN exfg.
   END METHOD.
END CLASS.