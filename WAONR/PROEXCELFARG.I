/*
               KSV Editor
    Copyright: (C) 2000-2001 Serguey Klimoff (bulkl0DD)
     Filename: PROEXCELFARG.I
      Comment: <comment>
   Parameters:
         Uses:progress f?rger excel f?rger 
      Used by:
      Created: 2009.06.03 14:58 ELPAO   
     Modified: 
*/

PROCEDURE profexcelfarg_UI :
   DEFINE INPUT PARAMETER profg AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER exfg AS INTEGER NO-UNDO.
   exfg = profg.
   IF profg = 1 THEN exfg = 11. /*M?RKBL?*/
   IF profg = 2 THEN exfg = 10. /*GR?N AONR*/
   IF profg = 3 THEN exfg = 14. /*BL?GR?N*/
   IF profg = 4 THEN exfg = 9.  /*M?RKR?D PLANR*/
   IF profg = 5 THEN exfg = 54.  /*lila*/
   IF profg = 6 THEN exfg = 12.  /*gr?nbrun*/
   IF profg = 7 THEN exfg = 16.  /*m?rkgr?*/
   IF profg = 8 THEN exfg = 15. /*GR?*/
   IF profg = 9 THEN exfg = 5.  /*BL?*/
   IF profg = 10 THEN exfg = 4. /*LJUSGR?N*/
   IF profg = 11 THEN exfg = 8. /*LJUSBL?*/
   IF profg = 12 THEN exfg = 3. /*R?D*/
   IF profg = 13 THEN exfg = 7. /*ROSA*/
   IF profg = 14 THEN exfg = 6. /*GUL*/  
   IF profg = 15 THEN exfg = 0.  /*vit*/
END PROCEDURE.
