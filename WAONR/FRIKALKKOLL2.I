/*
               KSV Editor
    Copyright: (C) 2000-2001 Serguey Klimoff (bulkl0DD)
     Filename: FRIKALKKOLL2.I
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2009.02.05 09:48 ELPAO   
     Modified: 2009.02.05 16:23 ELPAO    
     Modified: 
*/

{EFASTTEMP.I}
{KALKBEFTEMP.I}
DEFINE TEMP-TABLE esummafastkalktemp NO-UNDO LIKE efastkalktemp.
DEFINE TEMP-TABLE tidutrubrik NO-UNDO 
   FIELD UT AS CHARACTER
   FIELD ORDNING AS INTEGER
   FIELD SUMMARAD AS LOGICAL
   INDEX ORDNING ORDNING.

PROCEDURE friupp_UI :
   IF uppvaltemp.DELNRKOLL = TRUE THEN DO:
      FIND FIRST kalksumsum WHERE kalksumsum.AONR = valdaao.AONR AND
      kalksumsum.DELNR = valdaao.DELNR NO-ERROR.
      IF NOT AVAILABLE kalksumsum THEN DO:
         CREATE kalksumsum.  
         ASSIGN
         kalksumsum.TYP = 6
         kalksumsum.AONR = valdaao.AONR 
         kalksumsum.DELNR = valdaao.DELNR.
      END.
   END.
   ELSE DO:
      FIND FIRST kalksumsum WHERE kalksumsum.AONR = valdaao.AONR NO-ERROR.
      IF NOT AVAILABLE kalksumsum THEN DO:
         CREATE kalksumsum.  
         ASSIGN
         kalksumsum.TYP = 6
         kalksumsum.AONR = valdaao.AONR. 
      END.

   END.
   FOR EACH efastkalktemp:       
      ASSIGN
      kalksumsum.MONTTIMMAR = kalksumsum.MONTTIMMAR + efastkalktemp.F2
      kalksumsum.BELOPP = kalksumsum.BELOPP + efastkalktemp.ARBETE                       
      kalksumsum.BTIMMAR = kalksumsum.BTIMMAR + efastkalktemp.F1
      kalksumsum.MASKGBELOPP = kalksumsum.MASKGBELOPP + efastkalktemp.MASKINKOST + efastkalktemp.UTRUSTKOST
      kalksumsum.MTRL = kalksumsum.MTRL + efastkalktemp.MATERIEL 
      kalksumsum.OVRKR = kalksumsum.OVRKR + efastkalktemp.OVRIGT.    
   END.
END PROCEDURE.

 
