/*SUBMARK.P*/
DEFINE INPUT-OUTPUT PARAMETER ednum AS INTEGER NO-UNDO.  /*START POS FÖR NÄSTA KOLL*/   
DEFINE INPUT PARAMETER ednum3 AS INTEGER NO-UNDO.  /*TOTAL TECKEN LÄNGD*/
DEFINE INPUT PARAMETER edtext AS CHARACTER NO-UNDO. /*HELA ANMÄRKNINGEN*/
DEFINE INPUT PARAMETER edtecken AS INTEGER NO-UNDO.  /*ANTAL TECKEN PER RAD*/
DEFINE INPUT-OUTPUT PARAMETER retvar AS INTEGER NO-UNDO. /*RAD BRYT POS*/
DEFINE OUTPUT PARAMETER tidtext AS CHARACTER NO-UNDO. /*RESULTAT TEXT*/ 

   retvar = INDEX(edtext,CHR(10),ednum).         
   IF retvar = ednum THEN DO:        
      ASSIGN
      tidtext = " "            
      ednum = ednum + 1.
   END.
   ELSE IF retvar = 0 THEN DO:                                      
      /*tidtext = SUBSTRING(edtext,ednum,edtecken) VARFÖR INTE ?????*/
      RETURN.                                                 
   END.
   ELSE IF retvar - ednum > edtecken THEN DO:               
      ASSIGN                      
      tidtext = SUBSTRING(edtext,ednum,edtecken)
      ednum = ednum + edtecken.
   END.
   ELSE DO:                     
      ASSIGN                    
      tidtext = SUBSTRING(edtext,ednum,retvar - ednum)
      ednum = retvar + 1.
   END.
