   DEF VAR debvar AS DECIMAL  NO-UNDO.
DEF VAR debvar2 AS DECIMAL NO-UNDO.
DEF VAR kredvar AS DECIMAL  NO-UNDO.
DEF VAR diffvar AS DECIMAL   FORMAT "->9.99999" NO-UNDO.
DEF VAR kredvar2 AS DECIMAL NO-UNDO.
DEF VAR diffvar2 AS DECIMAL  NO-UNDO.
DEFINE VARIABLE cc AS DECIMAL NO-UNDO.
DEFINE VARIABLE bb AS DECIMAL NO-UNDO.
DEFINE VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE VARIABLE breddantal AS INTEGER NO-UNDO.
DEFINE VARIABLE utnrstart AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE utnrslut AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE utnr AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE bredd AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE nrcolstart AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE nrcolslut AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE nrcol AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE ekofil AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.              
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE summmallab AS DECIMAL NO-UNDO.
DEFINE VARIABLE summmallabch AS CHARACTER NO-UNDO.
DEFINE VARIABLE summmallaant AS DECIMAL NO-UNDO.
DEFINE VARIABLE summmallaantch AS CHARACTER NO-UNDO.
DEFINE VARIABLE beloppchar AS CHARACTER NO-UNDO.
DEFINE VARIABLE nolldummy AS CHARACTER NO-UNDO.
DEFINE VARIABLE verifnummervar AS INTEGER NO-UNDO.
/*

bb = 2.358.
cc = 2.3530.
debvar = debvar + ROUND(cc,2).
 kredvar = kredvar + ROUND(bb,2).  
   IF (kredvar - debvar) NE 0 THEN DO:
      diffvar = round(debvar - kredvar,2). 
      bb = bb + diffvar.
      MESSAGE diffvar bb VIEW-AS ALERT-BOX.
   END.
   RUN cobolantal_UI (INPUT 15,INPUT (bb * -1),OUTPUT beloppchar).   
   MESSAGE beloppchar VIEW-AS ALERT-BOX.
   RUN cobolantal_UI (INPUT 15,INPUT (cc),OUTPUT beloppchar).   
   MESSAGE beloppchar VIEW-AS ALERT-BOX.        
*/
DEFINE TEMP-TABLE slutut
   FIELD VDATUM AS DATE
   FIELD FTG AS CHARACTER
   FIELD DEBKRED AS LOGICAL 
   FIELD FELDEBKRED AS LOGICAL
   FIELD ANTAL AS DECIMAL 
   FIELD BELOPP AS DECIMAL       
   FIELD K1 AS CHARACTER
   FIELD K2 AS CHARACTER
   FIELD K3 AS CHARACTER
   FIELD K4 AS CHARACTER
   FIELD K5 AS CHARACTER
   FIELD BIL AS CHARACTER
   FIELD K2POS8 AS CHARACTER
   INDEX ORG IS PRIMARY FTG DEBKRED K1 K2 K3 K4 K5.
DEFINE TEMP-TABLE slututK LIKE slutut.
RUN ffK_ui (INPUT 73298072).
RUN ffK_ui (INPUT 108985600).


RUN ff_ui (INPUT 120000              ).
RUN ff_ui (INPUT 103250              ).
RUN ff_ui (INPUT 47200               ).
RUN ff_ui (INPUT 33902700            ).
RUN ff_ui (INPUT 1369200             ).
RUN ff_ui (INPUT 4271850             ).
RUN ff_ui (INPUT 656000              ).
RUN ff_ui (INPUT 89600               ).
RUN ff_ui (INPUT 110000              ).
RUN ff_ui (INPUT 238000              ).
RUN ff_ui (INPUT 210000              ).
RUN ff_ui (INPUT 561500              ).
RUN ff_ui (INPUT 22400               ).
RUN ff_ui (INPUT 220000              ).
RUN ff_ui (INPUT 226000              ).
RUN ff_ui (INPUT 220400              ).
RUN ff_ui (INPUT 7700                ).
RUN ff_ui (INPUT 156960              ).
RUN ff_ui (INPUT 1213000             ).
RUN ff_ui (INPUT 137500              ).
RUN ff_ui (INPUT 528960              ).
RUN ff_ui (INPUT 18480               ).
RUN ff_ui (INPUT 327000              ).
RUN ff_ui (INPUT 1117900             ).
RUN ff_ui (INPUT 129600              ).
RUN ff_ui (INPUT 3386900             ).
RUN ff_ui (INPUT 118500              ).
RUN ff_ui (INPUT 132240              ).
RUN ff_ui (INPUT 4620                ).
RUN ff_ui (INPUT 1102000             ).
RUN ff_ui (INPUT 38500               ).
RUN ff_ui (INPUT 654000              ).
RUN ff_ui (INPUT 20000               ).
RUN ff_ui (INPUT 528000              ).
RUN ff_ui (INPUT 56000               ).
RUN ff_ui (INPUT 220400              ).
RUN ff_ui (INPUT 7700                ).
RUN ff_ui (INPUT 170040              ).
RUN ff_ui (INPUT 23357600            ).
RUN ff_ui (INPUT 79632               ).
RUN ff_ui (INPUT 481600              ).
RUN ff_ui (INPUT 6081600             ).
RUN ff_ui (INPUT 511700              ).
RUN ff_ui (INPUT 4317600             ).
RUN ff_ui (INPUT 7490000             ).
RUN ff_ui (INPUT 84000               ).
RUN ff_ui (INPUT 588000              ).
RUN ff_ui (INPUT 4162400             ).
RUN ff_ui (INPUT 705600              ).
RUN ff_ui (INPUT 8047200             ).
RUN ff_ui (INPUT 104540              ).
RUN ff_ui (INPUT 3179000             ).
RUN ff_ui (INPUT 240800              ).
RUN ff_ui (INPUT 1646400             ).
RUN ff_ui (INPUT 84000               ).
RUN ff_ui (INPUT 448000              ).
RUN ff_ui (INPUT 257600              ).
RUN ff_ui (INPUT 210000              ).
RUN ff_ui (INPUT 5480000             ).
RUN ff_ui (INPUT 268800              ).
RUN ff_ui (INPUT 1503000             ).
RUN ff_ui (INPUT 304000              ).
RUN ff_ui (INPUT 1147300             ).
RUN ff_ui (INPUT 400000              ).
RUN ff_ui (INPUT 646500              ).
RUN ff_ui (INPUT 22400               ).
RUN ff_ui (INPUT 298100              ).
RUN ff_ui (INPUT 220400              ).
RUN ff_ui (INPUT 7700                ).
RUN ff_ui (INPUT 280000              ).
RUN ff_ui (INPUT 352640              ).
RUN ff_ui (INPUT 12320               ).
RUN ff_ui (INPUT 387000              ).
RUN ff_ui (INPUT 264000              ).
RUN ff_ui (INPUT 308560              ).
RUN ff_ui (INPUT 10780               ).
RUN ff_ui (INPUT 56000               ).
RUN ff_ui (INPUT 352640              ).
RUN ff_ui (INPUT 12320               ).
RUN ff_ui (INPUT 460400              ).
RUN ff_ui (INPUT 123200              ).
RUN ff_ui (INPUT 308560              ).
RUN ff_ui (INPUT 10780               ).
RUN ff_ui (INPUT 275000              ).
RUN ff_ui (INPUT 220400              ).
RUN ff_ui (INPUT 7700                ).
RUN ff_ui (INPUT 110000              ).
RUN ff_ui (INPUT 220400              ).
RUN ff_ui (INPUT 7700                ).
RUN ff_ui (INPUT 42000               ).
RUN ff_ui (INPUT 220400              ).
RUN ff_ui (INPUT 7700                ).
RUN ff_ui (INPUT 12807000            ).
RUN ff_ui (INPUT 323200              ).
RUN ff_ui (INPUT 459800              ).
RUN ff_ui (INPUT 20000               ).
RUN ff_ui (INPUT 82500               ).
RUN ff_ui (INPUT 200000              ).
RUN ff_ui (INPUT 5400000             ).
RUN ff_ui (INPUT 80000               ).
RUN ff_ui (INPUT 330000              ).
RUN ff_ui (INPUT 2592000             ).
RUN ff_ui (INPUT 300000              ).
RUN ff_ui (INPUT 40000               ).
RUN ff_ui (INPUT 1475000             ).
RUN ff_ui (INPUT 75000               ).
RUN ff_ui (INPUT 4984800             ).
RUN ff_ui (INPUT 489600              ).
RUN ff_ui (INPUT 359700              ).
RUN ff_ui (INPUT 979358              ).
RUN ff_ui (INPUT 51150               ).
RUN ff_ui (INPUT 138600              ).
RUN ff_ui (INPUT 95040               ).
RUN ff_ui (INPUT 979358              ).
RUN ff_ui (INPUT 51150               ).
RUN ff_ui (INPUT 138600              ).
RUN ff_ui (INPUT 95040               ).
RUN ff_ui (INPUT 2133000             ).
RUN ff_ui (INPUT 720000              ).
RUN ff_ui (INPUT 236000              ).
RUN ff_ui (INPUT 2832000             ).
RUN ff_ui (INPUT 1009035             ).
RUN ff_ui (INPUT 52700               ).
RUN ff_ui (INPUT 142800              ).
RUN ff_ui (INPUT 97920               ).
RUN ff_ui (INPUT 702000              ).
RUN ff_ui (INPUT 3306250             ).
RUN ff_ui (INPUT 6868000             ).
RUN ff_ui (INPUT 75000               ).
RUN ff_ui (INPUT 100000              ).
RUN ff_ui (INPUT 2592000             ).
/*
CREATE slututk.
FOR EACH slutut.
 slututK.BELOPP = slututK.BELOPP + slutut.BELOPP.
END.
MESSAGE slututK.BELOPP debvar kredvar diffvar VIEW-AS ALERT-BOX.
*/
PROCEDURE ff_UI :
   DEFINE INPUT PARAMETER ss AS DECIMAL NO-UNDO.
   CREATE slutut.
   slutut.BELOPP = ss / 1000.
END PROCEDURE.
PROCEDURE ffK_UI :
   DEFINE INPUT PARAMETER ss AS DECIMAL NO-UNDO.
   CREATE slututK.
   slututK.BELOPP = ss / 1000.
END PROCEDURE.
   FOR EACH slututk:
     kredvar = kredvar + ROUND(slututK.BELOPP,2).
     
   END.
   FOR EACH slutut:
      debvar = debvar + ROUND(slutut.BELOPP,3).
     
   END.
   
   IF (kredvar - debvar) NE 0 THEN DO:
      diffvar = debvar - kredvar. 
      FIND LAST slututk NO-ERROR.
      IF AVAILABLE slututk THEN DO:
         slututK.BELOPP = slututK.BELOPP + diffvar.
      END.
   END.
   FOR EACH slututk:
      DISP slututK.BELOPP FORMAT "->>>>>>>>>9.99999"  debvar FORMAT "->>>>>>>>>9.99999"  kredvar FORMAT "->>>>>>>>>9.99999" diffvar FORMAT "->>>>>>>>>9.99999". 
   END.
   /*
   FIND LAST slututk NO-ERROR.
   MESSAGE slututK.BELOPP debvar kredvar diffvar VIEW-AS ALERT-BOX.
   kredvar = 0.
   FOR EACH slututk:
     kredvar = kredvar + slututK.BELOPP.
     
   END.                     
   DISP diffvar FORMAT "->9.99999" 
        debvar FORMAT "->>>>>>>>>9.99999" 
        kredvar FORMAT "->>>>>>>>>9.99999" 
      WITH FRAME CC.
   */
   
   OUTPUT TO c:\protemp9\t.txt.
   FOR EACH slutut:
      RUN cobolantal_UI (INPUT 15,INPUT (slutut.BELOPP),OUTPUT beloppchar).   
      PUT beloppchar AT 1 SKIP.
   END.
   FOR EACH slututk:
      RUN cobolantal_UI (INPUT 15,INPUT (slututk.BELOPP),OUTPUT beloppchar).   
      PUT beloppchar AT 15 SKIP.
   END.
   OUTPUT CLOSE.

PROCEDURE cobolantal_UI:
   DEFINE INPUT PARAMETER nollantal AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER intal AS DECIMAL NO-UNDO.
   DEFINE OUTPUT PARAMETER uttalch AS CHARACTER NO-UNDO.
   DEFINE VARIABLE intalhel AS INTEGER NO-UNDO.
   DEFINE VARIABLE slutch AS CHARACTER NO-UNDO.
   intalhel = intal * 100.
   IF intalhel >= 0 THEN DO:
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "0" THEN slutch = "0".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "1" THEN slutch = "1".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "2" THEN slutch = "2".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "3" THEN slutch = "3".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "4" THEN slutch = "4".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "5" THEN slutch = "5".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "6" THEN slutch = "6".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "7" THEN slutch = "7".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "8" THEN slutch = "8".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "9" THEN slutch = "9".
   END.                                                             
   IF intalhel < 0 THEN DO:
      intalhel = intalhel * -1.
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "0" THEN slutch = "p".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "1" THEN slutch = "q".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "2" THEN slutch = "r".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "3" THEN slutch = "s".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "4" THEN slutch = "t".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "5" THEN slutch = "u".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "6" THEN slutch = "v".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "7" THEN slutch = "w".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "8" THEN slutch = "x".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "9" THEN slutch = "y".
   END.                            
   uttalch = STRING(intalhel).
   uttalch = SUBSTRING(uttalch,1,LENGTH(uttalch) - 1) + slutch.
   IF LENGTH(uttalch) = nollantal THEN RETURN.
   uttalch = SUBSTRING(nolldummy,1,nollantal - LENGTH(uttalch)) + uttalch.
END PROCEDURE.

      
      
