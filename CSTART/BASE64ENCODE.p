DEFINE INPUT PARAMETER cInFileName AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER cOutFileName AS CHAR NO-UNDO.

FUNCTION hexToInt RETURNS INTEGER (INPUT cHexStr AS CHAR):
/** concept written by Greg Higgens -- Downloaded from **/
/** PEG Tools **/
DEF VAR cHexStrList AS CHAR INIT "0123456789abcdef".
DEF VAR fc AS CHAR NO-UNDO.
DEF VAR ix AS INT  NO-UNDO.
  ASSIGN
    fc = SUBSTRING(cHexStr,1,1)
    ix = INDEX(cHexStrList, fc) - 1.
    IF fc = "" THEN RETURN 0.
    ELSE RETURN
      (ix * INTEGER(ENTRY(LENGTH(cHexStr),
         "1,16,256,4096,65536,1048576,16888216,268435456"))) +
	  hexToInt(SUBSTRING(cHexStr,2)).
END FUNCTION.

FUNCTION intToHex RETURNS CHAR (INPUT iValue AS INT):
/** concept written by Greg Higgens -- Downloaded from **/
/** PEG Tools **/
  DEF VAR cHex AS CHAR NO-UNDO INIT "0,1,2,3,4,5,6,7,8,9,a,b,c,d,e,f".
  RETURN (IF iValue > 15 THEN 
     intToHex( INTEGER(TRUNCATE(iValue / 16, 0))) ELSE "") + 
	       ENTRY(iValue MOD 16 + 1, cHex).
END FUNCTION.

FUNCTION intToBin RETURNS CHAR (INPUT iValue AS INT):
  DEF VAR cHex AS CHAR NO-UNDO.
  DEF VAR cRetValue AS CHAR NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR j AS INT NO-UNDO.
  DEF VAR cBin AS CHAR NO-UNDO INIT
      "0000,0001,0010,0011,0100,0101,0110,0111,1000,1001,1010,1011,1100,1101,1110,1111".
  cHex = intToHex(iValue).
  IF cHex = "" OR cHex = "0" THEN cHex = "00".
  IF LENGTH(cHex) = 1 THEN cHex = "0" + cHex.
  DO i = 1 TO LENGTH(cHex):
    IF CAN-DO("0,1,2,3,4,5,6,7,8,9", SUBSTRING(cHex,i,1)) THEN
      j = INTEGER(SUBSTRING(cHex,i,1)) + 1.
    ELSE DO:
      CASE SUBSTRING(cHex,i,1):
        WHEN "a" THEN
          j = 11.
        WHEN "b" THEN
          j = 12.
        WHEN "c" THEN
          j = 13.
        WHEN "d" THEN
          j = 14.
        WHEN "e" THEN
          j = 15.
        WHEN "f" THEN
          j = 16.
      END CASE.
    END.
    cRetValue = cRetValue + ENTRY(j,cBin).
  END.
  RETURN cRetValue.
END FUNCTION.

FUNCTION binToInt RETURNS INTEGER (INPUT cValue AS CHAR):
  DEF VAR i    AS INT  NO-UNDO.
  DEF VAR cHex AS CHAR NO-UNDO INIT "0,1,2,3,4,5,6,7,8,9,a,b,c,d,e,f".
  DEF VAR cBin AS CHAR NO-UNDO INIT
      "0000,0001,0010,0011,0100,0101,0110,0111,1000,1001,1010,1011,1100,1101,1110,1111".
  DEF VAR cHexValue AS CHAR NO-UNDO.

  DO i = 1 TO LENGTH(cValue) BY 4:
    cHexValue = cHexValue + ENTRY(LOOKUP(SUBSTRING(cValue,i,4),cBin),cHex).
  END.
  RETURN hexToInt(cHexValue).
END FUNCTION.

DEF VAR cBase64 AS CHAR NO-UNDO INIT
"A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,0,1,2,3,4,5,6,7,8,9,+,/".
DEF VAR cBin   AS CHAR NO-UNDO.
DEF VAR i      AS INT  NO-UNDO.
DEF VAR iOut   AS INT  NO-UNDO.
DEF VAR cOut   AS CHAR NO-UNDO.
DEF VAR r      AS RAW  NO-UNDO.
DEF VAR iByte1 AS INT  NO-UNDO.
DEF VAR iByte2 AS INT  NO-UNDO.
DEF VAR iByte3 AS INT  NO-UNDO.
DEF VAR iCharCount AS INT NO-UNDO.
INPUT FROM VALUE(cInFileName) BINARY.
OUTPUT TO VALUE(cOutFileName).
  ReadFile:
  REPEAT:
    LENGTH(r) = 3.
    IMPORT UNFORMATTED r.
      iByte1 = GET-BYTE(r,1).
      iByte2 = GET-BYTE(r,2).
      iByte3 = GET-BYTE(r,3).
      ASSIGN
        cBin = IF iByte1 <> ? THEN intToBin(iByte1) ELSE "========".
        cBin = IF iByte2 <> ? THEN cBin + intToBin(iByte2) ELSE cBin + "0000====".
        cBin = IF iByte3 <> ? THEN cBin + intToBin(iByte3) ELSE IF iByte2 = ? THEN cBin + "========" ELSE cBin + "00======".
      DO i = 1 TO 24 BY 6:
        IF SUBSTRING(cBin,i,6) <> "======" THEN DO:
          iOut = binToInt("00" + SUBSTRING(cBin,i,6)).
          /*  PUT UNFORMATTED ENTRY(iOut + 1,cBase64). **/
          cOut = cOut +  ENTRY(iOut + 1,cBase64).
          iCharCount = iCharCount + 1.
          IF iCharCount = 76 THEN DO:
            PUT UNFORMATTED cOut SKIP.
            ASSIGN
              cOut = ""
              iCharCount = 0.
          END.
        END.
      END. /** Do i = **/
  END. /** REPEAT **/
  IF iCharCount MODULO 4 = 0 THEN
          PUT UNFORMATTED cOut SKIP.
  ELSE IF iCharCount <> 0 THEN
    PUT UNFORMATTED cOut "=" SKIP.
