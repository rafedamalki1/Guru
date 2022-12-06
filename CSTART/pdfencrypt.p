/******************************************************************************

    Program:        pdfencrypt.p
    
    Description:    This program encrypts PDF stream content.

    Note(s):        The MD5LIB preprocessor must be modified in order to 
                    identify which MD5 checksum binary to use.  For example:
                    
                    On Windows:  set to md5.exe (supplied in package)
                    On Unix:     set to /usr/lib/md5sum

                    The Unix binary and location may differ from OS to OS.

******************************************************************************/

DEFINE INPUT PARAMETER pdfDir AS CHARACTER NO-UNDO.

&GLOBAL-DEFINE MD5LIB  c:\wika\pcm\web\tls\PDFinclude\md5.exe

/* Variables required for PDF Encryption */
DEFINE VARIABLE vowner-str          AS MEMPTR NO-UNDO.
DEFINE VARIABLE vuser-str           AS MEMPTR NO-UNDO.
DEFINE VARIABLE vrc4-key            AS MEMPTR NO-UNDO.
DEFINE VARIABLE m_O                 AS MEMPTR NO-UNDO.
DEFINE VARIABLE m_U                 AS MEMPTR NO-UNDO.
DEFINE VARIABLE p_U_Temp            AS MEMPTR NO-UNDO.

DEFINE VARIABLE hex_chr AS CHARACTER NO-UNDO INIT "0123456789abcdef".

/* Load pdfencryptlib and associate Encryption function */
DEFINE VARIABLE h_pdfencryptlib    AS HANDLE NO-UNDO.
RUN VALUE(pdfDir + "pdfencryptlib.p") PERSISTENT SET h_pdfencryptlib.

FUNCTION endecrypt RETURNS LOGICAL
        (INPUT BufferPtr    AS MEMPTR,
         INPUT PasswordPtr  AS MEMPTR):
  RUN endecrypt IN h_pdfencryptlib 
              (INPUT GET-POINTER-VALUE(BufferPtr), 
               INPUT GET-SIZE(BufferPtr),
               INPUT GET-POINTER-VALUE(PasswordPtr),
               INPUT GET-SIZE(PasswordPtr) ) .
  
  RETURN TRUE.
END FUNCTION. /* endecrypt */


/* Define some temp-tables that are required for producing the PDF document */
DEFINE TEMP-TABLE hexarray NO-UNDO
   FIELD hex-val   AS CHARACTER
   FIELD chr-val   AS INTEGER
INDEX hex-idx AS PRIMARY
      hex-val
INDEX chr-idx 
      chr-val.

DEFINE VARIABLE vstring     as character no-undo.
DEFINE VARIABLE vhex        as character no-undo.
DEFINE VARIABLE loop        AS INTEGER NO-UNDO.

FUNCTION int2hex RETURNS CHARACTER
        ( vi AS INTEGER ):
  DEFINE VARIABLE hexBit AS CHARACTER FORMAT "x(1)" EXTENT 16 NO-UNDO INIT
        ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'].

  IF vi < 16 THEN RETURN hexbit[vi + 1].

  RETURN int2hex( integer( TRUNCATE( vi / 16, 0 ) ) )
         + hexbit[ ( vi MODULO 16 ) + 1 ] . 
END FUNCTION. 

FUNCTION int2hexchar RETURNS CHARACTER
        ( vi AS INTEGER ):
  DEFINE VARIABLE chex AS CHARACTER NO-UNDO.
  chex = int2hex( vi ).
  RETURN '0x' + FILL( '0', 8 - LENGTH( chex ) ) + chex. 
END FUNCTION. 

RUN LoadHexArray.

FUNCTION hex RETURNS CHARACTER (INPUT asc-value AS INTEGER).
  DEF VAR j AS INT  NO-UNDO.
  DEF VAR h AS CHAR NO-UNDO.

  DO WHILE TRUE:
    j = asc-value MODULO 16.
    h = (IF j < 10 THEN STRING(j) ELSE CHR(ASC("A") + j - 10)) + h.
    IF asc-value < 16 THEN LEAVE.
    asc-value = (asc-value - j) / 16.
  END.

  IF LENGTH(h) = 1 THEN
    h = "0" + h.

  RETURN h.
END FUNCTION.

PROCEDURE LoadHexArray:
  DEFINE VARIABLE vHexLoop AS INTEGER NO-UNDO.

  DO vHexLoop = 0 TO 255:
    CREATE HexArray.
    ASSIGN HexArray.hex-val = hex(vHexLoop)
           HexArray.chr-val = vHexLoop.
  END.

END. /* LoadHexArray */

PROCEDURE DetermineOwnerKey:

  DEFINE INPUT        PARAMETER p_UniqueID      AS CHARACTER NO-UNDO.
  DEFINE INPUT        PARAMETER p_UserPassword  AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER p_OwnerPassword AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_md5   AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Loop  AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Byte  AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Key   AS MEMPTR  NO-UNDO.

  DEFINE VARIABLE L_md5hash   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_TxtFile   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_md5File   AS CHARACTER NO-UNDO.

  ASSIGN L_TxtFile = SESSION:TEMP-DIR + p_UniqueID + "-O.txt"
         L_md5File = SESSION:TEMP-DIR + p_UniqueID + "-O.md5".

  RUN InitString (p_OwnerPassword, INPUT-OUTPUT vowner-str).
  RUN InitString (p_UserPassword, INPUT-OUTPUT vuser-str).


  /* Use Algorithm 3.3 from the Adobe 1.4 Spec */

  /* Step 1 - Pad the Owner String to 32-bytes */
  IF LENGTH(p_OwnerPassword) < 32 THEN
    RUN PadString (LENGTH(p_OwnerPassword),
                   INPUT-OUTPUT vOwner-Str).


  /* Step 2 - pass the padded Owner Password String to md5 Encryption hash */

  OUTPUT TO VALUE(L_TxtFile).
    EXPORT vOwner-str.
  OUTPUT CLOSE.

  IF OPSYS = "UNIX" THEN DO:
    INPUT THROUGH VALUE("{&MD5LIB} " + L_TxtFile) NO-ECHO.
      IMPORT L_md5hash.
    INPUT CLOSE.
  END.

  ELSE DO:
    OS-COMMAND SILENT VALUE("{&MD5LIB} " + L_TxtFile + " > " + L_md5File) NO-ECHO.

    INPUT FROM VALUE(L_md5file) NO-ECHO NO-CONVERT.
      IMPORT L_md5hash.
    INPUT CLOSE.

    OS-DELETE VALUE(L_md5File).
  END.

  OS-DELETE VALUE(L_TxtFile).

  /* Step 3 - Ignore -- only for Rev 3 encryption so do not do md5 hash another
              50 times*/

  /* Step 4 - create an RC4 Encryption key using the first 5 characters of the
              MD5 hash obtained in Step 2 */
  SET-SIZE(vrc4-key)  = 0.
  SET-SIZE(vrc4-key)  = 5.
  L_Byte = 1.
  DO L_md5 = 1 TO 10 BY 2:
    FIND FIRST HexArray WHERE HexArray.Hex-Val = SUBSTR(L_md5hash,L_Md5,2).
    IF HexArray.chr-val = 0 THEN
      PUT-BYTE(vrc4-key,L_Byte) = 0.
    ELSE
      PUT-BYTE(vrc4-key,L_Byte) = ASC(CHR(HexArray.chr-val)).
    L_Byte = L_Byte + 1.
  END.

  /* Step 5 - pad or truncate the User Password */
  IF LENGTH(p_UserPassword) < 32 THEN
    RUN PadString (LENGTH(p_UserPassword),
                   INPUT-OUTPUT vUser-Str).

  /* Step 6 - Encrypt the result of Step 5 using RC4 Key obtained in Step 4 */
  endecrypt(vuser-str, vrc4-key).
  SET-SIZE(m_O) = GET-SIZE(vuser-str).
  m_O = vuser-str.
  SET-SIZE(L_Key) = 0.

  p_OwnerPassword = "".
  DO L_loop = 1 TO GET-SIZE(m_O):
    IF ASC(CHR(GET-BYTE(m_O,L_loop))) = -1 THEN
      p_OwnerPassword = p_OwnerPassword + "00".
    ELSE
      p_OwnerPassword = p_OwnerPassword + hex(ASC(CHR(GET-BYTE(m_O,L_loop)))).
  END.

END. /* DetermineOwnerKey */

PROCEDURE DetermineUserKey:

  DEFINE INPUT  PARAMETER       p_UniqueID      AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER       p_ID            AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER       p_Key           AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER       p_AllowPrint    AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER       p_AllowCopy     AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER       p_AllowModify   AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER       p_AllowAnnots   AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER       p_AllowForms    AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER       p_AllowExtract  AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER       p_AllowAssembly AS INTEGER   NO-UNDO.
  DEFINE OUTPUT PARAMETER       p_OrigKey       AS MEMPTR    NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER p_UserPassword  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER       p_IntID         AS INTEGER   NO-UNDO.

  DEFINE VARIABLE vLast-hash  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_md5     AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Byte    AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Loop    AS INTEGER NO-UNDO.
  DEFINE VARIABLE vP-str    AS INTEGER NO-UNDO.

  DEFINE VARIABLE vPad-mem  AS MEMPTR  NO-UNDO.
  DEFINE VARIABLE vP-mem    AS MEMPTR  NO-UNDO.
  DEFINE VARIABLE vID-mem   AS MEMPTR  NO-UNDO.
  DEFINE VARIABLE vTot-mem  AS MEMPTR  NO-UNDO.

  DEFINE VARIABLE L_TxtFile   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_md5File   AS CHARACTER NO-UNDO.

  ASSIGN L_TxtFile = SESSION:TEMP-DIR + p_UniqueID + "-U.txt"
         L_md5File = SESSION:TEMP-DIR + p_UniqueID + "-U.md5".

  /* Step 1 - Create an Encryption Key based on Algorithm 3.2 */
  
  /* Step 1A - Pad the User Password */
  RUN InitString (p_UserPassword, INPUT-OUTPUT vuser-str).

  IF LENGTH(p_UserPassword) < 32 THEN
    RUN PadString (LENGTH(p_UserPassword),
                   INPUT-OUTPUT vUser-Str).

  /* Step 1B - Pass concatenation of:
                 - Padded User Password 
                 - the value of the /O value
                 - the value of the /P entry 
                 - the first element of the /ID entry
               to the md5 hash function */

  SET-SIZE(vID-mem) = 16.    /* Set ID Memptr from /ID Hex string passed */
  L_Byte = 1.
  DO L_Loop = 1 TO 32 BY 2:
    FIND FIRST  HexArray WHERE HexArray.Hex-Val = SUBSTR(p_ID,L_loop,2).
    IF HexArray.chr-val = 0 THEN
      PUT-BYTE(vID-mem,L_Byte) = 0.
    ELSE
      PUT-BYTE(vID-mem,L_Byte) = ASC(CHR(HexArray.chr-val)).
    L_Byte = L_Byte + 1.
  END.
  
  /* If non Rev 3 (or 128 Bit Encryption) then the following must be 1 since 
     they are not valid in Rev 2 Encryption */
  IF p_Key = 40 THEN
    ASSIGN p_AllowForms    = 1
           p_AllowExtract  = 1
           p_AllowAssembly = 1.

  PUT-BITS(vP-Str,1,1)   = 0.   /* Reserved */
  PUT-BITS(vP-Str,2,1)   = 0.   /* Reserved */
  PUT-BITS(vP-Str,3,1)   = p_AllowPrint.      /* Print the Document */
  PUT-BITS(vP-Str,4,1)   = p_AllowModify.     /* Modify the Contents */
  PUT-BITS(vP-Str,5,1)   = p_AllowCopy.       /* Copy Graphics or Text */
  PUT-BITS(vP-Str,6,1)   = p_AllowAnnots.     /* Add or modify Annotations */
  PUT-BITS(vP-Str,7,1)   = 1.                 /* Reserved - must be 1 */
  PUT-BITS(vP-Str,8,1)   = 1.                 /* Reserved - must be 1 */
  PUT-BITS(vP-Str,9,1)   = p_AllowForms.      /* Fill in existing Form Fields */
  PUT-BITS(vP-Str,10,1)  = p_AllowExtract.    /* Extract text and Graphics - Rev 3 */
  PUT-BITS(vP-Str,11,1)  = p_AllowAssembly.   /* Assemble the document - Rev 3 */
  PUT-BITS(vP-Str,12,1)  = 1.   /* Print the doc - Rev 3 */
  PUT-BITS(vP-Str,13,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,14,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,15,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,16,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,17,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,18,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,19,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,20,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,21,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,22,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,23,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,24,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,25,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,26,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,27,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,28,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,29,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,30,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,31,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,32,1)  = 1.   /* Reserved */
  
  /* Return the value so we can set it in the Encryption Dictionary */
  p_IntID = Vp-Str. 

  SET-SIZE(vP-Mem) = 4.
  PUT-BYTE(vP-Mem,1) = GET-BITS(vP-str,1,8).
  PUT-BYTE(vP-Mem,2) = GET-BITS(vP-str,9,8).
  PUT-BYTE(vP-Mem,3) = GET-BITS(vP-str,17,8).
  PUT-BYTE(vP-Mem,4) = GET-BITS(vP-str,25,8).


  SET-SIZE(vTot-mem) = GET-SIZE(vuser-str) + GET-SIZE(m_O)
                     + GET-SIZE(vP-mem) + GET-SIZE(vID-mem).

  /* Add Padded User Password string to Total String */
  L_Byte = 1.
  DO L_Loop = 1 TO GET-SIZE(vuser-str):
    IF ASC(CHR(GET-BYTE(vuser-str,L_loop))) = -1 THEN DO:
      PUT-BYTE(vTot-mem,L_Byte) = 0.
    END.

    ELSE DO:
      FIND FIRST HexArray WHERE HexArray.Hex-Val = hex(ASC(CHR(GET-BYTE(vuser-str,L_loop)))).
      PUT-BYTE(vTot-mem,L_Byte) = ASC(CHR(HexArray.chr-val)).
    END.

    L_Byte = L_byte + 1.
  END.

  /* Add /O entry to Total String */
  DO L_Loop = 1 TO GET-SIZE(m_O):
    IF ASC(CHR(GET-BYTE(m_O,L_loop))) = -1 THEN DO:
      PUT-BYTE(vTot-mem,L_Byte) = 0.
    END.

    ELSE DO:
      FIND FIRST HexArray WHERE HexArray.Hex-Val = hex(ASC(CHR(GET-BYTE(m_O,L_loop)))).
      PUT-BYTE(vTot-mem,L_Byte) = ASC(CHR(HexArray.chr-val)).
    END.

    L_Byte = L_byte + 1.
  END.

  /* add the /P entry to Total String - low-order byte first */
  DO L_Loop = 1 TO GET-SIZE(vP-mem):
    IF ASC(CHR(GET-BYTE(vP-mem,L_loop))) = -1 THEN DO:
      PUT-BYTE(vTot-mem,L_Byte) = 0.
    END.
    ELSE DO:
      FIND FIRST HexArray WHERE HexArray.Hex-Val = hex(ASC(CHR(GET-BYTE(vP-mem,L_loop)))).
      PUT-BYTE(vTot-mem,L_Byte) = ASC(CHR(HexArray.chr-val)).
    END.

    L_Byte = L_byte + 1.
  END.

  /* Add the /ID entry to Total String */
  DO L_Loop = 1 TO GET-SIZE(vID-mem):
    IF ASC(CHR(GET-BYTE(vID-mem,L_loop))) = -1 THEN DO:
      PUT-BYTE(vTot-mem,L_Byte) = 0.
    END.

    ELSE DO:
      FIND FIRST HexArray WHERE HexArray.Hex-Val = hex(ASC(CHR(GET-BYTE(vID-mem,L_loop)))).
      PUT-BYTE(vTot-mem,L_Byte) = ASC(CHR(HexArray.chr-val)).
    END.

    L_Byte = L_byte + 1.
  END.

  OUTPUT TO VALUE(L_TxtFile).
    EXPORT vTot-mem.
  OUTPUT CLOSE.

  IF OPSYS = "UNIX" THEN DO:
    INPUT THROUGH VALUE("{&MD5LIB} " + L_TxtFile) NO-ECHO.
      import vlast-Hash.
    INPUT CLOSE.
  END.

  ELSE DO:
    OS-COMMAND SILENT VALUE("{&MD5LIB} " + L_TxtFile + " > " + L_md5File) NO-ECHO.

    INPUT FROM VALUE(L_md5File) NO-ECHO NO-CONVERT.
      IMPORT vlast-hash.
    INPUT CLOSE.

    OS-DELETE VALUE(L_md5File).
  END.

  OS-DELETE VALUE(L_TxtFile).

  SET-SIZE(vrc4-key) = 0.
  SET-SIZE(Vrc4-key) = 5.
  L_Byte = 1.
  DO L_md5 = 1 TO 10 BY 2:
    IF SUBSTR(vlast-hash, L_md5,2) = "00" THEN DO:
      PUT-BYTE(vrc4-key,L_Byte) = 0.
    END.
    ELSE DO:
      FIND FIRST HexArray WHERE HexArray.Hex-Val = SUBSTR(vlast-hash,L_md5,2).
      PUT-BYTE(vrc4-key,L_Byte) = ASC(CHR(HexArray.chr-val)).
    END.
    L_Byte = L_Byte + 1.
  END.

  /* Now that we've got the original encryption key - store it for future use */
  SET-SIZE(p_OrigKey) = 5.
  p_OrigKey = vrc4-key.

  RUN PadString (0,
                 INPUT-OUTPUT vPad-mem).

  /* Plus run the encryption routine on the padded User password with this key */
  endecrypt(vpad-mem, vrc4-key).
  
  /* But since we are outputting in hex we need to convert back to hex values */
  p_UserPassword = "".
  DO L_loop = 1 TO GET-SIZE(vpad-mem):
    IF ASC(CHR(GET-BYTE(vpad-mem,L_loop))) = -1 THEN
      p_UserPassword = p_UserPassword + "00".
    ELSE
      p_UserPassword = p_UserPassword + hex(ASC(CHR(GET-BYTE(vpad-mem,L_loop)))).
  END.

END. /* DetermineUserKey */

PROCEDURE InitString:
  DEFINE INPUT PARAMETER pString        AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER pMemPtr AS MEMPTR NO-UNDO.

  DEFINE VARIABLE l_Loop    AS INTEGER NO-UNDO.

  /* No Password  */
  IF LENGTH(pString) > 0 AND LENGTH(pString) <= 32 THEN DO:
    SET-SIZE(pMemPtr) = 32.

    DO L_Loop = 1 TO LENGTH(pString):
      PUT-BYTE(pMemPtr, l_Loop) = ASC(SUBSTR(pString,l_Loop,1)).
    END.
  END.
END. /* InitString */

PROCEDURE PadString:
  DEFINE INPUT PARAMETER        pLength   AS INTEGER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER pMemPtr   AS MEMPTR NO-UNDO.

  DEFINE VARIABLE L_chr   AS MEMPTR NO-UNDO.
  DEFINE VARIABLE L_ctr   AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_pad   AS INTEGER NO-UNDO.

  /* SET-SIZE(L_chr) = 1. */
  /* Set Password Pad String */ 
  SET-SIZE(L_chr) = 32.
  PUT-BYTE(L_Chr,1)  = ASC(CHR(40)).
  PUT-BYTE(L_Chr,2)  = ASC(CHR(191)).
  PUT-BYTE(L_Chr,3)  = ASC(CHR(78)).
  PUT-BYTE(L_Chr,4)  = ASC(CHR(94)).
  PUT-BYTE(L_Chr,5)  = ASC(CHR(78)).
  PUT-BYTE(L_Chr,6)  = ASC(CHR(117)).
  PUT-BYTE(L_Chr,7)  = ASC(CHR(138)).
  PUT-BYTE(L_Chr,8)  = ASC(CHR(65)).
  PUT-BYTE(L_Chr,9)  = ASC(CHR(100)).
  PUT-BYTE(L_Chr,10) = 0.
  PUT-BYTE(L_Chr,11) = ASC(CHR(78)).
  PUT-BYTE(L_Chr,12) = ASC(CHR(86)).
  PUT-BYTE(L_Chr,13) = ASC(CHR(255)).
  PUT-BYTE(L_Chr,14) = ASC(CHR(250)).
  PUT-BYTE(L_Chr,15) = ASC(CHR(1)).
  PUT-BYTE(L_Chr,16) = ASC(CHR(8)).
  PUT-BYTE(L_Chr,17) = ASC(CHR(46)).
  PUT-BYTE(L_Chr,18) = ASC(CHR(46)).
  PUT-BYTE(L_Chr,19) = 0.
  PUT-BYTE(L_Chr,20) = ASC(CHR(182)).
  PUT-BYTE(L_Chr,21) = ASC(CHR(208)).
  PUT-BYTE(L_Chr,22) = ASC(CHR(104)).
  PUT-BYTE(L_Chr,23) = ASC(CHR(62)).
  PUT-BYTE(L_Chr,24) = ASC(CHR(128)).
  PUT-BYTE(L_Chr,25) = ASC(CHR(47)).
  PUT-BYTE(L_Chr,26) = ASC(CHR(12)).
  PUT-BYTE(L_Chr,27) = ASC(CHR(169)).
  PUT-BYTE(L_Chr,28) = ASC(CHR(254)).
  PUT-BYTE(L_Chr,29) = ASC(CHR(100)).
  PUT-BYTE(L_Chr,30) = ASC(CHR(83)).
  PUT-BYTE(L_Chr,31) = ASC(CHR(105)).
  PUT-BYTE(L_Chr,32) = ASC(CHR(122)).
  
  SET-SIZE(pMemPtr) = 32.

  l_Ctr = pLength + 1.
  l_Pad = 1. 
  DO WHILE TRUE:
    IF ASC(CHR(GET-BYTE(l_Chr,l_pad))) = -1 THEN
      PUT-BYTE( pMemPtr, L_Ctr) = 0. 
    ELSE
      PUT-BYTE( pMemPtr, L_Ctr) = ASC(CHR(GET-BYTE(l_Chr,l_pad))). 

    l_Ctr = l_Ctr + 1.
    L_pad = L_pad + 1.
    IF L_Ctr > 32 THEN LEAVE.
  END. /* While True */

  SET-SIZE(L_Chr) = 0.
END. /* PadString */

PROCEDURE PutMemPtr:
  DEFINE INPUT PARAMETER pMemPtr AS MEMPTR NO-UNDO.
  DEFINE INPUT PARAMETER pFile   AS CHARACTER NO-UNDO.

  DEFINE VARIABLE l_Loop  AS INTEGER NO-UNDO.

  OUTPUT TO VALUE(pFile) BINARY NO-MAP NO-CONVERT.
    DO L_Loop = 1 TO GET-SIZE(pMemPtr):
      IF GET-BYTE(pMemPtr,L_Loop) = 0 THEN
        PUT CONTROL NULL.
      ELSE 
        PUT CONTROL CHR(GET-BYTE(pMemPtr,L_Loop)).
    END.
  OUTPUT CLOSE.
END. /* PutMemPtr */

PROCEDURE EncryptContent:
  DEFINE INPUT  PARAMETER p_EncryptKey     AS MEMPTR    NO-UNDO.
  DEFINE INPUT  PARAMETER p_Original       AS MEMPTR    NO-UNDO.
  DEFINE OUTPUT PARAMETER p_Encrypted      AS MEMPTR    NO-UNDO.

  DEFINE VARIABLE L_Hash    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_NewKey  AS MEMPTR NO-UNDO.
  DEFINE VARIABLE L_Copy    AS MEMPTR NO-UNDO.
  DEFINE VARIABLE L_Keyptr  AS MEMPTR NO-UNDO.
  DEFINE VARIABLE L_Loop    AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Byte    AS INTEGER NO-UNDO.

  DEFINE VARIABLE L_Content   AS MEMPTR NO-UNDO.

  SET-SIZE(L_KeyPtr) = 0.
  SET-SIZE(L_Content) = 0.
  SET-SIZE(L_KeyPtr) = 10.
  SET-SIZE(L_Content) = GET-SIZE(p_Original).
  
  L_Content = p_Original.

  endecrypt(L_Content, p_Encryptkey).

  p_Encrypted = L_Content.

  SET-SIZE(L_KeyPtr) = 0.
  SET-SIZE(L_Content) = 0.
  SET-SIZE(L_KeyPtr) = 0.
  SET-SIZE(L_Content) = 0.

END. /* EncryptContent */

PROCEDURE GetEncryptKey:
  DEFINE INPUT  PARAMETER p_UniqueID        AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER p_Object          AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER p_Gen             AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER p_StreamKey       AS MEMPTR    NO-UNDO.
  DEFINE OUTPUT PARAMETER p_EncryptKey      AS MEMPTR    NO-UNDO.

  DEFINE VARIABLE L_Hex     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_Hash    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_Keyptr  AS MEMPTR NO-UNDO.
  DEFINE VARIABLE L_Loop    AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Byte    AS INTEGER NO-UNDO.

  DEFINE VARIABLE L_TxtFile   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_md5File   AS CHARACTER NO-UNDO.

  ASSIGN L_TxtFile = SESSION:TEMP-DIR + p_UniqueID + "-key.txt"
         L_md5File = SESSION:TEMP-DIR + p_UniqueID + "-key.md5".

  SET-SIZE(L_KeyPtr) = 10.

  /* Take first 5-bytes of original Rc4-Key ... */
  PUT-BYTES(l_KeyPtr,1) = GET-BYTES(p_StreamKey,1,5).

  /* Append Object Number - Low Order Bytes First */
  L_Hex = SUBSTR(int2hexchar(p_Object),9,2).
  FIND HexArray WHERE HexArray.Hex-Val = L_Hex NO-ERROR.
  IF HexArray.chr-val = 0 THEN
    PUT-BYTE(L_KeyPtr,6) = 0.
  ELSE
    PUT-BYTE(L_KeyPtr,6) = ASC(CHR(HexArray.chr-val)).

  L_Hex = SUBSTR(int2hexchar(p_Object),7,2).
  FIND HexArray WHERE HexArray.Hex-Val = L_Hex NO-ERROR.
  IF HexArray.chr-val = 0 THEN
    PUT-BYTE(L_KeyPtr,7) = 0.
  ELSE
    PUT-BYTE(L_KeyPtr,7) = ASC(CHR(HexArray.chr-val)).

  L_Hex = SUBSTR(int2hexchar(p_Object),5,2).
  FIND HexArray WHERE HexArray.Hex-Val = L_Hex NO-ERROR.
  IF HexArray.chr-val = 0 THEN
    PUT-BYTE(L_KeyPtr,8) = 0.
  ELSE
    PUT-BYTE(L_KeyPtr,8) = ASC(CHR(HexArray.chr-val)).

  /* Append Generation Number - currently always zero */
  PUT-BYTE( L_KeyPtr, 9)   = 0. 
  PUT-BYTE( L_KeyPtr, 10)  = 0. 

  /* Then run the md5 hash against that 10 byte key - orig + obj + gen */
  OUTPUT TO VALUE(l_TxtFile).
    EXPORT L_KeyPtr.
  OUTPUT CLOSE.

  IF OPSYS = "UNIX" THEN DO:
    INPUT THROUGH VALUE("{&MD5LIB} " + L_TxtFile) NO-ECHO.
      IMPORT L_Hash.
    INPUT CLOSE.
  END.
  
  ELSE DO:
    OS-COMMAND SILENT VALUE("{&MD5LIB} " + L_TxtFile + " > " + L_md5File) NO-ECHO.

    INPUT FROM VALUE(L_md5File) NO-ECHO NO-CONVERT.
      IMPORT L_HasH.
    INPUT CLOSE.

    OS-DELETE VALUE(L_md5File).
  END.

  OS-DELETE VALUE(L_TxtFile).

  /* Now take the first n + 5 bytes of the resulting md5 hash as the new key */
  L_Byte = 1.
  DO L_loop = 1 TO 20 BY 2:
    IF SUBSTR(L_Hash,L_loop,2) = "00" THEN
      PUT-BYTE(p_EncryptKey,L_Byte) = 0.
    ELSE DO:
      FIND FIRST HexArray WHERE HexArray.Hex-Val = SUBSTR(L_Hash,L_Loop,2).
      PUT-BYTE(p_EncryptKey,L_Byte) = ASC(CHR(HexArray.chr-val)).
    END.

    L_Byte = L_Byte + 1.
  END.

  SET-SIZE(L_KeyPtr) = 0.

END. /* GetEncryptKey */

PROCEDURE BuildDocId:
  DEFINE INPUT  PARAMETER pUniqueID AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER pID       AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_Loop      AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_TxtFile   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_md5File   AS CHARACTER NO-UNDO.

  DEFINE VARIABLE mID     AS MEMPTR NO-UNDO.

  ASSIGN L_TxtFile = SESSION:TEMP-DIR + pUniqueID + "-ID.txt"
         L_md5File = SESSION:TEMP-DIR + pUniqueID + "-ID.md5".

  /* Set Memory Size and Contents */
  SET-SIZE(mID) = LENGTH(pUniqueiD).
  PUT-STRING(mID,1,LENGTH(pUniqueID)) = pUniqueID.

  OUTPUT TO VALUE(L_TxtFile).
    EXPORT mID.
  OUTPUT CLOSE.

  IF OPSYS = "UNIX" THEN DO:
    INPUT THROUGH VALUE("{&MD5LIB} " + L_TxtFile) NO-ECHO.
      import pID.
    INPUT CLOSE.
  END.

  ELSE DO:
    OS-COMMAND SILENT VALUE( "{&MD5LIB} " 
                             + L_TxtFile
                             + " > "
                             + L_md5File ) .

    INPUT FROM VALUE(L_Md5File) NO-ECHO NO-CONVERT.
      IMPORT pID.
    INPUT CLOSE.

    OS-DELETE VALUE(L_md5File).
  END.

  OS-DELETE VALUE(L_TxtFile).

  /* Reallocate memory */
  SET-SIZE(mID) = 0.

END. /* BuildDocID */
