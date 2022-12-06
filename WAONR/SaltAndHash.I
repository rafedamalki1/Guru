
/*------------------------------------------------------------------------
    File        : SaltAndHash.I
    Purpose     : Här skapas nyckeln så att lösenordet kan bli saltat.
                 cSalt vårt start lösenord för saltet 
    Syntax      :rSalt nyckel

    Description : 

    Author(s)   : 
    Created     : Tue Feb 26 09:31:29 CET 2019
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE VARIABLE rKey AS RAW NO-UNDO. 
DEFINE VARIABLE rSalt AS RAW NO-UNDO. 
DEFINE VARIABLE cSalt AS CHARACTER NO-UNDO. 


cSalt =  Guru.Konstanter:Salt. 
PUT-STRING(rSalt, 1, 8) = cSalt. 
SECURITY-POLICY:PBE-KEY-ROUNDS = Guru.Konstanter:Rounds. 
SECURITY-POLICY:PBE-HASH-ALGORITHM = Guru.Konstanter:AlgoR . 
SECURITY-POLICY:SYMMETRIC-ENCRYPTION-ALGORITHM = Guru.Konstanter:EncAlgoR.
/*
cSalt =  CHR(74) + CHR(97) + CHR(103) + CHR(103) + CHR(105) + CHR(108) + CHR(108) + CHR(97) + CHR(114). 
PUT-STRING(rSalt, 1, 8) = cSalt. 
SECURITY-POLICY:PBE-KEY-ROUNDS = 1000. 
SECURITY-POLICY:PBE-HASH-ALGORITHM = CHR(83) + CHR(72) + CHR(65) + CHR(45) + CHR(49) . 
SECURITY-POLICY:SYMMETRIC-ENCRYPTION-ALGORITHM = CHR(65) + CHR(69) + CHR(83) + CHR(95) + CHR(67) + CHR(66) + CHR(67) + CHR(95) + CHR(49) + CHR(50) + CHR(56). 
*/
/*
SHA-1
AES_CBC_128
*/


