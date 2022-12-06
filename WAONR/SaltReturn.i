
/*------------------------------------------------------------------------
    File        : SaltReturn.i
    Purpose     : Först krypteras lösenordet
                 Sedan tas det krypterade lösenordet och krypteras en gång till 
    Syntax      : rSalt är vår krypterings nyckel

    Description : retur är det hashde och saltade lösenordet.
    Author(s)   : 
    Created     : Thu May 09 09:48:38 CEST 2019
    Notes       :
  ----------------------------------------------------------------------*/
 saltpswd = ENCODE(saltpswd).
 rKey = GENERATE-PBE-KEY(saltpswd, rSalt). 
 RETURN STRING(BASE64-ENCODE(rKey)).

