
/*------------------------------------------------------------------------
    File        : SaltReturn.i
    Purpose     : F�rst krypteras l�senordet
                 Sedan tas det krypterade l�senordet och krypteras en g�ng till 
    Syntax      : rSalt �r v�r krypterings nyckel

    Description : retur �r det hashde och saltade l�senordet.
    Author(s)   : 
    Created     : Thu May 09 09:48:38 CEST 2019
    Notes       :
  ----------------------------------------------------------------------*/
 saltpswd = ENCODE(saltpswd).
 rKey = GENERATE-PBE-KEY(saltpswd, rSalt). 
 RETURN STRING(BASE64-ENCODE(rKey)).

