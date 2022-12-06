/*Skapa en domän*/
CREATE _sec-authentication-domain.
ASSIGN
_sec-authentication-domain._domain-name = "RESTDomain"
_sec-authentication-domain._domain-type = "_oeusertable"
_sec-authentication-domain._domain-access-code = audit-policy:encrypt-audit-mac-key("testar")
_sec-authentication-domain._domain-enabled = YES.

/*Skapa rolltyp/er*/
CREATE _Sec-role.
ASSIGN _Role-name = "PSCUser"
_Role-description = "User level role".
_Role-creator = "". 
