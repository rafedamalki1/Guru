
   
   /*xoneprojDUB.p*/       
define buffer a for aonrtab.
output to c:\aa\dubaoIFS.txt.
for each aonrtab no-lock.
  find first a where a.aonr = aonrtab.aonr and a.delnr = aonrtab.delnr and recid(a) ne recid(aonrtab) no-lock no-error.
  if available a then do:
     put unformatted aonrtab.omrade " " aonrtab.aonr  " " aonrtab.delnr  " " aonrtab.aonravdatum skip.
     put unformatted a.omrade " " a.aonr " " a.delnr " " a.aonravdatum  skip.
    /*IF aonrtab.aonravdatum = 01/01/1991 or A.aonravdatum = 01/01/1991  THEN do:
        put unformatted aonrtab.omrade " " aonrtab.aonr  " " aonrtab.delnr  " " aonrtab.aonravdatum skip.
        put unformatted a.omrade " " a.aonr " " a.delnr " " a.aonravdatum  skip.
     END.   
     else IF aonrtab.aonravdatum ge 01/01/2018 or A.aonravdatum ge 01/01/2018  THEN do:
        put unformatted aonrtab.omrade " " aonrtab.aonr  " " aonrtab.delnr  " " aonrtab.aonravdatum skip.
        put unformatted a.omrade " " a.aonr " " a.delnr " " a.aonravdatum  skip.
     END.*/   

  end.
 end.
 output close.
