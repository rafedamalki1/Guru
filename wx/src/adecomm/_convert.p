/*

  File: _fconvert.p

  Description: 
     Converts an American numeric format to European and vice-versa.
     American format uses periods as decimals and commas as thousands
     separators; European format uses commas as decimals and periods as
     thousands separators.
   
  Input Parameters:
      pType      A-TO-E  American to European
                 E-TO-A  European to American
      pInFormat  The input format to convert
      
  Output Parameters:   
       pOutFormat The resulting output format
       
  Note: In this function, CHR(6) is used as a temporary replacement 
         character because it is assume that the value should not be found
         in the format string.

  Author: Bob Ryan
  Created: 6/6/94
 

  Copyright (c) PROGRESS SOFTWARE CORPORATION 1992-1993 - AllRights Reserved
*/

define input parameter  pType      as char no-undo.
define input parameter  pInFormat  as char no-undo.
define output parameter pOutFormat as char no-undo.

define var FirstPass  as char no-undo.
define var SecondPass as char no-undo.

if pInFormat = "" or pType = "" then do:
  pOutFormat = pInFormat.
  return.
end.

if caps(pType) =  "A-TO-E" then
  assign
    FirstPass  = replace(pInFormat, ".", chr(6)) 
    SecondPass = replace(FirstPass, ",", ".") 
    pOutFormat = replace(SecondPass, chr(6), ",") .
else
  assign    
    FirstPass  = replace(pInFormat,",",chr(6)) 
    SecondPass = replace(FirstPass,".",",") 
    pOutFormat = replace(SecondPass,chr(6),".") .
    
    
