
/*------------------------------------------------------------------------
    File        : ladda\CompileXcode.p
    Purpose     : 
prowin32.exe -rx -p STARTload_df.p
https://knowledgebase.progress.com/articles/Knowledge/P147079
COMPILE XCODE
xcode [ -k key ] -d directory [ files] [ - ]
    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue May 18 14:12:50 CEST 2021
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE VARIABLE outlib AS CHARACTER NO-UNDO.
DEFINE VARIABLE inlib  AS CHARACTER NO-UNDO. 
outlib = "D:\WebGuru\Xcode\Ladda".
inlib = "ladda\*.*".
COMPILE C:\delad\pro116\GuruAnders\wtid\LADDA\STARTload_df.p SAVE  XCODE   "-d D:\WebGuru\Xcode\Ladda\STARTload_df.p" .

