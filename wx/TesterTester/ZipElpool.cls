 
 /*------------------------------------------------------------------------
    File        : ZipElpool
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpkl
    Created     : Wed Apr 27 13:30:54 CEST 2011
    Notes       : Denna fil kräver assemblies med referens till Ionic.Zip.dll
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS Start.ZipElpool: 
   
   /*Skapar zipfil från angiven katalog med angivet filnamn (hamnar i rooten på fromDir)*/
   METHOD PUBLIC VOID MakeZip(fromDir AS CHARACTER, filnamn AS CHARACTER):
      DEFINE VARIABLE zip1 AS CLASS Ionic.Zip.ZipFile NO-UNDO.

      DO ON ERROR UNDO, LEAVE:
         zip1 = NEW Ionic.Zip.ZipFile().
/*         zip1:AddDirectory(fromDir,  System.IO.Path:GetFileName(fromDir)).*/
         zip1:AddDirectory(fromDir).
         zip1:Save(filnamn).

      END.
      CATCH err AS Progress.Lang.Error :
         MESSAGE STRING(err)
         VIEW-AS ALERT-BOX.
      END CATCH.

   END METHOD.
   
   /*Skapar Selfextractor från angiven katalog med angivet extract-to-dir samt angivet filnamn (hamnar i rooten på fromDir)*/
   METHOD PUBLIC VOID MakeSelfEx(fromDir AS CHARACTER, todir AS CHARACTER, filnamn AS CHARACTER):                      
      DEFINE VARIABLE zip1      AS CLASS                              Ionic.Zip.ZipFile NO-UNDO.                          
      DEFINE VARIABLE exOptions AS Ionic.Zip.SelfExtractorSaveOptions NO-UNDO.
      
      DO ON ERROR UNDO, LEAVE:   
         zip1 = NEW Ionic.Zip.ZipFile().
         exOptions = NEW Ionic.Zip.SelfExtractorSaveOptions().
          
         exOptions:DefaultExtractDirectory = todir.
         exOptions:ExtractExistingFile = Ionic.Zip.ExtractExistingFileAction:OverwriteSilently.
         zip1:AddDirectory(fromDir).
         
         zip1:SaveSelfExtractor(filnamn, exOptions).
          
/*         RETURN "Katalog " + fromdir + " sparad som " + filnamn + " med default extract dir: " + todir.*/
          
         CATCH err AS Progress.Lang.Error :
/*            RETURN "Kunde ej skapa filen".*/
         END CATCH.
      END.
                                    
   
   END METHOD.
   
   
   /* Använd så här:
   DEFINE VARIABLE zippare as Start.ZipElpool no-undo.
   zippare = new Start.ZipElpool().
   zippare:Extrahera("c:\sokvag\filnamn.zip", "c:\temp\wtid\").
   */
   METHOD PUBLIC VOID Extrahera(zipfile AS CHARACTER, todir AS CHARACTER):
      DEFINE VARIABLE zip AS CLASS Ionic.Zip.ZipFile NO-UNDO.                    

      zip = Ionic.Zip.ZipFile:Read(zipfile).      
      zip:ExtractAll(todir, Ionic.Zip.ExtractExistingFileAction:OverwriteSilently).   
   END METHOD.
END CLASS.