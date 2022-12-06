 
 /*------------------------------------------------------------------------
    File        : GuruWordApp
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Wed Mar 26 09:46:16 CET 2014
    Notes       :  ANVÄNDS EJ
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Microsoft.Office.Interop.Word.*.


CLASS Controls.GuruWordApp INHERITS Microsoft.Office.Interop.Word.ApplicationClass: 
   DEFINE VARIABLE doc1  AS Microsoft.Office.Interop.Word.Document                            NO-UNDO.
   DEFINE VARIABLE doc2  AS Microsoft.Office.Interop.Word.Document                            NO-UNDO.
   DEFINE VARIABLE doc3  AS Microsoft.Office.Interop.Word.Document                            NO-UNDO.
   
   CONSTRUCTOR PUBLIC GuruWordApp ():
      SUPER().
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

   END CONSTRUCTOR.
   
   METHOD PUBLIC VOID OppnaDoc(path AS CHARACTER):
      Elpool.Helpers:OppnaDoc(THIS-OBJECT, path).
   END METHOD.
   
   METHOD PUBLIC Microsoft.Office.Interop.Word.Document AppendDoc(path AS CHARACTER):
      DEFINE VARIABLE doc  AS Document                            NO-UNDO.
      Elpool.Helpers:LaggTillFil(THIS-OBJECT, path).
      /* LENA 20140404
      Elpool.Helpers:Sidbrytning(THIS-OBJECT).*/
      
      doc = THIS-OBJECT:ActiveDocument.
      RETURN doc.
   END METHOD.
   
   METHOD PUBLIC Microsoft.Office.Interop.Word.Document MergeDocs(path1 AS CHARACTER, path2 AS CHARACTER):
      DEFINE VARIABLE doc1  AS Document                            NO-UNDO.
      DEFINE VARIABLE doc2  AS Document                            NO-UNDO.
      DEFINE VARIABLE doc3  AS Document                            NO-UNDO.
    
      Elpool.Helpers:OppnaDoc(THIS-OBJECT, path1).
      doc1 = THIS-OBJECT:ActiveDocument.
      
      Elpool.Helpers:OppnaDoc(THIS-OBJECT, path2).
      doc2 = THIS-OBJECT:ActiveDocument.
      
      doc3 = THIS-OBJECT:MergeDocuments(doc1, doc2, Microsoft.Office.Interop.Word.WdCompareDestination:wdCompareDestinationNew, Microsoft.Office.Interop.Word.WdGranularity:wdGranularityWordLevel, true, true, true,true, true, true, true, true, true, true, "", "", Microsoft.Office.Interop.Word.WdMergeFormatFrom:wdMergeFormatFromRevised).
      /*doc3:AcceptAllRevisions().*/
      /*doc1:Close(TRUE, System.Reflection.Missing:Value, System.Reflection.Missing:Value).
      doc2:Close(TRUE, System.Reflection.Missing:Value, System.Reflection.Missing:Value).*/
      RETURN doc3.
   END METHOD.
   
END CLASS.