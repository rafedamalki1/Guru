/*xwordapiex.p*/

&scoped-define wdFindContinue            1
&scoped-define wdReplaceNone             0
&scoped-define wdReplaceAll              2
&scoped-define wdHeaderFooterPrimary     1
&scoped-define wdHeaderFooterFirstPage   2
&scoped-define wdHeaderFooterEvenPages   3
&scoped-define wdDoNotSaveChanges        0
&scoped-define wdSaveChanges            -1
&scoped-define wdOriginalDocumentFormat  1

DEFINE VARIABLE chWordApplication AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chDocument AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chRange AS COM-HANDLE NO-UNDO.


DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.

kommando = "c:\tmp\dok1.doc".
/* COM Handle management */
define temp-table tt-ch no-undo
  field ch as com-handle
  index pu-ch is unique primary ch.

PROCEDURE chw :
/*------------------------------------------------------------------------------
  Purpose:     add com handle to temp table
  Parameters:  com handle
  Notes:
------------------------------------------------------------------------------*/
  define input parameter P as com-handle.
  find first tt-ch where tt-ch.ch = P no-lock no-error.
  if not available(tt-ch) then
  do:
    create tt-ch.
    assign tt-ch.ch = P.
  end.
END PROCEDURE.



      create "Word.Application" chWordApplication.

       chDocument = chWordApplication:Documents:Open(kommando,,,,,,,,,).
       run chw(chDocument).
       chRange = chDocument:Content.
       run chw(chRange).
       put unformatted "  Referece Check:":U LF.
       for each x-files no-lock:
         lookfor = entry(1, x-files.old-name, ".":U).
         lnkaddr = new-name(lookfor).
         chRange:Find:Execute(lookfor, False, False, False, False, False, True,
                              {&wdFindContinue}, False, "", {&wdReplaceNone}).
         if chRange:Find:Found then
         do:
           put unformatted "    replace ":U lookfor " with ":U lnkaddr LF.
           chRange:Find:Execute(lookfor, False, False, False, False, False, True,
                                {&wdFindContinue}, False, lnkaddr, {&wdReplaceAll}).
         end.
       end.
       cnt = chDocument:Sections:Count.
       do ndx = 1 to cnt:
         chRange = 
chDocument:Sections:Item(ndx):Headers:Item({&wdHeaderFooterPrimary}):Range.
         run chw(chRange).
         if length(chRange:Text) > 1 then
         do:
           put unformatted "  Primary Header (":U ndx ") Check:":U LF.
           for each x-files no-lock:
             lookfor = entry(1, x-files.old-name, ".":U).
             lnkaddr = new-name(lookfor).
             chRange:Find:Execute(lookfor, False, False, False, False, False, True,
                                  {&wdFindContinue}, False, "", {&wdReplaceNone}).
             if chRange:Find:Found then
             do:
               put unformatted "    replace ":U lookfor " with ":U lnkaddr LF.
               chRange:Find:Execute(lookfor, False, False, False, False, False, True,
                                    {&wdFindContinue}, False, lnkaddr, 
{&wdReplaceAll}).
             end.
           end.
         end.
         chRange = 
chDocument:Sections:Item(ndx):Headers:Item({&wdHeaderFooterFirstPage}):Range.
         run chw(chRange).
         if length(chRange:Text) > 1 then
         do:
           put unformatted "  First Page Header (":U ndx ") Check:":U LF.
           for each x-files no-lock:
             lookfor = entry(1, x-files.old-name, ".":U).
             lnkaddr = new-name(lookfor).
             chRange:Find:Execute(x-files.old-name, False, False, False, False, False, 
True,
                                  {&wdFindContinue}, False, "", {&wdReplaceNone}).
             if chRange:Find:Found then
             do:
               put unformatted "    replace ":U lookfor " with ":U lnkaddr LF.
               chRange:Find:Execute(lookfor, False, False, False, False, False, True,
                                    {&wdFindContinue}, False, lnkaddr, 
{&wdReplaceAll}).
             end.
           end.
         end.
         chRange = 
chDocument:Sections:Item(ndx):Headers:Item({&wdHeaderFooterEvenPages}):Range.
         run chw(chRange).
         if length(chRange:Text) > 1 then
         do:
           put unformatted "  Even Page Header (":U ndx ") Check:":U LF.
           for each x-files no-lock:
             lookfor = entry(1, x-files.old-name, ".":U).
             lnkaddr = new-name(lookfor).
             chRange:Find:Execute(x-files.old-name, False, False, False, False, False, 
True,
                                  {&wdFindContinue}, False, "", {&wdReplaceNone}).
             if chRange:Find:Found then
             do:
               put unformatted "    replace ":U lookfor " with ":U lnkaddr LF.
               chRange:Find:Execute(lookfor, False, False, False, False, False, True,
                                    {&wdFindContinue}, False, lnkaddr, 
{&wdReplaceAll}).
             end.
           end.
         end.
       end.
       cnt = chDocument:HyperLinks:Count.
       if cnt = 0 then
         put unformatted "  No Links":U LF.
       else
       do:
         put unformatted "  # Links: ":U cnt LF.
         do ndx = 1 to cnt:
           chLink = chDocument:HyperLinks:Item(ndx).
           run chw(chLink).
           lookfor = chLink:Address.
           if index(lookfor, ".pdf":U) > 0 then
             lookfor = replace(lookfor, ".pdf":U, ".doc":U).
           if search(FILL-IN-MAIN + "\":U + lookfor) = ? then
             exist = " does not exist":U.
           else
             exist = "".
           lnkaddr = new-name(chLink:Address).
           put unformatted "    ":U ndx " ":U chLink:Address " -> " lnkaddr exist LF.
           if not lnkaddr begins "X":U then
           do:
             chHypRng = chLink:Range.
             run chw(chHypRng).
             chLink:Range:Fields:Item(1):Result:Select.
             chLink:Delete.
             chDocument:Hyperlinks:Add(chHypRng, lnkaddr, "").
           end.
         end.
         do ndx = 1 to cnt:
           chLink = chDocument:HyperLinks:Item(ndx).
           run chw(chLink).
           put unformatted "    ":U ndx " ":U chLink:Address " [":U chLink:Range:Text 
"]":U LF.
         end.
       end.
       chDocument:SaveAs(FILL-IN-MAIN + "\Done\":U + 
new-name(tt-files.old-name),,,,,,,,,,).
       chDocument:Close({&wdDoNotSaveChanges}, {&wdOriginalDocumentFormat}, False).

 for each tt-ch exclusive-lock:
   release object tt-ch.ch no-error.
   delete tt-ch.
 end.

 chWordApplication:Visible = True.
 chWordApplication:Quit().
 release object chWordApplication no-error.
