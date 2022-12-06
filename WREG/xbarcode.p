DEFINE VARIABLE input-string AS CHARACTER   NO-UNDO.
DEFINE VARIABLE bar-code-string AS CHARACTER   NO-UNDO.
DEFINE TEMP-TABLE bartemp NO-UNDO
   FIELD anr AS CHARACTER
   FIELD ben AS CHARACTER
   FIELD barc AS CHARACTER.
CREATE bartemp.
ASSIGN
bartemp.ANR = "0600010"
bartemp.BEN = "STAG 0001    25 kvmm".
CREATE bartemp.
ASSIGN
bartemp.ANR = "0600012"
bartemp.BEN = "STAG EBR 0001/11".
CREATE bartemp.
ASSIGN
bartemp.ANR = "0600013"
bartemp.BEN = "EBR-SATS 0001 SMIDD".
CREATE bartemp.
ASSIGN
bartemp.ANR = "0600014"
bartemp.BEN = "STAG EBR 0001/11".



FOR EACH bartemp:

Run bar-code-39 (input  bartemp.ANR,
                 input  "P030780",
                 output bar-code-string).
   bartemp.BARC = bar-code-string.
END.
OUTPUT TO DD.TXT.
FOR EACH bartemp:
   PUT bartemp.ANR AT 1 bartemp.BEN CHR(10).
   PUT control bartemp.BARC.
   PUT CHR(10).
END.
OUTPUT CLOSE.


/*  Create bar-code-stringcode, 39 for HP
**  Original code posted on Peg by Joern Winther
**  Adapted by Arthur Fink
**
** The procedure creates ESC-sequences for a bar-code-stringcode in
** Code39 format
** The printer cursor position is restored after printing.
**
** Input:  input-string    as char no-undo.  The char string to translate.
**         code-spec       as char no-undo.  The bar-code spec. see later
**
** Output: bar-code-string as char no-undo.  The generated bar-code-stringcode
**
**
** Structure of code-spec:
**         1.    P for horizontal bar-code
**               L for vertical bar-code
**         2-3   width of small bar-codes and spaces in dots (norm 03)
**         4-5   width of wide bar-code and spaces in dots (norm 07-08)
**         6-    Height of bar-code in dots (norm 80)
*/

Procedure bar-code-39:

  def input  parameter input-string     as char no-undo.
  def input  parameter code-spec        as char no-undo.
  def output parameter bar-code-string  as char no-undo.

  def var clr as log init true no-undo.
  def var wk  as char no-undo.
  def var drw as char no-undo.
  def var dir as char no-undo.
  def var k   as char no-undo.
  def var att as char no-undo.
  def var kod as char no-undo
                 init "1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ.? *$/+%".

  att =
  "100100001001100001101100000000110001100110000001110000000100101100100100"
+ "001100100000110100100001001001001001101001000000011001100011000001011000"
+ "000001101100001100001001100000011100100000011001000011101000010000010011"
+ "100010010001010010000000111100000110001000110000010110110000001011000001"
+ "111000000010010001110010000011010000010000101110000100011000100010010100"
+ "010101000010100010010001010000101010".

  def var i   as int no-undo.
  def var j   as int no-undo.
  def var l   as int no-undo.
  def var brd as int no-undo.
  def var jst as int no-undo.

  assign 
    i = if substring (code-spec, 1, 1) = "P" then 1 else 2
    bar-code-string = "~E&f0S~E*c100G"
    drw = substring ("ab", i, 1) 
        + substring (code-spec, 6) 
        + substring ("ba", i, 1) + "P"
    dir = substring ("XY", i, 1)
    k   = "*" + input-string + "*"
    .

  do i = 1 to length (k):
    assign
      j = index (kod, substring (k, i, 1)) * 9 - 8
      wk = substring (att, j, 9) + "0"
      .
    do l = 1 to 10:
      assign
        brd = if substring(wk,l,1) = "0" then 2 else 4
        bar-code-string = bar-code-string 
            + (if clr then "~E*c" + substring (code-spec, brd, 2) + drw
               else "~E*p+" + string (integer (substring (code-spec, brd, 2)) 
                            + jst) + dir)
        jst = integer (substring (code-spec, brd, 2))
        clr = not clr
        .
    end. /* of looping 1 to 10 */
  end. /* of going through string 'k' */
 
  assign
    bar-code-string = bar-code-string + "~E&f1S" 
    wk = ""
    .
end.


