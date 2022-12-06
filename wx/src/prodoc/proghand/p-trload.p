/* p-trload.p */

DEFINE VARIABLE i AS INTEGER.
DEFINE VARIABLE event AS CHARACTER FORMAT "x(6)".
DEFINE VARIABLE proc-name AS CHARACTER FORMAT "x(32)".
DEFINE VARIABLE table-name AS CHARACTER FORMAT "x(32)".
DEFINE VARIABLE field-name AS CHARACTER FORMAT "x(32)".
DEFINE VARIABLE rCRC AS INTEGER.

INPUT FROM "_file-tr.dat". /* Table trigger data */
i = 0.
REPEAT:
  SET table-name event proc-name.
  RCODE-INFO:FILE-NAME = proc-name.
  rCRC = RCODE-INFO:CRC-VALUE.
  FIND _file WHERE _file-name = table-name.
  FIND _file-trig WHERE _file-recid = RECID(_file) AND
                        _event = event.
  SET _file-trig._proc-name = proc-name
      _file-trig._trig-crc = rCRC.
  ASSIGN _file-trig._Override = TRUE.
  i = i + 1.
END.

INPUT CLOSE.
MESSAGE i "_file-trig records updated.".

INPUT FROM "_field-t.dat". /* Field trigger data */
i = 0.
REPEAT:
  SET table-name field-name event proc-name.
  RCODE-INFO:FILE-NAME = proc-name.
  rCRC = RCODE-INFO:CRC-VALUE.
  FIND _file WHERE _file-name = table-name.
  FIND _field WHERE _file-recid = RECID(_file) AND 
                    _field-name = field-name.
  FIND _field-trig WHERE _field-trig._file-recid = RECID(_file) AND
                         _field-trig._field-recid = RECID(_field) AND
                         _event = event.
  SET _field-trig._proc-name = proc-name
      _field-trig._trig-crc = rCRC.
  ASSIGN _field-trig._Override = TRUE.
  i = i + 1.
END.

INPUT CLOSE.
MESSAGE i "_field-trig records updated.".

