
# TODO handle semantic tags properly
# TODO check what endian-ness is required for proper CBOR
# TODO static assert that we are on little endian

type
    FieldMajorKind* {.pure.} = enum
        PositiveInteger
        NegativeInteger
        ByteString
        TextString
        Array
        Map
        SemanticTag
        Primitive

    PrimitiveKind* {.pure.} = enum
        SimpleValueTiny ## 0-23
        SimpleValueByte ## 0-255
        Single          ## float16
        Float           ## float32
        Double          ## float64
        Break           ## To escape arrays and maps of indefinite length.

    SimpleValueKind* {.pure.} = enum
        False     ## Boolean false; represented by the number 20.
        True      ## Boolean true; represented by the number 21.
        Null      ## Explicitly no data; represented by the number 22.
        Undefined ## Implicity no data; represented by the number 23.

    WriterAction* {.pure.} = enum
        Close
        Write
        Flush

    WriterActuator* = proc(action: WriterAction; data: pointer; data_len: int) {.closure.}

    ReaderAction* {.pure.} = enum
        Close
        Read

    ReaderActuator* = proc(action: WriterAction; data: pointer; data_len: int; written_len: var int) {.closure.}

    CborWriter* = object
        actuator*: WriterActuator

    CborReader* = object
        actuator*: ReaderActuator

# These just commad and control the writer object, which pawns all the
# work off on a closure.
# =======================================================================

proc put*(self: var CborWriter; data: pointer; data_len: int) {.inline.} =
    ## Writes raw bytes to the writer's output stream.
    if self.actuator != nil:
        self.actuator(WriterAction.Write, data, data_len)

proc close*(self: var CborWriter) =
    ## Closes the underlying stream and shuts down the actuator.
    self.actuator(WriterAction.Close, nil, 0)
    self.actuator = nil

proc flush*(self: var CborWriter) =
    ## Suggests the underlying stream be flushed/synced to whatever is
    ## behind it. What this actually means is highly dependent on the actuator;
    ## it may very well do nothing at all.
    if self.actuator != nil:
        self.actuator(WriterAction.Flush, nil, 0)

# Informational functions.
# =======================================================================

proc measure_integer_for_heading*(i: uint64): int =
    ## Counts how many bytes in addition to the field heading are required to store a number.
    if i < 24: return 0
    if i <= uint8.high: return 1
    if i <= uint16.high: return 2
    if i <= uint32.high: return 4
    if i <= uint64.high: return 8

# Header generation.
# =======================================================================

proc encode_field_heading*(major, additional: int): uint8 =
    ## Low-level routine to pack a major and additional type.
    assert major <= 7
    assert additional <= 31
    result = ((major shl 5) + additional).uint8

proc encode_field_heading*(
    major: FieldMajorKind;
    length: uint64;
    out_length_bytes: var int;
    unknown_length: bool = false): uint8 =
        ## Low-level routine to generate a field heading given a field type and payload size.

        var major_byte = 0
        var additional_byte = 0

        # find the nibble to identify this type
        case major
        of PositiveInteger:
            major_byte = 0
        of NegativeInteger:
            major_byte = 1
        of ByteString:
            major_byte = 2
        of TextString:
            major_byte = 3
        of Array:
            major_byte = 4
        of Map:
            major_byte = 5
        of SemanticTag:
            major_byte = 6
        of Primitive:
            major_byte = 7

        # only some types may have indefinite lengths
        if unknown_length:
            case major
            of TextString, Array, Map:
                additional_byte = 31
                out_length_bytes = 0
            else:
                raise new_exception(ValueError,
                    "Given major type does not permit unknown lengths.")
        else:
            out_length_bytes = measure_integer_for_heading(length)
            case out_length_bytes
            of 0:
                additional_byte = length.int
            of 1:
                additional_byte = 24
            of 2:
                additional_byte = 25
            of 4:
                additional_byte = 26
            of 8:
                additional_byte = 27
            else:
                raise new_exception(ValueError,
                    "Failed to calculate header size.")

        return encode_field_heading(major_byte, additional_byte)

proc decode_field_heading*(heading: uint8): (int, int) =
    ## Low-level routine to unpack major and additional type from a heading byte.
    var major = 0
    var additional = 0

    additional = heading.int and 0x1F
    major = (heading.int shr 5) and 0x07

    return (major, additional)

# These writers do actual work.
# =======================================================================

proc write*(writer: var CborWriter; value: string) =
    var write_len: int
    var header = encode_field_heading(TextString, value.len.uint64, write_len)
    put(writer, addr header, 1)
    if write_len > 0:
        put(writer, unsafeAddr value, write_len)
    put(writer, unsafeAddr value[0], value.len)

proc write*(writer: var CborWriter; value: openarray[uint8]) =
    var write_len: int
    var header = encode_field_heading(ByteString, value.len.uint64, write_len)
    put(writer, addr header, 1)
    if write_len > 0:
        put(writer, unsafeAddr value, write_len)
    put(writer, unsafeAddr value[0], value.len)

proc write*(writer: var CborWriter; value: bool) =
    var header: uint8 = if value:
        encode_field_heading(7, 21)
    else:
        encode_field_heading(7, 20)
    put(writer, addr header, 1)

proc write_nil*(writer: var CborWriter) =
    ## Writes a marker for a nil object in to the writer stream.
    var header: uint8 = encode_field_heading(7, 22)
    put(writer, addr header, 1)

proc write_undefined*(writer: var CborWriter) =
    ## Writes a marker for an undefined value in to the writer stream.
    var header: uint8 = encode_field_heading(7, 23)
    put(writer, addr header, 1)

proc write_break*(writer: var CborWriter) =
    ## Writes a marker to break an unknown length object.
    var header: uint8 = encode_field_heading(7, 31)
    put(writer, addr header, 1)

proc write*(writer: var CborWriter; value: float32) =
    var header: uint8 = encode_field_heading(7, 26)
    put(writer, addr header, 1)
    put(writer, unsafeAddr value, value.sizeof)

proc write*(writer: var CborWriter; value: float64) =
    var header: uint8 = encode_field_heading(7, 27)
    put(writer, addr header, 1)
    put(writer, unsafeAddr value, value.sizeof)

proc write*(writer: var CborWriter; value: uint64) =
    var write_len: int
    var header = encode_field_heading(PositiveInteger, value, write_len)
    put(writer, addr header, 1)
    if write_len > 0:
        put(writer, unsafeAddr value, write_len)

proc write*(writer: var CborWriter; value: int64) =
    if value >= 0:
        write(writer, value.uint64)
    else:
        var mvalue = (value * -1).uint64
        var write_len: int
        var header = encode_field_heading(NegativeInteger, mvalue, write_len)
        put(writer, addr header, 1)
        if write_len > 0:
            put(writer, addr mvalue, write_len)

proc write_array_header*(writer: var CborWriter; length: uint64; unknown_length: bool = false) =
    ## Writes a header for an array; used when manually writing the data type to a stream.

    var length_bytes: int
    var header = encode_field_heading(Array, length,  length_bytes,unknown_length)
    put(writer, addr header, 1)
    if length_bytes > 0:
        write(writer, length)

proc write*[T](writer: var CborWriter; thing: openarray[T]) =
    write_array_header(writer, thing.len, false)
    for x in thing:
        write(x)

proc write_map_header*(writer: var CborWriter; length: uint64; unknown_length: bool = false) =
    ## Writes a header for a map; used when manually writing the data type to a stream.

    var length_bytes: int
    var header = encode_field_heading(Map, length, length_bytes, unknown_length)
    put(writer, addr header, 1)
    if length_bytes > 0:
        write(writer, length)

# These writers just turn around and call the correct writer for their
# type.
# =======================================================================

proc write*(writer: var CborWriter; value: uint8) {.inline.} =
    ## Generic writer. Converts `value` to the largest unsigned int type and writes it.
    write(writer, value.uint64)

proc write*(writer: var CborWriter; value: uint16) {.inline.} =
    ## Generic writer. Converts `value` to the largest unsigned int type and writes it.
    write(writer, value.uint64)

proc write*(writer: var CborWriter; value: uint32) {.inline.} =
    ## Generic writer. Converts `value` to the largest unsigned int type and writes it.
    write(writer, value.uint64)

proc write*(writer: var CborWriter; value: uint) {.inline.} =
    ## Generic writer. Converts `value` to the largest unsigned int type and writes it.
    write(writer, value.uint64)

proc write*(writer: var CborWriter; value: int8) {.inline.} =
    ## Generic writer. Converts `value` to the largest int type and writes it.
    write(writer, value.uint64)

proc write*(writer: var CborWriter; value: int16) {.inline.} =
    ## Generic writer. Converts `value` to the largest int type and writes it.
    write(writer, value.uint64)

proc write*(writer: var CborWriter; value: int32) {.inline.} =
    ## Generic writer. Converts `value` to the largest int type and writes it.
    write(writer, value.uint64)

proc write*(writer: var CborWriter; value: int) {.inline.} =
    ## Generic writer. Converts `value` to the largest int type and writes it.
    write(writer, value.uint64)

