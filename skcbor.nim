
# TODO handle semantic tags properly
# TODO handle infinite length arrays/strings

import endians

const
    EUNKNOWN_FIELD_LENGTH = "Field has an unrecognized length marker"
    EINVALID_TYPE = "Message contained an invalid type"
    ENO_SINGLES = "16-bit floating point messages are not (yet) supported"

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

    ReaderActuator* = proc(action: ReaderAction; data: pointer; data_len: int; read_len: var int) {.closure.}

    CborWriter* = object
        actuator*: WriterActuator

    BoxedValue* = object
        kind*: FieldMajorKind
        kind2*: PrimitiveKind
        kind3*: SimpleValueKind
        data*: array[8, uint8]
        sdata*: string
        bdata*: seq[uint8]

    CborReader* = object
        actuator*: ReaderActuator
        header*: uint8
        length*: int64

    CborSaxReader* = object
        on_boolean*          : proc (semtag: int64; value: bool) {.closure.}
        on_nil*              : proc (semtag: int64) {.closure.}
        on_positive_integer* : proc (semtag: int64; box: BoxedValue) {.closure.}
        on_negative_integer* : proc (semtag: int64; box: BoxedValue) {.closure.}
        on_undefined*        : proc (semtag: int64) {.closure.}
        on_string_text*      : proc (semtag: int64; box: BoxedValue) {.closure.}
        on_string_binary*    : proc (semtag: int64; box: BoxedValue) {.closure.}
        on_float32*          : proc (semtag: int64; box: BoxedValue) {.closure.}
        on_float64*          : proc (semtag: int64; box: BoxedValue) {.closure.}
        on_map_start*        : proc (semtag: int64; size: int) {.closure.}
        on_map_end*          : proc () {.closure.}
        on_array_start*      : proc (semtag: int64; size: int) {.closure.}
        on_array_end*        : proc () {.closure.}

# Deals with boxed values.
# =======================================================================

proc unbox*(self: BoxedValue; value: var float64; default: float64; lossy: bool): bool {.discardable.} =
    result = true
    case self.kind:
    of Primitive:
        case self.kind2:
        of Float:
            var x: float32
            copymem(addr x, unsafeAddr self.data[0], float32.sizeof)
            value = x.float64
        of Double:
            copymem(addr value, unsafeAddr self.data[0], float64.sizeof)
        else:
            value = default
            result = false
    else:
        value = default
        result = false

proc unbox*(self: BoxedValue; value: var bool; default: bool; lossy: bool): bool {.discardable.} =
    result = true
    case self.kind:
    of Primitive:
        case self.kind2
        of SimpleValueByte:
          case self.kind3:
            of True:
                value = true
            of False:
                value = false
            of Null, Undefined:
                if lossy:
                    value = false
                else:
                    value = default
                    result = false
        else:
            value = default
            result = false
    else:
        value = default
        result = false

proc unbox*(self: BoxedValue; value: var uint64; default: uint64; lossy: bool): bool {.discardable.} =
    result = true
    case self.kind:
    of PositiveInteger:
        copymem(addr value, unsafeAddr self.data[0], uint64.sizeof)
    of NegativeInteger:
        if lossy:
            value = 0'u
        else:
            value = default
            result = false
    else:
        value = default
        result = false

proc unbox*(self: BoxedValue; value: var int64; default: int64; lossy: bool): bool {.discardable.} =
    result = true
    var x: uint64
    case self.kind:
    of NegativeInteger:
        copymem(addr x, unsafeAddr self.data[0], uint64.sizeof)
        if x <= int64.high.uint64:
            value = (x.int64 * -1)
        else:
            if lossy:
                value = int64.low
            else:
                value = default
                result = false
    of PositiveInteger:
        copymem(addr x, unsafeAddr self.data[0], uint64.sizeof)
        if x <= int64.high.uint64:
            value = x.int64
        else:
            if lossy:
                value = int64.high
            else:
                value = default
                result = false
    else:
        value = default
        result = false

proc unbox*[T:int|int32|int16|int8](self: BoxedValue; value: var T; default: T; lossy: bool): bool {.discardable.} =
    var holder: int64
    if self.unbox(holder, default.int64, lossy):
        if holder > T.high:
            if lossy:
                value = T.high
                return true
            else:
                value = default
                return false
        else:
            value = holder.T
            return true
    else:
        value = default
        return false

proc unbox*[T:uint|uint32|uint16|uint8](self: BoxedValue; value: var T; default: T; lossy: bool): bool {.discardable.} =
    var holder: int64
    if self.unbox(holder, default.uint64, lossy):
        if holder > T.high:
            if lossy:
                value = T.high
                return true
            else:
                value = default
                return false
        else:
            value = holder.T
            return true
    else:
        value = default
        return false

proc unbox*(self: BoxedValue; value: var string; default: string = ""; lossy: bool = false): bool {.discardable.} =
    if self.kind == TextString:
        set_len(value, self.sdata.len)
        if self.sdata.len > 0:
            copymem(addr value[0], unsafeAddr self.sdata[0], self.sdata.len)
    else:
        set_len(value, len(default))
        if default.len > 0:
            copymem(addr value[0], unsafeAddr default[0], len(default))
        result = false

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

# Opens a CBOR writer to do fancy things.
# =======================================================================

proc open_to_buffer*(self: var CborWriter; buffer: ref seq[uint8]) =
    ## Opens the CBOR writer and configures it to append incoming
    ## data to the end of the provided buffer.
    self.actuator = proc(action: WriterAction; data: pointer; data_len: int) =
        case action
        of WriterAction.Write:
            let x = buffer[].len
            set_len(buffer[], len(buffer[]) + data_len)
            copymem(addr buffer[][x], data, data_len)
        of WriterAction.Flush: discard
        of WriterAction.Close: discard

proc open_to_file*(self: var CborWriter; f: File) =
    ## Opens the CBOR writer and configures it to append incoming
    ## data to a file stream.
    self.actuator = proc(action: WriterAction; data: pointer; data_len: int) =
        case action
        of WriterAction.Write:
            # XXX how should we handle errors here?
            discard f.writebuffer(data, data_len)
        of WriterAction.Flush:
            f.flushfile()
        of WriterAction.Close:
            f.close()

proc open_to_buffer*(self: var CborReader; buffer: ref seq[uint8]) =
    ## Opens the CBOR reader and configures it to pull data from the
    ## provided buffer.
    var pos = 0
    self.actuator = proc(action: ReaderAction; data: pointer; data_len: int; read_len: var int) =
        case action
        of ReaderAction.Read:
            if likely((pos + data_len) <= len(buffer[])):
                copymem(data, addr buffer[][pos], data_len)
                inc pos, data_len
                read_len = data_len
            else:
                read_len = 0
        of ReaderAction.Close: discard

proc open_to_file*(self: var CborReader; f: File) =
    ## Opens the CBOR reader and configures it to pull data from the
    ## provided file.
    self.actuator = proc(action: ReaderAction; data: pointer; data_len: int; read_len: var int) =
        case action
        of ReaderAction.Read:
            read_len = f.readbuffer(data, data_len)
        of ReaderAction.Close:
            f.close()

# These just command and control the reader object, which pawns all the
# work off on a closure.
# =======================================================================

proc get*(self: var CborReader; data: pointer; data_len: int): bool {.inline.} =
    ## Attempts to read some data from the underlying stream. Returns
    ## whether the data could be read.
    ##
    ## We expect to retrieve the entirety of data we have asked for.
    ## Otherwise no data from this call is consumed and it is up to the
    ## caller whether to abort or wait for the rest of the data to come
    ## through.
    if self.actuator != nil:
        var actually: int
        self.actuator(ReaderAction.Read, data, data_len, actually)
        return actually == data_len
    else:
        return false

proc close*(self: var CborReader) =
    ## Closes the underlying stream and shuts down the actuator.
    var unused: int
    self.actuator(ReaderAction.Close, nil, 0, unused)
    self.actuator = nil

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

proc write_raw*(writer: var CborWriter; value: uint64) =
    var x: array[8, uint8]
    big_endian64(addr x, unsafeAddr value)
    let y = measure_integer_for_heading(value)
    case y
    of 0, 1:
        put(writer, addr x[7], 1)
    of 2:
        put(writer, addr x[6], 2)
    of 4:
        put(writer, addr x[4], 4)
    of 8:
        put(writer, addr x, 8)
    else:
        raise new_exception(ValueError, "Unknown integer size")

proc write_raw*(writer: var CborWriter; value: uint64; len: int) =
    var x: array[8, uint8]
    big_endian64(addr x, unsafeAddr value)
    case len
    of 0, 1:
        put(writer, addr x[7], 1)
    of 2:
        put(writer, addr x[6], 2)
    of 4:
        put(writer, addr x[4], 4)
    of 8:
        put(writer, addr x, 8)
    else:
        raise new_exception(ValueError, "Unknown integer size")

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
    var x: float32
    big_endian32(addr x, unsafeAddr value)
    put(writer, addr header, 1)
    put(writer, unsafeAddr x, value.sizeof)

proc write*(writer: var CborWriter; value: float64) =
    var header: uint8 = encode_field_heading(7, 27)
    var x: float64
    big_endian64(addr x, unsafeAddr value)
    put(writer, addr header, 1)
    put(writer, unsafeAddr x, value.sizeof)

proc write*(writer: var CborWriter; value: uint64) =
    var write_len: int
    var header = encode_field_heading(PositiveInteger, value, write_len)
    put(writer, addr header, 1)
    write_raw(writer, value, write_len)

proc write*(writer: var CborWriter; value: int64) =
    if value >= 0:
        write(writer, value.uint64)
    else:
        var mvalue = (value * -1).uint64
        var write_len: int
        var header = encode_field_heading(NegativeInteger, mvalue, write_len)
        put(writer, addr header, 1)
        write_raw(writer, mvalue, write_len)

proc write*(writer: var CborWriter; value: string) =
    var write_len: int
    var header = encode_field_heading(TextString, value.len.uint64, write_len)
    put(writer, addr header, 1)
    if write_len > 0:
        var x = len(value).uint64
        write(writer, x)
    put(writer, unsafeAddr value[0], value.len)

proc write*(writer: var CborWriter; value: openarray[uint8]) =
    var write_len: int
    var header = encode_field_heading(ByteString, value.len.uint64, write_len)
    put(writer, addr header, 1)
    if write_len > 0:
        var x = len(value).uint64
        write(writer, x)
    put(writer, unsafeAddr value[0], value.len)

proc write_array_header*(writer: var CborWriter; length: uint64; unknown_length: bool = false) =
    ## Writes a header for an array; used when manually writing the data type to a stream.

    var length_bytes: int
    var header = encode_field_heading(Array, length,  length_bytes,unknown_length)
    put(writer, addr header, 1)
    if length_bytes > 0:
        write_raw(writer, length)

template write*[T](writer: var CborWriter; thing: openarray[T]) =
    write_array_header(writer, thing.len, false)
    for x in thing:
        write(x)

proc write_map_header*(writer: var CborWriter; length: uint64; unknown_length: bool = false) =
    ## Writes a header for a map; used when manually writing the data type to a stream.

    var length_bytes: int
    var header = encode_field_heading(Map, length, length_bytes, unknown_length)
    put(writer, addr header, 1)
    if length_bytes > 0:
        write_raw(writer, length)

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

# Hypest literacy action.
# =======================================================================

proc try_read*(reader: var CborReader; value: var BoxedValue): bool =
    ## Attempts to read either an immediate value or a marker for a
    ## complicated value. An immediate value is one such as a boolean,
    ## a string or an integer, which can be read in to a small struct and
    ## returned as a boxed value.
    ##
    ## A complicated value is an array or a map. Those contain multiple other
    ## values which might also be complicated values, and as such can only
    ## be parsed as a stream of events (SAX style) or to some form of tree.
    ## This function is too low of a level to make storage decisions like this
    ## for you so we choose to output the data as boxed value representing
    ## the heading of this event.
    ##
    ## Does not attempt to enforce any security policy such as ignoring data
    ## of particular sizes. Binary or text strings will be loaded in full.

    proc clear(reader: var CborReader) =
        reader.header = 0
        reader.length = 0

    if reader.header == 0:
        # we have not read a header byte yet, so lets try that
        let all_is_well = reader.get(addr reader.header, 1)
        if not all_is_well:
            return false
    else:
        # we previously failed to read a value; the header byte
        # remains set. so we will try to read that value again
        discard

    zeromem(addr value.data[0], value.data.sizeof)

    if reader.length == 0:
        proc readlength(reader: var CborReader; value: var BoxedValue): bool {.inline.} =
            result = true
            let frags = decode_field_heading(reader.header)
            case frags[0]
            of ord(PositiveInteger):
                value.kind = PositiveInteger
                case frags[1]:
                of 0..23:
                    value.data[7] = frags[1].uint8
                    return true
                of 24: return reader.get(addr value.data[7], 1)
                of 25: return reader.get(addr value.data[6], 2)
                of 26: return reader.get(addr value.data[4], 4)
                of 27: return reader.get(addr value.data[0], 8)
                else:
                    raise new_exception(ValueError, EUNKNOWN_FIELD_LENGTH)
            of ord(NegativeInteger):
                value.kind = NegativeInteger
                case frags[1]:
                of 0..23:
                    value.data[7] = frags[1].uint8
                    return true
                of 24: return reader.get(addr value.data[7], 1)
                of 25: return reader.get(addr value.data[6], 2)
                of 26: return reader.get(addr value.data[4], 4)
                of 27: return reader.get(addr value.data[0], 8)
                else:
                    raise new_exception(ValueError, EUNKNOWN_FIELD_LENGTH)
            of ord(ByteString):
                value.kind = ByteString
                case frags[1]:
                of 0..23:
                    value.data[7] = frags[1].uint8
                    return true
                of 24: return reader.get(addr value.data[7], 1)
                of 25: return reader.get(addr value.data[6], 2)
                of 26: return reader.get(addr value.data[4], 4)
                of 27: return reader.get(addr value.data[0], 8)
                else:
                    raise new_exception(ValueError, EUNKNOWN_FIELD_LENGTH)
            of ord(TextString):
                value.kind = TextString
                case frags[1]:
                of 0..23: value.data[7] = frags[1].uint8
                of 24: return reader.get(addr value.data[7], 1)
                of 25: return reader.get(addr value.data[6], 2)
                of 26: return reader.get(addr value.data[4], 4)
                of 27: return reader.get(addr value.data[0], 8)
                else:
                    raise new_exception(ValueError, EUNKNOWN_FIELD_LENGTH)
            of ord(Array):
                value.kind = Array
                case frags[1]:
                of 0..23:
                    value.data[7] = frags[1].uint8
                    return true
                of 24: return reader.get(addr value.data[7], 1)
                of 25: return reader.get(addr value.data[6], 2)
                of 26: return reader.get(addr value.data[4], 4)
                of 27: return reader.get(addr value.data[0], 8)
                else:
                    raise new_exception(ValueError, EUNKNOWN_FIELD_LENGTH)
            of ord(Map):
                value.kind = Map
                case frags[1]:
                of 0..23:
                    value.data[7] = frags[1].uint8
                    return true
                of 24: return reader.get(addr value.data[7], 1)
                of 25: return reader.get(addr value.data[6], 2)
                of 26: return reader.get(addr value.data[4], 4)
                of 27: return reader.get(addr value.data[0], 8)
                else:
                    raise new_exception(ValueError, EUNKNOWN_FIELD_LENGTH)
            of ord(SemanticTag):
                # TODO should actually interpret the semtag
                return try_read(reader, value)
            of ord(Primitive):
                value.kind = Primitive
                case frags[1]
                of 0..23:
                    value.kind2 = SimpleValueTiny
                    case frags[1]:
                    of ord(False) + 20:
                        value.kind3 = False
                    of ord(True) + 20:
                        value.kind3 = True
                    of ord(Null) + 20:
                        value.kind3 = Null
                    of ord(Undefined) + 20:
                        value.kind3 = Undefined
                    else:
                        raise new_exception(ValueError, EINVALID_TYPE)
                    return true
                of ord(SimpleValueByte) + 23:
                    value.kind2 = SimpleValueByte
                    return reader.get(addr value.data[7], 1)
                of ord(Single) + 23:
                    value.kind2 = Single
                    raise new_exception(ValueError, ENO_SINGLES)
                of ord(Float) + 23:
                    value.kind2 = Float
                    return reader.get(addr value.data[4], 4)
                of ord(Double) + 23:
                    value.kind2 = Double
                    return reader.get(addr value.data[0], 8)
                of ord(Break) + 23:
                    value.kind2 = Break
                    return true
                else:
                    raise new_exception(ValueError, EINVALID_TYPE)
            else:
                raise new_exception(ValueError, EINVALID_TYPE)

        # run interior logic and reset header byte if successful
        result = readlength(reader, value)
        if result:
            when cpu_endian == little_endian:
                var buffer: array[8, uint8]
                copymem(addr buffer[0], addr value.data[0], 8)
                swap_endian64(addr reader.length, addr buffer[0])
            else:
                copymem(addr reader.length, addr value.data[0], 8)
        else:
            return false

    case value.kind:
    of TextString:
        set_len(value.sdata, reader.length)
        if reader.length > 0:
            return reader.get(addr value.sdata[0], reader.length.int)
    of ByteString:
        set_len(value.bdata, reader.length)
        if reader.length > 0:
            return reader.get(addr value.bdata[0], reader.length.int)
    else:
        copymem(addr value.data[0], addr reader.length, 8)

    clear(reader)

proc reset*(reader: var CborReader) =
    ## Resets internal state of the reader but leaves the actuator alone.
    reader.header = 0
    reader.length = 0

iterator values*(reader: var CborReader; box: var BoxedValue): bool =
    ## Repeatedly reads values in to the supplied box, until the
    ## reader stops for some reason.
    while try_read(reader, box) == true:
        yield true

proc read*(reader: var CborReader; sax: CborSaxReader) =
    var box: BoxedValue
    for x in values(reader, box):
        case box.kind
        of PositiveInteger: sax.on_positive_integer(0, box)
        of NegativeInteger: sax.on_negative_integer(0, box)
        of ByteString: sax.on_string_binary(0, box)
        of TextString: sax.on_string_text(0, box)
        of Primitive:
            case box.kind2
            of SimpleValueTiny:
                case box.kind3:
                of False: sax.on_boolean(0, false)
                of True: sax.on_boolean(0, true)
                of Null: sax.on_nil(0)
                of Undefined: sax.on_undefined(0)
            of SimpleValueByte: discard # TODO
            of Single: discard # TODO
            of Float: sax.on_float32(0, box)
            of Double: sax.on_float64(0, box)
            of Break: discard # TODO
        of SemanticTag: discard # TODO
        of Array: discard # TODO
        of Map: discard # TODO

when is_main_module:
    var tests = 0
    echo("TAP version 13")

    proc ok(condition: bool) =
        inc tests
        if condition:
            echo("ok ", tests)
        else:
            echo("not ok ", tests)

    var buffer: ref seq[uint8]
    var writer: CborWriter
    var reader: CborReader
    var box: BoxedValue
    new(buffer)
    open_to_buffer(writer, buffer)
    open_to_buffer(reader, buffer)

    write_nil(writer)
    write(writer, false)
    write(writer, 500)
    write(writer, "i am soup")

    echo("# read a nil")
    let x = try_read(reader, box)
    ok(x == true)
    ok(box.kind == Primitive)
    ok(box.kind2 == SimpleValueTiny)
    ok(box.kind3 == Null)

    echo("# read a boolean")
    let y = try_read(reader, box)
    ok(y == true)
    ok(box.kind == Primitive)
    ok(box.kind2 == SimpleValueTiny)
    ok(box.kind3 == False)

    echo("# read an integer")
    let z = try_read(reader, box)
    ok(z == true)
    ok(box.kind == PositiveInteger)

    echo("# read a string")
    let w = try_read(reader, box)
    var sdata: string
    ok(w == true)
    ok(box.kind == TextString)
    unbox(box, sdata)
    ok(sdata == "i am soup")

    echo("1..", tests)
