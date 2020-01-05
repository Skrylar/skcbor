  - [Measuring encoding length](#measuring-encoding-length)
  - [Computing header bytes](#computing-header-bytes)
  - [Boxed values](#boxed-values)
  - [Writing](#writing)
      - [Opening streams for writing](#opening-streams-for-writing)
      - [Low-level stream commands](#low-level-stream-commands)
      - [Writing special types](#writing-special-types)
      - [Writing numbers](#writing-numbers)
      - [Writing booleans](#writing-booleans)
      - [Writing floating point
        numbers](#writing-floating-point-numbers)
      - [Writing parts of arrays](#writing-parts-of-arrays)
      - [Writing parts of maps](#writing-parts-of-maps)
      - [Writing strings and binary
        blobs](#writing-strings-and-binary-blobs)
      - [Writing arbitrary arrays](#writing-arbitrary-arrays)
      - [Convenience procedures](#convenience-procedures)
  - [Reading](#reading)
      - [Managing a reader](#managing-a-reader)
      - [Reading boxed values](#reading-boxed-values)
  - [License](#license)

SkCBOR provides stream writing and stream reading procedures that
operate on the [Concise Binary Object Representation](https://cbor.io)
format. It can be used to implement a SAX style parser which can in turn
be used to either read values directly to an in-memory format or to a
DOM format.

# Measuring encoding length

``` nim
proc measure_integer_for_heading*(i: uint64): int
```

Inspects a number and determines whether it fits in a tiny field or
requires a longer one to store.

If the number is tiny and would fit within a header byte, `0` is
returned. Otherwise the number of bytes required to store the number is
returned.

CBOR uses a fixed set of possibilities: `0`, `1`, `2`, `4` or `8` bytes
to store an integer. This is contrast to Protocol Buffers and Unicode
where numbers are encoded as a varying number of `0xEF` segments.

# Computing header bytes

``` nim
proc encode_field_heading*(major, additional: int): uint8
proc encode_field_heading*(major: FieldMajorKind; length: uint64; out_length_bytes: var int; unknown_length: bool = false): uint8
```

Encodes the major and additional fields in to a single byte. Comes in a
variant which works on raw integers and one which works on the symbolic
meaning of those integers.

# Boxed values

    proc unbox*(self: BoxedValue; value: var float64; default: float64    ; lossy: bool): bool {.discardable.}
    proc unbox*(self: BoxedValue; value: var bool   ; default: bool       ; lossy: bool): bool {.discardable.}
    proc unbox*(self: BoxedValue; value: var uint64 ; default: uint64     ; lossy: bool): bool {.discardable.}
    proc unbox*(self: BoxedValue; value: var int64  ; default: int64      ; lossy: bool): bool {.discardable.}
    proc unbox*(self: BoxedValue; value: var string ; default: string = ""; lossy: bool = false): bool {.discardable.}

Attempts to retrieve a value from a box. `self` is the box that will be
unpacked. `value` is a variable where an unpacked value will be stored.
`default` is what will be stored in `value` if type conversion failed.
`lossy` determines if a lossy conversion is allowed.

A *lossy conversion* is one which may involve changing type and possibly
losing precision. For example it *might* be possible to unpack an
integer as a string but doing so requires conversion steps which lose
the usefulness of the original integer type.

# Writing

    type
        CborWriter* = object
            actuator*: WriterActuator

A `CborWriter` holds state related to pushing bits of values encoded as
CBOR to some kind of backing store. This can be anything from a file to
a ring buffer going out to the network.

    type
        WriterActuator* = proc(action: WriterAction;
                               data: pointer;
                               data_len: int) {.closure.}

The writer actuator takes care of writing, flushing and closing some
underlying stream when writing CBOR values.

## Opening streams for writing

    proc open_to_buffer*(self: var CborWriter; buffer: ref seq[uint8])

This procedure prepares the writer to append data to the end of the
supplied stream.

`buffer` is a reference so it may be captured in a closure.

## Low-level stream commands

    proc put*  (self: var CborWriter; data: pointer; data_len: int) {.inline.}
    proc close*(self: var CborWriter)
    proc flush*(self: var CborWriter)

These procedures are the public API which control a `CborWriter`.

  - put
    Instructs the writer to stuff `data_len` number of bytes taken from
    the `data` buffer and store those in some way.
  - close
    Instructs the writer to close the underlying stream.
  - flush
    Instructs the writer to flush any buffered changes to the underlying
    stream.

Primarily, the `put` operation is used. The `write` family of procedures
uses `put` to push the actual bytes to write. `close` and `flush` are
not used by the `write` API and are there mostly because whoever is
doing the writing might have a need of it.

    proc decode_field_heading*(heading: uint8): (int, int) =

Takes a single byte; extracts the major and additional byte from it.

    proc write_raw*(writer: var CborWriter; value: uint64)
    proc write_raw*(writer: var CborWriter; value: uint64; len: int)

Writes a number to the writer directly. It will calculate the smallest
number of bytes to use (if `len` is not supplied) or the requested
number of bytes (if `len` is supplied.)

The variant which accepts the `len` parameter is intended for calls that
have already written a header. The amount of bytes needed to store a
payload length were already calculated by `measure_integer_for_heading`
in those cases. Calculating the amount of bytes to write a second time
is redundant.

Mostly for internal use.

## Writing special types

    proc write_nil*      (writer: var CborWriter)
    proc write_undefined*(writer: var CborWriter)
    proc write_break*    (writer: var CborWriter)

Writes a special value to the CBOR stream.

  - nil
    An explicit nil/null value.
  - undefined
    A value which is not defined; this is kind of like nil, but
    explicitly means the value was not set.
  - break
    Breaks a sequence with an undefined length.

## Writing numbers

    proc write*(writer: var CborWriter; value: uint64)
    proc write*(writer: var CborWriter; value: int64)

Writes values of the largest word type to the writer.

Note that trying to write a positive `int64` is identical to writing a
`uint64` in CBOR.

## Writing booleans

    proc write*(writer: var CborWriter; value: bool)

Writes a special `true` or `false` value to the writer, depending on the
supplied boolean.

## Writing floating point numbers

    proc write*(writer: var CborWriter; value: float32)
    proc write*(writer: var CborWriter; value: float64)

Writes a fixed-size floating point number to the writer.

## Writing parts of arrays

    proc write_array_header*(writer: var CborWriter; length: uint64; unknown_length: bool = false)

Writes a header that indicates some number of array elements follow. You
must then write that many CBOR objects. If `unknown_length` is true, you
must call `write_break` when there are no more elements being written.

## Writing parts of maps

    proc write_map_header*(writer: var CborWriter; length: uint64; unknown_length: bool = false)

Writes a header that indicates some number of key/value pair elements
follow. You must then write that many key/value pairs. A key/value pair
is a CBOR value which represents the key, followed immediately by
another CBOR value belonging to that key. If `unknown_length` is true,
you must call `write_break` when there are no more elements being
written.

## Writing strings and binary blobs

    proc write*(writer: var CborWriter; value: string)

Writes a string header, length of the string, and the string itself to
the writer. The `TextString` type is used in the underlying CBOR.

    proc write*(writer: var CborWriter; value: openarray[uint8])

Writes a byte string header, length of the byte string, and the bytes
themselves to the writer. The `ByteString` type is used in the
underlying CBOR.

## Writing arbitrary arrays

    template write*[T](writer: var CborWriter; thing: openarray[T])

This function accepts an open array of any type. It writes an array
header with the number of elements in the open array. Then it writes, in
order, every element of the open array using a `write` function.

Open arrays of non-primitive types are compatible *provided* you have a
`write` function defined for them.

## Convenience procedures

    proc write*(writer: var CborWriter; value: uint  ) {.inline.}
    proc write*(writer: var CborWriter; value: uint8 ) {.inline.}
    proc write*(writer: var CborWriter; value: uint16) {.inline.}
    proc write*(writer: var CborWriter; value: uint32) {.inline.}
    proc write*(writer: var CborWriter; value: int   ) {.inline.}
    proc write*(writer: var CborWriter; value: int8  ) {.inline.}
    proc write*(writer: var CborWriter; value: int16 ) {.inline.}
    proc write*(writer: var CborWriter; value: int32 ) {.inline.}

These write functions allow you to write numbers from explicit bit
lengths without having to convert them first. They are converted to the
system’s longest word type, then intelligently encoded as the smallest
numeric type we can.

# Reading

    type
        CborReader* = object
            actuator*: ReaderActuator
            ...

A reader object holds state related to reading bytes which are then used
to decode CBOR values.

  - actuator
    A closure which handles retrieving bytes on behalf of decoding
    procedures.

<!-- end list -->

    type
        ReaderActuator* = proc(action: ReaderAction;
                               data: pointer;
                               data_len: int;
                               read_len: var int) {.closure.}

The reader’s actuator is responsible for fielding requests for bytes. It
may be asked to read `data_len` number of bytes in to the provided
`data` buffer, and then write the number of bytes actually read in to
`read_len`. If `read_len` and `data_len` do not match then no data
should be consumed by the reader.

Note that `try_read` will try to load an entire object at once (except
for arrays and maps.) So a text or byte string needs to load entirely in
to whatever memory the actuator uses behind the scenes. Storing large
blocks of data within messenging formats like CBOR or JSON is generally
a bad idea to begin with, so the impact of this downside is
questionable.

## Managing a reader

    proc open_to_buffer*(self: var CborReader; buffer: ref seq[uint8])

Opens a reader to

`buffer` is a reference so it may be captured in a closure.

    proc get*(self: var CborReader; data: pointer; data_len: int): bool {.inline.}

Instructs the reader to retrieve `data_len` number of bytes and store
them in `data`. Returns `true` if all requested data was loaded. Returns
`false` if not all of the data was ready to load or could not be loaded.

NOTE: This function may change to return number of bytes read, since
that is used elsewhere.

    proc close*(self: var CborReader)

Instructs the reader to close its underlying stream.

## Reading boxed values

    proc try_read*(reader: var CborReader; value: var BoxedValue): bool

Attempts to read a value from a reader. Returns `true` if a boxed value
was read from the reader and written to `value`, otherwise returns
`false.`

Maps and arrays are read simply as headers that indicate their element
count. Other types of data are read as an entire boxed value.

# License

SkCBOR is made available under the *Mozilla Public License v2*, such
that:

  - You may use the module commercially without having to be open
    source,
  - Changes to the module itself must be released back as open source.
