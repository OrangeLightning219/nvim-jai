#import "Bit_Operations";

MP_Type :: enum u8 #specified
{
    INVALID :: 0xFF;
    // https://github.com/msgpack/msgpack/blob/master/spec.md#formats
    POSITIVE_FIX_INT :: 0x00;
    FIX_MAP :: 0x80;
    FIX_ARRAY :: 0x90;
    FIX_STRING :: 0xa0;
    NIL :: 0xc0;
    BOOL_FALSE :: 0xc2;
    BOOL_TRUE :: 0xc3;
    BINARY_8 :: 0xc4;
    BINARY_16 :: 0xc5;
    BINARY_32 :: 0xc6;
    EXT_8 :: 0xc7;
    EXT_16 :: 0xc8;
    EXT_32 :: 0xc9;
    FLOAT_32 :: 0xca;
    FLOAT_64 :: 0xcb;
    UINT_8 :: 0xcc;
    UINT_16 :: 0xcd;
    UINT_32 :: 0xce;
    UINT_64 :: 0xcf;
    INT_8 :: 0xd0;
    INT_16 :: 0xd1;
    INT_32 :: 0xd2;
    INT_64 :: 0xd3;
    FIX_EXT_1 :: 0xd4;
    FIX_EXT_2 :: 0xd5;
    FIX_EXT_4 :: 0xd6;
    FIX_EXT_8 :: 0xd7;
    FIX_EXT_16 :: 0xd8;
    STRING_8 :: 0xd9;
    STRING_16 :: 0xda;
    STRING_32 :: 0xdb;
    ARRAY_16 :: 0xdc;
    ARRAY_32 :: 0xdd;
    MAP_16 :: 0xde;
    MAP_32 :: 0xdf;
    NEGATIVE_FIX_INT :: 0xe0;
}

MP_Parser :: struct
{
    at: *u8;
}

test :: (parser: $T) -> (asd: float, zxc: **MP_Parser)
{

}

get_type :: ( parser: *MP_Parser ) -> MP_Type
{
    result := MP_Type.INVALID;
    byte := <<parser.at;
    if byte >= 0x00 && byte <= 0x7f { result = .POSITIVE_FIX_INT; }
    else if byte >= 0x80 && byte <= 0x8f { result = .FIX_MAP; }
    else if byte >= 0x90 && byte <= 0x9f { result = .FIX_ARRAY; }
    else if byte >= 0xa0 && byte <= 0xbf { result = .FIX_STRING; }
    else if byte >= 0xe0 && byte <= 0xff { result = .NEGATIVE_FIX_INT; }
    else
    {
        result = cast(MP_Type) byte;
    }

    return result;
}

parse_array_length :: ( parser: *MP_Parser ) -> u32
{
    result: u32 = 0xFFFFFFFF;
    type := get_type( parser );

    if type ==
    {
        case .FIX_ARRAY;
            result = <<parser.at & 0b00001111;
            parser.at += 1;

        case .ARRAY_16;
            result = byte_swap( << cast( *u16 ) ( parser.at + 1 ) );
            parser.at += 3;

        case .ARRAY_32;
            result = byte_swap( << cast(*u32) ( parser.at + 1 ) );
            parser.at += 5;
    }

    return result;
}

parse_uint :: ( parser: *MP_Parser ) -> u32
{
    result: u32 = 0xFFFFFFFF;
    type := get_type( parser );
    if type ==
    {
        case .POSITIVE_FIX_INT;
            result = <<parser.at & 0b01111111;
            parser.at += 1;

        case .UINT_8;
            result = <<( parser.at + 1 );
            parser.at += 2;

        case .UINT_16;
            result = byte_swap( << cast( *u16 ) ( parser.at + 1 ) );
            parser.at += 3;

        case .UINT_32;
            result = byte_swap( << cast( *u32 ) ( parser.at + 1 ) );
            parser.at += 5;
    }
    return result;
}

parse_string :: ( parser: *MP_Parser ) -> string
{
    result: string;
    type := get_type( parser );
    if type ==
    {
        case .FIX_STRING;
            result.count= <<parser.at & 0b00011111;
            result.data = parser.at + 1;
            parser.at += result.count + 1;

        case .STRING_8;
            result.count = <<( parser.at + 1 );
            result.data = parser.at + 2;
            parser.at += result.count + 2;

        case .STRING_16;
            result.count = byte_swap( << cast( *u16 ) ( parser.at + 1 ) );
            result.data = parser.at + 3;
            parser.at += result.count + 3;

        case .STRING_32;
            result.count = byte_swap( << cast( *u32 ) ( parser.at + 1 ) );
            result.data = parser.at + 5;
            parser.at += result.count + 5;
    }

    return result;
}

MP_Encoder :: struct
{
    at: *u8;
    length: u32;
    allocated_size: u32;
};

encode_uint :: ( value: u32, encoder: *MP_Encoder ) 
{
    if value < 128
    {
        assert(encoder.length + 1 <= encoder.allocated_size);
        <<encoder.at = cast( u8 ) value;
        encoder.at += 1;
        encoder.length += 1;
    }
    else if value <= 0xFF
    {
        assert(encoder.length + 2 <= encoder.allocated_size);
        <<encoder.at = cast(u8) MP_Type.UINT_8;
        encoder.at += 1;
        <<encoder.at = cast( u8 ) value;
        encoder.at += 1;
        encoder.length += 2;
    }
    else if value <= 0xFFFF
    {
        assert(encoder.length + 3 <= encoder.allocated_size);
        <<encoder.at = cast(u8) MP_Type.UINT_16;
        encoder.at += 1;
        <<( cast( *u16 ) encoder.at ) = byte_swap( cast( u16 ) value );
        encoder.at += 2;
        encoder.length += 3;
    }
    else
    {
        assert(encoder.length + 5 <= encoder.allocated_size);
        <<encoder.at = cast(u8) MP_Type.UINT_32;
        encoder.at += 1;
        <<( cast( *u32 ) encoder.at ) = byte_swap( cast( u32 ) value );
        encoder.at += 4;
        encoder.length += 5;
    }
}

encode_nil :: ( encoder: *MP_Encoder )
{
    assert(encoder.length + 1 <= encoder.allocated_size);
    <<encoder.at = cast(u8) MP_Type.NIL;
    encoder.at += 1;
    encoder.length += 1;
}

encode_map :: ( length: u32, encoder: *MP_Encoder )
{
    if length < 16
    {
        assert(encoder.length + 1 <= encoder.allocated_size);
        <<encoder.at = (cast(u8) MP_Type.FIX_MAP) | cast( u8 ) length;
        encoder.at += 1;
        encoder.length += 1;
    }
    else if length <= 0xFFFF
    {
        assert(encoder.length + 3 <= encoder.allocated_size);
        <<encoder.at = cast(u8) MP_Type.MAP_16;
        encoder.at += 1;
        <<( cast( *u16 ) encoder.at ) = byte_swap( cast( u16 ) length );
        encoder.at += 2;
        encoder.length += 3;
    }
    else
    {
        assert(encoder.length + 5 <= encoder.allocated_size);
        <<encoder.at = cast(u8) MP_Type.MAP_32;
        encoder.at += 1;
        <<( cast( *u32 ) encoder.at ) = byte_swap( cast( u32 ) length );
        encoder.at += 4;
        encoder.length += 5;
    }
}

encode_array :: ( length: u32, encoder: *MP_Encoder )
{
    if length < 16
    {
        assert(encoder.length + 1 <= encoder.allocated_size);
        <<encoder.at = (cast(u8) MP_Type.FIX_ARRAY) | cast( u8 ) length;
        encoder.at += 1;
        encoder.length += 1;
    }
    else if length <= 0xFFFF
    {
        assert(encoder.length + 3 <= encoder.allocated_size);
        <<encoder.at = cast(u8) MP_Type.ARRAY_16;
        encoder.at += 1;
        <<( cast( *u16 ) encoder.at ) = byte_swap( cast( u16 ) length );
        encoder.at += 2;
        encoder.length += 3;
    }
    else
    {
        assert(encoder.length + 5 <= encoder.allocated_size);
        <<encoder.at = cast(u8) MP_Type.ARRAY_32;
        encoder.at += 1;
        <<( cast( *u32 ) encoder.at ) = byte_swap( cast( u32 ) length );
        encoder.at += 4;
        encoder.length += 5;
    }
}

encode_string :: ( s: string, encoder: *MP_Encoder )
{
    length := s.count;

    if length < 32
    {
        assert(encoder.length + 1 <= encoder.allocated_size);
        <<encoder.at =  (cast(u8) MP_Type.FIX_STRING)| cast( u8 ) length;
        encoder.at += 1;
        encoder.length += 1;
    }
    else if length <= 0xFF
    {
        assert(encoder.length + 2 <= encoder.allocated_size);
        <<encoder.at = cast(u8) MP_Type.STRING_8;
        encoder.at += 1;
        <<encoder.at = cast( u8 ) length;
        encoder.at += 1;
        encoder.length += 2;
    }
    else if length <= 0xFFFF
    {
        assert(encoder.length + 3 <= encoder.allocated_size);
        <<encoder.at = cast(u8) MP_Type.STRING_16;
        encoder.at += 1;
        <<( cast( *u16 ) encoder.at ) = byte_swap( cast( u16 ) length );
        encoder.at += 1;
        encoder.length += 3;
    }
    else
    {
        assert(encoder.length + 5 <= encoder.allocated_size);
        <<encoder.at = cast(u8) MP_Type.STRING_32;
        encoder.at += 1;
        <<( cast( *u32 ) encoder.at ) = byte_swap( cast( u32 ) length );
        encoder.at += 1;
        encoder.length += 5;
    }

    assert(encoder.length + length <= encoder.allocated_size);
    for i: 0..length - 1
    {
        <<encoder.at = s.data[ i ];
        encoder.at += 1;
    }
    encoder.length += cast( u32 ) length;
}

encode_bool :: ( value: bool, encoder: *MP_Encoder )
{
    assert(encoder.length + 1 <= encoder.allocated_size);
    if value
    {
        <<encoder.at = xx MP_Type.BOOL_TRUE;
    }
    else
    {
        <<encoder.at = xx MP_Type.BOOL_FALSE;
    }
    encoder.at += 1;
    encoder.length += 1;
}
