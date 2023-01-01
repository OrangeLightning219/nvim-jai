#import "Basic";
#import "Compiler";
#import "File";
#import "Hash_Table";
#import "String";

is_whitespace :: inline ( c: u8 ) -> bool
{
    result := c == #char " " || c == #char "\t" || c == #char "\\";
    return result;
}

eat_all_whitespace :: ( using tokenizer: *Tokenizer )
{
    while true
    {
        if is_whitespace( text.data[ 0 ] )
        {
            advance( *text );
        }
        else if text.data[ 0 ] == #char "/" && text.data[ 1 ] == #char "/"
        {
            while text.data[ 0 ] && text.data[ 0 ] != #char "\n"
            {
                advance( *text );
            }
        }
        else if text.data[ 0 ] == #char "/" && text.data[ 1 ] == #char "*"
        {
            open_comments := 1;
            advance( *text, 2 );
            while text.data[ 0 ] && open_comments > 0
            {
                if text.data[ 0 ] == #char "*" && text.data[ 1 ] == #char "/"
                {
                    open_comments -= 1;
                }
                else if text.data[ 0 ] == #char "/" && text.data[ 1 ] == #char "*"
                {
                    open_comments += 1;
                }
                
                if text.data[ 0 ] == #char "\n"
                {
                    line_count += 1;
                }
                
                advance( *text );
            }
            advance( *text );
        }
        else 
        {
            break;
        }
    }
}

Token_Type :: enum 
{
    UNKNOWN;
    
    OPEN_PAREN;
    CLOSE_PAREN;
    OPEN_BRACKET;
    CLOSE_BRACKET;
    OPEN_BRACE;
    CLOSE_BRACE;
    
    COLON;
    DOUBLE_COLON;
    ARROW;
    SEMICOLON;
    COMMA;
    OPERATOR;
    
    IDENTIFIER;
    KEYWORD;
    DIRECTIVE;
    
    NUMBER;
    STRING;
    
    NEW_LINE;
    
    END_OF_STREAM;
}

Tokenizer :: struct 
{
    text: string;
    line_count: u32;
}

Token :: struct 
{
    type: Token_Type;
    text: string;
}

operator == :: ( a: Token, b: Token ) -> bool
{
    return a.type == b.type && a.text == b.text;
}

operator == :: ( a: Token, b: string ) -> bool #symmetric 
{
    return a.text == b;
}

get_token :: ( using tokenizer: *Tokenizer ) -> Token
{
    eat_all_whitespace( tokenizer );
    
    result: Token;
    result.text.count = 1;
    result.text.data = text.data;
    
    c := <<text.data;
    if text.count > 0
    {
        advance( *text );
    }
    else 
    {
        result.type = .END_OF_STREAM;
        return result;
    }
    
    check_next_operator_char :: ( tokenizer: *Tokenizer, token: *Token, chars: ..u8 ) -> bool
    {
        for chars
        {
            if it == tokenizer.text.data[ 0 ]
            {
                token.text.count = 2;
                advance( *tokenizer.text );
                return true;
            }
        }
        
        return false;
    }
    
    if c == 
    {
    case #char "("; result.type = .OPEN_PAREN;
    case #char ")"; result.type = .CLOSE_PAREN;
    case #char "["; result.type = .OPEN_BRACKET;
    case #char "]"; result.type = .CLOSE_BRACKET;
    case #char "{"; result.type = .OPEN_BRACE;
    case #char "}"; result.type = .CLOSE_BRACE;
    case #char ";"; result.type = .SEMICOLON;
    case #char ","; result.type = .COMMA;
    case #char "\n"; result.type = .NEW_LINE; line_count += 1;
        // / /=
    case #char "/";
        result.type = .OPERATOR;
        check_next_operator_char( tokenizer, *result, #char "=" );
        
        // ^ ^=
    case #char "^";  #through;
        // % %=
    case #char "%";  #through;
        // > >= 
    case #char ">";  #through;
        // + +=
    case #char "+";  #through;
        // * *=
    case #char "*";  #through;
        // = == 
    case #char "=";
        result.type = .OPERATOR;
        check_next_operator_char( tokenizer, *result, #char "=" );
        
        // < <= <<
    case #char "<";
        result.type = .OPERATOR;
        check_next_operator_char( tokenizer, *result, #char "=", #char "<" );
        
        // | || |=
    case #char "|";
        result.type = .OPERATOR;
        check_next_operator_char( tokenizer, *result, #char "=", #char "|" );
        
        // & && &=
    case #char "&";
        result.type = .OPERATOR;
        check_next_operator_char( tokenizer, *result, #char "=", #char "&" );
        
        // ! != 
    case #char "!";
        if check_next_operator_char( tokenizer, *result, #char "=" ) { result.type = .OPERATOR; }
        else { result.type = .UNKNOWN; }
        
        // : :: :=
    case #char ":";
        if check_next_operator_char( tokenizer, *result, #char ":" ) { result.type = .DOUBLE_COLON; }
        else if check_next_operator_char( tokenizer, *result, #char "=" ) { result.type = .OPERATOR; }
        else { result.type = .COLON; }
        
        // - -= ->
    case #char "-";
        result.type = .OPERATOR;
        if check_next_operator_char( tokenizer, *result, #char ">" ) { result.type = .ARROW; }
        else check_next_operator_char( tokenizer, *result, #char "=" );
        
    case #char "\r";
        assert( text.data[ 0 ] == #char "\n" );
        result.type = .NEW_LINE;
        result.text.count = 2;
        advance( *text );
        line_count += 1;
        
    case #char "#";
        result.type = .DIRECTIVE;
        while text.data[ 0 ] && ( is_alpha( text.data[ 0 ] ) || text.data[ 0 ] == #char "," )
        {
            if text.data[ 0 ] == #char ","
            {
                advance( *text );
                eat_all_whitespace( tokenizer );
            }
            else 
            {
                advance( *text );
            }
        }
        result.text.count = text.data - result.text.data;
        
    case #char "\"";
        result.type = .STRING;
        while text.data[ 0 ] && text.data[ 0 ] != #char "\""
        {
            if text.data[ 0 ] == #char "\\" && text.data[ 1 ]
            {
                advance( *text );
            }
            advance( *text );
        }
        
        if text.data[ 0 ] == #char "\""
        {
            advance( *text );
        }
        result.text.count = text.data - result.text.data;
        
    case;
        if is_alnum( c )
        {
            result.type = .IDENTIFIER;
            while text.data[ 0 ] && is_alnum( text.data[ 0 ] )
            {
                advance( *text );
            }
            result.text.count = text.data - result.text.data;
            
            keywords :: string.[ "if", "ifx", "else", "return", "for", "while", "break", "continue", "inline", 
                                 "struct", "enum", "enum_flags", "using", "case", "then" ];
            for keywords
            {
                if result == it
                {
                    result.type = .KEYWORD;
                    break;
                }
            }
        }
        else if is_digit( c )
        {
            result.type = .NUMBER;
            while is_digit( text.data[ 0 ] )
            {
                advance( *text );
            }
            
            if text.data[ 0 ] == #char "." || text.data[ 0 ] == #char "b"
            {
                while is_digit( text.data[ 0 ] )
                {
                    advance( *text );
                }
            }
            else if text.data[ 0 ] == #char "x"
            {
                while is_digit( text.data[ 0 ] ) || is_alpha( text.data[ 0 ] )
                {
                    advance( *text );
                }
            }
            result.text.count = text.data - result.text.data;
        }
        else 
        {
            result.type = .UNKNOWN;
        }
    }
    
    return result;
}

peek_token :: inline ( tokenizer: Tokenizer ) -> Token
{
    return get_token( *tokenizer );
}

Declarations :: struct 
{
    functions: [ .. ]Function_Declaration;
    structs: [ .. ]Struct_Declaration;
}

Struct_Declaration :: struct 
{
    name: string;
    line: u32;
    column: u32 = 1;
    type: string; // struct, enum, enum_flags, union
}

Argument :: struct 
{
    name: string;
    type: string;
}

Function_Declaration :: struct 
{
    name: string;
    line: u32;
    column: u32 = 0;
    arguments: [ .. ]Argument;
    returns: [ .. ]Argument;
}

parse_files_and_encode :: ( files: []string, encoder: *MP_Encoder )
{
    parse_state: Table( string, *Declarations );
    
    for file: files
    {
        file_content, ok := read_entire_file( file );
        if ok
        {
            declarations := New( Declarations );
            functions := *declarations.functions;
            structs := *declarations.structs;
            
            tokenizer: Tokenizer;
            tokenizer.text = file_content;
            
            parsing := true;
            
            previous_token: Token;
            
            while parsing
            {
                token := get_token( *tokenizer );
                using token;
                
                if type == 
                {
                case .DOUBLE_COLON;
                    next_token := peek_token( tokenizer );
                    if next_token.type == .KEYWORD && next_token != "inline"
                    {
                        s: Struct_Declaration;
                        s.name = previous_token.text;
                        s.line = tokenizer.line_count + 1;
                        s.type = next_token.text;
                        
                        array_add( structs, s );
                    }
                    else if next_token.type == .KEYWORD || next_token.type == .OPEN_PAREN
                    {
                        f: Function_Declaration;
                        f.name = previous_token.text;
                        
                        if previous_token.type == .OPERATOR
                        {
                            f.name = tprint( "operator %", previous_token.text );
                        }
                        
                        f.line = tokenizer.line_count + 1;
                        
                        if next_token.type == .KEYWORD
                        {
                            token = get_token( *tokenizer );
                        }
                        
                        token = get_token( *tokenizer ); // (
                        
                        while type != .CLOSE_PAREN
                        {
                            token = get_token( *tokenizer );
                            next_token = peek_token( tokenizer );
                            
                            if type == .IDENTIFIER && next_token.type == .COLON
                            {
                                arg: Argument;
                                arg.name = text;
                                
                                token = get_token( *tokenizer ); // :
                                token = get_token( *tokenizer );
                                
                                arg.type = text;
                                if type == .OPEN_PAREN
                                {
                                    open_parens := 1;
                                    
                                    while open_parens > 0
                                    {
                                        token = get_token( *tokenizer );
                                        if type == .OPEN_PAREN then open_parens += 1;
                                        else if type == .CLOSE_PAREN then open_parens -= 1;
                                    }
                                    next_token = peek_token( tokenizer );
                                    
                                    if next_token.type == .ARROW
                                    {
                                        token = get_token( *tokenizer ); // ->
                                        token = get_token( *tokenizer );
                                        if type == .OPEN_PAREN
                                        {
                                            open_parens = 1;
                                            
                                            while open_parens > 0
                                            {
                                                token = get_token( *tokenizer );
                                                if type == .OPEN_PAREN then open_parens += 1;
                                                else if type == .CLOSE_PAREN then open_parens -= 1;
                                            }
                                            token = get_token( *tokenizer ); // , or )
                                        }
                                        else 
                                        {
                                            token = get_token( *tokenizer ); // , or )
                                        }
                                    }
                                    else 
                                    {
                                        token = get_token( *tokenizer ); // , or )
                                    }
                                    arg.type.count = text.data - arg.type.data;
                                }
                                else 
                                {
                                    while type != .COMMA && type != .CLOSE_PAREN
                                    {
                                        token = get_token( *tokenizer );
                                    }
                                    arg.type.count = text.data - arg.type.data;
                                }
                                array_add( *f.arguments, arg );
                            }
                        }
                        
                        next_token = peek_token( tokenizer );
                        
                        if next_token.type == .ARROW
                        {
                            token = get_token( *tokenizer ); // ->
                            next_token = peek_token( tokenizer );
                            
                            if next_token.type == .OPEN_PAREN
                            {
                                token = get_token( *tokenizer );
                            }
                            
                            end_type: Token_Type = .OPEN_BRACE;
                            if type == .OPEN_PAREN then end_type = .CLOSE_PAREN;
                            
                            while type != end_type
                            {
                                token = get_token( *tokenizer );
                                
                                if type == end_type then break;
                                
                                next_token = peek_token( tokenizer );
                                
                                ret: Argument;
                                ret.type = text;
                                if type == .IDENTIFIER && next_token.type == .COLON
                                {
                                    ret.name = text;
                                    token = get_token( *tokenizer ); // :
                                    token = get_token( *tokenizer );
                                    
                                    ret.type = text;
                                    if type == .OPEN_PAREN
                                    {
                                        open_parens := 1;
                                        
                                        while open_parens > 0
                                        {
                                            token = get_token( *tokenizer );
                                            if type == .OPEN_PAREN then open_parens += 1;
                                            else if type == .CLOSE_PAREN then open_parens -= 1;
                                        }
                                        token = get_token( *tokenizer ); // , or ) or \n or ->
                                        
                                        if type == .ARROW
                                        {
                                            next_token = peek_token( tokenizer );
                                            
                                            if next_token.type == .OPEN_PAREN
                                            {
                                                token = get_token( *tokenizer );
                                                open_parens = 1;
                                                
                                                while open_parens > 0
                                                {
                                                    token = get_token( *tokenizer );
                                                    if type == .OPEN_PAREN then open_parens += 1;
                                                    else if type == .CLOSE_PAREN then open_parens -= 1;
                                                }
                                                token = get_token( *tokenizer ); // , or ) or \n
                                            }
                                            else 
                                            {
                                                while type != .COMMA && type != end_type
                                                {
                                                    token = get_token( *tokenizer );
                                                }
                                            }
                                        }
                                    }
                                    else 
                                    {
                                        while type != .COMMA && type != end_type
                                        {
                                            token = get_token( *tokenizer );
                                        }
                                    }
                                }
                                else if type == .OPEN_PAREN
                                {
                                    open_parens := 1;
                                    
                                    while open_parens > 0
                                    {
                                        token = get_token( *tokenizer );
                                        if type == .OPEN_PAREN then open_parens += 1;
                                        else if type == .CLOSE_PAREN then open_parens -= 1;
                                    }
                                    token = get_token( *tokenizer ); // , or ) or \n or ->
                                    
                                    if type == .ARROW
                                    {
                                        next_token = peek_token( tokenizer );
                                        
                                        if next_token.type == .OPEN_PAREN
                                        {
                                            token = get_token( *tokenizer );
                                            open_parens = 1;
                                            
                                            while open_parens > 0
                                            {
                                                token = get_token( *tokenizer );
                                                if type == .OPEN_PAREN then open_parens += 1;
                                                else if type == .CLOSE_PAREN then open_parens -= 1;
                                            }
                                            token = get_token( *tokenizer ); // , or ) or \n
                                        }
                                        else 
                                        {
                                            while type != .COMMA && type != end_type
                                            {
                                                token = get_token( *tokenizer );
                                            }
                                        }
                                    }
                                }
                                else 
                                {
                                    while type != .COMMA && type != end_type && type != .DIRECTIVE
                                    {
                                        token = get_token( *tokenizer );
                                    }
                                }
                                
                                ret.type.count = text.data - ret.type.data;
                                ret.type = replace( ret.type, "\n", "" );
                                ret.type = replace( ret.type, "\r", "" );
                                ret.type = trim_right( ret.type );
                                array_add( *f.returns, ret );
                                
                                if type == .DIRECTIVE
                                {
                                    token = get_token( *tokenizer );
                                }
                            }
                        }
                        
                        array_add( functions, f );
                    }
                case .END_OF_STREAM;
                    parsing = false;
                }
                
                previous_token = token;
            }
            
            table_set( *parse_state, file, declarations );
            // print( "File: %, lines: %\n", file, tokenizer.line_count );
        }
    }
    
    encode_map( cast( u32 )files.count, encoder );
    for parse_state
    {
        file := it_index;
        encode_string( file, encoder );
        encode_map( 2, encoder );
        {
            declarations := it;
            functions := declarations.functions;
            structs := declarations.structs;
            
            encode_string( "structs", encoder );
            encode_array( cast( u32 )structs.count, encoder );
            for s: structs
            {
                encode_map( 5, encoder );
                
                encode_string( "lnum", encoder );
                encode_uint( s.line, encoder );
                
                encode_string( "col", encoder );
                encode_uint( s.column, encoder );
                
                encode_string( "name", encoder );
                encode_uint( cast( u32 )s.name.count, encoder );
                
                encode_string( "symbol_type", encoder );
                encode_uint( cast( u32 )s.type.count, encoder );
                
                encode_string( "ordinal", encoder );
                encode_string( tprint( "% :: %", s.name, s.type ), encoder );
                // print( "struct %, line: %\n", s.name, s.line );
            }
            
            encode_string( "functions", encoder );
            encode_array( cast( u32 )functions.count, encoder );
            for f: functions
            {
                display: string;
                display = tprint( "% :: ( ", f.name );
                
                encode_map( 6, encoder );
                
                encode_string( "lnum", encoder );
                encode_uint( f.line, encoder );
                
                encode_string( "col", encoder );
                encode_uint( f.column, encoder );
                
                encode_string( "name", encoder );
                encode_uint( cast( u32 )f.name.count, encoder );
                
                encode_string( "arguments", encoder );
                encode_array( cast( u32 )f.arguments.count, encoder );
                
                for arg: f.arguments
                {
                    encode_map( 2, encoder );
                    encode_string( "name", encoder );
                    encode_uint( cast( u32 )arg.name.count, encoder );
                    
                    encode_string( "type", encoder );
                    encode_uint( cast( u32 )arg.type.count, encoder );
                    
                    display = tprint( "%1%2: %3, ", display, arg.name, arg.type );
                }
                
                if f.arguments.count == 0
                {
                    display.count -= 1;
                }
                else 
                {
                    display.count -= 2;
                }
                
                display = tprint( "% ) -> ( ", display );
                
                encode_string( "returns", encoder );
                return_count := ifx f.returns.count == 0 then 1 else f.returns.count;
                encode_array( cast( u32 )return_count, encoder );
                
                for ret: f.returns
                {
                    encode_map( 2, encoder );
                    encode_string( "name", encoder );
                    encode_uint( cast( u32 )ret.name.count, encoder );
                    
                    encode_string( "type", encoder );
                    encode_uint( cast( u32 )ret.type.count, encoder );
                    display = tprint( "%1%2%3%4, ", display, ret.name, ifx ret.name.count == 0 then "" else ": ", ret.type );
                }
                
                if f.returns.count == 0
                {
                    encode_map( 2, encoder );
                    encode_string( "name", encoder );
                    encode_uint( 0, encoder );
                    
                    encode_string( "type", encoder );
                    encode_uint( 4, encoder ); // void
                    display = tprint( "%void )", display );
                }
                else 
                {
                    display.count -= 2;
                    display = tprint( "% )", display );
                }
                // print( "%\n", display );
                encode_string( "ordinal", encoder );
                encode_string( display, encoder );
            }
        }
    }
}

