#import "Basic";
#import "Socket";
#import "File";
#import "File_Utilities";
#import "Hash_Table";
#import "String";
#import "Process";

#load "message_pack.jai";
#load "parser.jai";

File_State :: struct 
{
    modtime: Apollo_Time;
    size: s64;
}

operator == :: ( a: File_State, b: File_State ) -> bool #symmetric 
{
    return a.modtime == b.modtime && a.size == b.size;
}

main :: ()
{
    failed := socket_init();
    if !failed
    {
        print( "socket_init failed\n" );
        return;
    }
    
    info: *addrinfo;
    hints: addrinfo;
    
    hints.ai_family = AF_INET;
    hints.ai_socktype = .SOCK_STREAM;
    hints.ai_protocol = .IPPROTO_TCP;
    hints.ai_flags = .AI_PASSIVE;
    
    args := get_command_line_arguments();
    assert( args.count >= 2 );
    
    p := args[ 1 ];
    port := cast( *u8 )alloc( p.count + 1 );
    memcpy( port, p.data, p.count );
    port[ p.count ] = 0;
    
    result := getaddrinfo( null, port, *hints, *info );
    
    if result != 0
    {
        print( "getaddrinfo failed %d\n", result );
        WSACleanup();
        return;
    }
    
    listen_socket := INVALID_SOCKET;
    
    listen_socket = socket( info.ai_family, info.ai_socktype, info.ai_protocol );
    
    if listen_socket == INVALID_SOCKET
    {
        print( "socket creation failed %\n", get_last_socket_error() );
        WSACleanup();
        return;
    }
    
    result = bind( listen_socket, info.ai_addr, cast( s32 )info.ai_addrlen );
    
    if result == SOCKET_ERROR
    {
        print( "bind failed %\n", get_last_socket_error() );
        closesocket( listen_socket );
        WSACleanup();
        return;
    }
    
    result = listen( listen_socket, SOMAXCONN );
    
    if result == SOCKET_ERROR
    {
        print( "listen failed %\n", result );
        closesocket( listen_socket );
        WSACleanup();
        return;
    }
    
    client_socket := INVALID_SOCKET;
    client_socket = accept( listen_socket );
    
    if client_socket == INVALID_SOCKET
    {
        print( "accept failed %\n", get_last_socket_error() );
        closesocket( listen_socket );
        WSACleanup();
        return;
    }
    
    receive_buffer: [ 512 ]u8;
    
    print( "Listening for messages...\n" );
    
    parser: MP_Parser;
    
    RESPONSE_BUFFER_SIZE :: 1024 * 1024;
    response_buffer: *u8 = alloc( RESPONSE_BUFFER_SIZE );
    encoder: MP_Encoder;
    encoder.at = response_buffer;
    encoder.allocated_size = RESPONSE_BUFFER_SIZE;
    
    file_states: Table( string, File_State );
    file_states.allocator = context.allocator;
    
    running := true;
    
    context.allocator = temp;
    while running
    {
        reset_temporary_storage();
        bytes_received := recv( client_socket, receive_buffer.data, receive_buffer.count, 0 );
        
        if bytes_received > 0
        {
            parser.at = receive_buffer.data;
            
            array_length := parse_array_length( *parser );
            assert( array_length == 4 );
            
            interaction_type := parse_uint( *parser );
            assert( interaction_type == 0 );
            
            message_id := parse_uint( *parser );
            command := parse_string( *parser );
            
            arg_count := parse_array_length( *parser );
            assert( arg_count == 0 ); // no arguments for now
            
            print( "Received command %\n", command );
            
            encoder.at = response_buffer;
            encoder.length = 0;
            
            encode_array( 4, *encoder );
            encode_uint( 1, *encoder );
            encode_uint( message_id, *encoder );
            encode_nil( *encoder );
            
            if command == "Exit"
            {
                encode_uint( 0, *encoder );
                print( "Exiting...\n" );
                running = false;
            }
            else if command == "GetDeclarations"
            {
                files_to_parse: [ .. ]string;
                all_files := file_list( get_working_directory(), recursive = true );
                
                for file: all_files
                {
                    if ends_with( file, ".jai" )
                    {
                        modtime, size, ok := file_modtime_and_size( file );
                        assert( ok );
                        file_state: File_State;
                        file_state.modtime = modtime;
                        file_state.size = size;
                        current_state, found := table_find( *file_states, file );
                        if found
                        {
                            if current_state.modtime != modtime || current_state.size != size
                            {
                                array_add( *files_to_parse, file );
                                table_set( *file_states, file, file_state );
                            }
                        }
                        else 
                        {
                            array_add( *files_to_parse, file );
                            table_set( *file_states, file, file_state );
                        }
                    }
                }
                
                send_update := files_to_parse.count != 0;
                
                print( "%\n", files_to_parse );
                
                encode_array( 2, *encoder );
                encode_bool( send_update, *encoder );
                
                if send_update
                {
                    parse_files_and_encode( files_to_parse, *encoder );
                }
                else 
                {
                    encode_nil( *encoder );
                }
            }
            
            bytes_sent := send( client_socket, response_buffer, cast( s32 )encoder.length, 0 );
            
            if bytes_sent == SOCKET_ERROR
            {
                print( "send failed %d\n", get_last_socket_error() );
                closesocket( client_socket );
                WSACleanup();
                return;
            }
            else 
            {
                print( "Response sent\n" );
            }
        }
    }
    
    result = shutdown( client_socket, SD_SEND );
    
    if result == SOCKET_ERROR
    {
        print( "shutdown failed %d\n", get_last_socket_error() );
    }
    
    closesocket( client_socket );
    WSACleanup();
}
