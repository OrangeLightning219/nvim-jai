#import "Basic";
#import "Compiler";
#import "File";
#import "Hash_Table";
#import "String";

#load "message_pack.jai";

build :: () {
    context.allocator = temp;
    w := compiler_create_workspace("Parser");
    if !w {
        print("Workspace creation failed.\n");
        return;
    }
    set_build_options_dc(.{do_output=false});


    options := get_build_options(w);
    args := options.compile_time_command_line;
    if args.count < 3
    {
        print("Please provide files to parse\n");
        return;
    }

    set_build_options(options, w);

    compiler_begin_intercept(w);

    files: Table(string, int);
    for index: 2 .. args.count - 1
    {
        file := args[index];
        if file != #file
        {
            table_add(*files, file, 0);
        }
    }

    for files
    {
        print("Adding file: %\n", it_index);
        add_build_file(it_index, w);
    }


    RESPONSE_BUFFER_SIZE :: 1024 * 1024;
    encoder: MP_Encoder;
    encoder_base: *u8 = alloc(RESPONSE_BUFFER_SIZE);
    encoder.at = encoder_base;
    encoder.allocated_size = RESPONSE_BUFFER_SIZE;

    all_functions: Table(string, [..]*Code_Procedure_Header);

    while true 
    {
        message := compiler_wait_for_message();
        
        if message.kind == 
        {
          case .PHASE;
            phase := cast(*Message_Phase) message;
            if phase.phase == .TYPECHECKED_ALL_WE_CAN
            {
                print("File count: %\n", files.count);
                encode_map(cast(u32)files.count, *encoder);
                for files
                {
                    file := it_index;
                    encode_string(file, *encoder);
                    encode_map(1, *encoder);
                    {
                        functions, found := table_find(*all_functions, it_index);
                        assert(found);
                        encode_string("functions", *encoder);
                        encode_array(cast(u32)functions.count, *encoder);
                        for header: functions
                        {
                            display: string;
                            display = tprint("% :: ( ", header.name);

                            encode_map(6, *encoder);

                            encode_string("lnum", *encoder);
                            encode_uint(cast(u32)header.l0, *encoder);

                            encode_string("col", *encoder);
                            encode_uint(cast(u32)header.c0, *encoder);

                            encode_string("name", *encoder);
                            encode_uint(cast(u32)header.name.count, *encoder);

                            encode_string("arguments", *encoder);
                            encode_array(cast(u32)header.arguments.count, *encoder);

                            for arg: header.arguments
                            {
                                encode_map(2, *encoder);
                                encode_string("name", *encoder);
                                encode_uint(cast(u32)arg.name.count, *encoder);

                                type := get_type_string(arg.type_inst.result);
                                if arg.type_inst.result.type == .PROCEDURE
                                {
                                    t := cast(*Type_Info_Procedure) arg.type_inst.result;
                                    type = tprint("( ");

                                    for proc_arg: t.argument_types
                                    {
                                        type = tprint("%1%2, ", type, get_type_string(proc_arg));
                                    }
                                    
                                    if t.argument_types.count == 0
                                    {
                                        type.count -= 1;
                                    }
                                    else
                                    {
                                        type.count -= 2;
                                    }

                                    type = tprint("% ) -> ( ", type);

                                    for proc_ret: t.return_types
                                    {
                                        type = tprint("%1%2, ", type, get_type_string(proc_ret));
                                    }
                                    
                                    if t.return_types.count == 0
                                    {
                                        type = tprint("%void )", type);   
                                    }
                                    else
                                    {
                                        type.count -= 2;
                                        type = tprint("% )", type);                                    
                                    }
                                }
                                encode_string("type", *encoder);
                                encode_uint(cast(u32)type.count, *encoder);
                                display = tprint("%1%2: %3, ", display, arg.name, type);
                            }

                            if header.arguments.count == 0
                            {
                                display.count -= 1;   
                            }
                            else
                            {
                                display.count -= 2;
                            }

                            display = tprint("% ) -> ( ", display);

                            encode_string("returns", *encoder);
                            return_count := ifx header.returns.count == 0 then 1 else header.returns.count;
                            encode_array(cast(u32)return_count, *encoder);

                            for ret: header.returns
                            {
                                encode_map(2, *encoder);
                                encode_string("name", *encoder);
                                encode_uint(cast(u32)ret.name.count, *encoder);

                                type := get_type_string(ret.type_inst.result);
                                encode_string("type", *encoder);
                                encode_uint(cast(u32)type.count, *encoder);
                                display = tprint("%1%2%3%4, ", display, ret.name, ifx ret.name.count == 0 then "" else ": ", type);
                            }

                            if header.returns.count == 0
                            {
                                encode_map(2, *encoder);
                                encode_string("name", *encoder);
                                encode_uint(0, *encoder);

                                encode_string("type", *encoder);
                                encode_uint(4, *encoder); // void
                                display = tprint("%void )", display);   
                            }
                            else
                            {
                                display.count -= 2;
                                display = tprint("% )", display);
                            }
                            print("%\n", display);
                            encode_string("ordinal", *encoder);
                            encode_string(display, *encoder);
                        }
                        
                    }
                }
                data: string;
                data.data = encoder_base;
                data.count = encoder.length;
                write_entire_file("declarations.response", data);
                exit(2);
            }
            
          case .TYPECHECKED;
            typechecked := cast(*Message_Typechecked) message;

            for typechecked.all 
            {
                expression : *Code_Node = it.expression;
                file: *Message_File = expression.enclosing_load;
                if file 
                {
                    path := file.fully_pathed_filename;
                    _, found := table_find(*files, path);

                    if found
                    {
                        if(expression.kind == .PROCEDURE_HEADER)
                        {
                            header := cast(*Code_Procedure_Header) expression;
                            if header.name != ""
                            {
                                list := table_find_pointer(*all_functions, path);
                                if list == null
                                {
                                    new_list := New([..]*Code_Procedure_Header);
                                    array_add(new_list, header);
                                    table_add(*all_functions, path, <<new_list);   
                                }
                                else
                                {
                                    array_add(list, header);       
                                }    
                            }
                        }
                    }
                }
            }
            
          case .COMPLETE;
            break;
        }
    }
    compiler_end_intercept(w);
}

get_type_string :: (type: *Type_Info) -> string
{
    result: string;
    if type.type ==
    {
        case .STRUCT;
            t := cast(*Type_Info_Struct) type;
            result = tprint("%", t.name);
        case .INTEGER;
            t := cast(*Type_Info_Integer) type;
            sign := ifx t.signed then "s" else "u";
            result = tprint("%1%2", sign, t.runtime_size * 8);
        case .FLOAT;
            result = tprint("f%", type.runtime_size * 8);
        case .BOOL;
            result = tprint("bool");
        case .STRING;
            result = tprint("string");
        case .ARRAY;
            t := cast(*Type_Info_Array) type;
            if t.array_type ==
            {
                case .FIXED;
                    result = tprint("[%]", t.array_count);
                case .VIEW;
                    result = tprint("[]");
                case .RESIZABLE;
                    result = tprint("[..]");
            }
            result = tprint("%1%2", result, get_type_string(t.element_type));
        case .ENUM;
            t := cast(*Type_Info_Enum) type;
            result = tprint("%", t.name);
        case .POINTER;
            t := cast(*Type_Info_Pointer) type;
            result = tprint("*%", get_type_string(t.pointer_to));
        case .ANY;
            result = tprint("Any");
        case .POLYMORPHIC_VARIABLE;
            result = tprint("$T");
    }
    return result;
}

#run build();

