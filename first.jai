
#import "Basic";
#import "Compiler";
#import "File";
#import "String";

build :: () { 
    w := compiler_create_workspace( "Target Program" );
    if !w { 
        print( "Workspace creation failed.\n" );
        return;
    }
    
    options := get_build_options( w );
    options.output_executable_name = "nvim-jai";
    options.output_path = "build";
    
    set_build_options( options, w );
    
    compiler_begin_intercept( w );
    
    add_build_file( "main.jai", w );
    
    compiler_end_intercept( w );
    
    set_build_options_dc( .{ do_output = false } );
}

#run build();

