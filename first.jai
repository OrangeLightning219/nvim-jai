
#import "Basic";
#import "Compiler";
#import "File";
#import "String";

build :: () {
    w := compiler_create_workspace("Target Program");
    if !w {
        print("Workspace creation failed.\n");
        return;
    }

    options := get_build_options(w);
    options.output_executable_name = "nvim-jai";
    options.output_path = "build";
    
    files := string.["main.jai", "message_pack.jai"];

    nvim_jai, ok := file_open(".nvim-jai", for_writing = true, keep_existing_content = false);
    assert(ok);
    
    current_dir := get_working_directory();
    file_content: string;
    for file: files
    {
        file_content = tprint("%1%2/%3\n", file_content, replace(current_dir, "\\", "/"), file);
    }
    file_content.count -= 1;
    file_write(*nvim_jai, file_content);    

    file_close(*nvim_jai);
    
    set_build_options(options, w);

    compiler_begin_intercept(w);

    for file: files
    {
        add_build_file(file, w);
    }

    compiler_end_intercept(w);

    set_build_options_dc(.{do_output=false});
}

#run build();
