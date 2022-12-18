
local pickers = require("telescope.pickers")
local finders = require("telescope.finders")
local sorters = require ("telescope.sorters")
local make_entry = require ("telescope.make_entry")
local actions = require("telescope.actions")
local action_state = require("telescope.actions.state")
local conf = require("telescope.config").values
local putils = require("telescope.previewers.utils")
local themes = require('telescope.themes')
local notify = require('notify')

local nvim_jai = {}

function nvim_jai.setup(opts)
    opts = opts or {}

    if opts["path_display"] == nil then
        opts["path_display"] = "hidden"  
    end
    if opts["symbol_width"] == nil then
        opts["symbol_width"] = 90
    end

    if nvim_jai.previewer == nil then
        conf.dynamic_preview_title = true
        nvim_jai.previewer = conf.qflist_previewer(opts)
    end    

    vim.api.nvim_create_user_command('JaiFindDeclaration', nvim_jai.show_declarations_picker, {nargs = 0, desc = ''}) 
    vim.api.nvim_create_user_command('CompileJai', nvim_jai.compile, {nargs = 0, desc = ''}) 
    vim.api.nvim_create_user_command('ExitJai', nvim_jai.exit, {nargs = 0, desc = ''}) 

    vim.api.nvim_create_user_command('JaiTest', nvim_jai.test, {nargs = 0, desc = ''})
    
    if nvim_jai.channel_id == nil then
        -- local job = require('plenary.job')
        -- job:new({
        --     command = "nvim-cpp",
        --     cwd = vim.fn.getcwd(),
        -- }):start()
        -- vim.loop.sleep(100)
        nvim_jai.channel_id = vim.fn.sockconnect("tcp", "localhost:12345", {rpc = true})
        nvim_jai.get_declarations()
        -- nvim_jai.exit()
    end
    -- local result = vim.fn.rpcrequest(nvim_jai.channel_id, "GetDeclarations")
end

function nvim_jai.test()
    local result = vim.fn.rpcrequest(nvim_jai.channel_id, "GetDeclarations")
    local updated = result[1];
    if updated then
        print("Success");
    else
        print("Fail");
    end
end

function nvim_jai.get_declarations()
    if nvim_jai.channel_id == nil then
        return {}
    end
    local result = vim.fn.rpcrequest(nvim_jai.channel_id, "GetDeclarations")
    local updated = result[1]

    if updated then
        -- print(vim.inspect(result))
        local files = result[2]
        local entries = nvim_jai.declaration_entry_cache or {}
        for file, declarations in pairs(files) do
            local functions = declarations["functions"]
            -- local structs = declarations["structs"]
            -- local macros = declarations["macros"]
            
            for index, funct in ipairs(functions) do
                local entry = funct
                entry["bufnr"] = vim.uri_to_bufnr(file)
                entry["path"] = file
                entry["symbol_type"] = "function"
                table.insert(entries, entry)
            end

            -- for index, struct in ipairs(structs) do
            --     local entry = struct
            --     entry["bufnr"] = vim.uri_to_bufnr(file)
            --     entry["path"] = file
            --     entry["symbol_type"] = "struct"
            --     entry["struct_type"] = struct["type"]
            --     table.insert(entries, entry)
            -- end

            -- for index, macro in ipairs(macros) do
            --     local entry = macro
            --     entry["bufnr"] = vim.uri_to_bufnr(file)
            --     entry["path"] = file
            --     entry["symbol_type"] = "macro"
            --     table.insert(entries, entry)
            -- end
        end
        nvim_jai.declaration_entry_cache = entries
        return entries
    else
        return nvim_jai.declaration_entry_cache or {}
    end
end

function nvim_jai.create_declaration_entry(entry)
    entry["start"] = 0
    entry["finish"] = 0
    entry["display"] = function(self, picker)
        local highlights = {}
        if entry["symbol_type"] == "macro" then
            highlights = {{{0, #entry["ordinal"]}, "Define"}}
        elseif entry["symbol_type"] == "struct" then
            highlights = 
            {
                {{0, #entry["struct_type"]}, "Keyword"}, 
                {{#entry["struct_type"] + 1, #entry["ordinal"]}, "Type"}
            }
        else -- function
            local type_length = 2 --entry["return_type"]
            local name_length = entry["name"]
            highlights = 
            {
                {{0, name_length}, "Function"},
                {{name_length + 1, name_length + 3}, "Keyword"}, -- ::
                {{name_length + 4, name_length + 5}, "Delimiter"} -- (
            }

            local arg_offset = name_length + 6
            for index, arg in ipairs(entry["arguments"]) do
                local arg_type_length = arg["type"]
                local arg_name_length = arg["name"]
                arg_offset = arg_offset + arg_name_length + 2;
                table.insert(highlights, {{arg_offset - 2, arg_offset - 1}, "Delimiter"}) -- :
                table.insert(highlights, {{arg_offset, arg_offset + arg_type_length}, "Type"})
                arg_offset = arg_offset + arg_type_length + 2
                table.insert(highlights, {{arg_offset - 2, arg_offset - 1}, "Delimiter"}) -- ,
            end
            if #entry["arguments"] == 0 then
                arg_offset = arg_offset + 1 
            end
            table.insert(highlights, {{arg_offset - 1, arg_offset}, "Delimiter"}) -- )

            table.insert(highlights, {{arg_offset + 1, arg_offset + 3}, "Keyword"}) -- ->
            
            local ret_offset = arg_offset + 6
            table.insert(highlights, {{ret_offset - 2, ret_offset - 1}, "Delimiter"}) -- (
            for index, ret in ipairs(entry["returns"]) do
                local ret_type_length = ret["type"]
                local ret_name_length = ret["name"]
                if ret_name_length ~= 0 then
                    ret_offset = ret_offset + ret_name_length + 2;
                    table.insert(highlights, {{ret_offset - 2, ret_offset - 1}, "Delimiter"}) -- :
                end
                table.insert(highlights, {{ret_offset, ret_offset + ret_type_length}, "Error"})
                ret_offset = ret_offset + ret_type_length + 2
                table.insert(highlights, {{ret_offset - 2, ret_offset - 1}, "Delimiter"}) -- ,
            end
            table.insert(highlights, {{ret_offset - 1, ret_offset}, "Delimiter"}) -- )
        end
        return entry["ordinal"], highlights
    end
    
    return entry
end

function nvim_jai.default_declaration_action(prompt_bufnr, map)
    actions.close(prompt_bufnr)
    local selection = action_state.get_selected_entry()
    local location = 
    {
        uri = selection.path,
        range = 
        {
            start = { line = selection.lnum - 1, character = selection.col },
            ["end"] = { line = selection.lnum - 1, character = selection.col },
        },
    }
    local jump_successful = vim.lsp.util.jump_to_location(location, "utf-8", true)
end

function nvim_jai.show_declarations_picker(opts)
    opts = opts or {}
    vim.api.nvim_set_hl(0, "TelescopeMatching", {link = "String"})

    local picker = pickers.new(opts, 
    {
        prompt_title = "Find symbol",
        previewer = nvim_jai.previewer,
        finder = finders.new_table(
        {
            results = nvim_jai.get_declarations(),
            entry_maker = nvim_jai.create_declaration_entry
        }),
        sorter = conf.generic_sorter(opts),
        attach_mappings = function(prompt_bufnr, map)
            actions.select_default:replace(function()
                nvim_jai.default_declaration_action(prompt_bufnr, map)
            end)
            return true
        end,
    })
    picker:find()
end

function nvim_jai.compile()
    if nvim_jai.channel_id == nil then
        return {}
    end
    -- print("Compilation started")
    -- vim.api.nvim_echo({{"Compilation started", "None" }}, false, {})
    local notify_config = {render = "minimal", stages = "fade", fps = 60 }
    local result = vim.fn.rpcrequest(nvim_jai.channel_id, "Compile")

    local started = result["started"]
    local messages = result["messages"] or {}
    
    if started and #messages > 0 then
        local entries = {}
        for index, message in ipairs(messages) do
            local entry = {}
            -- entry["bufnr"] = vim.uri_to_bufnr("E:\\Projects\\nvim-cpp\\main.cpp")
            entry["filename"] = message["filename"] or ""
            entry["lnum"] = message["lnum"] or 1
            entry["col"] = message["col"] or 1
            entry["nr"] = message["nr"] or ""
            entry["text"] = message["text"]
            entry["type"] = message["type"]
            table.insert(entries, entry)
        end
        -- print(vim.inspect(entries))
        notify("Compilation failed", "error", notify_config)
        vim.fn.setqflist(entries, "r")
        vim.api.nvim_command("bot copen")
        -- vim.api.nvim_command("wincmd p")
    elseif started and #messages == 0 then
        -- print("Compilation successful")
        -- vim.api.nvim_echo({{"Compilation successful", "None"}}, false, {})
        notify("Compilation successful", "info", notify_config)
        vim.api.nvim_command("cclose")
    else
        -- print("Compilation failed")
        -- vim.api.nvim_echo({{"Compilation failed", "None"}}, false, {})
        notify("Compilation failed", "error", notify_config)
        vim.api.nvim_command("cclose")
    end
end

function nvim_jai.exit()
    if nvim_jai.channel_id ~= nil then
        result = vim.fn.rpcrequest(nvim_jai.channel_id, "Exit")
        vim.fn.chanclose(nvim_jai.channel_id)
        nvim_jai.channel_id = nil
    end
end

nvim_jai.setup()

return nvim_jai
