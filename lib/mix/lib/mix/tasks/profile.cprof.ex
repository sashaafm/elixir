defmodule Mix.Tasks.Profile.Cprof do
  use Mix.Task

  @shortdoc "Profiles the given file or expression with cprof"

  @moduledoc """
  """

  @switches [parallel: :boolean, require: :keep, eval: :keep, config: :keep, func_spec: :string,
             halt: :boolean, compile: :boolean, deps_check: :boolean, limit: :integer, 
             module: :string, trace_pattern: :string, start: :boolean, archives_check: :boolean, 
             warmup: :boolean, elixir_version_check: :boolean, parallel_require: :keep]

  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    {opts, head} = OptionParser.parse_head!(args,
      aliases: [r: :require, p: :parallel, e: :eval, c: :config],
      strict: @switches)

    # TODO: Remove on v2.0
    opts =
      Enum.flat_map(opts, fn
        {:parallel_require, value} ->
          IO.warn "the --parallel-require option is deprecated in favour of using " <>
            "--parallel to make all requires parallel and --require VAL for requiring"
          [require: value, parallel: true]
        opt ->
          [opt]
      end)

    {file, argv} =
      case {Keyword.has_key?(opts, :eval), head} do
        {true, _}    -> {nil, head}
        {_, [h | t]} -> {h, t}
        {_, []}      -> {nil, []}
      end

    System.argv(argv)
    process_config opts

    # Start app after rewriting System.argv,
    # but before requiring and evaling
    Mix.Task.run "app.start", args
    process_load opts

    _ = if file do
      if File.regular?(file) do
        Code.require_file(file)
      else
        Mix.raise "No such file: #{file}"
      end
    end

    unless Keyword.get(opts, :halt, true), do: Process.sleep(:infinity)
    :ok
  end

  defp process_config(opts) do
    Enum.each opts, fn
      {:config, value} ->
        Mix.Task.run "loadconfig", [value]
      _ ->
        :ok
    end
  end

  defp process_load(opts) do
    require_runner =
      if opts[:parallel] do
        &Kernel.ParallelRequire.files/1
      else
        fn(files) -> Enum.each(files, &Code.require_file/1) end
      end

    Enum.each opts, fn
      {:require, value} ->
        case filter_patterns(value) do
          [] ->
            Mix.raise "No files matched pattern #{inspect value} given to --require"
          filtered ->
            require_runner.(filtered)
        end
      {:eval, value} ->
        profile_code(value, opts)
      _ ->
        :ok
    end
  end

  defp filter_patterns(pattern) do
    Path.wildcard(pattern)
    |> Enum.uniq
    |> Enum.filter(&File.regular?/1)
  end

  # Profiling functions

  defp profile_code(code_string, opts) do
    content = 
      quote do
        unquote(__MODULE__).profile(fn ->
          unquote(Code.string_to_quoted!(code_string))
        end, unquote(opts))
      end
    # Use compile_quoted since it leaves less noise than eval_quoted
    Code.compile_quoted(content)
  end

  @doc false
  def profile(fun, opts) do
    fun
    |> profile_and_analyse(opts)
    |> print_output

    cleanup
  end

  defp profile_and_analyse(fun, opts) do
    if Keyword.get(opts, :warmup, true) do
      IO.puts "Warmup..."
      fun.()
    end

    num_matched_functions = case Keyword.get(opts, :func_spec) do
      nil ->
        case Keyword.get(opts, :trace_pattern) do
          nil -> :cprof.start()
          pattern -> pattern |> parse_pattern |> :cprof.start
        end
      func_spec -> # --trace-pattern is ignored if --func-spec present
        case(parse_func_spec(func_spec)) do
          module when not is_tuple(module) -> :cprof.start(module)
          {module, function} -> :cprof.start(module, function)
          {module, function, arity} -> :cprof.start(module, function, arity)
        end
    end

    apply(fun, [])

    :cprof.pause() 

    limit = Keyword.get(opts, :limit) 
    module = Keyword.get(opts, :module)

    analysis_result = case {limit, module} do
      {nil, nil} -> :cprof.analyse()
      {limit, nil} -> :cprof.analyse(limit)
      {limit, module} -> 
        module_atom = string_to_existing_module_atom(module)
        case limit do
          nil -> :cprof.analyse(module_atom)
          _   -> :cprof.analyse(module_atom, limit)
        end
    end

    {num_matched_functions, analysis_result}
  end

  defp string_to_existing_module_atom(":'Elixir'." <> module), do: String.to_existing_atom(module)
  defp string_to_existing_module_atom(":" <> module), do: String.to_existing_atom(module)
  defp string_to_existing_module_atom(module), do: Module.concat([module])

  defp is_elixir_module(module) when is_atom(module), do: function_exported?(module, :__info__, 1)
 
  defp module_name_for_printing(module) do
    module_name = Atom.to_string(module)

    if is_elixir_module(module) do
      "Elixir." <> rem = module_name
      rem
    else
      ":" <> module_name
    end
  end

  defp parse_func_spec(func_spec), do: func_spec |> Code.string_to_quoted! |> do_parse_func_spec

  defp do_parse_func_spec({_, _, [{_, _, [m]}, :'_', :'_']}), do: m
  defp do_parse_func_spec({_, _, [{_, _, [m]}, f, :'_']}), do: {m, f}
  defp do_parse_func_spec({_, _, [{_, _, [m]}, f, a]}), do: {m, f, a}
  defp do_parse_func_spec({_, _, [m, :'_', :'_']}), do: m
  defp do_parse_func_spec({_, _, [m, f, :'_']}), do: {m, f}
  defp do_parse_func_spec({_, _, [m, f, a]}), do: {m, f, a}

  defp parse_pattern("on_load"), do: :on_load
  defp parse_pattern(pattern), do: pattern |> Code.string_to_quoted! |> do_parse_pattern

  defp do_parse_pattern({_, _, [{_, _, [m]}, f, a]}), do: {m, f, a}
  defp do_parse_pattern({_, _, [m, f, a]}), do: {m, f, a}

  defp print_output({num_matched_functions, {all_call_count, mod_analysis_list}}) do
    print_total_row(all_call_count)
    Enum.each(mod_analysis_list, &print_analysis_result/1)
    print_number_of_matched_functions(num_matched_functions)
  end

  defp print_output({num_matched_functions, {_mod, _call_count, _mod_fun_list} = mod_analysis}) do
    print_analysis_result(mod_analysis)
    print_number_of_matched_functions(num_matched_functions)
  end

  defp cleanup(), do: :cprof.stop

  defp print_number_of_matched_functions(num_matched_functions) do
    IO.puts "Profile done over #{num_matched_functions} matching functions"
  end
  
  defp print_total_row(all_call_count) do
    IO.puts ""
    print_row(["s", "s", "s"], ["", "CNT", ""])
    print_row(["s", "B", "s"], ["Total", all_call_count, ""])
  end

  defp print_analysis_result({module, total_module_count, module_fun_list}) do
    module = module_name_for_printing(module)
    print_module(module, total_module_count, "", "<--")
    Enum.each(module_fun_list, &print_function(&1, "  "))
  end

  defp print_module(module, count, prefix \\ "", suffix \\ "") do
    print_row(
      ["s", "B", "s"],
      ["#{prefix}#{module}", count, suffix]
    )
  end

  defp print_function({fun, count}, prefix \\ "", suffix \\ "") do
    print_row(
      ["s", "B", "s"],
      ["#{prefix}#{function_text(fun)}", count, suffix]
    )
  end

  defp function_text({module, function, arity}) do
    Exception.format_mfa(module, function, arity)
  end

  defp function_text(other), do: inspect(other)

  @columns [-60, 12, 5]
  defp print_row(formats, data) do
    Stream.zip(@columns, formats)
    |> Stream.map(fn({width, format}) -> "~#{width}#{format}" end)
    |> Enum.join
    |> :io.format(data)

    IO.puts ""
  end
end
