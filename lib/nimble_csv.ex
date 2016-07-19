defmodule NimbleCSV do
  @moduledoc ~S"""
  NimbleCSV is a small and fast parsing and dumping library.

  It works by building highly-inlined CSV parsers, designed
  to work with strings, enumerables and streams:

      NimbleCSV.define(MyParser, separator: ",", escape: "\"")

  Once defined, we can parse data accordingly:

      iex> MyParser.parse_string "name,age\njohn,27"
      [["john","27"]]

  See the `define/2` function for the list of functions that
  would be defined in `MyParser`.

  ## Parsing

  NimbleCSV is by definition restricted in scope to do only
  parsing (and dumping). For example, the example above
  discarded the headers when parsing the string, as NimbleCSV
  expects developers to handle those explicitly later.
  For example:

      "name,age\njohn,27"
      |> MyParser.parse_string
      |> Enum.map(fn [name, age] ->
        %{name: name, age: String.to_integer(age)}
      end)

  This is particularly useful with the parse_stream functionality
  that receives and returns a stream. For example, we can use it
  to parse files line by line lazily:

      "path/to/csv/file"
      |> File.stream!(read_ahead: 100_000)
      |> MyParser.parse_stream
      |> Stream.map(fn [name, age] ->
        %{name: name, age: String.to_integer(age)}
      end)

  By default this library ships with `NimbleCSV.RFC4180`, which
  is the most common implementation of CSV parsing/dumping available
  using comma as separators and double-quote as escape. If you
  want to use it your codebase, simply alias it to CSV and enjoy:

      iex> alias NimbleCSV.RFC4180, as: CSV
      iex> CSV.parse_string "name,age\njohn,27"
      [["john","27"]]

  ## Dumping

  NimbleCSV can dump any enumerable to either iodata or to streams:

      iex> IO.iodata_to_binary MyParser.dump_to_iodata([~w(name age), ~w(mary 28)])
      "name,age\nmary,28\n"

      iex> MyParser.dump_to_stream([~w(name age), ~w(mary 28)])
      #Stream<...>

  """

  defmodule ParseError do
    defexception [:message]
  end

  # @doc ~S"""
  # Defines a new parser/dumper.

  # It accepts the following options:

  #   * `:separator`- the CSV separator, defaults to `","`
  #   * `:escape`- the CSV escape, defaults to `"\""`
  #   * `:moduledoc` - the documentation for the generated module

  # ## Parser/Dumper API

  # It exports the following parser functions:

  #   * `parse_enumerable/2` - eager parsing from a list or another enumerable
  #   * `parse_string/2` - eagar parsing from a string
  #   * `parse_stream/2` - lazy parsing from a stream

  # The second argument for the functions above is a list of options
  # currently supporting:

  #   * `:headers` - if headers exist and, if so, they are discarded

  # It also exports the following dump functions:

  #   * `dump_to_iodata/1` - eagerly dump an enumerable into iodata
  #     (a list of binaries and bytes and other lists).
  #   * `dump_to_stream/1` - lazily dumps from an enumerable to a stream.
  #     It returns a stream that emits each row as iodata.

  # """
  # def define(module, options) do
  #   defmodule module do
  #     @moduledoc Keyword.get(options, :moduledoc)
  #     @separator Keyword.get(options, :separator, ",")
  #     @escape Keyword.get(options, :escape, "\"")

      ## Parser

  @doc """
  Lazily parses CSV from a stream.
  """
  def parse_stream(stream, opts \\ []) do
    {state, separator, escape} = init_parser(opts)
    Stream.transform(stream, fn -> state end, &parse(&1, &2, separator, escape), &finalize_parser(&1, escape))
  end

  @doc """
  Eagerly parses CSV from an enumerable.
  """
  def parse_enumerable(enumerable, opts \\ []) do
    {state, separator, escape} = init_parser(opts)
    {lines, state} = Enum.flat_map_reduce(enumerable, state, &parse(&1, &2, separator, escape))
    finalize_parser(state, escape)
    lines
  end

  @doc """
  Eagerly parses CSV from a string.
  """
  def parse_string(string, opts \\ []) do
    newline = :binary.compile_pattern(["\r\n", "\n"])
    {0, byte_size(string)}
    |> Stream.unfold(fn
      {_, 0} ->
        nil
      {offset, length} ->
        case :binary.match(string, newline, scope: {offset, length}) do
          {newline_offset, newline_length} ->
            difference = newline_length + newline_offset - offset
            {:binary.part(string, offset, difference),
             {newline_offset + newline_length, length - difference}}
          :nomatch ->
            {:binary.part(string, offset, length), {offset + length, 0}}
        end
    end)
    |> parse_enumerable(opts)
  end

  defp init_parser(opts) do
    state     = if Keyword.get(opts, :headers, true), do: :header, else: :line
    separator = Keyword.get(opts, :separator, ",")
    escape    = Keyword.get(opts, :escape, "\"")
    separator_size = byte_size(separator)
    escape_size    = byte_size(escape)
    separator_pattern = :binary.compile_pattern(separator)
    escape_pattern    = :binary.compile_pattern(escape)
    {state, {separator, separator_size, separator_pattern}, {escape, escape_size, escape_pattern}}
  end

  defp finalize_parser({:escape, _, _, _}, {escapeb, _, _}) do
    raise ParseError, "expected escape character #{escapeb} but reached the end of file"
  end
  defp finalize_parser(_, _) do
    :ok
  end

  defp to_enum(result) do
    case result do
      {:line, row} -> {[row], :line}
      {:header, _} -> {[], :line}
      {:escape, _, _, _} = escape -> {[], escape}
    end
  end

  defp parse(line, {:escape, entry, row, state}, separator, escape) do
    to_enum escape(line, entry, row, state, separator, escape)
  end

  defp parse(line, state, separator, escape) do
    to_enum separator(line, [], state, separator, escape)
  end

  defp separator(line, row, state, {separatorb, separator_size, separator_pattern} = separator, {escapeb, escape_size, escape_pattern} = escape) do
    case :binary.match(line, escape_pattern) do
      {0, _} ->
        <<^escapeb::binary-size(escape_size), rest::binary>> = line
        escape(rest, "", row, state, separator, escape)

      {pos, _} ->
        pos = pos - 1
        case line do
          <<prefix::size(pos)-binary, ^separatorb::binary-size(separator_size), ^escapeb::binary-size(escape_size), rest::binary>> ->
            escape(rest, "", row ++ :binary.split(prefix, separator_pattern, [:global]), state, separator, escape)
          _ ->
            raise ParseError, "unexpected escape character #{escapeb} in #{inspect line}"
        end

      :nomatch ->
        offset = byte_size(line)
        crlf = offset - 2
        lf = offset - 1
        pruned =
          case line do
            <<prefix::size(crlf)-binary, ?\r, ?\n>> -> prefix
            <<prefix::size(lf)-binary, ?\n>> -> prefix
            prefix -> prefix
          end
        {state, row ++ :binary.split(pruned, separator_pattern, [:global])}
    end
  end

  defp escape(line, entry, row, state, {separatorb, separator_size, _separator_pattern} = separator, {escapeb, escape_size, escape_pattern} = escape) do
    case :binary.match(line, escape_pattern) do
      {offset, _} ->
        case line do
          <<prefix::size(offset)-binary, ^escapeb::binary-size(escape_size), ^escapeb::binary-size(escape_size), rest::binary>> ->
            escape(rest, entry <> prefix <> escapeb, row, state, separator, escape)
          <<prefix::size(offset)-binary, ^escapeb::binary-size(escape_size), ^separatorb::binary-size(separator_size), rest::binary>> ->
            separator(rest, row ++ [entry <> prefix], state, separator, escape)
          <<prefix::size(offset)-binary, ^escapeb::binary-size(escape_size), ?\r, ?\n>> ->
            {state, row ++ [entry <> prefix]}
          <<prefix::size(offset)-binary, ^escapeb::binary-size(escape_size), ?\n>> ->
            {state, row ++ [entry <> prefix]}
          <<prefix::size(offset)-binary, ^escapeb::binary-size(escape_size)>> ->
            {state, row ++ [entry <> prefix]}
          _ ->
            raise ParseError, "unexpected escape character #{escapeb} in #{inspect line}"
        end
      :nomatch ->
        {:escape, entry <> line, row, state}
    end
  end

  @compile {:inline, init_parser: 1, to_enum: 1, parse: 4}

  ## Dumper

  @doc """
  Eagerly dump an enumerable into iodata (a list of binaries and bytes and other lists).
  """
  def dump_to_iodata(enumerable, opts \\ []) do
    {check, minseparator, minescape, escape, replace} = init_dumper(opts)
    Enum.map(enumerable, &dump(&1, check, minseparator, minescape, escape, replace))
  end

  @doc """
  Lazily dumps from an enumerable to a stream.

  It returns a stream that emits each row as iodata.
  """
  def dump_to_stream(enumerable, opts \\ []) do
    {check, minseparator, minescape, escape, replace} = init_dumper(opts)
    Stream.map(enumerable, &dump(&1, check, minseparator, minescape, escape, replace))
  end

  defp init_dumper(opts) do
    escape    = Keyword.get(opts, :escape, "\"")
    separator = Keyword.get(opts, :separator, ",")
    check = :binary.compile_pattern([escape, "\r", "\n"])
    {check, to_minimum(separator), to_minimum(escape), escape, escape <> escape}
  end

  defp to_minimum(<<c>>),  do: c
  defp to_minimum(binary), do: binary

  defp dump([], _check, _minseparator, _minescape, _escape, _replace) do
    [?\n]
  end
  defp dump([entry], check, _minseparator, minescape, escape, replace) do
    [maybe_escape(entry, check, minescape, escape, replace), ?\n]
  end
  defp dump([entry | entries], check, minseparator, minescape, escape, replace) do
    [maybe_escape(entry, check, minescape, escape, replace), minseparator | dump(entries, check, minseparator, minescape, escape, replace)]
  end

  defp maybe_escape(entry, check, minescape, escape, replace) do
    entry = to_string(entry)
    case :binary.match(entry, check) do
      {_, _} ->
        [minescape, :binary.replace(entry, escape, replace, [:global]), minescape]
      :nomatch ->
        entry
    end
  end

  @compile {:inline, init_dumper: 1, maybe_escape: 5, to_minimum: 1}
end
