defmodule StockServer.StockCSVReader do
  def read(stock) do
    {:ok, io_device} = :file.open(file_path(stock), [:read])

    collect = fn(line, collection) ->
      collection ++ parse(line)
    end

    {:ok, collector} = :ecsv.process_csv_file_with(io_device, collect, [])

    collector
  end

  defp file_path(stock) do
    "data/#{String.downcase(atom_to_binary(stock))}.csv"
  end

  defp parse({:newline, [price|_]}) do
    [list_to_float(price)]
  end

  defp parse({:eof}) do
    []
  end
end
