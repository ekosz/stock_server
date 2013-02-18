defmodule StockServer.Stock do
  use GenServer.Behaviour

  @delay 2_000 # 2 secound delay

  ## API

  @doc"""
  Starts the a server for a current stock
  """
  def start_link(stock, time_to_wait // @delay) do
    :gen_server.start_link(__MODULE__, {stock, time_to_wait}, [])
  end

  ## Callbacks

  def init({stock, time_to_wait}) do
    :timer.send_interval(time_to_wait, :interval)
    {:ok, stock}
  end

end
