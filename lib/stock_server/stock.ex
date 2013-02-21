defmodule StockServer.Stock do
  use GenServer.Behaviour

  ## API

  @doc"""
  Starts the a server for a current stock
  """
  def start_link(stock) do
    :gen_server.start_link(__MODULE__, stock, [])
  end

  ## Callbacks

  def init(stock) do
    {:ok, stock}
  end

end
