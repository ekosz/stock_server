defmodule StockServer do
  use GenServer.Behaviour

  ## API
  
  def start_link do
    :gen_server.start_link(__MODULE__, [], [])
  end

end
