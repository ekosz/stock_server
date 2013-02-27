defmodule StockServer do
  use Application.Behaviour

  ## API
  
  def start(_type, args) do
    StockServer.ConnectionSup.start_link
  end

end
