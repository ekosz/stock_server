defmodule StockServer do
  use Application.Behaviour

  ## API
  
  @doc """
  Starts the application
  """
  def start(_type, args) do
    StockServer.ConnectionSup.start_link
  end

end
