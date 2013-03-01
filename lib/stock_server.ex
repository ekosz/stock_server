defmodule StockServer do
  use Application.Behaviour

  ## API
  
  @doc """
  Starts the application
  """
  def start(_type, args) do
    StockServerSup.start_link(args)
  end

end
