defmodule StockServerSup do
  use Supervisor.Behaviour

  ## API
  
  def start_link(_args) do
    :supervisor.start_link({:local, __MODULE__}, __MODULE__, [])
  end

  ## Callbacks

  def init([]) do
    children = [ worker(StockServer.ConnectionSup, []),
                 worker(StockServer.TimerServer, []),
                 worker(StockServer.StockNotifier, []) ]
    supervise(children, strategy: :one_for_one)
  end
end
