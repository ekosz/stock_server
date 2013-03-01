defmodule StockServer.StockSup do
  use Supervisor.Behaviour

  ## API
  def start_link do
    :supervisor.start_link({:local, __MODULE__}, __MODULE__, [])
  end

  ## Callbacks

  def init([]) do
    stocks = [:APPL]

    workers = Enum.map stocks, fn(stock) ->
      worker(StockServer.Stock, [stock])
    end

    supervise(workers, strategy: :one_for_one)
  end
end
