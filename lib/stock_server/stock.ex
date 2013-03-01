defmodule StockServer.Stock do
  use GenServer.Behaviour

  ## API

  @doc"""
  Starts the a server for a current stock
  """
  def start_link(stock) do
    :gen_server.start_link({:local, stock}, __MODULE__, [stock], [])
  end

  def current_price(id) do
    :gen_server.call(id, :current_price)
  end

  ## Callbacks

  defrecord State, prices: nil, current_price: nil, ticker: nil

  def init([ticker]) do
    StockServer.StockNotifier.join_feed(self())

    prices = StockServer.StockCSVReader.read(ticker)
    {:ok, State[prices: prices, current_price: hd(prices), ticker: ticker]}
  end

  def handle_call(:current_price, _from, state) do
    {:reply, state.current_price, state}
  end

  def handle_call(_msg, _from, _state) do
    super
  end

  def handle_cast({:buy, stock, _amount, _price}, State[ticker: ticker] = state) when ticker == stock do
    # Chage price here
    {:noreply, state}
  end

  def handle_cast({:sell, stock, _amount, _price}, State[ticker: ticker] = state) when ticker == stock do
    # Change price here
    {:noreply, state}
  end

  def handle_cast(_msg, _state) do
    super
  end

end
