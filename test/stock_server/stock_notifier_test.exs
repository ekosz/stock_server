Code.require_file "../../test_helper.exs", __FILE__

defmodule StockServerTest.StockNotifier do
  use ExUnit.Case, async: true

  defmodule StockMock do
    use GenServer.Behaviour

    def start_link do
      :gen_server.start_link(__MODULE__, [], [])
    end

    def handle_call({:was_called, msg}, _from, messages) do
      {:reply, List.member?(messages, msg), messages}
    end

    def handle_cast(msg, messages) do
      messages = messages ++ [msg]
      {:noreply, messages}
    end
  end

  test "notifies stocks servers of buys" do
    assert {:ok, stock}   = StockMock.start_link

    StockServer.StockNotifier.join_feed(stock)

    StockServer.StockNotifier.notify_buy(:APPL, 10_000, 5.12)
    :timer.sleep(1)
    assert :gen_server.call(stock, {:was_called, {:buy, :APPL, 10_000, 5.12}})
  end

  test "notifies stocks servers of sells" do
    assert {:ok, stock}   = StockMock.start_link

    StockServer.StockNotifier.join_feed(stock)

    StockServer.StockNotifier.notify_sell(:APPL, 10_000, 5.12)
    :timer.sleep(1)
    assert :gen_server.call(stock, {:was_called, {:sell, :APPL, 10_000, 5.12}})
  end
end

