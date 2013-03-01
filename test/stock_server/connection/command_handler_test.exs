Code.require_file "../../../test_helper.exs", __FILE__

defmodule StockServerTest.Connection.CommandHandler do
  use ExUnit.Case, async: true

  import StockServer.Connection.CommandHandler, only: [handle_command: 2]
  alias StockServer.Connection.State, as: State

  test "quiting" do
    state = State.new
    assert {:stop, _message, State[]} = handle_command('quit', state)
  end

  test "registering" do
    state = State.new
    assert {:ok, _message, State[name: 'Eric']} = handle_command('register Eric', state)
  end

  test "trying a command before registering" do
    state = State.new
    assert {:error, "not_registered", State[]} = handle_command('buy appl 100', state)
  end

  test "buying stock" do
    state = State.new(name: "Eric", stocks: [APPL: 10])
    assert {:ok, "BOUGHT APPL 1"<>_, State[stocks: [APPL: 11]]} = handle_command('buy appl 1', state)
  end

  test "buying too much stock" do
    state = State.new(name: "Eric", cash: 0)
    assert {:error, "insufficient_cash", _} = handle_command('buy appl 1', state)
  end

  test "selling stock" do
    state = State.new(name: "Eric", stocks: [APPL: 10])
    assert {:ok, "SOLD APPL 1"<>_, State[stocks: [APPL: 9]]} = handle_command('sell appl 1', state)
  end

  test "selling too much stock" do
    state = State.new(name: "Eric", stocks: [APPL: 10])
    assert {:error, "insufficient_stocks", State[stocks: [APPL: 10]]} = handle_command('sell appl 11', state)
  end

  test "listing >0 stocks" do
    state = State.new(name: "Eric", stocks: [APPL: 10, MSFT: 5])
    assert {:ok, "APPL 10 MSFT 5 ", State[stocks: [APPL: 10, MSFT: 5]]} = handle_command('current_stocks', state)
  end

  test "listing 0 stocks" do
    state = State.new(name: "Eric", stocks: [])
    assert {:error, "no_stocks", State[stocks: []]} = handle_command('current_stocks', state)
  end

  test "getting the price of a stock" do
    state = State.new(name: "Eric")
    assert {:ok, stock_price, _state} = handle_command('price APPL', state)

    assert Regex.match? %r/\d+\.\d{2}/, stock_price
  end

  test "getting the current cash amount" do
    state = State.new(name: "Eric", cash: 100.3)

    assert {:ok, "100.30", _state} = handle_command('current_cash', state)
  end
end
