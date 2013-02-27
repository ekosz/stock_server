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

  test "buying initial stock" do
    state = State.new(name: "Eric")
    assert {:ok, "added 1 of APPL to 0", State[stocks: [APPL: 1]]} = handle_command('buy appl 1', state)
  end

  test "buying more stock" do
    state = State.new(name: "Eric", stocks: [APPL: 10])
    assert {:ok, "added 1 of APPL to 10", State[stocks: [APPL: 11]]} = handle_command('buy appl 1', state)
  end

  test "selling stock" do
    state = State.new(name: "Eric", stocks: [APPL: 10])
    assert {:ok, "removed 1 of APPL from 10", State[stocks: [APPL: 9]]} = handle_command('sell appl 1', state)
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
end
