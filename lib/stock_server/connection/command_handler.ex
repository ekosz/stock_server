defmodule StockServer.Connection.CommandHandler do

  alias StockServer.Connection.State, as: State

  def handle_command('quit'++_, state) do
    {:stop, "Goodbye", state}
  end

  def handle_command('register '++team, state) do
    {:ok, "registered", state.name(line(team))}
  end

  def handle_command(_command, State[name: nil] = state) do
    {:error, "not_registered", state}
  end

  def handle_command('current_stocks'++_, State[stocks: []] = state) do
    {:error, "no_stocks", state}
  end

  def handle_command('current_stocks'++_, state) do
    message = Enum.reduce state.stocks, "", fn(stock, acc) ->
      acc <> "#{elem stock, 0} #{elem stock, 1} "
    end
    {:ok, message, state}
  end

  def handle_command('buy '++rest, state) do
    [stock, amount] = retrive_stock_and_amount(rest)
    current_amount = Keyword.get(state.stocks, stock, 0)

    state = state.stocks(
      Keyword.put(state.stocks, stock, current_amount+amount)
    )

    {:ok, "added #{amount} of #{stock} to #{current_amount}", state}
  end

  def handle_command('sell '++rest, state) do
    [stock, amount] = retrive_stock_and_amount(rest)
    current_amount = Keyword.get(state.stocks, stock, 0)
    sell(stock, amount, current_amount, state)
  end

  def handle_command(_command, state) do
    {:error, "unknown_command", state}
  end

   defp sell(_stock, amount, current_amount, state) when amount > current_amount do
    {:error, "insufficient_stocks", state}
  end

  defp sell(stock, amount, current_amount, state) do
    state = state.stocks(
      Keyword.put(state.stocks, stock, current_amount - amount)
    )

    {:ok, "removed #{amount} of #{stock} from #{current_amount}", state}
  end

  defp retrive_stock_and_amount(string) do
    [stock, amount] = :string.tokens(string, '\r\n ')
    stock  = binary_to_atom(String.upcase(list_to_binary(stock)))
    amount = list_to_integer(amount)
    [stock, amount]
  end

  defp line(string) do
    hd :string.tokens(string, '\r\n')
  end  
end
