defmodule StockServer.StockNotifier do
  @behavior :gen_event

  # API

  def start_link do
    :gen_event.start_link
  end

  def join_feed(pid, to_pid) do
    handler_id = {StockServer.StockNotifier, make_ref()}
    :gen_event.add_sup_handler(pid, handler_id, [to_pid])
    handler_id
  end

  def notify_buy(pid, stock, amount, price) do
    :gen_event.notify(pid, {:buy, stock, amount, price})
  end

  def notify_sell(pid, stock, amount, price) do
    :gen_event.notify(pid, {:sell, stock, amount, price})
  end

  # GenEvent Callbacks

  def init([pid]) do
    {:ok, pid}
  end

  def handle_event(event, pid) do
    :ok = :gen_server.cast(pid, event)
    {:ok, pid}
  end

  def handle_call(_msg, pid) do
    {:ok, :ok, pid}
  end

  def handle_info(_msg, pid) do
    {:ok, pid}
  end

  def code_change(_old_vsn, pid, _extra) do
    {:ok, pid}
  end
     
  def terminate(_reason, _state) do
    :ok
  end

end
