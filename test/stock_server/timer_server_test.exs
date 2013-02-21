Code.require_file "../../test_helper.exs", __FILE__

defmodule StockServerTest.TimerServer do
  use ExUnit.Case, async: true

  @tick_rate 10

  def setup_all do
    assert {:ok, pid} = StockServer.TimerServer.start_link(@tick_rate)
    assert is_pid(pid)
    :ok
  end

  test "getting the current time" do
    assert StockServer.TimerServer.current_time == 0
    :timer.sleep(@tick_rate * 5)
    assert StockServer.TimerServer.current_time == 5
  end
end
