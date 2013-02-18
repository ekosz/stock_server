Code.require_file "../test_helper.exs", __FILE__

defmodule StockServerTest do
  use ExUnit.Case

  test "starting the server" do
    assert {:ok, pid} = StockServer.start_link
    assert is_pid(pid)
  end
end
