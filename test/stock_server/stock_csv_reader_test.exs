Code.require_file "../../test_helper.exs", __FILE__

defmodule StockServerTest.StockCSVReader do
  use ExUnit.Case

  test "reading a CSV file" do
    test_data = StockServer.StockCSVReader.read(:TEST)
    assert length(test_data) == 3
    assert Enum.at!(test_data, 0) == 1.50
    assert Enum.at!(test_data, 1) == 1.51
    assert Enum.at!(test_data, 2) == 1.52
  end
end
