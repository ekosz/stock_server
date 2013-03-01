defmodule StockServer.Mixfile do
  use Mix.Project

  def project do
    [ app: :stock_server,
      version: "0.0.1",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [ registered: [:stock_server],
      env: [{:port, 3000}, {:tickrate, [test: 10, dev: 2_000]}],
      mod: { StockServer, [] } ]
  end

  # Returns the list of dependencies in the format:
  # { :foobar, "0.1", git: "https://github.com/elixir-lang/foobar.git" }
  defp deps do
    [
      { :ecsv, "1", github: "refuge/ecsv" }
    ]
  end
end
