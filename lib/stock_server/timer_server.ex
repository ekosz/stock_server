defmodule StockServer.TimerServer do
  use GenServer.Behaviour

  @defualt_timer 2_000 # 2 secounds

  defrecord State, time: 0, tick_rate: nil

  ## API

  def start_link(interval // @defualt_timer) do
    :gen_server.start_link({:local, __MODULE__}, __MODULE__, interval, [])
  end

  def current_time do
    :gen_server.call(__MODULE__, :current_time)
  end

  ## Callbacks

  def init(time_to_wait) do
    {:ok, _ } = :timer.send_after(time_to_wait, :tick)
    {:ok, State.new(tick_rate: time_to_wait)}
  end

  def handle_call(:current_time, _from, state) do
    {:reply, state.time, state}
  end

  def handle_call(_msg, _from, _state) do
    super
  end

  def handle_info(:tick, state) do
    {:ok, _} = :timer.send_after(state.tick_rate, :tick)
    {:noreply, updated_time(state)}
  end

  def handle_info(_msg, _state) do
    super
  end

  ## Privates

  defp updated_time(state) do
    State.new(time: state.time+1, tick_rate: state.tick_rate)
  end

end
