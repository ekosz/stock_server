defmodule StockServer.TimerServer do
  use GenServer.Behaviour

  @defualt_timer 2_000 # 2 secounds

  defrecord State, time: 0

  ## API

  def start_link(interval // @defualt_timer) do
    :gen_server.start_link({:local, __MODULE__}, __MODULE__, [interval], [])
  end

  def current_time do
    :gen_server.call(__MODULE__, :current_time)
  end

  ## Callbacks

  def init(time_to_wait) do
    :timer.send_interval(time_to_wait, :interval)
    {:ok, State.new}
  end

  def handle_call(:current_time, _from, state) do
    {:ok, state.time, state}
  end

  def handle_call(_msg, _from, _state) do
    super
  end

  def handle_info(:interval, state) do
    {:noreply, updated_time(state)}
  end

  def handle_info(_msg, _state) do
    super
  end

  ## Privates

  defp updated_time(state) do
    State.new(time: state.time+1)
  end

end
