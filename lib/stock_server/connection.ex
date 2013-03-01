defrecord StockServer.Connection.State, lsocket: nil, name: nil, stocks: [], cash: 100_000.00

defmodule StockServer.Connection do
  use GenServer.Behaviour

  alias StockServer.Connection.State, as: State
  import StockServer.ConnectionSup, only: [start_socket: 0]
  import StockServer.Connection.CommandHandler, only: [handle_command: 2]

  ## API

  @doc """
  Starts a process listening on a socket.
  """
  def start_link(socket) do
    :gen_server.start_link(__MODULE__, [socket], [])
  end

  ## Callback

  def init([socket]) do
    :gen_server.cast(self(), :accept)
    {:ok, State[lsocket: socket]}
  end

  def handle_cast(:accept, State[lsocket: listen_socket] = state) do
    {:ok, accept_socket} = :gen_tcp.accept(listen_socket)
    start_socket()
    {:noreply, state}
  end

  defp send(socket, message, args) do
    :ok = :gen_tcp.send(socket, :io_lib.format(message<>"~n", args))
    :ok = :inet.setopts(socket, [{:active, :once}])
    :ok
  end

  def handle_info({:tcp, socket, reply}, state) when is_binary(reply) do
    self <- {:tcp, socket, binary_to_list(reply)}
    {:noreply, state}
  end

  def handle_info({:tcp, socket, reply}, state) do
    response = try do
      handle_command(reply, state)
    rescue
      error in _ -> 
        :error_logger.error_report(error)
        {:error, "server_error", state}
    end

    case response do
      {:ok, message, new_state} ->
        send(socket, "OK "<>message, [])
        {:noreply, new_state}

      {:error, message, new_state} ->
        send(socket, "ERROR "<>message, [])
        {:noreply, new_state}

      {:stop, message, new_state} ->
        send(socket, message, [])
        :gen_tcp.close(socket)
        {:stop, :normal, new_state}
    end
  end
  
  def handle_info({:tcp_closed, _socket}, state) do
    {:stop, :normal, state}
  end

  def handle_info({:tcp_error, _socket}, state) do
    {:stop, :normal, state}
  end

end
