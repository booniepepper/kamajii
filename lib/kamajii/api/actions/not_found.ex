defmodule Kamajii.API.Actions.NotFound do
  use Raxx.SimpleServer
  alias Kamajii.API

  @impl Raxx.SimpleServer
  def handle_request(_request, _state) do
    error = %{title: "Action not found"}

    response(:not_found)
    |> API.set_json_payload(%{errors: [error]})
  end
end
