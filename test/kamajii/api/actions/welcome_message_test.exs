defmodule Kamajii.API.Actions.WelcomeMessageTest do
  use ExUnit.Case

  alias Kamajii.API.Actions.WelcomeMessage

  test "returns welcome message for a name" do
    request =
      Raxx.request(:POST, "/")
      |> Kamajii.API.set_json_payload(%{push: "Fiona"})

    response = WelcomeMessage.handle_request(request, %{})

    assert response.status == 201
    # assert {"content-type", "application/json"} in response.headers
    # assert false = response.body
    # assert message == "Bye Fiona."
  end

  test "returns bad request for bad payload" do
    request =
      Raxx.request(:POST, "/")
      |> Kamajii.API.set_json_payload(%{})

    response = WelcomeMessage.handle_request(request, %{})

    assert response.status == 400
  end
end
