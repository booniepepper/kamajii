defmodule Kamajii.Mixfile do
  use Mix.Project

  def project do
    [
      app: :kamajii,
      version: "0.1.0",
      elixir: "~> 1.12.3",
      elixirc_paths: elixirc_paths(Mix.env()),
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases()
    ]
  end

  def application do
    [extra_applications: [:logger], mod: {Kamajii.Application, []}]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp deps do
    [
      {:ace, "~> 0.19.0"},
      {:raxx_logger, "~> 0.2.2"},
      {:jason, "~> 1.0"}
    ]
  end

  defp aliases() do
    []
  end
end
