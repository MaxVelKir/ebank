defmodule EbankWeb.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Start the Telemetry supervisor
      {Phoenix.PubSub, name: EbankWeb.PubSub},
      # Start the Endpoint (http/https)
      EbankWeb.Endpoint
      # Start a worker by calling: EbankWeb.Worker.start_link(arg)
      # {EbankWeb.Worker, arg}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: EbankWeb.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    EbankWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
