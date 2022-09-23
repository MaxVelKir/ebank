defmodule EbankWeb.PageController do
  use EbankWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
