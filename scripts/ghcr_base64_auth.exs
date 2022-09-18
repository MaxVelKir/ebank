Mix.install([
  {:jason, "~> 1.0"}
])

[user, token] = System.argv()

%{
  "auths" => %{
    "https://ghcr.io" => %{
      "username" => "#{user}",
      "password" => "#{token}",
      "auth" => Base.encode64("#{user}:#{token}")
    }
  }
}
|> Jason.encode!()
|> Base.encode64()
|> IO.puts()

