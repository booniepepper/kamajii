require "kemal"

get "/" do |env|
  env.response.content_type = "application/json"
  {message: "greetings"}.to_json
end

error 404 do |env|
  env.response.content_type = "application/json"
  {error: "USAGE: GET /"}.to_json
end

Kemal.run
