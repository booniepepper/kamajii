# Kemal is a microservice framework. (Like Sinatra/Flask)
# Docs: https://kemalcr.com
require "kemal"

get "/" do |env|
  env.response.content_type = "application/json"
  {message: "greetings"}.to_json
end

post "/push/:stack" do |env|
  stack = env.params.url["stack"]
  item = env.params.body.inspect
  env.response.content_type = "application/json"
  {stack: stack, item: item}.to_json
end

error 404 do |env|
  env.response.content_type = "application/json"
  {error: "USAGE: GET /"}.to_json
end

Kemal.run
