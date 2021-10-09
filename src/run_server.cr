# Kemal is a microservice framework. (Like Sinatra/Flask)
# Docs: https://kemalcr.com
require "kemal"
require "kamajii"

def errorable(&block)
  begin
    yield
  rescue ex
    puts ex.inspect
  end
end

get "/" do |env|
  content = env.request.body

  request = content ? content.gets_to_end : "{}"

  env.response.content_type = "application/json"
  {message: "greetings", request: request}.to_json
end

post "/push/:stack" do |env|
  stack = env.params.url["stack"]
  content = env.request.body

  env.response.status = HTTP::Status::INTERNAL_SERVER_ERROR
  errorable do
    item = content ? content.gets_to_end : ""

    Kamajii.push stack, item

    env.response.content_type = "text/plain"
    env.response.status = HTTP::Status::CREATED
    "created"
  end
end

get "/pop/:stack" do |env|
  stack = env.params.url["stack"]

  env.response.status = HTTP::Status::INTERNAL_SERVER_ERROR
  errorable do
    content = Kamajii.pop stack

    env.response.content_type = "text/plain"
    env.response.status = HTTP::Status::CREATED
    content
  end
end

# TODO: Remove when ready to move to some kind of "production" state
post "/kill" do |env|
  spawn do
    sleep Time::Span.new(seconds: 10)
    exit
  end
  "Exiting in 10 seconds..."
end

error 404 do |env|
  env.response.content_type = "application/json"
  {error: "USAGE: GET /"}.to_json
end

Kemal.run do |config|
  server = config.server.not_nil!
  server.bind_tcp "localhost", 3000
end
