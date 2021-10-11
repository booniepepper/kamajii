require "./kamajii"
require "socket"

def respond_ok_to(io : IO)
  io.puts "OK"
end

def respond_idk_to(io : IO, y_tho : String)
  io.puts "ERROR #{y_tho}"
end

def handle_client(client : TCPSocket)
  puts "Connection established with #{client.inspect}"
  client.each_line do |message|
    chars = message.chars
    action = chars.take_while {|c| c != ' '}.join
    stack = chars.skip(action.size + 1).take_while {|c| c != ' '}.join

    if action.size == 0
      respond_idk_to client, "no action"
      next
    elsif stack.size == 0
      respond_idk_to client, "no stack"
      next
    end

    case action
    when "push"
      start = action.size + stack.size + 2
      Kamajii.push stack, message.size > start ? message[start...] : ""
      respond_ok_to client
    when "pop"
      client.puts(Kamajii.pop stack)
    when "peek"
      client.puts(Kamajii.peek stack)
    else
      respond_idk_to client, "unknown action #{action}"
      next
    end
  end
  puts "Connection terminated with #{client.inspect}"
end

def run_server(host = "0.0.0.0", port = 2001)
  server = TCPServer.new(host, port)
  puts "Listening on #{host}:#{port}"
  while client = server.accept?
    spawn handle_client(client)
  end
end

run_server
