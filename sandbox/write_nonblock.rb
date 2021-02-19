require 'socket'

client = TCPSocket.new('localhost', 4481)
payload = 'Lorem ipsum' * 100_000

written = client.write_nonblock(payload)
p payload.size
p written
written < payload.size
