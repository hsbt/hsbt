require 'sinatra'

get "/" do
  erb :form
end

post '/upload' do
  
  filename = params[:file][:filename]
  file = params[:file][:tempfile]
  
  send_file file, :filename => filename, :type => 'application/octet-stream'
end
