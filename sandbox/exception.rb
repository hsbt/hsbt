class AException < Exception; end

begin
  begin
    raise AException
  rescue AException
    raise
  end
rescue Exception => e
  p e.class
end
