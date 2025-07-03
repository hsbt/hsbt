class TR
  def initialize(calls = [])
    @calls = calls
  end

  def method_missing name, *args
    @calls << [name, args]
  end
end

Marshal.dump TR.new
