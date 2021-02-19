class E < StandardError
  def initialize
    super("にほんご\n改行")
  end
end

raise E
