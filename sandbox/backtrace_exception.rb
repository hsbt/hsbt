class Exception
  def set_backtrace_with_extension
    set_backtrace_without_extension(*args)
  end

  alias set_backtrace_without_extension set_backtrace
  alias set_backtrace set_backtrace_with_extension
end

raise

