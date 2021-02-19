8.times do
  fork do
    p :f
  end
end
