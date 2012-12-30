if $servlet_context
  require Rails.root.join 'lib/jobs/delete_old_statuses'
end
