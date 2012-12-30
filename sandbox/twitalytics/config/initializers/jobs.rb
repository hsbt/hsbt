if $servlet_context
  require Rails.root.join 'app/jobs/delete_old_statuses'
end
