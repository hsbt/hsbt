require 'sqlite3'

db = SQLite3::Database.new "NoteStore.sqlite"
db.results_as_hash = true
db.execute('select * from ZICCLOUDSYNCINGOBJECT') do |row|
  p row
end
