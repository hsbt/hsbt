class CreateCards < ActiveRecord::Migration
  def change
    create_table :cards do |t|
      t.integer :number

      t.timestamps null: false
    end
  end
end
