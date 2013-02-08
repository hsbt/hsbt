class RemoveSpecificColumn < ActiveRecord::Migration
  def change
    remove_column :inquiries, :inq_type
  end
end
