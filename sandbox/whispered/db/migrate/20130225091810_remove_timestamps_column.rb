class RemoveTimestampsColumn < ActiveRecord::Migration
  def change
    remove_column :inquiries, :created_at
    remove_column :inquiries, :updated_at
  end
end
