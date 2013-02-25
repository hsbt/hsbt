class AddOriginalDateColumn < ActiveRecord::Migration
  def change
    add_column :inquiries, :update_date, :string
    add_column :inquiries, :create_date, :string
  end
end
