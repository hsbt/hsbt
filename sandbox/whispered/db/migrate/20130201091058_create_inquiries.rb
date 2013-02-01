class CreateInquiries < ActiveRecord::Migration
  def change
    create_table :inquiries do |t|
      t.string :name
      t.string :email
      t.integer :inq_type
      t.string :message

      t.timestamps
    end
  end
end
