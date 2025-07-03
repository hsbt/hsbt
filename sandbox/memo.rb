# -*- coding: utf-8 -*-
class User
  attr_accessor :products

  def initialize
    @products = []
  end

  def product_count
    @timer ||= Time.now
    @count ||= products.size

    # 1sec 以内の連続アクセスは無視する
    if @timer < Time.now - 1
      @count = products.size
      @timer = Time.now
    end

    @count
  end
end

u = User.new
p u.product_count

u.products << :a

p u.product_count

sleep 3

p u.product_count
