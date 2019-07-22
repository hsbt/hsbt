# -*- coding: utf-8 -*-
require 'spec_helper'

describe 'ログイン' do
  before do
    @user = create(:user)
  end

  it '直接 /users に飛んだときにエラーになる' do
    visit '/users'
    page.should_not have_content 'ログインしました'
    page.should have_content 'ログインしてください'
  end

  it 'visit を1回実行する' do
    visit '/signin'
    fill_in 'session_username', with: @user.username
    fill_in 'session_password', with: "foobar"
    click_button 'ログイン'

    page.should have_content 'ログインしました'
  end

  it '失敗する' do
    visit '/signin'
    fill_in 'session_username', with: @user.username
    fill_in 'session_password', with: "foo"
    click_button 'ログイン'
    page.should have_content 'ログインしてください'
  end

  it 'visit を2回実行する' do
    visit '/signin'
    fill_in 'session_username', with: @user.username
    fill_in 'session_password', with: "foobar"
    click_button 'ログイン'

    visit '/signin'
    page.should have_content 'ログインしてください'
    visit '/users'
    page.should have_content 'ログインしました'
  end
end
