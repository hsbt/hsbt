Whispered::Application.routes.draw do
  root :to => 'welcome#index'

  get '/auth/:provider/callback', :to => 'sessions#create'
  get '/signout' => 'sessions#destroy', :as => :signout

  resources :inquiries, :only => [:index, :show] do
    collection do
      get 'search'
    end
  end
end
