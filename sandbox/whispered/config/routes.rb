Whispered::Application.routes.draw do
  root :to => "inquiries#index"

  match '/auth/:provider/callback', :to => 'sessions#create'
  match "/signout" => "sessions#destroy", :as => :signout

  resources :inquiries, :only => [:index, :show] do
    collection do
      get "search"
    end
  end
end
