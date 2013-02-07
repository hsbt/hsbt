ActiveKokyaku::Application.routes.draw do
  root :to => "inquiries#index"
  resources :inquiries, :except => [:new, :create, :destroy] do
    collection do
      get "search"
    end
  end
end
