ActiveKokyaku::Application.routes.draw do
  root :to => "inquiries#index"

  resources :inquiries, :only => [:index, :show] do
    collection do
      get "search"
    end
  end
end
