ActiveKokyaku::Application.routes.draw do
  root :to => "inquiries#index"
  resources :inquiries
end
