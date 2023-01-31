json.array!(@cards) do |card|
  json.extract! card, :id, :number
  json.url card_url(card, format: :json)
end
