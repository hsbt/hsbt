rooms = [4, 4, 2, 2, 2]

members = %w[
  kosaki
  nobu
  usa
  ko1
  knu
  a_matsuda
  k0kubun
  mame
  hsbt
  glass
  naruse
  sonots
  akr
  tarui
]

members.shuffle!; p rooms.map { |n| members.shift(n) }

members = %w[
  kosaki
  nobu
  usa
  ko1
  knu
  a_matsuda
  k0kubun
  mame
  hsbt
  glass
  naruse
  sonots
  akr
  tarui
]

members.shuffle!; p members.shift(7); p members
