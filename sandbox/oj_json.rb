require 'active_support/all'

# "{\"foo\":\"0/1\"}"
p ({foo: Rational(1/2)}).to_json

require 'oj'
require 'oj_mimic_json'

# stack level too deep (SystemStackError)
p ({foo: Rational(1/2)}).to_json
