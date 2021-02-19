(require 'smartchr)

(define-key ruby-mode-map (kbd ">") (smartchr '(">" " => " " => '`!!''" " => \"`!!'\"")))
(define-key ruby-mode-map (kbd "{") (smartchr '("{" "{ `!!' }" "do `!!' end" "{|`!!'| }" "do |`!!'| end")))
(define-key ruby-mode-map (kbd "#") (smartchr '("#" "#{`!!'}"))) 
(define-key ruby-mode-map (kbd "%") (smartchr '("%" "%{`!!'}"))) 
(define-key ruby-mode-map (kbd "W") (smartchr '("W" "%w[`!!']")))
