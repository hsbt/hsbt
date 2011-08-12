# -*- coding: utf-8 -*-

class Living
  attr :hp, true
  attr :max_hp, true
  attr :name, true
  attr :attack_power, true
end

class Enemey < Living
  attr :exp, true
end

class Slime < Enemey
  def initialize
    self.name = "SLIME"
    self.max_hp = 10
    self.hp = self.max_hp
    self.attack_power = 4
    self.exp = 1
  end
end

class Dragon < Enemey
  def initialize
    self.name = "DRAGON"
    self.max_hp = 20
    self.hp = self.max_hp
    self.attack_power = 8
    self.exp = 3000
  end
end

class Player < Living
  attr :mp, true

  def initialize
    self.name = "PLAYER"
    self.max_hp = 10
    self.hp = self.max_hp
    self.mp = 20
    self.attack_power = 3
  end
end

class Buttle
  def initialize
    @enemey_classes = [Slime, Dragon]
    @player = Player.new
    @turn_count = 0
  end
  
  def finish_battle
    puts "#{@enemey.name}をたおした"
    
    puts
    puts "==========================="
    puts "かかったターン数#{@turn_count}"
    puts "経験値#{@enemey.exp}かくとく"
    
    gets
  end
  
  def check_enemey_hp
    if @enemey.hp <= 0
      @enemey.hp = 0
      finish_battle()
    else
      enemey_attack()
    end
  end
  
  def game_over
    puts "#{@player.name}はたおれました"
    
    puts
    puts "==========================="
    puts "ゲームオーバー"
    
    gets
  end
  
  def check_player_hp
    if @player.hp <= 0
      @player.hp = 0
      game_over()
    else
      query_command()
    end
  end
  
  def enemey_attack
    damage_point = rand(@enemey.attack_power) + 1
    
    puts
    puts "==========================="
    puts "#{@enemey.name}のこうげき"
    @player.hp -= damage_point
    puts "#{@player.name}に#{damage_point}のダメージ"
    
    gets
    
    check_player_hp()
  end
  
  def player_attack
    damage_point = rand(@player.attack_power) + 1
    
    puts
    puts "==========================="
    puts "#{@player.name}のこうげき"
    @enemey.hp -= damage_point
    puts "#{@enemey.name}に#{damage_point}のダメージ"
    
    gets
    
    check_enemey_hp()
  end
  
  def player_hoimi
    cure_point = rand(8) + 1
    
    puts
    puts "==========================="
    puts "#{@player.name}はホイミをとなえた"
    @player.hp = [@player.hp + cure_point, @player.max_hp].min
    puts "HPが#{cure_point}回復した"
    
    gets
    
    enemey_attack()
  end

  def query_command
    @turn_count += 1

    puts
    puts "==========================="
    puts "#{@player.name}のHP: #{@player.hp}"
    puts " 1 たたかう"
    puts " 2 ホイミ"
    puts "コマンド? [1]"
    
    command = gets.chomp
    
    case command
    when "ホイミ", "2"
      player_hoimi()
    when "にげる", "3"
      player_escape()
    else
      player_attack()
    end
  end
  
  def encount
    @enemey = @enemey_classes.sample.new
    
    puts "==========================="
    puts "#{@enemey.name}があらわれた"
    
    gets
    
    query_command()
  end

  def start
    encount()
  end
end

class ButtleEnglish
  def initialize
    @enemey_classes = [Slime, Dragon]
    @player = Player.new
    @turn_count = 0
  end
  
  def finish_battle
    puts "#{@player.name} win!"
    
    puts
    puts "==========================="
    puts "Turn count: #{@turn_count}"
    puts "#{@player.name} get #{@enemey.exp} EXP"
    
    gets
  end
  
  def check_enemey_hp
    if @enemey.hp <= 0
      @enemey.hp = 0
      finish_battle()
    else
      enemey_attack()
    end
  end
  
  def game_over
    puts "#{@player.name} lose..."
    
    puts
    puts "==========================="
    puts "GAME OVER"
    
    gets
  end
  
  def check_player_hp
    if @player.hp <= 0
      @player.hp = 0
      game_over()
    else
      query_command()
    end
  end
  
  def enemey_attack
    damage_point = rand(@enemey.attack_power) + 1
    
    puts
    puts "==========================="
    puts "#{@enemey.name} attack."
    @player.hp -= damage_point
    puts "#{@player.name} damaged #{damage_point} point(s)."
    
    gets
    
    check_player_hp()
  end
  
  def player_attack
    damage_point = rand(@player.attack_power) + 1
    
    puts
    puts "==========================="
    puts "#{@player.name} attack"
    @enemey.hp -= damage_point
    puts "#{@enemey.name} damaged #{damage_point} point(s)."
    
    gets
    
    check_enemey_hp()
  end
  
  def player_hoimi
    cure_point = rand(8) + 1
    
    puts
    puts "==========================="
    puts "#{@player.name} call hoimi."
    @player.hp = [@player.hp + cure_point, @player.max_hp].min
    puts "#{@player.name} cured #{cure_point} point(s)"
    
    gets
    
    enemey_attack()
  end

  def query_command
    @turn_count += 1

    puts
    puts "==========================="
    puts "#{@player.name} HP: #{@player.hp}"
    puts " 1 Attack"
    puts " 2 Hoimi"
    puts "Command? [1]"
    
    command = gets.chomp
    
    case command
    when "Hoimi", "2"
      player_hoimi()
    else
      player_attack()
    end
  end
  
  def encount
    @enemey = @enemey_classes.sample.new
    
    puts "==========================="
    puts "#{@player.name} encounted a #{@enemey.name}."
    
    gets
    
    query_command()
  end

  def start
    encount()
  end
end


if ARGV.shift == "english"
  ButtleEnglish.new.start
else
  Buttle.new.start
end
