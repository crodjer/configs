Pry.config.editor = "emacs"


Pry.config.commands.command "clear", "Clear terminal and reset its size" do |*args|
  begin
    require 'terminfo'
    Readline.set_screen_size *TermInfo.screen_size
  rescue
  end
  print `clear`
  ""
end

def timed
    start = Time.now
    yield
    Time.now - start
end


Pry.config.commands.alias_command "cl", "clear"
Pry.config.commands.alias_command "l", "ls"
Pry.config.commands.alias_command "sl", "ls"

# vim:set ft=ruby:
