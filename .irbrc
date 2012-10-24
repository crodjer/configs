require 'rubygems'

# This makes pry itself and all pry gems available
$LOAD_PATH.push(*Dir["#{ENV['HOME']}/.prygems/gems/*/lib"]).uniq!

begin
  # Use Pry everywhere
  require 'pry'
rescue LoadError => e
end

def clean
  print `clear`
  true
end

if defined? Pry

  Pry.start
  exit

else
  require 'irb'
  begin
    require 'hirb-unicode'

    Hirb.enable :output => {
      'Object' => {
        :class=>:auto_table,
        :ancestor=>true
      }
    }

    module IRB
      begin
        require "readline"
        require 'irb/completion'
        require 'terminfo'

        class ReadlineInputMethod < InputMethod
          def gets
            Readline.set_screen_size *TermInfo.screen_size
            if l = readline(@prompt, false)
              HISTORY.push(l) unless l.empty?
              @line[@line_no += 1] = "#{l}\n"
            else
              @eof = true
              l
            end
          end
        end
      rescue LoadError
      end
    end
  rescue LoadError => e
    puts e
  end
end
