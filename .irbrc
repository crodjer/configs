require 'rubygems'

if defined?(::Bundler)
  global_gemset = ENV['GEM_PATH'].split(':').grep(/ruby.*@global/).first
  if global_gemset
    all_global_gem_paths = Dir.glob("#{global_gemset}/gems/*")
    all_global_gem_paths.each do |p|
      gem_path = "#{p}/lib"
      $LOAD_PATH << gem_path
    end
  end
end

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
end
