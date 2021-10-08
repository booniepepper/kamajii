require "file_utils"

# TODO: Write documentation for `Kamajii`
module Kamajii
  extend self

  KAMAJII_HOME = ENV["KAMAJII_HOME"]? || Path.home

  def push(stack : Path | String, contents : String)
    dir = stackdir stack
    item = File.join dir, "item"
    nexxt = File.join dir, "next"

    if File.exists? item
      if File.exists? nexxt
        temp_nexxt = File.join dir, "temp_next"

        FileUtils.mv nexxt, temp_nexxt
        Dir.mkdir nexxt
        FileUtils.mv temp_nexxt, File.join(nexxt, "next")
        FileUtils.mv item, File.join(nexxt, "item")
      else
        Dir.mkdir nexxt
        FileUtils.mv item, File.join(nexxt, "item")
      end
    end

    File.write item, contents
  end

  def pop(stack : Path | String) : String?
    dir = stackdir stack
    item = File.join dir, "item"

    content = nil
    if File.exists? item
      content = File.read item
      FileUtils.rm item
    end

    nexxt = File.join dir, "next"
    if File.exists? nexxt
      temp_nexxt = File.join dir, "temp_next"
      FileUtils.mv nexxt, temp_nexxt
      Dir.each_child temp_nexxt do |child|
        FileUtils.mv File.join(temp_nexxt, child), File.join(dir, child)
      end
      FileUtils.rmdir temp_nexxt
    end

    content
  end

  private def stackdir(stack : Path | String) : String
    home = KAMAJII_HOME
    path = File.join(home, ".kamajii", stack)
    Dir.mkdir_p(path)
    path
  end
end
