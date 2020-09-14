require 'parser/current'
require 'json'

# For now, we will only guess gems that come preinstalled in Polygott.
# The mapping between what you require and the gem name is not always 1-to-1,
# so we will need some sort of mapping like what we do with PyPi if we want
# to do complete ruby gem guessing.
$allowed_gems = { 
  "sinatra" => "sinatra",
  "sinatra/base" => "sinatra",
  "stripe" => "stripe"
}

def guess_gems(code, guesses)
  root = Parser::CurrentRuby.parse(code)
  traverse_node(root, guesses)
end

def traverse_node(node, guesses)
  if node.class != Parser::AST::Node
    return
  end
  
  # Looking for any s(:send, nil, :require, s(:str, "gem"))
  if node.type == :send && node.children[1] == :require
    req_node = node.children[2]
    if req_node.class == Parser::AST::Node && req_node.type == :str
      process_require(req_node.children.first, guesses)
      return
    end
  end

  node.children.each { |child|
    traverse_node(child, guesses)
  }
end

def process_require(req_str, guesses)
  if req_str.empty?
    return
  end

  # Skip absolute or relative requires
  if req_str.start_with?('/') || req_str.start_with?('.')
    return
  end

  gem = $allowed_gems[req_str]
  if gem
    guesses[gem] = true
  end
end

guesses = {}

Dir.glob("**/*.rb").reject {|f| f['./.bundle'] }.each { |file|
  guess_gems(File.read(file), guesses)
}

puts guesses.to_json()
