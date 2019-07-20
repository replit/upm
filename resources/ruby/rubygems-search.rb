# This is a Ruby script which uses the Gems gem to search RubyGems. It
# takes one command-line argument, the search query (which may contain
# spaces), and writes the list of search results (see rubygemsInfo) to
# stdout as JSON.

require 'gems'
require 'json'

puts Gems.search(ARGV[0]).to_json
