# This is a Ruby script which uses the Gems gem to retrieve package
# metadata from RubyGems. It takes one command-line argument, the name
# of the package to look up, and writes the result (see rubygemsInfo)
# to stdout as JSON.

require 'gems'
require 'json'

puts Gems.info(ARGV[0]).to_json
