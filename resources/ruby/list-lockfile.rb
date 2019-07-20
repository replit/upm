# This is a Ruby script which dumps relevant information from the
# Gemfile.lock to stdout in JSON format. The JSON is a map from
# package names to versions, both strings.

require 'bundler'
require 'json'

# https://stackoverflow.com/a/40098825/3538165
lockfile = Bundler::LockfileParser.new(Bundler.read_file(Bundler.default_lockfile))

result = {}
lockfile.specs.each do |spec|
  result[spec.name] = spec.version
end

puts result.to_json
