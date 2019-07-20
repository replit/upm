# This is a Ruby script which dumps relevant information from the
# Gemfile to stdout in JSON format. The JSON is a map from package
# names to specs, both strings.

require 'bundler'
require 'json'

dsl = Bundler::Dsl.new

result = {}
dsl.eval_gemfile("Gemfile").each do |dep|
  result[dep.name] = dep.requirement.requirements.map{ |req| req.join(" ") }.join(", ")
end

puts result.to_json
