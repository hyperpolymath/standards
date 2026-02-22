Gem::Specification.new do |spec|
  spec.name          = "palimpsest-license"
  spec.version       = "0.4.0"
  spec.authors       = ["Palimpsest License Contributors"]
  spec.email         = ["hello@palimpsestlicense.org"]

  spec.summary       = "Ruby/Rails integration for Palimpsest License"
  spec.description   = "Provides middleware and utilities for integrating the Palimpsest License into Ruby and Rails applications"
  spec.homepage      = "https://palimpsestlicense.org"
  spec.license       = "Palimpsest-0.4"

  spec.required_ruby_version = ">= 2.7.0"

  spec.metadata["homepage_uri"] = spec.homepage
  spec.metadata["source_code_uri"] = "https://github.com/palimpsest-license/palimpsest-license"
  spec.metadata["changelog_uri"] = "https://github.com/palimpsest-license/palimpsest-license/blob/main/CHANGELOG.md"

  spec.files = Dir["lib/**/*", "README.md", "LICENSE.md"]
  spec.require_paths = ["lib"]

  spec.add_dependency "rack", ">= 2.0"

  spec.add_development_dependency "rspec", "~> 3.12"
  spec.add_development_dependency "rack-test", "~> 2.0"
  spec.add_development_dependency "rubocop", "~> 1.50"
end
