Gem::Specification.new do |s|
  s.name        = 'spittoon'
  s.version     = '0.1.0'
  s.summary     = 'Sequential-art comic-strip generator'
  s.description = 'Spittoon is a comic strip generator. It is an implementation of the Microsoft Comic Chat algorithm described in the paper, Comic Chat by Kurlander, Skelly, and Salesin'
  s.authors     = ['Ian Langworth']
  s.files       = ['lib/spittoon.rb', 'lib/usage_helper.rb']
  s.add_runtime_dependency 'rmagick', '~> 2.0', '>= 2.0.0'
  s.license       = 'MIT'
end
