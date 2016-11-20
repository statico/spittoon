#!/usr/bin/env ruby
#
# Copyright (c) 2010 Ian Langworth
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#
#
#
# == Synopsis
#
# make_comic: Uses a configration file to generate comics.
#
# == Usage
#
# -c, --config_file:
#   A path to a YAML file. The contents will be used to configure Spittoon.
#   See Spittoon::Config for details.
#
# -s, --spec_file:
#   A path to a YAML or plaintext file. The contents will be used to create the
#   comic strip. See Spittoon::Spec for details.
#
# -o, --output_file:
#   A file path to which the final image will be written.
#
# -h, --help:
#   (Optional): Show help on this program's usage.
#

require 'getoptlong'
require 'usage_helper'
require 'spittoon'

opts = GetoptLong.new(
  ['--help', '-h', GetoptLong::NO_ARGUMENT],
  ['--config_file', '-c', GetoptLong::REQUIRED_ARGUMENT],
  ['--spec_file', '-s', GetoptLong::REQUIRED_ARGUMENT],
  ['--output_file', '-o', GetoptLong::REQUIRED_ARGUMENT]
)

spec = nil
output_file = nil

opts.each do |opt, arg|
  case opt
    when '--help'
      UsageHelper::usage(0)
    when '--config_file'
      Spittoon.load_config(arg)
    when '--spec_file'
      spec = Spittoon::Spec.new(File.new(arg).read)
    when '--output_file'
      output_file = arg
  end
end

if output_file.nil? or ARGV.length != 0 or not Spittoon.initialized?
 UsageHelper::usage(1)
end

if spec.nil?
  if STDERR.tty?
    STDERR.puts('--spec_file not present, reading spec from stdin...')
  end
  spec = Spittoon::Spec.new(STDIN.read)
 if spec.nil?
   UsageHelper::usage(1)
 end
end
Spittoon.generate_comic(spec).write(output_file) do
  self.quality = 80
  self.depth = 8
  self.compression = Magick::ZipCompression
end
