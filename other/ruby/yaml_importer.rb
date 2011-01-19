#!/usr/bin/env ruby

# Copyright 2011 Jeremiah Peschka
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

require 'rubygems'
require 'riak'
require 'yaml'
require 'optparse'

options = {}

optparse = OptionParser.new do |opts|
  opts.banner = "Usage: load_yaml.rb [options]"

  options[:file] = nil
  opts.on('-f', '--file FILE', String, 'YAML to load') do |file|
    options[:file] = file
  end

  options[:bucket] = nil
  opts.on('-b', '--bucket BUCKET', String, 'Bukket for mah data!') do |bucket|
    options[:bucket] = bucket
  end

  options[:host] = 'localhost'
  opts.on('-h', '--host HOSTNAME', String, 'IP/hostname for the Riak cluster') do |host|
    options[:host] = host
  end

  options[:port] = 8091
  opts.on('-p', '--port PORT', Integer, 'Port number') do |port|
    options[:port] = port
  end

  opts.on('--help', 'Display this screen') do
    puts opts
    exit
  end
end

optparse.parse!

client = Riak::Client.new(:port => options[:port],
                          :host => options[:host],
                          :http_backend => :Excon)

bukket = client.bucket(options[:bucket], :keys => false)

w_props = { :w => 0,
  :dw => 0,
  :returnbody => false
}

records = YAML::load_stream(File.open(options[:file]))

records[0].each do |record|
  o = bukket.new(record[0])
  o.data = record[1]
  o.content_type = 'application/json'
  o.store(w_props)
end
