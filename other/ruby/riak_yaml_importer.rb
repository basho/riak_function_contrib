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

require 'yaml'
require 'riak'

def import_folder(host, port, bucket, file_list)
  file_list.each do |file|
    import_file(host, port, bucket, file)
  end
end

def import_file(host, port, bucket, file)
  client = Riak::Client.new(:host => host,
                            :port => port,
                            :http_backend => :Excon)
  
  bucket = client.bucket(bucket)
  
  w_props = { 
    :w => 0,
    :dw => 0,
    :returnbody => false
  }
  
  records = YAML::load_stream(File.open(file))

  records[0].each do |record|
    o = bucket.new(record[0])
    o.data = record[1]
    o.content_type = 'application/json'
    o.store(w_props)
  end
end