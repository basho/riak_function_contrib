# Importing YAML

_Contributed By:_ [Jeremiah Peschka](https://github.com/peschkaj)

[Source File on GitHub](https://github.com/basho/riak_function_contrib/blob/master/other/ruby/yaml_importer.rb)

This Ruby program allows you to import a single YAML file into a bucket. The YAML will be converted to JSON. The YAML is expected to be structured as

```yaml
key_name:
  attribute_1: value
  attribute_2: value
```

Usage information

```
Usage: yaml_importer_.rb [options]
    -f, --file FILE                  YAML to load
    -d, --directory DIRECTORY        Folder to recurse
    -b, --bucket BUCKET              Bukket for mah data!
    -h, --host HOSTNAME              IP/hostname for the Riak cluster
    -p, --port PORT                  Port number
        --help                       Display this screen
```

The [[riak_yaml_importer|https://github.com/basho/riak_function_contrib/blob/master/other/ruby/riak_yaml_importer.rb]] library contains the functionality that is used by yaml_importer. There are two functions in riak_yaml_importer: 

```ruby
import_folder(host, port, bucket, file_list)
```

which iterates over a list of files and calls:

```ruby
import_file(host, port, bucket, file)
```
