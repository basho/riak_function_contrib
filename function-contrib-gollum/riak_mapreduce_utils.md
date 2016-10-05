Overview
========

The riak_mapreduce_utils module contains a collection of seven mapreduce utility functions implemented in Erlang.

The first one is a map phase delete function called **map_delete**, which allows deletes to be performed locally where the data resides. This complements the [reduce phase delete function](http://contrib.basho.com/delete_keys.html) that is already available. It can be configured to only process records belonging to a specific Bucket.

The next 2 functions, **map_indexlink** and **map_indexinclude** allow master-detail relationships to be defined and managed through secondary indexes (2i) instead of links. These functions rely on a reference to the master record being set as a secondary index on each of the detail records. Unlike when using links to manage master-detail relationships, this method allows detail records to be added or deleted without having to update the master record. 

**map_indexlink** allows retrieval of master record from the detail record based on the 2i in a manner similar to how links work.

**map_indexinclude** allows retrieval of detail records linked to a master record through an index query. 

The fourth one is a map phase function called **map_metafilter**. This allows records to be filtered out from the result set based on Bucket and meta data (2i and user metadata).

**map_id** returns readable bucket and key pairs, while the **map_key** function just returns readable keys.

The last one, **map_datasize** returns the size of the stored object.

Installation
============

Check out and compile the module by opening an erlang shell with the directory the module file resides in as part of the path as shown below.

    $> erl -pa .
    Erlang R15B (erts-5.9) [source] [smp:2:2] [async-threads:0] [hipe] [kernel-poll:false]
    
    Eshell V5.9  (abort with ^G)
    1> c(riak_mapreduce_utils.erl).
    {ok,riak_mapreduce_utils}
    2> 

The compiled file must then be deployed to all nodes in the cluster. This is done by placing the *riak_mapreduce_utils.beam* file in a directory indicated by the *add_paths* parameter in the *riak_kv* section of the *app.config* file. Please see the [Basho Wiki](http://wiki.basho.com/Configuration-Files.html) for further details.

Function Reference
==================

This section contains more detailed information about the configuration, usage and behaviour of the functions provided in this module.

The examples provided assume we have test data in the two buckets named **master** and **detail**. The records in the detail bucket have a reference to the appropriate master in a binary secondary index called **fk_master_bin**. 

The records in the **details** bucket also have a secondary integer index defined named **idx_int** as well as a metadata field called **type**. These have been added for use with the last of the four functions, **map_metafilter**.

As these mapping functions do not operate on the record data in any way, this has been set to a generic string.

The example records can be created through *curl* as follows:

    $> curl -X POST \
        -d 'm1_data' \
        http://localhost:8098/buckets/master/keys/m1

    $> curl -X POST \
        -d 'm2_data' \ 
        http://localhost:8098/buckets/master/keys/m2

    $> curl -X POST \
        -H 'x-riak-index-fk_master_bin: m1' \
        -H 'x-riak-index-idx_int: 12' \
        -H 'x-riak-meta-type: X' \
        -d 'd1_data' \ 
        http://localhost:8098/buckets/detail/keys/d1

    $> curl -X POST \
        -H 'x-riak-index-fk_master_bin: m1' \
        -H 'x-riak-index-idx_int: 68' \
        -H 'x-riak-meta-type: Y' \
        -d 'd2_data' \ 
        http://localhost:8098/buckets/detail/keys/d2

    $> curl -X POST \
        -H 'x-riak-index-fk_master_bin: m1' \
        -H 'x-riak-index-idx_int: 125' \
        -H 'x-riak-meta-type: X' \
        -d 'd3_data' \ 
        http://localhost:8098/buckets/detail/keys/d3

    $> curl -X POST \
        -H 'x-riak-index-fk_master_bin: m2' \
        -H 'x-riak-index-idx_int: 9' \
        -H 'x-riak-meta-type: Y' \
        -d 'd4_data' \ 
        http://localhost:8098/buckets/detail/keys/d4

map_id()
----------------

The **map_id** function takes one optional argument, the name of the bucket to print IDs from. If specified, records belonging to a bucket different from the one specified will be filtered out. If the parameter is not specified, the IDs of all records that are passed to the function will be output.

###Example

This example lists all IDs currently present in the *detail* bucket.

    $> curl -XPOST http://localhost:8098/mapred -H 'Content-Type: application/json' -d '{
	     "inputs":"detail",
	     "query":[{"map":{"language":"erlang","module":"riak_mapreduce_utils",
	                     "function":"map_id"}}]}'
	                     
    [["detail","d2"],["detail","d3"],["detail","d4"],["detail","d1"]]
    $>

map_key()
----------------

The **map_key** function takes one optional argument, the name of the bucket to print keys from. If specified, records belonging to a bucket different from the one specified will be filtered out. If the parameter is not specified, the keys of all records that are passed to the function will be output.

###Example

This example specifies that only keys belonging to the *detail* bucket are to be output, and the record belonging to the *master* bucket is therefore suppressed.

    $> curl -XPOST http://localhost:8098/mapred -H 'Content-Type: application/json' -d '{
        "inputs":[["detail","d3"],["master","m1"],["detail", "d1"]],
        "query":[{"map":{"language":"erlang","module":"riak_mapreduce_utils",
        "function":"map_key","arg":"detail"}}]}'
	                     
    ["d3","d1"]
    $>

map_delete()
------------

The **map_delete** function takes one optional argument, the name of the bucket to delete records from. If this parameter (a string) is defined, it will only delete records that belong to this bucket. If no argument is specified, it will delete any record passed into it. 

The function returns the number of records deleted and can be used together with **riak_kv_mapreduce:reduce_sum** to get the total number of records deleted through the job.

###Examples

**Delete all records in the detail bucket**

This example deletes all records belonging to the *detail* bucket and returns a count of the number of records deleted.

    $> curl -XPOST http://localhost:8098/mapred -H 'Content-Type: application/json' -d '{
	     "inputs":"detail",
	     "query":[{"map":{"language":"erlang","module":"riak_mapreduce_utils",
	                     "function":"map_delete","keep":false}},
	              {"reduce":{"language":"erlang","module":"riak_kv_mapreduce",
	                        "function":"reduce_sum"}}]}'
    [4]
    $>

**Delete only record passed in that belong to the *detail* bucket**

The following example contains a filter for only deleting records belonging to the detail bucket. Of the 2 records passed in, only the one from the detail bucket is deleted. 

    $> curl -XPOST http://localhost:8098/mapred -H 'Content-Type: application/json' -d '{
	     "inputs":[["master", "m1"],["detail", "d4"]],
	     "query":[{"map":{"language":"erlang","module":"riak_mapreduce_utils",
	                     "function":"map_delete","keep":false,"arg":"detail"}},
	              {"reduce":{"language":"erlang","module":"riak_kv_mapreduce",
	                        "function":"reduce_sum"}}]}'
    [1]
    $>

map_indexlink()
---------------

The **map_indexlink** function allows retrieval of related records through secondary index links in a way similar to how link walking works. It does assume that the link is specified through a secondary index, which contains a reference (key) to another record. This allows the retrieval of e.g. a master record based on a detail record.

The function takes a JSON formatted argument that specifies how the linking is done and what index that is to be used. 

The function can be configured to return either both the source and target records or just the resulting target record.

###Configuration

The configuration string must be a correctly formatted JSON document and may contain the following 4 fields:

**source** - Name of the bucket containing records to be processed. If a record is encountered that does not belong to the specified bucket, it will be passed straight through based on the **keep** parameter described below. 

This is an optional field, and if no **source** is specified, all records passed in will be processed.

**target** - This field is mandatory and must contain the name of the bucket the operation should link to. The value of this parameter will be used together with the index value to create the target object ID.

**indexname** - This field is mandatory and must contain the name of the index to be used for the linking.

**keep** - This parameter is optional and defaults to *true*. It specifies whether the input should be kept as part of the result set or not. If set to false, only records retrieved through the link are returned. 

Below is an example of a valid configuration based on the testdata specified above. 

    "{
        "source":"detail",
        "target":"master",
        "indexname":"fk_master_bin",
        "keep":"false"
    }"

If a single detail record is processed by a function with this configuration, it will return just the master record as it is configured to not keep the input record in the result set.

Note that the link is evaluated once for every record passed in, so if multiple detail records belonging to the same master record are passed on, there may be multiple occurances of the master record as a result.

###Examples

**Retrieving master record related to single detail record**

The following example takes a single detail record as input and returns this record (*detail:d2*) together with the master record (*master:m1*) it is related to as no value was specified for the **keep** parameter passed to the function.

    $> curl -XPOST http://localhost:8098/mapred -H 'Content-Type: application/json' -d '{
	     "inputs":[["detail","d2"]],
	     "query":[{"map":{"language":"erlang","module":"riak_mapreduce_utils",
	                     "function":"map_indexlink","keep":false,
	                     "arg":"{\"source\":\"detail\",\"target\":\"master\",
	                     \"indexname\":\"fk_master_bin\"}"}},
	              {"map":{"language":"erlang","module":"riak_mapreduce_utils",
	                     "function":"map_id"}}]}'
    [["detail","d2"],["master","m1"]]
    $>

**Retrieving master record related to single detail record while dropping the input**

This example returns the master record only (*master:m1*), as the input to the map phase function (*detail:d2*) is configured to be dropped. 

    $> curl -XPOST http://localhost:8098/mapred -H 'Content-Type: application/json' -d '{
        "inputs":[["detail","d2"]],
        "query":[{"map":{"language":"erlang","module":"riak_mapreduce_utils",
                        "function":"map_indexlink","keep":false,
                        "arg":"{\"source\":\"detail\",\"target\":\"master\",
                        \"indexname\":\"fk_master_bin\",\"keep\":\"false\"}"}},
                 {"map":{"language":"erlang","module":"riak_mapreduce_utils",
                        "function":"map_id"}}]}'
    [["master","m1"]]
    $>
    
###Limitations

As the function relies on secondary indexes, it requires a storage backend that supports secondary indexes to be used, e.g. eleveldb.

map_indexinclude()
------------------

In the example data provided, The link between detail records and their respective master is created through a secondary index on the detail records (*fk_master_bin*). **map_indexinclude** makes it possible to retrieve such detail records based on the master record.

This function complements **map_indexlink** in that it allows processing of the relationship the other way.

The function takes a JSON formatted argument that specifies how the linking is done and what index that is to be used. 

###Configuration

The configuration string must be a correctly formatted JSON document and may contain the following 4 fields: 

**source** - Name of the bucket containing records to be processed. This is a mandatory field and only records belonging to this bucket will be processed. If a record is encountered that does not belong to the specified bucket, it will be passed straight through based on the **keep** parameter described below

**target** - This field is mandatory and must contain the name of the bucket the operation should link to. 

**indexname** - Thi field is mandatory and must contain the name of the index on the target bucket that is to be used.

**keep** - This parameter is optional and detaults to *true*. It specifies whether the input should be kept as part of the result set or not. If set to false, only records retrieved through the link are returned. 

###Example

**Retrieve all detail records related to a specific master record**

This example takes a single master record and retrieves all related detail records. AS **keep** is set to *false*, the original master record that the fetch was based on is excluded from the result set, and only the appropriate detail records are returned.

    $> curl -XPOST http://localhost:8098/mapred -H 'Content-Type: application/json' -d '{
        "inputs":[["master","m1"]],
        "query":[{"map":{"language":"erlang","module":"riak_mapreduce_utils",
                        "function":"map_indexinclude","keep":false,
                        "arg":"{\"source\":\"master\",\"target\":\"detail\",
                        \"indexname\":\"fk_master_bin\",\"keep\":\"false\"}"}},
                 {"map":{"language":"erlang","module":"riak_mapreduce_utils",
                        "function":"map_id"}}]}'
    [["detail","d3"],["detail","d2"],["detail","d1"]]
    $>
   
###Limitations

As the function relies on secondary indexes, it requires a storage backend that supports secondary indexes to be used, e.g. eleveldb.

map_metafilter()
----------------

**map_metafilter** allow records to be filtered out from the result set through configurable criteria based on content agnostic meta data, i.e. secondary indexes, user meta data and bucket name.

If a record matches all the criteria configured for the filter, it will be dropped/excluded while other records will be passed through to the next step.

###Configuration

The configuration string must be a correctly formatted and escaped JSON document and may contain the following 2 fields, out of which at least one needs to be present:

**source** - Name of the bucket containing records to be filtered. If a record is encountered that does not belong to the specified bucket, it will be passed straight through and not processed with respect to criteria by the function.

This is an optional field, and if no **source** is specified, all records passed in will be processed.

**criteria** - This field contains a list of criteria. These will be evaluated against the record, and if ALL of them evaluate to true, the record will be filtered out.

A criteria consists of an *operation*, a *field* to operate on and a *parameter*.

Fields can be either a secondary index (specified as *"index:\<index name\>"*) or user metadata field (specified as *"meta:\<meta data name\>"*). 

The following operations are allowed:

- **"eq"** takes a single parameter and will evaluate to *true* if the field value is equal to the parameter. Example: "["eq","index:idx_int","12"]"

- **"neq"** takes a single parameter and will evaluate to *true* if the field value is NOT equal to the parameter. Example: "["neq","index:idx_int","12"]"

- **"greater_than"** takes a single parameter and will evaluate to *true* if the field value is strictly greater than the parameter value. If the field value is an integer, an attempt will be made to convert the parameter to integer and perform the evaluation based on these. If this fails, the field value will be converted to binary and compared to the parameter. Example: "["greater_than","index:idx_int","12"]"

- **"greater_than_eq"** takes a single parameter and will evaluate to *true* if the field value is greater than or equal to the parameter value. If the field value is an integer, an attempt will be made to convert the parameter to integer and perform the evaluation based on these. If this fails, the field value will be converted to binary and compared to the parameter. Example: "["greater_than_eq","meta:X-Riak-Meta-Type","Y"]"

- **"less_than"** takes a single parameter and will evaluate to *true* if the field value is strictly less than the parameter value. If the field value is an integer, an attempt will be made to convert the parameter to integer and perform the evaluation based on these. If this fails, the field value will be converted to binary and compared to the parameter. Example: "["less_than","index:idx_int","56"]"

- **"less_than_eq"** takes a single parameter and will evaluate to *true* if the field value is less than or equal to the parameter value. If the field value is an integer, an attempt will be made to convert the parameter to integer and perform the evaluation based on these. If this fails, the field value will be converted to binary and compared to the parameter. Example: "["less_than_eq","index:idx_int","12"]"

Below is an example of a valid configuration based on the testdata specified above. 

    "{
        "source":"detail",
        "criteria":[["eq","meta:X-Riak-Meta-Type","X"],["less_than","index:idx_int","15"]]
    }"

###Examples

**Filtering all records from a specific bucket**

This example filters out all records belonging to the *master* bucket and keeps everything else.

    $> curl -XPOST http://localhost:8098/mapred -H 'Content-Type: application/json' -d '{
        "inputs":[["master","m1"],["detail","d2"],["detail","d3"]],
        "query":[{"map":{"language":"erlang","module":"riak_mapreduce_utils",
                         "function":"map_metafilter","keep":false,
                         "arg":"{\"source\":\"master\"}"}},
                 {"map":{"language":"erlang","module":"riak_mapreduce_utils",
                        "function":"map_id"}}]}'
    [["detail","d2"],["detail","d3"]]
    $>

**Filtering based on meta field**

This example filters based on the *X-Riak-Meta-Type* field and drops all records that have this field set to *X*.

    $> curl -XPOST http://localhost:8098/mapred -H 'Content-Type: application/json' -d '{
        "inputs":"detail",
        "query":[{"map":{"language":"erlang","module":"riak_mapreduce_utils",
                         "function":"map_metafilter","keep":false,
                         "arg":"{\"criteria\":[[\"eq\",\"meta:X-Riak-Meta-Type\",\"X\"]]}"}},
                 {"map":{"language":"erlang","module":"riak_mapreduce_utils",
                         "function":"map_id"}}]}'
    [["detail","d4"],["detail","d2"]]
    $>

**Filtering based on index field**

This example shows how to filter based on a secondary integer index. It filters out all *detail* records that have an index value less than or equal to 68.

    $> curl -XPOST http://localhost:8098/mapred -H 'Content-Type: application/json' -d '{
        "inputs":"detail",
        "query":[{"map":{"language":"erlang","module":"riak_mapreduce_utils",
                         "function":"map_metafilter","keep":false,
                         "arg":"{\"source\":\"detail\",
                         \"criteria\":[[\"less_than_eq\",\"index:idx_int\",\"68\"]]}"}},
                 {"map":{"language":"erlang","module":"riak_mapreduce_utils",
                         "function":"map_id"}}]}'
    [["detail","d3"]]
    $>

**Composite filtering of specific bucket**

The example below shows how to filter out all the records that have an integer index value between 10 and 100, including the edge values.  

    $> curl -XPOST http://localhost:8098/mapred -H 'Content-Type: application/json' -d '{
        "inputs":"detail",
        "query":[{"map":{"language":"erlang","module":"riak_mapreduce_utils",
                         "function":"map_metafilter","keep":false,
                         "arg":"{\"source\":\"detail\",
                         \"criteria\":[[\"greater_than_eq\",\"index:idx_int\",\"10\"],
                                       [\"less_than_eq\",\"index:idx_int\",\"100\"]]}"}},
                 {"map":{"language":"erlang","module":"riak_mapreduce_utils",
                         "function":"map_id"}}]}'
    [["detail","d4"],["detail","d3"]]
    $>

map_datasize()
----------------

The **map_datasize** function returns the size of the value stored in bytes. If there are siblings present, it returns the combined size of these values.

###Example

This example sums up the size of data for all records in the *master* bucket.

    $> curl -XPOST http://localhost:8098/mapred -H 'Content-Type: application/json' -d '{
        "inputs":"master",
	"query":[{"map":{"language":"erlang","module":"riak_mapreduce_utils","function":"map_datasize"}},
	{"reduce":{"language":"erlang","module":"riak_kv_mapreduce","function":"reduce_sum"}}]}'
    [14]
    $>


License & Copyright
-------------------

Copyright ©2012 Christian Dahlqvist, WhiteNode Software Ltd

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.

