Riak Function Contrib is a joint effort between Basho, the company behind Riak, and the Riak community to compile a library of functions and other useful code that can be used in applications everywhere. 

# Where to start with Riak Function Contrib

* If you're looking for an overview of the project, keep reading
* All the code for this site can be found on the [Riak Function Contrib Repo on GitHub](https://github.com/basho/riak_function_contrib)
* Scroll down to the **Contributing** section to learn how to add a function to the repo
* The list of available MapReduce functions can be found [[here|map-reduce-functions]]
* The Pre- and Post- Commit Functions are [[here|pre-and-post-commits]]
* Other Functions (Importing/Exporting Data, Bucket Reloading, etc.) can be [[found here|other-functions]]

## Functions in Riak

The ability to query Riak past the standard GET, PUT and UPDATE functionality that a key/value store provides is made possible through MapReduce functions.(You can, of course, use [Riak Search](http://wiki.basho.com/display/RIAK/Riak+Search), but it serves a different purpose than MapReduce). Additionally, Riak provides the ability to run  Pre- and Post-Commit Hooks, which are functions that are invoked before or after a riak_object is persisted. 

MapReduce and Pre- and Post- Commits enable you to extend your Riak's capabilities and build powerful querying and additional functionality into your applications.  

## The Role of Riak Function Contrib

One barrier to using these, however, is having to create numerous functions to use with your application. So, in the spirit of truly useful and collaborative projects like [clojure-contrib](https://github.com/richhickey/clojure-contrib), we are aiming to erase that barrier by tapping the collective power of the community to help us build out a library of contributed functions. 

With that in mind, the goal of Riak Function Contrib is three-fold: 
	 
1. Build a robust library of functions that developers can use in applications running on Riak 
2. Encourage participation from the community around MapReduce, Pre- and Post- Commit Hooks, and other Functions
3. Expand the amount of "built in" functions that ship with Riak 
		 
## How To Use This Site

You can use this page to: 
		 
* Search for [[MapReduce|map-reduce-functions]], [[Pre-/Post-Commit|pre-and-post-commits]] or [[other functions|other-functions]] that may be suitable for your needs 
* Learn how to contribute a function to the repo (see below)
* If you're looking for the Function Contrib Repo on Github, [go here](https://github.com/basho/riak_function_contrib).

## Contributing 

### Why Contribute to Riak Function Contrib?
		 
* Have you ever driven a Zonda from Florence to Bologna going 250 kmh the entire way? 
* Have you ever ran with the bulls at Pamplona? 
* Have you ever chugged an entire cola without stopping? 
* Have you ever rescued seven puppies from a burning building? 
* Have you ever done a 265 meter freedive off the coast New Zealand for kicks? 

__*None of these activities are nearly as exhilarating and rewarding as contributing your code to Riak Function Contrib.*__

If you have some code to share, head over to the [Riak Function Contrib repo on GitHub](https://github.com/basho/riak_function_contrib) to get started.
