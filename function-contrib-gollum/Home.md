Riak Function Contrib is a joint effort between Basho, the company behind Riak, and the Riak community, to compile a library of functions that can be used in applications everywhere. 

# Where to start with Riak Function Contrib

* If you're looking for an overview of the project, keep reading. 
* Scroll down to the *Contributing* sections to learn how to add a function to the repo
* The list of available MapReduce functions can be found [[here|map-reduce-functions]]
* The Pre- and Post- Commit Functions are [[here|pre-and-post-commits]].
* Other Functions (Importing/Exporting Data, Bucket Reloading, etc.) can be [[found here|other-functions]]

## Functions in Riak

The ability to query Riak past the standard GET, PUT and UPDATE functionality that a key/value store provides is made possible through MapReduce functions. Additionally, Riak provides the ability to run  Pre- and Post-Commit Hooks, which are functions that are invoked before or after a riak_object is persisted. 

MapReduce and Pre- and Post- Commits enable you to extend your Riak's capabilities and build powerful querying and additional functionality into your applications.  

### The Role of Riak Function Contrib

One barrier to using these, however, is having to create numerous functions to use with your application. So, based on the idea of the awesome [clojure-contrib] repo, we are aiming to erase that barrier by tapping the collective power of the community to help us build out a library of contributed functions. 

The goal of Riak Function Contrib is three-fold: 
	 
1. Build a working library of functions to be used in applications running on Riak 
2. Encourage participation from the community around MapReduce and Pre- and Post- Commit Code 
3. Expand the amount of "built in" functions that ship with Riak 
		 
### How To Use This Site

You can use this page to: 
		 
* Learn how to contribute a function to the repo 
* Search for either a MapReduce Function or a Pre-/Post-Commit Function that may be suitable for your needs 
* If you're simply looking for the Function Contrib Repo on Github, [[go here|]].

## Contributing 

### Why Contribute to Riak Function Contrib?
		 
* Have you ever driven a Zonda from Florence to Bologna going 250 kmh the entire way? 
* Have you ever ran with the bulls at Pamplona, Spain? 
* Have you ever chugged an entire cola without stopping? 
* Have you ever rescued seven puppies from a burning building? 
* Have you ever done a 265 meter freedive off the coast New Zealand for kicks? 

*None of these activities are nearly as exhilarating and rewarding as contributing your code to Riak Function Contrib.*

If you have some code to share, head over to the [Riak Function Contrib repo on GitHub](https://github.com/basho/riak_function_contrib) to get started.


*Also, if you don't have a function to contribute but want to contrbute in some other way (say, perhaps, by adding some styling to this site), that would be awesome, too!*
