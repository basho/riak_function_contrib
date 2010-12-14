# Riak Function Contrib

Riak Function Contrib is a community-powered library of MapReduce, Pre-/Post-Commit Hook, and other functions. It exists for several reasons:

1. So Riak users can contribute functions they've written back to the community in one centralized, easy-to-manage location
2. To provide users who are new to Riak with a list of previously-created and tested functions that may suit their usage needs 
3. To lower the barrier to entry to using and mastering MapReduce and Pre-/Post-Commit Hook Functions in Riak

## Usage

To use the code in this repo you can either browse the source files of the functions in the directory of your choice or head over to the [Riak Function Contrib site](http://contrib.basho.com) to search for useful code and read in depth descriptions provided by function authors.

## Issues or Questions

If, at any point, you have a question or issue, email _mark@basho.com_ or join the [Riak Mailing List](http://lists.basho.com/mailman/listinfo/riak-users_lists.basho.com) and submit your question there. 

## Contributing

Have a function to share with the rest of the Riak Community? Great. Here is how you do it:

1. Fork this repo to your own GitHub account (You can read up on forking [here](http://help.github.com/forking/) if you need a refresher)
2. Add your function source file to the appropriate repo (explained in depth below)
3. Add your overview file to the appropriate repo (again, explained below)
4. Send a pull request 
5. Kick back, smile, and relish in the fact that your functions are helping Riak users everywhere

### Your Function Source File

Step 2 above is "Add your function source file to the appropriate repo." The MapReduce Functions live in the `mapreduce` repo, and are broken up by languages (Erlang or JavaScript). Pre- and Post- Commit source code lives in the `hooks` directory and is broken down by type (Pre and Post). For instance, if you have a JavaScript MapReduce Function to contribute, it will [live in this directory](https://github.com/basho/riak_function_contrib/tree/master/mapreduce/js/).

**What should the source file contain?**
 
1. _Apache 2.0 License Boilerplate_. All files submitted to the Riak Function Contrib Repo must include the Apache 2.0 boilerplate. If you're unfamiliar with the Apache license and how to include it in your code, please take a moment and read up on it [here](http://www.apache.org/licenses/LICENSE-2.0.html).

2. Your code. This is fairly self-explanatory. In addition to the code, make sure to include adequate comments and notation. [Here's a great example](https://github.com/basho/riak_function_contrib/blob/master/mapreduce/js/sorting-by-field.js).

### Your Overview File

In addition to your source, you're encourage to take a few minutes and put together an overview file. Why? Because your contribution to Riak Function Contrib will visible in two locations: 1) as part of the actual code repo in the form of your source file and 2) as an overview page on the [Riak Function Contrib site](http://contrib.basho.com) For example, the overview page for [this function](https://github.com/basho/riak_function_contrib/blob/master/mapreduce/js/sorting-by-field.js) lives 
[here](http://contrib.basho.com/sorting-by-field.html)

This is actually pretty simple. We use [Gollum](https://github.com/github/gollum) to generate this site. What that means is that your overview file can be written in either [Markdown](http://en.wikipedia.org/wiki/Markdown) or [Textile](http://en.wikipedia.org/wiki/Textile_(markup_language). Gollum will take care of converting them to html and, once you've sent the pull request, we'll regenerate the site and make sure your contribution goes live.

And, since we are using Gollum, you can build the site locally to make sure everything looks right before you send your pull request. Specifically, here's what you need to do:

1. [Install Gollum](https://github.com/github/gollum)
2. [Install Gollum Site](https://github.com/dreverri/gollum-site) (This is a simple static site generator for Gollum written by Basho's very own Dan Reverri)
3. cd into your local copy of the Riak Function Contrib repo
4. `$ gollum-site generate && gollum-site serve`
5. Head to http://localhost:8000/

This should get the site running locally and you should be able to view your additions. **You must commit your changes locally in order for Gollum to generate them**

### A General Note on File Naming

Try to name your source file in such a way that it describes what the function might be used for. For example, if the code is a JavaScript reduce function that is good for filtering out large objects, you might name it "large-object-filter-reduce.js"

Also, be sure to name your overview file with the same name as your source file (save for the extension, of course). So, to continue with the example given above, if your source file is named "large-object-filter-reduce.js" you would name your overview file "large-object-filter-reduce.textile"

## Other Resources

[MapReduce on the Riak Wiki](http://wiki.basho.com/display/RIAK/MapReduce)
[Pre- and Post-Commit Hooks on the Riak Wiki](http://wiki.basho.com/display/RIAK/Pre-+and+Post-Commit+Hooks)
[The Riak Fast Track](http://wiki.basho.com/display/RIAK/The+Riak+Fast+Track)




