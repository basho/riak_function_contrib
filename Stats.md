# Stats Function Object

_Contributed By:_ Daniel Einspanjer

[Source File on GitHub](https://github.com/basho/riak_function_contrib/blob/master/mapreduce/js/stats.js)
 
This is a function object that can be used to generate commonly useful statistics on an array of numbers passed in. Supports count, sum, min, max, percentiles, mean, variance, and stddev.

*Javascript source:*

```js
var Stats = function(data) {
    var result = {};

    data.sort(function(a,b){return a-b;});
    result.count = data.length;

    // Since the data is sorted, the minimum value
    // is at the beginning of the array, the median
    // value is in the middle of the array, and the
    // maximum value is at the end of the array.
    result.min = data[0];
    result.max = data[data.length - 1];

    var ntileFunc = function(percentile){
        if (data.length == 1) return data[0];
        var ntileRank = ((percentile/100) * (data.length - 1)) + 1;
        var integralRank = Math.floor(ntileRank);
        var fractionalRank = ntileRank - integralRank;
        var lowerValue = data[integralRank-1];
        var upperValue = data[integralRank];
        return (fractionalRank * (upperValue - lowerValue)) + lowerValue;
    }

    result.percentile25 = ntileFunc(25);
    result.median = ntileFunc(50);
    result.percentile75 = ntileFunc(75);
    result.percentile99 = ntileFunc(99);

    // Compute the mean and variance using a
    // numerically stable algorithm.
    var sqsum = 0;
    result.mean = data[0];
    result.sum = result.mean * result.count;
    for (var i = 1;  i < data.length;  ++i) {
        var x = data[i];
        var delta = x - result.mean;
        var sweep = i + 1.0;
        result.mean += delta / sweep;
        sqsum += delta * delta * (i / sweep);
        result.sum += x;
    }
    result.variance = sqsum / result.count;
    result.sdev = Math.sqrt(result.variance);


    return result;
}
```

## Example Usage

Common use would be to have a map that emmits an array of numbers and the following code in your reduce phase:

```js
var dataSizes = [545,423,8928,120,0,0,1233,50];
var numEvents = [30,25,300,10,0,0,50,2];

var sizeStats = Stats(dataSizes);
var eventStats = Stats(numEvents);

/*
JSON.stringify(sizeStats, null, 2);
{
  "count": 8,
  "min": 0,
  "max": 8928,
  "percentile25": 37.5,
  "median": 271.5,
  "percentile75": 717,
  "percentile99": 8389.349999999999,
  "mean": 1412.375,
  "sum": 11299,
  "variance": 8220487.734374999,
  "sdev": 2867.1392945538937
}

JSON.stringify(eventStats, null, 2);
{
  "count": 8,
  "min": 0,
  "max": 300,
  "percentile25": 1.5,
  "median": 17.5,
  "percentile75": 35,
  "percentile99": 282.49999999999994,
  "mean": 52.125,
  "sum": 417,
  "variance": 9049.109374999998,
  "sdev": 95.12680681595486
}
*/
```

## TODO

* Truncate percentages (configurable?)
* Add argument for short circuiting sort?
