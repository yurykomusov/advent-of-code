// https://adventofcode.com/2022/day/4

using System.Diagnostics;
using System.IO;

var path = Path.Combine(Environment.CurrentDirectory, "input\\Day4.txt");
var lines = File.ReadAllLines(path);


var rangeCouples = lines
    .Select(line => line
        .Split(",")
        .Select(rangeStr => 
            rangeStr
                .Split("-")
                .Select(int.Parse)
                .ToArray())
        .Select(range => (range[0], range[1]))
        .ToArray())
    .Select(ranges => (ranges[0], ranges[1]));


// var result = rangeCouples.Count(couple => 
//     IsSub(couple.Item1, couple.Item2) ||
//     IsSub(couple.Item2, couple.Item1)); 


var result = rangeCouples.Count(couple => IsOverlap(couple.Item1, couple.Item2)); 

Console.WriteLine(result);

// Debug.Assert(IsOverlap((1,3), (4,5)) == false);
// Debug.Assert(IsOverlap((4,5), (1,3)) == false);
// Debug.Assert(IsOverlap((1,3), (2,4)) == true);
// Debug.Assert(IsOverlap((1,1), (3,3)) == false);

//Console.WriteLine(result);

//rangeCouples.Count(couple => IsOverlap(couple));

static bool IsSub((int, int) a, (int, int) b)
{
    return (a.Item1 <= b.Item1) && (a.Item2 >= b.Item2); 
}

static bool IsOverlap((int, int) a, (int, int) b)
{
    Console.WriteLine($"a={a},b={b}");
    // 1-3 4-5 false
    // 4-5 1-3 false

    // 1-3 2-4 true
    // 1-1 3-3 false

    if (a.Item1 <= b.Item1)
        return a.Item2 >= b.Item1;
    else
        return b.Item2 >= a.Item1;
}