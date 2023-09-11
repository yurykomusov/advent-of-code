// https://adventofcode.com/2022/day/1

var path = Path.Combine(Environment.CurrentDirectory, "input\\Day1.txt");

var lines = System.IO.File.ReadLines(path);

var buckets = ToBuckets(lines);

var bucketSums = buckets.Select(b => b.Select(int.Parse).Sum());

var result = bucketSums.OrderByDescending(b => b).Take(3).Sum();

Console.WriteLine(result);

public static IEnumerable<IEnumerable<string>> ToBuckets(IEnumerable<string> lines)
{
    var bucket = new List<string>();

    foreach (var line in lines)
    {
        if (!String.IsNullOrEmpty(line))
            bucket.Add(line);
        else
        {
            yield return bucket;
            bucket = new List<string>();
        }
    }
}