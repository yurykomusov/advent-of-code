// https://adventofcode.com/2022/day/3

using System.IO;

var path = Path.Combine(Environment.CurrentDirectory, "input\\Day3.txt");
var lines = File.ReadAllLines(path);

var result = lines
    .Chunk(3)
    .Select(chunk => chunk
        .Select(c => new HashSet<char>(c.ToCharArray()))
        .Aggregate((a, b) => new HashSet<char>(a.Intersect(b)))
        .Select(GetPriority)
        .First())
    .Sum();

Console.WriteLine(result);


static int GetPriority(char c) =>
    Char.IsLower(c)
        ? (int)c - 96
        : (int)c - 38;
