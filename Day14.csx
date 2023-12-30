#nullable enable

using System.Data;
using System.Diagnostics.Metrics;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text.RegularExpressions;
using System.Threading;

var lines = File.ReadAllLines(Environment.CurrentDirectory + "//input/Day14.txt");

var pathes = lines.Select(line => 
    Regex.Matches(line, "\\d{1,3}")
        .Select(s => s.Value)
        .Chunk(2)
        .Select(c => (x: int.Parse(c[0]), y: int.Parse(c[1])))
);

const int maxSize = 700;

var map = new Terrain[maxSize, maxSize];

foreach (var path in pathes)
    ApplyPath(map, path);

var floorLevel = pathes.Max(p => p.Max(pp => pp.y)) + 2;

ApplyPath(map, new[] { (0, floorLevel), (maxSize - 1, floorLevel)});


(int, int) source = (500, 0);
(int, int) currentPosition = (source.Item1, source.Item2);

int i = 0;
int restCount = 0;

bool isSourceReached = false;
bool isAbyssReached = false;

do
{
    var resolution = CalculatePhysics(map[currentPosition.Item1, currentPosition.Item2 + 1], map[currentPosition.Item1 - 1, currentPosition.Item2 + 1], map[currentPosition.Item1 + 1, currentPosition.Item2 + 1]);

    var nextPosition = resolution switch
    {
        Resolution.Rest => source,
        Resolution.FurtherDown => (currentPosition.Item1, currentPosition.Item2 + 1),
        Resolution.SlideLeft => (currentPosition.Item1 - 1, currentPosition.Item2 + 1),
        Resolution.SlideRight => (currentPosition.Item1 + 1, currentPosition.Item2 + 1),
        _ => throw new Exception("should not be happening")
    };

    if (resolution is Resolution.Rest)
    {
        map[currentPosition.Item1, currentPosition.Item2] = Terrain.Sand;
        restCount++;
    }
        
    
    //Console.WriteLine($"Frame #{i++}. Rest Count: {restCount} Resolution: {resolution}, NextPosition: {nextPosition}, Active: ({currentPosition.Item1}, {currentPosition.Item2})");

    if (i % 10_000 == 0)
    {
        Console.Clear();
        PrintMap(map, currentPosition);
    }

    isAbyssReached = nextPosition.Item2 == floorLevel + 1;
    isSourceReached = resolution is Resolution.Rest && nextPosition == currentPosition;
    currentPosition = nextPosition;
}
while (!isAbyssReached || !isSourceReached);

Console.WriteLine($"Rest count: {restCount}. Abyss reached: {isAbyssReached}. Source reached: {isSourceReached}");
Console.ReadKey();


static void ApplyPath(Terrain[,] map, IEnumerable<(int x, int y)> path)
{
    var pathArray = path.ToArray();


    for (var i = 0; i < pathArray.Length - 1; i++)
        ApplyLine(map, pathArray[i], pathArray[i + 1]);

}

static void ApplyLine(Terrain[,] map, (int x, int y) start, (int x, int y) end)
{
    if (start.x == end.x) // vertical line
    {
        var startIndex = Math.Min(start.y, end.y);
        var endIndex = Math.Max(start.y, end.y);

        for (var i = startIndex; i <= endIndex; i++)
            map[start.x, i] = Terrain.Rock;

        return;
    }        
    else if (start.y == end.y) // horizontal line
    {
        var startIndex = Math.Min(start.x, end.x);
        var endIndex = Math.Max(start.x, end.x);

        for (var i = startIndex; i <= endIndex; i++)
            map[i , start.y] = Terrain.Rock;

        return;
    }

    throw new Exception("Are yoy kiddin' me?");
}

static void PrintMap(Terrain[,] map, (int x, int y) current)
{
    for (int j = 0; j < 200; j++)
    {
        for (int i = 350; i < 650; i++)
            Console.Write((map[i, j], current) switch
            {
                (_, (int x, int y) c) when c.x == i && c.y == j => '+', // show active spot
                (Terrain.Air, _) => '.',
                (Terrain.Rock, _) => '#',
                (Terrain.Sand, _) => 'o',
                _ => ' '
            });
        Console.WriteLine();
    }
}

enum Resolution
{
    Rest,
    FurtherDown,
    SlideLeft,
    SlideRight
}

enum Terrain
{
    Air,
    Rock,
    Sand,
}

static Resolution CalculatePhysics(Terrain down, Terrain downLeft, Terrain downRight)
{
    return (down, downLeft, downRight) switch
    {
        (Terrain.Air, _, _) => Resolution.FurtherDown,
        (Terrain.Rock or Terrain.Sand, Terrain.Air, _) => Resolution.SlideLeft,
        (Terrain.Rock or Terrain.Sand, _, Terrain.Air) => Resolution.SlideRight,
        (Terrain.Rock or Terrain.Sand, _, _) => Resolution.Rest,
        _ => throw new Exception("Go to sleep, mate!")
    };
}