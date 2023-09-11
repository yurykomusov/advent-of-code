// https://adventofcode.com/2022/day/10

var input = File.ReadAllLines(Environment.CurrentDirectory + "\\input\\Day10.txt");
var register = 1;
var tmp = 0;

int Store(int value)
{
    var t = register;
    register += value;   
    return t;
}

int Reset()
{
    return register;
}

input
    .Select(line => 
        line.Split(" ") switch
        {   
            ["addx", var value] => new [] { Reset(), Store(int.Parse(value)) },
            ["noop"] => new [] { Reset() },
            _ => throw new Exception("Can't read the value")
        })
    .SelectMany(s => s)
    .Select((value, index) => (value, index: index))
    .Select(item => Math.Abs((item.index % 40) - item.value) <= 1 ? "#" : ".")
    .Chunk(40)
    .ToList()
    .ForEach(c => Console.WriteLine(c.Aggregate((a, b) => a + b)));