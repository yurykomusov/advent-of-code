// https://adventofcode.com/2022/day/6

var path2 = Path.Combine(Environment.CurrentDirectory, "input\\Day6.txt");
var input = File.ReadAllText(path2);
//var input = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg";

int size = 14;
int pointer = 0;

Queue<char> queue = new(size);

for (var i = 0; i < size; i++)
    queue.Enqueue(input[pointer + i]);

while (pointer < (input.Length + size))
{
    var found = CheckUnique(queue);

    if (found)
    {
        Console.WriteLine("result " + (pointer + size));
        return;
    }
    else
    {
        queue.Dequeue();
        queue.Enqueue(input[pointer++ + size]);
    }
}

bool CheckUnique(IEnumerable<char> items)
{
    var hashset = new HashSet<char>();
    
    items
        .ToList()
        .ForEach(i => hashset.Add(i));

    return hashset.Count() == size;
}