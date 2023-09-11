// https://adventofcode.com/2022/day/7

#nullable enable

var path = Path.Combine(Environment.CurrentDirectory, "input\\Day7.txt");
var input = File.ReadLines(path);

var someElement = input.Aggregate((Item?)null, (item, line) =>
{
    // if (item is not null)
    //     Console.WriteLine($"current item.directory: {item.directory}, size: {item.size}");

    return line.Split(" ") switch
    {
        ["$", "cd", ".."] => item?.Parent, // switching to parent directory
        ["$", "cd", var argument] => item.CreateChild(argument),
        ["$", "ls"] => item, // i don't think it makes sense to handle it
        ["dir", _] => item, // no need to handle it
        [var size, var fileName] => item!.IncrementSize(int.Parse(size)),
        _ => throw new Exception("Not expected input")
    };
});

if (someElement is null)
    throw new Exception("Something is wrong");

var root = someElement.Root();
var result = 0;
int currentlyMin = Int32.MaxValue;

Console.WriteLine(someElement);
Console.WriteLine(Traverse(root));
Console.WriteLine(currentlyMin);


int Traverse(Item item)
{
    var childrenSize = item.Children.Sum(Traverse);    
    var totalSize =  item.Size + childrenSize;

    if (item.Parent == null)
    {
        
        Console.WriteLine($"root total size is: {totalSize}");
        Console.WriteLine($"space available: {70_000_000 - totalSize}");
        Console.WriteLine($"space need for update is: {30_000_000 - (70_000_000 - totalSize)}");
    }
        
    ExchangeMin(totalSize, 5349983);

    if (totalSize < 100_000 && totalSize > 0)
        result += totalSize;

    return totalSize;
}

void ExchangeMin(int value, int limit)
{
    if (value < limit)
        return;

    currentlyMin = Math.Min(currentlyMin, value);
}


class Item
{
    public Item(string directory, Item? parent = null, int size = 0)
    {
        this.Directory = directory;
        this.Parent = parent;
        this.Size = size;
    }

    public string Directory {get; set; }

    public Item? Parent { get; set; }

    public int Size { get; set; }

    public List<Item> Children { get; set; } = new List<Item>();
}

static Item IncrementSize(this Item item, int amount)
{
    item.Size += amount;
    return item;
}

static Item CreateChild(this Item? parent, string directory, int size = 0)
{
    var child = new Item(directory, parent);

    parent?.Children.Add(child);

    return child;
}

static Item Root(this Item item)
{
    if (item.Parent == null)
        return item;

    return Root(item.Parent);
}

#nullable disable