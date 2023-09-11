// https://adventofcode.com/2022/day/13

#nullable enable

using System.Diagnostics;

var lines = File.ReadAllLines(Environment.CurrentDirectory + "//input/Day13.txt");

var packets = lines.Chunk(3)
    .SelectMany(c => c[0..^1])
    .Select(Item.Parse2)
    .ToList();

var divider1 = Item.Parse2("[[2]]");
var divider2 = Item.Parse2("[[6]]");

packets.Add(divider1);
packets.Add(divider2);

packets.Sort();

var result = (packets.IndexOf(divider1) + 1) * (packets.IndexOf(divider2) + 1);
Console.WriteLine(result);

//packets.ForEach(Console.WriteLine);

// var result = pairs.ToList().Select((p, i) => (p, i)).Sum((pi) => Compare(pi.p.First, pi.p.Second) <= 0 ? (pi.i + 1) : 0);

// Console.WriteLine(result);

//Debug.Assert(Compare("[1]", "[10]") < 0);
// Debug.Assert(Compare("[1,2,3]", "[1,2]") > 0);
// //Debug.Assert(Compare("1", "[2]") < 0);
// //Debug.Assert(Compare("[1]", "2") < 0);
// //Debug.Assert(Compare("1", "[1]") == 0);
// Debug.Assert(Compare("[]", "[6]") < 0);
// Debug.Assert(Compare("[1,2]", "[1]") > 0);
// Debug.Assert(Compare("[1,[]]", "[1,[]]") == 0);
// Debug.Assert(Compare("[[[1],[2]]]", "[[[1],[2]]]") == 0);
// Debug.Assert(Compare("[[10,3,[[4],0,8]],[7,6,[]]]", "[[10,3,[[4],0,8]],[7,6,[]]]") == 0);


// Debug.Assert(Item.Parse2("") == new Item());
// Debug.Assert(Item.Parse2("1") == new Item(1));
// Debug.Assert(Item.Parse2("[]")!.Value is null);
// Debug.Assert(Item.Parse2("[1,2,10]") is not null and { InnerItems: { Count: 3 } });
// Debug.Assert(Item.Parse2("[1,[1,2]]") is { InnerItems: { Count: 2 } });
// Debug.Assert(Item.Parse2("[[1,2],[1,2]]") is { InnerItems: { Count: 2 } });
// Debug.Assert(Item.Parse2("[[]]") is { InnerItems: { Count: 1 } });


Console.WriteLine("All good");


static int Compare(string input1, string input2)
{
    var item1 = Item.Parse2(input1);
    var item2 = Item.Parse2(input2);

    return item1!.CompareTo(item2);
}



class Item : IComparable<Item>
{
    public Item()
    {

    }

    public Item(int value)
    {
        Value = value;
    }

    public Item(Item? parent)
    {
        Parent = parent;
        parent?.AppendChild(this);
    }

    public Item AppendChild(Item item)
    {
        InnerItems.Add(item);
        return item;
    }

    public int? Value { get; set; }
    public List<Item> InnerItems { get; private set; } = new();

    public Item? Parent { get; private set; }

    // [1,[2,3],4]
    // [[1,2],3,4]
    // [1]
    // [[1]]
    // [[1],[1]]
    // [1,2,3]
    public static Item? Parse2(string input)
    {
        var chars = input.ToCharArray();

        Item? current = null;
        List<char> buffer = new();

        for (var i = 0; i < input.Length; i++)
        {
            if (input[i] == '[')
                current = new Item(current);
            else if (input[i] is >= '0' and <= '9')
            {
                buffer.Add(input[i]);
            }
            else if (input[i] is ',')
            {
                if (buffer.Any())
                {
                    current!.AppendChild(new Item(int.Parse(String.Concat(buffer))));
                    buffer.Clear();
                }
            }
            else if (input[i] is ']')
            {
                if (buffer.Any())
                {
                    current!.AppendChild(new Item(int.Parse(String.Concat(buffer))));
                    buffer.Clear();
                }
                if (current!.Parent is not null)
                    current = current!.Parent;
            }
        }
        return current;
    }

    public static Item Parse(string input)
    {
        if (input.Length == 0)
            return new Item();
        if (int.TryParse(input, out var i))
            return new Item(i);

        var root = new Item();
        var stripped = input[1..^1];
        var finish = false;

        do
        {
            var sub = SubArray(stripped, out var hasMore);
            root.AppendChild(Parse(String.Concat(sub)));

            finish = !hasMore;
        }
        while (!finish);

        return root;
    }

    public static char[] SubArray(string input, out bool hasMore)
    {
        var level = 0;

        var result = input.TakeWhile((c, idx) =>
            c == ',' && level > 0 ||
            level + (c == '[' ? 1 : c == ']' ? -1 : 0) == 0)
            .ToArray();

        hasMore = result.Length < input.Length;

        return result switch
        {
            [.., ','] => result[..^1],
            _ => result
        };
    }

    public static bool IsDigit(char c) => c is (>= '0' and <= '9');

    public int CompareTo(Item? other)
    {
        if (other is null) return 1;

        if (this.InnerItems.Count == 0 && other.InnerItems.Count == 0)
        {
            return (this.Value, other.Value) switch
            {
                (null, null) => 0,
                (null, _) => -1,
                (_, null) => 1,
                (int a, int b) => a - b,
            };
        }
        else if (this.InnerItems.Count == 0 && other.InnerItems.Count > 0)
        {
            var rslt = this.CompareTo(other.InnerItems[0]);
            if (rslt == 0)
                rslt = 1 - other.InnerItems.Count;
            return rslt;
        }
        else if (this.InnerItems.Count > 0 && other.InnerItems.Count == 0)
        {
            var rslt = this.InnerItems[0].CompareTo(other);
            if (rslt == 0)
                rslt = this.InnerItems.Count - 1;
            return rslt;
        }
        else // both not empty arrays
        {
            var pairs = this.InnerItems.Zip(other.InnerItems);

            var result = pairs.Select(i => i.First.CompareTo(i.Second)).FirstOrDefault(i => i != 0);
            if (result == 0)
                result = (this.InnerItems.Count - other.InnerItems.Count);

            return result;
        }
        throw new InvalidOperationException("Unhandled comparison");
    }
}