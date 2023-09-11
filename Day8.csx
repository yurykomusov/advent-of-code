// https://adventofcode.com/2022/day/8

var path = Path.Combine(Environment.CurrentDirectory, "input\\Day8.txt");
var input = File.ReadLines(path);

var grid = BuildArray(input);

static bool IsVisible(int[,] grid, int x, int y)
{
    if (x == 0 || y == 0)
        return true;

    if (x == grid.GetLength(0) - 1 || y == grid.GetLength(1) - 1)
        return true;

    var left = IsVisibleLeft(grid, x, y);
    var right = IsVisibleRight(grid, x, y);
    var top = IsVisibleTop(grid, x, y);
    var bottom = IsVisibleBottom(grid, x, y);

    var result =
        IsVisibleLeft(grid, x, y) ||
        IsVisibleRight(grid, x, y) ||
        IsVisibleTop(grid, x, y) ||
        IsVisibleBottom(grid, x, y);

    if (!result)
        Console.WriteLine($"{x} {y} not visible");

    return result;
}

static bool IsVisibleLeft(int[,] grid, int x, int y)
{
    if (x == 0) return true;

    return Enumerable.Range(0, x).All(xx => grid[xx, y] < grid[x, y]);
}

static bool IsVisibleRight(int[,] grid, int x, int y)
{
    if (x == grid.GetLength(0) - 1) return true;

    return Enumerable.Range(x + 1, grid.GetLength(0) - (x + 1)).All(xx => grid[xx, y] < grid[x, y]);
}

static bool IsVisibleTop(int[,] grid, int x, int y)
{
    if (y == 0) return true;

    return Enumerable.Range(0, y).All(yy => grid[x, yy] < grid[x, y]);
}

static bool IsVisibleBottom(int[,] grid, int x, int y)
{
    if (y == grid.GetLength(1) - 1) return true;

    return Enumerable.Range(y + 1, grid.GetLength(1) - (y + 1)).All(yy => grid[x, yy] < grid[x, y]);
}

static int GetScenicIndex(int[,] grid, int x, int y)
{
    var left = x == 0 
        ? 0 
        : (Enumerable.Range(0, x)
            .Reverse()
            .TakeWhile(xx => grid[xx, y] < grid[x, y])
            .Count(i => i != 0) + 1);

    var right = x == grid.GetLength(0) - 1
        ? 0
        : (Enumerable.Range(x + 1, grid.GetLength(0) - (x + 1))
            .TakeWhile(xx => grid[xx, y] < grid[x, y])
            .Count(i => i != grid.GetLength(0) - 1) + 1);

    var top = y == 0 
        ? 0 
        : (Enumerable.Range(0, y)
            .Reverse()
            .TakeWhile(yy => grid[x, yy] < grid[x, y])
            .Count(i => i != 0) + 1);

    var bottom = y == grid.GetLength(1) - 1
        ? 0
        : (Enumerable.Range(y + 1, grid.GetLength(1) - (y + 1))
            .TakeWhile(yy => grid[x, yy] < grid[x, y])
            .Count(i => i != grid.GetLength(1) - 1) + 1);

    var rst =  left * right * top * bottom;
    return rst;
}

var result = (
    from i in Enumerable.Range(0, grid.GetLength(0))
    from j in Enumerable.Range(0, grid.GetLength(1))
    select GetScenicIndex(grid, i, j))
    .Max();

Console.WriteLine(result);

int[,] BuildArray(IEnumerable<string> lines)
{
    var array = lines
        .Select(row => row
            .ToCharArray()
            .Select(c => c.ToString())
            .Select(int.Parse)
            .ToArray())
        .ToArray();

    var result = new int[array.Length, array.Length];

    for (var i = 0; i < array.Length; i++)
        for (var j = 0; j < array.Length; j++)
        {
            result[j, i] = array[i][j];
        }

    return result;
}


static int IndexSafe(this int[,] grid, int x, int y)
{
    try
    {
        return grid[x, y];
    }
    catch (IndexOutOfRangeException)
    {
        return -1;
    }
}




