// https://adventofcode.com/2022/day/12

using System.Collections.Generic;
using System.Text;

var lines = File.ReadAllLines(Environment.CurrentDirectory + "//input/Day12.txt");

var board = lines.Select(line => line.ToCharArray()).ToArray();
var yMax = board.Length;
var xMax = board[0].Length;

var start = IndexOf(board, 'E');
var end = IndexOf(board, 'S');

PriorityQueue<(int x, int y), int> result = new();

Dictionary<(int x, int y), int> queues = new();

Check(start.x, start.y);
Print(board, queues);
SetupQueue();

Console.WriteLine($"Result ({queues.Count}): " + result.Peek() + " " + queues[result.Peek()]);

void Check(int x, int y, int pathSize = 0)
{
    //Print(board, queues);
    //Console.WriteLine($"checking ({x} {y}); current value: {board[y][x]}");

    var left = (x: x - 1, y);
    var right = (x: x + 1, y);
    var top = (x, y: y - 1);
    var bottom = (x, y: y + 1);

    var hasValue = queues.TryGetValue((x, y), out var existing);

    if (hasValue && existing <= pathSize)
        return;

    var min = Math.Min(pathSize, existing);
    var newVal = hasValue ? min : pathSize;
    queues[(x, y)] = newVal;

    //Console.WriteLine($"setting up ({x} {y}) <= {newVal}");

    if (IsEligiblePosition(left) && IsEligibleSymbol(board[y][x], board[left.y][left.x]))
        Check(left.x, left.y, pathSize + 1);

    if (IsEligiblePosition(right) && IsEligibleSymbol(board[y][x], board[right.y][right.x]))
        Check(right.x, right.y, pathSize + 1);

    if (IsEligiblePosition(top) && IsEligibleSymbol(board[y][x], board[top.y][top.x]))
        Check(top.x, top.y, pathSize + 1);

    if (IsEligiblePosition(bottom) && IsEligibleSymbol(board[y][x], board[bottom.y][bottom.x]))
        Check(bottom.x, bottom.y, pathSize + 1);
}

bool IsEligibleSymbol(char source, char target)
{
    if (source == 'E') return true;
    if (target == 'S') return true;

    if (source >= target)
        return source - target <= 1;

    return true;
}

(int x, int y) IndexOf(char[][] array, char charToFind)
{
    var x = 0;
    var y = 0;

    foreach (var line in array)
    {
        x = 0;
        foreach (var symbol in line)
        {
            if (charToFind == symbol)
                return (x, y);

            x++;
        }
        y++;
    }
    return (-1, -1);
}

bool IsEligiblePosition((int x, int y) point)
{
    (var x, var y) = point;

    var isEligible = x >= 0 &&
        y >= 0 &&
        x < xMax &&
        y < yMax;

    // if (!isEligible)
    //     Console.WriteLine($"Not Eligible position: ({x}, {y})");

    return isEligible;
}

void SetupQueue()
{
    var x = 0;
    var y = 0;

    foreach (var line in board)
    {
        x = 0;
        foreach (var symbol in line)
        {
            if (queues.TryGetValue((x, y), out var val))
                if (symbol == 'a')
                    result.Enqueue((x, y), val);

            x++;
        }
        y++;
    }

}

void Print(char[][] board, Dictionary<(int x, int y), int> queue)
{
    var y = 0;
    var x = 0;

    foreach (var line in board)
    {
        Console.WriteLine($"{y,2}-> " + line.Select((item, idx) => queue.TryGetValue((idx, y), out var val) ? '*' : item).Aggregate("", (a, b) => a + b));
        y++;
    }
}