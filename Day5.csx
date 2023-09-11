// https://adventofcode.com/2022/day/5

using System.Diagnostics;
using System.IO;
using System.Text.RegularExpressions;

/*

                    [Q]     [P] [P]
                [G] [V] [S] [Z] [F]
            [W] [V] [F] [Z] [W] [Q]
        [V] [T] [N] [J] [W] [B] [W]
    [Z] [L] [V] [B] [C] [R] [N] [M]
[C] [W] [R] [H] [H] [P] [T] [M] [B]
[Q] [Q] [M] [Z] [Z] [N] [G] [G] [J]
[B] [R] [B] [C] [D] [H] [D] [C] [N]
 1   2   3   4   5   6   7   8   9 


*/

var path = Path.Combine(Environment.CurrentDirectory, "input\\Day5.txt");
var lines = File.ReadAllLines(path);
var stacks = new Stack<char>[9];

stacks[0] = new Stack<char>("BQC".ToCharArray());
stacks[1] = new("RQWZ".ToCharArray());
stacks[2] = new("BMRLV".ToCharArray());
stacks[3] = new("CZHVTW".ToCharArray());
stacks[4] = new("DZHBNVG".ToCharArray());
stacks[5] = new("HNPCJFVQ".ToCharArray());
stacks[6] = new("DGTRWZS".ToCharArray());
stacks[7] = new("CGMNBWZP".ToCharArray());
stacks[8] = new("NJBMWQFP".ToCharArray());

lines
    .Select(ParseMove)
    .Select(move =>
    {
        Console.WriteLine(move);

        var tmpStack = new Stack<char>();

        for (var i = 0; i < move.amount; i++)
            tmpStack.Push(stacks[move.from - 1].Pop());

        foreach (var item in tmpStack)
            stacks[move.to - 1].Push(item);

        return move;
    })
    .ToArray();

var result = String.Join(null, stacks.Select(s => s.Peek()));

Console.WriteLine(result);

Move ParseMove(string line)
{
    var regex = new Regex(@"\d{1,3}");

    var matches = regex.Matches(line);

    return new Move(
        int.Parse(matches[0].Value), 
        int.Parse(matches[1].Value), 
        int.Parse(matches[2].Value));
}

record Move(int amount, int from, int to);

