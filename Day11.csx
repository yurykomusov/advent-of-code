// https://adventofcode.com/2022/day/11

#nullable enable

using System.Diagnostics;
using System.Text.RegularExpressions;
var lines = File.ReadAllLines(Environment.CurrentDirectory + "//input/Day11.txt");

var regexNumbers = new Regex(@"\d{1,3}");

var items = lines
    .Chunk(7)
    .Select(c => new Item(
        id: int.Parse(regexNumbers.Match(c[0]).Value),
        operation: c[2].Trim(' ').Split(" ", StringSplitOptions.RemoveEmptyEntries) switch //  Operation: new = old * 7
        {
            [_, _, _, _, "*", "old"] => exp => new Expression(exp, 2, '^'),
            //[_, _, _, _, "*", "old"] => exp => new Expression(exp,  exp, '*'),
            [_, _, _, _, "+", var parameter] => exp => exp.operation == '+'
                ? exp with { b = exp.b + long.Parse(parameter) }
                : new Expression(exp, long.Parse(parameter), '+'),
            [_, _, _, _, "*", var parameter] => exp => exp.operation == '*'
                ? exp with { b = exp.b + long.Parse(parameter) }
                : new Expression(exp, long.Parse(parameter), '*'),
            _ => throw new Exception("Unidentified expression")
        },
        test: int.Parse(regexNumbers.Match(c[3]).Value),
        targetTrue: int.Parse(regexNumbers.Match(c[4]).Value),
        targetFalse: int.Parse(regexNumbers.Match(c[5]).Value),
        items:
            new(regexNumbers.Matches(c[1]).Select(i => new Expression(null, 0, null, long.Parse(i.Value))).ToArray())
        ))
    .ToArray();


Console.WriteLine("Pre-cycle");
var counts = Enumerable.Repeat(0, items.Length).ToArray();

foreach (var item in items)
    Print(item);

var sw = new Stopwatch();

for (var i = 0; i < 10_000; i++)
{
    if ((i + 1) % 100 == 0)
    {
        sw.Restart();
    }

    foreach (var item in items)
    {
        Handle(item, items, i);
    }

    if ((i + 1) % 100 == 0)
    {
        Console.WriteLine("Cycle " + (i + 1) + $", Elapsed: {sw.Elapsed}");
        Console.WriteLine($"counts: {String.Join(',', counts)}");

        // foreach (var item in items)
        //     Print(item);
    }

}

Console.WriteLine($"counts: {String.Join(',', counts.OrderByDescending(i => i).Take(2).Aggregate((decimal)1, (a, b) => (decimal)a * (decimal)b))}");
//Console.WriteLine($"counts: {String.Join(',', counts)}");

Console.ReadKey();

void Handle(Item item, Item[] others, int index)
{
    while (item.items.Any())
    {
        var i = item.items.Dequeue();

        var expression = item.operation(i);

        if (expression is null)
            throw new Exception("expression is null");

        //var val = Evaluate(expression);


        var testValue = Test(expression, item.test);
        var target = testValue == 0
            ? others[item.targetTrue]
            : others[item.targetFalse];

        //Console.WriteLine($"\t{val} % {item.test} = {testValue}. moving to bucket {target.id}");

        counts[item.id]++;

        // if (index % 100 == 0)
        //     expression = new Expression(null, null, null, Evaluate(expression));

        target.items.Enqueue(expression);
    }
}

void Print(Item item)
{
    Console.WriteLine("bucket " + item.id + ": " + String.Join(',', item.items.Select(Evaluate)));
}

long Evaluate(Expression? e)
{
    return e switch
    {
        { value: > 0 } => e.value,
        { operation: '^' } => Evaluate(e.a) * Evaluate(e.a),
        { operation: '*' } => Evaluate(e.a) * e.b,
        { operation: '+' } => Evaluate(e.a) + e.b,
        _ => throw new Exception("Wut?")
    };
}

long Test(Expression? exp, int parameter)
{
    var result = exp switch
    {
        { value: > 0 } => exp.value,
        { operation: '^' } => Square(Test(exp.a, parameter)) % parameter,
        { operation: '*' } => (Test(exp.a, parameter) * exp.b) % parameter,
        { operation: '+' } => ((Test(exp.a, parameter) % parameter) + (exp.b % parameter)) % parameter,
        _ => throw new Exception("Unknown operation")
    };

    return result;
}

static long Square(long result) => result * result;

record Item(
    int id,
    Func<Expression, Expression> operation,
    int test,
    int targetTrue,
    int targetFalse,
    Queue<Expression> items
);

record Expression(
    Expression? a,
    long b,
    char? operation,
    long value = 0
);