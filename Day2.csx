// https://adventofcode.com/2022/day/2


var path = Path.Combine(Environment.CurrentDirectory, "input\\Day2.txt");
var lines = File.ReadAllLines(path);

var result = lines
    .Select(line => line.Split(' '))
    .Select(splitLine => (opponent: splitLine[0][0], you: splitLine[1][0]))
    .Sum(a => MakeRound(a.opponent, a.you));

Console.WriteLine($"Result: {result}");

return 0;

public static int MakeRound(char opponent, char expectedResult)
{
    var you = CalculateYouMove(opponent, expectedResult);
    var winBonus = CalcWinBonus(opponent, you);
    var shapeBonus = CalcShapeBonus(you);

    return winBonus + shapeBonus;
}

public static char CalculateYouMove(char opponent, char expectedResult)
{
    return (opponent, expectedResult) switch
    {
        // A x - rock
        // B Y- paper
        // C Z - scissors
        // lose
        ('A', 'X') => 'Z',
        ('B', 'X') => 'X',
        ('C', 'X') => 'Y',

        // draw
        ('A', 'Y') => 'X',
        ('B', 'Y') => 'Y',
        ('C', 'Y') => 'Z',

        // win
        ('A', 'Z') => 'Y',
        ('B', 'Z') => 'Z',
        ('C', 'Z') => 'X',
        _ => throw new Exception($"Not predictable result {opponent}, {expectedResult}")
    };
}


    // A x - rock
    // B Y- paper
    // C Z - scissors
public static int CalcWinBonus(char opponent, char you) =>
    
    (opponent, you) switch
    {
        // draw
        ('A', 'X') or 
        ('B', 'Y') or 
        ('C', 'Z') => 3,

        // you win
        ('A', 'Y') or 
        ('B', 'Z') or
        ('C', 'X') => 6,

        // you lose

        ('A', 'Z') or
        ('B', 'X') or
        ('C', 'Y') => 0,
        _ => throw new Exception("Unpredictable result")
    };


private static int CalcShapeBonus(char shape) =>
    shape switch
    {
        'X' => 1,
        'Y' => 2,
        'Z' => 3,
        _ => throw new Exception("Invalid character")
    };