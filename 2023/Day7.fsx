open System.IO

type Card =
    | Number of int
    | Character of char

    member this.rank =
        match this with
        | Number n -> n
        | Character c ->
            match c with
            | 'T' -> 10
            | 'J' -> 1
            | 'Q' -> 12
            | 'K' -> 13
            | 'A' -> 14
            | c -> failwith <| sprintf "invalid character: %c" c

let toShortString (cards : Card array): string =
    let characters = 
        cards
        |> Array.map (
            fun c -> 
                match c with
                | Number 10 -> "T"
                | Number n -> n.ToString()
                | Character c -> c.ToString()
        )
    String.concat "" characters

let (|Joker|_|) (card : Card) = 
    match card with
    | Character 'J' -> Some()
    | _ -> None

let parse (symbol: char) =
    match symbol with
    | s when symbol > '1' && symbol <= '9' -> Number(s.ToString() |> int)
    | 'T' -> Number 10
    | 'J' -> Character 'J'
    | 'Q' -> Character 'Q'
    | 'K' -> Character 'K'
    | 'A' -> Character 'A'
    | s -> failwith <| sprintf "invalid character: %c" s

let parseSet (line: string) =
    let characters = line.ToCharArray()

    characters |> Array.map parse

let isNOfKind (n: int) (cards: Card array) =
    let cardsCounts = cards |> Array.countBy (fun c -> c)
    cardsCounts |> Array.exists (fun (_, count) -> count = n)

let isTwoPairs (cards: Card array) =
    cards
    |> Array.countBy (fun c -> c)
    |> Array.filter (fun (_, count) -> count >= 2)
    |> Array.length = 2

let isFullHouse (cards: Card array) =
    let has3 = isNOfKind 3 cards
    let has2pairs = isTwoPairs cards
    has3 && has2pairs

let isTwoOfKind = isNOfKind 2
let isThreeOfKind = isNOfKind 3
let isFourOfKind = isNOfKind 4
let isFiveOfKind = isNOfKind 5

type Combination =
    | FiveOfKind
    | FourOfKind
    | FullHouse
    | ThreeOfKind
    | TwoPair
    | OnePair
    | HighCard
    
    member this.rank = 
        match this with
        | FiveOfKind -> 7
        | FourOfKind -> 6
        | FullHouse -> 5
        | ThreeOfKind -> 4
        | TwoPair -> 3
        | OnePair -> 2
        | HighCard -> 1 

let handleJoker (combination: Combination)  = 
    match combination with
    | FiveOfKind -> FiveOfKind
    | FourOfKind -> FiveOfKind
    | FullHouse -> FourOfKind
    | ThreeOfKind -> FourOfKind
    | TwoPair -> FullHouse
    | OnePair -> ThreeOfKind
    | HighCard -> OnePair

let findCombination (cards: Card array) = 
    let original = 
        match cards with
        | s when isFiveOfKind s -> FiveOfKind
        | s when isFourOfKind s -> FourOfKind
        | s when isFullHouse s -> FullHouse
        | s when isThreeOfKind s -> ThreeOfKind
        | s when isTwoPairs s -> TwoPair
        | s when isTwoOfKind s -> OnePair
        | _ -> HighCard

    let isThreeJokers = 
        (match original with | ThreeOfKind -> true | _ -> false) && 
        (cards |> Array.filter (fun i -> match i with | Joker -> true | _ -> false) |> Array.length = 3)

    let isTwoJokers = 
        (match original with | OnePair -> true | _ -> false) && 
        (cards |> Array.filter (fun i -> match i with | Joker -> true | _ -> false) |> Array.length = 2)

    if isThreeJokers then // corner case
        printfn "CORNER CASE: %s" (toShortString cards)
        FourOfKind
    elif isTwoJokers then
        printfn "ANOTHER CORNER CASE: %s" (toShortString cards)
        ThreeOfKind
    else
        cards 
            |> Array.filter (fun c -> 
                match c with
                | Joker -> true
                | _ -> false)
            |> Array.fold (fun state _ -> handleJoker state) original

let calcHandPower (cards: Card array) = 
    let combination = findCombination cards

    let customRank (card: Card) = 
        match cards with
        // | [| Joker; Joker; Joker; Joker; Joker |] -> 11
        | _ -> card.rank 


    let powerOfHighestCards = 
        cards 
        |> Array.rev 
        |> Array.indexed 
        |> Array.sumBy (fun (index, item) -> (float (customRank item)) * (15.  ** index))


    float combination.rank * (15.** 6) + powerOfHighestCards

    
async {
    let! lines = File.ReadAllLinesAsync("Day7.txt") |> Async.AwaitTask

    lines
    |> Array.map _.Split(' ')
    |> Array.map (fun arr -> (arr[0], arr[1]))
    |> Array.map (fun (cardString, bidString) -> (parseSet cardString, int bidString))
    |> Array.sortBy (fun (cards, _) -> (calcHandPower cards))
    // |> Array.iter (fun (hand, bid) -> printfn "%s %A" <| (toShortString hand) <| (findCombination hand))
    
    |> Array.indexed
    |> Array.sumBy (fun (index, (_, bid)) -> bid * (index + 1))
    |> printfn "result: %A" 

} |> Async.RunSynchronously
