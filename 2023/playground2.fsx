open System
open System.Collections

let qMaskStr = "?.????????#???"
let str2 = "#.##.....#####"

let qMask = BitArray qMaskStr.Length
let brokenMask = BitArray qMaskStr.Length

let testArr = BitArray qMaskStr.Length

for i in 0..qMaskStr.Length - 1 do
    testArr.Set(i, str2[i] = '#')

for i in 0..qMaskStr.Length - 1 do
    qMask.Set(i, qMaskStr[i] = '?')

for i in 0..qMaskStr.Length - 1 do
    brokenMask.Set(i, qMaskStr[i] = '#')


let print (arr : BitArray) = 
    for i in 0..arr.Length - 1 do
        if arr.Item(i) then
            printf "1"
        else 
            printf "0"
    printfn ""

let test (questionMask : BitArray) (brokenMask : BitArray) (testArr : BitArray) = 
    print questionMask
    print brokenMask
    print testArr
    testArr.And(brokenMask)
test qMask brokenMask testArr |> print