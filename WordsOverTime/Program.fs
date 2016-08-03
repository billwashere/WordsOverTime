module Wot
open WordsOverTime.DataStructures
    
let words = seq {
    let toIgnore = System.IO.File.ReadAllLines("IgnoredWords.txt") |> Set.ofArray
    for word in System.IO.File.ReadAllLines("TWL06.txt") do
        assert(word |> Seq.forall IsLetter)
        if not (toIgnore.Contains word) && word.Length >= 3 then
            yield word }

let alphabet = ['A'..'Z']

let trie = new WordsOverTime.DataStructures.TrieNode()

let wordlist = new WordsOverTime.DataStructures.TrieNode()
let rand = System.Random()

for w in words do
    trie.Add(w)

let rec IsWord(s:string, t:TrieNode) =
    if t=null then false
    elif s.Length=0 then t.IsEndOfWord
    else IsWord(s.Substring(1), t.[ s.[0] ])


let getChildren(t:TrieNode) =
    alphabet |> List.choose(fun x ->
    match t.[x] with
    | null -> None
    | _ -> Some x)   

let getChildren2(t:TrieNode) =
    alphabet |> List.choose(fun x ->
    match t.[x] with
    | null -> None
    | c -> Some c)

let rec RandomWord2(t:TrieNode, min_length:int, curr:int, string_curr: string) =
   let children = getChildren(t)
   if children.IsEmpty && t.IsEndOfWord = true && curr > min_length then
    Some(string_curr)
   elif children.IsEmpty && t.IsEndOfWord = true && curr <= min_length then
    None
   elif children.IsEmpty && t.IsEndOfWord = false then
    None
   else
       if curr > min_length && t.IsEndOfWord = true && rand.NextDouble() >= 0.5 then
            Some(string_curr)
       else
          let idx = rand.Next(0,children.Length-1)
          RandomWord2(t.[children.[idx]],min_length, curr+1,string_curr+children.[idx].ToString())  

let RandomWord(t:TrieNode, min_length:int) =
    RandomWord2(t,min_length,0,"")

let rec StrNode(s:string,t:TrieNode) =
    if s.Length = 0 then
        t
    elif t.[ s.[0] ] = null then
        t
    else
        StrNode(s.Substring(1), t.[ s.[0] ])

let inline min3 one two three = 
    if one < two && one < three then one
    elif two < three then two
    else three


let wagnerFischerLazy (s: string,t: string) =
    let m = s.Length
    let n = t.Length
    let d = Array2D.create (m+1) (n+1) -1
    let rec dist =
        function
        | i, 0 -> i
        | 0, j -> j
        | i, j when d.[i,j] <> -1 -> d.[i,j]
        | i, j ->
            let dval = 
                if s.[i-1] = t.[j-1] then dist (i-1, j-1)
                else
                    min3
                        (dist (i-1, j)   + 1) // a deletion
                        (dist (i,   j-1) + 1) // an insertion
                        (dist (i-1, j-1) + 1) // a substitution
            d.[i, j] <- dval; dval 
    dist (m, n)

type PlayerType = | Human | Computer

type Player = { Name:string; Type:PlayerType }

type GameState(words:TrieNode,players : Player list) =
    let words = words
    let players = players
    let wordlist = new WordsOverTime.DataStructures.TrieNode()

[<EntryPoint>]
let main argv = 
    let srndword = RandomWord(trie,4)
    match srndword with
    | None -> System.Console.WriteLine("Can't generate word")
    | Some(rndword) ->  
        printfn "%A" rndword
        let line = System.Console.ReadLine().Trim()
        if(IsWord(line,trie) = true) then
          if(IsWord(line,wordlist) = false) then
              if wagnerFischerLazy(rndword,line) = 1 then 
                wordlist.Add(line)
                if getChildren2(StrNode(line,trie)).IsEmpty = true then //no children doesnt mean no choices
                   System.Console.WriteLine("no more choices")  
              else
                System.Console.WriteLine("Only one letter") 
          else
            System.Console.WriteLine("Can't do same file")
        else
            System.Console.WriteLine("Not a word")
    0 // return an integer exit code

