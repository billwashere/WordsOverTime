namespace WordsOverTime

  module DataStructures = begin
    val IsLetter : c:char -> bool
    [<AllowNullLiteralAttribute ()>]
    type TrieNode =
      class
        new : unit -> TrieNode
        new : TrieNode option -> TrieNode
        member Add : word:string -> unit
        member private Add : word:string * i:int -> unit
        member IsEndOfWord : bool
        member Item : c:char -> TrieNode with get
        member private Item : c:char -> TrieNode with set
      end
  end

