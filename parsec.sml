signature Monad = sig
    type a
end

type 'a Parser = string -> ('a * string) list

fun result a : 'a Parser = fn s => [(a, s)]

fun bind (p : 'a Parser) (f : 'a -> 'b Parser) : 'b Parser =
  fn s =>
     let
	 val procPair = fn (a, s) => (f a) s
     in
	 List.concat (List.map procPair (p s))
     end
