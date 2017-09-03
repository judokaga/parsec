signature Monad =
  sig
    type 'a M
    val result : 'a -> 'a M
    val bind : 'a M -> ('a -> 'b M) -> 'b M
  end

functor MONAD(a: Monad) =
  struct
  end

structure Parser : Monad =
  struct
    type 'a M = string -> ('a * string) list

    fun result a : 'a M = fn s => [(a, s)]

    fun bind (p : 'a M) (f : 'a -> 'b M) : 'b M =
      fn s => let val procPair = fn (a, s) => (f a) s
	      in List.concat (List.map procPair (p s))
	      end
  end

type 'a Parser = string -> ('a * string) list

fun result a : 'a Parser = fn s => [(a, s)]

fun bind (p : 'a Parser) (f : 'a -> 'b Parser) : 'b Parser =
  fn s => let val procPair = fn (a, s) => (f a) s
	  in List.concat (List.map procPair (p s))
	  end
