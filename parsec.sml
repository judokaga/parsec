signature Monad =
  sig
    type 'a M
    val result : 'a -> 'a M
    val bind : 'a M -> ('a -> 'b M) -> 'b M
  end

signature Monad0Plus =
  sig
    include Monad
    val zero : 'a M
    val ++ : ('a M * 'a M) -> 'a M
  end

functor MONAD (m : Monad) =
  struct
    open m
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

functor State(type s) : Monad =
  struct
    type ('a, 's) State = 's -> 'a * 's
    type 'a M = ('a, s) State

    fun result a : 'a M = fn s => (a, s)

    fun bind (p : 'a M) (f : 'a -> 'b M) : 'b M =
      fn s => let val procPair = fn (a, s) => (f a) s
	      in procPair (p s)
	      end
  end

structure TestState =
  struct
    structure IntState = State(type s = int)
    val x = IntState.result "hello"
    val y = x 3
  end
