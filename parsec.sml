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
    type 'a M = s -> 'a * s

    fun result a : 'a M = fn s => (a, s)

    fun bind (p : 'a M) (f : 'a -> 'b M) : 'b M =
      fn s => let val procPair = fn (a, s) => (f a) s
	      in procPair (p s)
	      end
  end

signature SM =
  sig
    type s
    structure m : Monad
  end

functor StateM(sm : SM) : Monad =
  struct
    type 'a M = sm.s -> ('a * sm.s) sm.m.M

    fun result a : 'a M = fn s => sm.m.result (a, s)

    fun bind (x : 'a M) (y : 'a -> 'b M) : 'b M =
      fn s0 => let val v = x s0
		   fun f (a, s) = (y a) s
	       in sm.m.bind v f
	       end
  end

structure TestState =
  struct
    structure IntState = State(type s = int)
    val x = IntState.result "hello"
    val y = x 3
  end

structure TestStateM =
  struct
    structure IntStateM = StateM(
	struct
	  type s = int
	  structure m = State(type s = string)
	end
      )
  end
