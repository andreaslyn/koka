

type nat {
  Zero
  Succ(: nat)
}


fun natequal(m : nat, n : nat) : bool {
  match(m) {
    Zero ->
      match(n) {
        Zero -> True
        Succ(_) -> False
      }
    Succ(m1) ->
      match(n) {
        Zero -> False
        Succ(n1) -> natequal(m1, n1)
      }
  }
}


val one : nat = Succ(Zero)
val two : nat = Succ(one)
val three : nat = Succ(two)
val four : nat = Succ(three)
val five : nat = Succ(four)
val six : nat = Succ(five)
val seven : nat = Succ(six)
val eight : nat = Succ(seven)
val nine : nat = Succ(eight)
val ten : nat = Succ(nine)


fun plus(m : nat, n : nat) : nat {
  match(n) {
    Zero -> m
    Succ(n1) -> plus(Succ(m), n1)
  }
}


fun mul(m : nat, n : nat) : nat {
  match(n) {
    Zero -> Zero
    Succ(n1) -> plus(mul(m, n1), m)
  }
}


fun makeNatlist(n : nat, acc : list<nat>) : list<nat> {
  match(n) {
    Zero -> acc
    Succ(n1) -> makeNatlist(n1, Cons(Succ(n1), acc))
  }
}


val natlist : list<nat> = makeNatlist(mul(ten, five), [])


val triplelist : list<(nat, nat, nat)> =
  flatmap(natlist, fn(a) {
    flatmap(natlist, fn(b) {
      flatmap(natlist, fn(c) {
        [(a, b, c)]
      })
    })
  })


fun isPythagorean(ns : (nat, nat, nat)) : bool {
  match(ns) {
    (a, b, c) -> natequal( plus(mul(a, a), mul(b, b)), mul(c, c) )
  }
}


fun main() : <console> () {
  println(length(filter(triplelist, isPythagorean)))
}
