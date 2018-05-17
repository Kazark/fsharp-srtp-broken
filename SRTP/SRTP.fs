module SRTP

type Either<'l,'r> = Left of 'l | Right of 'r
type Maybe<'a> = Either<unit,'a>

let k x _ = x
let inline flip f x y = f y x
let inline addfst x y = (x, y)

module Either =
    let match_ fl fr : Either<'l,'r> -> 'a =
        function Right x -> fr x | Left x -> fl x

    let rmap f : Either<'l,'a> -> Either<'l,'b> =
        function Right x -> Right (f x) | Left e -> Left e

    let rbind f : Either<'l,'a> -> Either<'l,'b> =
        function Right x -> f x | Left e -> Left e

module Ls =
    let apply (fs : list<'a -> 'b>) (ls : list<'a>) : list<'b> =
        List.collect (flip List.map ls) fs

    let inline cons (x : 'a) (xs : list<'a>) : list<'a> = List.Cons (x, xs)

type HKTPred() =
    static member inline SameTypeCtr (x : Either< ^a,^b>, y : Either< ^c, ^d>) : unit = ()
    static member inline SameTypeCtr (x : Result< ^a,^b>, y : Result< ^c, ^d>) : unit = ()
    static member inline SameTypeCtr (x : ^a * ^b, y : ^c * ^d) : unit = ()
    static member inline SameTypeCtr (x : ^a * ^b * ^c, y : ^d * ^e * ^f) : unit = ()

    static member inline IsParameterizedOver (x : Either< ^a, ^b>, y : ^b) : unit = ()
    static member inline IsParameterizedOver (x : Result< ^a, ^b>, y : ^b) : unit = ()
    static member inline IsParameterizedOver (x : ^a * ^b, y : ^b) : unit = ()
    static member inline IsParameterizedOver (x : ^a * ^b * ^c, y : ^c) : unit = ()

    static member inline IsParameterizedOverA (x : Either< ^a, ^b>, y : ^a) : unit = ()
    static member inline IsParameterizedOverA (x : Result< ^a, ^b>, y : ^a) : unit = ()
    static member inline IsParameterizedOverA (x : ^a * ^b, y : ^a) : unit = ()
    static member inline IsParameterizedOverA (x : ^a * ^b * ^c, y : ^a) : unit = ()

    static member inline IsParameterizedOverB (x : Either< ^a, ^b>, y : ^b) : unit = ()
    static member inline IsParameterizedOverB (x : Result< ^a, ^b>, y : ^b) : unit = ()
    static member inline IsParameterizedOverB (x : ^a * ^b, y : ^b) : unit = ()
    static member inline IsParameterizedOverB (x : ^a * ^b * ^c, y : ^b) : unit = ()

type Multifunctor() =
    static member inline mapB (f : ^b -> ^b0, (a : ^a, b : ^b)) : ^a * ^b0 = (a, f b)
    static member inline mapB (f : ^b -> ^b0, x : Either< ^a,^b>) : Either< ^a,^b0> = Either.rmap f x
    static member inline mapB (f : ^b -> ^b0, (a : ^a, b : ^b, c : ^c)) : ^a * ^b0 * ^c = (a, f b, c)

let inline mapB_< ^hkt, ^mf, ^a0, ^b0, ^a, ^b when (^mf  or ^a) : (static member mapB : (^a0 -> ^b0) * ^a -> ^b)
                  and  (^hkt or ^a) : (static member SameTypeCtr          : ^a * ^b  -> unit)
                  and  (^hkt or ^a) : (static member IsParameterizedOverB : ^a * ^a0 -> unit)
                  and  (^hkt or ^b) : (static member IsParameterizedOverB : ^b * ^b0 -> unit)>
          (f : ^a0 -> ^b0, x : ^a) : ^b =
    ((^mf or ^a) : (static member mapB : (^a0 -> ^b0) * ^a -> ^b) (f, x))

let inline mapB (f : ^a0 -> ^b0) (x : ^a) : ^b
  when (Multifunctor or ^a) : (static member mapB                 : (^a0 -> ^b0) * ^a -> ^b) =
    mapB_<HKTPred,Multifunctor,^a0,^b0,^a,^b> (f, x)

type Multiapplicative() =
    static member inline wrapB (_ : Either< ^a,^b>, x0 : ^b) : Either< ^a,^b> = Right x0
    static member inline wrapB (_ : Result< ^a,^b>, x0 : ^b) : Result< ^a,^b> = Error x0

let inline wrapB_< ^mf, ^a, ^b when (^mf or ^b) : (static member wrapB : ^b * ^a -> ^b)> (instance : ^b, x : ^a) : ^b =
    ((^mf or ^b) : (static member wrapB : ^b * ^a -> ^b) (instance, x))
let inline wrapB (x : ^a) : ^b when (Multiapplicative or ^b) : (static member wrapB : ^b * ^a -> ^b) =
    wrapB_<Multiapplicative,^a,^b> (Unchecked.defaultof< ^b>, x)

type Multimonad() =
    static member inline bindB (f : ^b -> Either< ^a,^b0>, x : Either< ^a,^b>) : Either< ^a,^b0> = Either.rbind f x
    static member inline bindB (f : ^b -> Result< ^a,^b0>, x : Result< ^a,^b>) : Result< ^a,^b0> = failwith "TODO: why u no haz Result.errorBind"

let inline bindB_< ^mf, ^f, ^a, ^b when (^mf or ^a) : (static member bindB : ^f * ^a -> ^b)> (f : ^f, x : ^a) : ^b =
    ((^mf or ^a) : (static member bindB : ^f * ^a -> ^b) (f, x))
let inline bindB (f : ^f) (x : ^a) : ^b when (Multimonad or ^a) : (static member bindB : ^f * ^a -> ^b) =
    bindB_<Multimonad,^f,^a,^b> (f, x)

type Multitraverse() =
    static member inline traverseBB (f : ^b -> ^b0, (a : ^a, b : ^b)) : ^b1
        when (Multifunctor or ^b0) : (static member mapB : _ * ^b0 -> ^b1) =
            mapB_<HKTPred,Multifunctor,_,_,_,_> (addfst a, (f b))

    static member inline traverseBB (f : ^a -> ^a0, (_ : ^b, b : ^a, _ : ^c) as t) : ^a1
        when (Multifunctor or ^a0) : (static member mapB : _ * ^a0 -> ^a1) =
            mapB_<HKTPred,Multifunctor,_,_,_,_> ((fun x -> mapB (k x) t), f b)

    static member inline traverseBB (f : ^b -> ^b0, x : Either< ^a,^b>) : ^b1
        when (^b0 or Multiapplicative)   : (static member wrapB : _ -> ^b0)
        and  (^b0 or Multifunctor) : (static member mapB  : _*_ -> _ )
        and  (^b1 or Multiapplicative)   : (static member wrapB : _ -> ^b1)
        and  (^b1 or Multifunctor) : (static member mapB  : _*_ -> _ ) =
            Either.match_ (Left >> wrapB) (mapB Right << f) x

/// (a -> m b) -> t a -> m t b
let inline traverseBB_< ^hkt, ^mf, ^a, ^b, ^c, ^d, ^c2 when (^mf or ^a) : (static member traverseBB : (^a -> ^b) * ^c -> ^d)
                        and (^hkt or ^c) : (static member IsParameterizedOverB : ^c * ^a -> unit)
                        and (^hkt or ^b) : (static member SameTypeCtr : ^b * ^d -> unit)
                        and (^hkt or ^d) : (static member IsParameterizedOverB : ^d * ^c2 -> unit)
                        and (^hkt or ^c) : (static member SameTypeCtr : ^c * ^c2 -> unit)>
           (f : ^a -> ^b, x : ^c) : ^d =
    ((^mf or ^a) : (static member traverseBB : (^a -> ^b) * ^c -> ^d) (f, x))

let inline traverseBB (f : ^a -> ^b) (x : ^c) : ^d
    when (Multitraverse or ^c) : (static member traverseBB : (^a -> ^b) * ^c -> ^d)
    and  (Multimonad or ^b)    : (static member bindB      : (_ -> ^b) * ^b -> ^b) =
        // The intent of requiring monad is just trying to be lawful
        traverseBB_<HKTPred,Multitraverse,^a,^b,^c,^d,_> (f, x)

