#lang scribble/manual
@(require "../run.rkt"
          (for-label azelf))

@title[#:tag "map"]{Map}

@racketmodname[azelf]内置Map，取代racket的@racket[hash]。
所谓HashMap，相当于JavasScript中的Object、TypeScript中的Recrod。

@section[#:tag "map-type"]{Map定义}

@defproc[(Map? [a any/c]) boolean?]{
是否为Map。
}

@defproc[(Map/c [k any/c] [v any/c]) contract?]{
@racket[contract]构造器。
}

@defform[(Map pattern ...)]{
Map模式匹配关键字，使用与@racket[hash-table]一致。

@examples[
#:eval sb
(match map-empty [(Map) #t])
(match (hashmap 1 2 3 4) [(Map (k v) ...) k])
]
}

@section[#:tag "map-contructor"]{Map构造函数}

@defproc[(hashmap [key k] [value v] ...) (Map/c k v)]{
构造新的@racket[Map?]。

@examples[
#:eval sb
(hashmap 1 2 3 4)
(hashmap (array 1) 1 (array 2) 2)
]
}

@defproc[(list->map [xs (listof (cons/c k v))]) (Map/c k v)]{
从列表中生成@racket[Map?]。

@examples[
#:eval sb
(list->map '((1 2) (3 4)))
]
}

@defthing[map-empty Map?]{
空Map。
}

@defproc[(map-singleton [key k] [value v]) (Map/c k v)]{
@examples[
#:eval sb
(map-singleton 1 2)
]
}

@section[#:tag "map-update"]{Map更新}

@defproc[(map-insert [key k] [value v] [hash (Map/c k v)]) (Map/c k v)]{
插入一个值，已存在则覆盖。

@examples[
#:eval sb
(map-insert 2 10 (map-singleton 1 1))
(map-insert 1 10 (map-singleton 1 1))
]
}

@defproc[(map-remove [key k] [hash (Map/c k v)]) (Map/c k v)]{
删除一个值。
@examples[
#:eval sb
(map-remove 2 (map-singleton 1 1))
(map-remove 1 (map-singleton 1 1))
]
}

@defproc[(map-adjust [f (-> v v)] [k Ord?] [hash (Map/c k v)]) (Map/c k v)]{

@examples[
#:eval sb
(define (++ s) (string-append "new" s))

(map-adjust ++ 5 (hashmap 5 "a" 3 "b"))
(map-adjust ++ 7 (hashmap 5 "a" 3 "b"))
(map-adjust ++ 7 map-empty)
]
}

@defproc[(map-alter [f (-> (Maybe/c v) (Maybe/c v))] [k Ord?] [hash (Map/c k v)]) (hash (Map/c k v))]{
集增、删、改一体的函数，具体行为由@racket[f]控制。

@examples[
#:eval sb
(define h (hashmap 1 2 3 4))

(define (remove-key v) nothing)

(define (update-key v) (Just 10))
(map-alter remove-key 3 h)
(map-alter update-key 1 h)
]
}

@section[#:tag "map-lookup"]{Map取值}

@defproc[(map-key? [key k] [hash (Map/c k v)]) boolean?]{
@examples[
#:eval sb
(map-key? 1 map-empty)
(map-key? 2 (map-singleton 2 1))
]
}

@defproc[(map-get [key k] [hash (Map/c k v)]) (Maybe/c v)]{
@examples[
#:eval sb
(map-get 1 map-empty)
(map-get 1 (hashmap 1 2))
]
}

@defproc*[([(map-keys [hash (Map/c k v)]) (Array/c k)]
           [(map-values [hash (Map/c k v)]) (Array/c v)])]{
不多解释。
}

@defproc[(map-size [hash Map?]) exact-nonnegative-integer?]{
Map总键值对数量。

@examples[
#:eval sb
(map-size map-empty)
]
}

@section[#:tag "map-interactivate"]{Map相互交互}

@defproc[(map-union [ha (Map/c k v)] [hb (Map/c k v)]) (Map/c k v)]{
合并两个Map，@racket[hb]向@racket[ha]覆盖。

@examples[
#:eval sb
(map-union (hashmap 1 2 3 4) (hashmap 2 3 1 4))
]
}

@section[#:tag "map-destruction"]{Map解构}

@defproc[(map->list [h (Map/c k v)]) (listof (cons/c k v))]{
@examples[
#:eval sb
(map->list (hashmap 1 2 3 4))
]
}

@defproc[(map->hash [h (Map/c k v)]) (hash/c k v)]{
@examples[
#:eval sb
(map->hash (hashmap 1 2 3 4))
]
}

@section[#:tag "map-syntax"]{Map语法糖}

@racketmodname[azelf]提供了类似于JavaScript的object定义语法：

@codeblock{
{ 1 2 }
{ 1 (array 1 2) }
(map-size {})
}
