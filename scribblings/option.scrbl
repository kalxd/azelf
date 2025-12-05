#lang scribble/manual

@require[(for-label azelf)]

@title{option}

@defproc[(option/map
    [ma (Option a)]
    [f (-> a b)])
    (Option b)]{
    一个全新的解析模式。
}
