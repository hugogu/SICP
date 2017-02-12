module Stream

type Stream<'a>(empty:bool, head:'a, tail:unit -> Stream<'a>) = 
  new(head:'a, tail:unit -> Stream<'a>) = Stream(false, head, tail)
  member this.Head = match empty with 
                      | false -> head
                      | true -> invalidOp "An empty stream has no head."
  member this.Tail = tail()
  member this.IsEmpty = empty
  member this.iterate action = 
    if not empty then 
      action this.Head; this.Tail.iterate action
  member this.iterateWhen action condition = 
    if not empty then
      action this.Head;
      if condition this.Head then
        this.Tail.iterateWhen action condition
  static member Empty = 
    Stream<'a>(true, Unchecked.defaultof<'a>, 
               fun unit -> invalidOp "An empty stream has no tail.")
  static member Map func (s:Stream<'a>) = 
    if not s.IsEmpty then 
      Stream(func(s.Head), fun () -> Stream.Map func s.Tail)
    else
      Stream.Empty
  static member Infinit producer head = 
    let rec stream = 
      fun () -> Stream(head, fun () -> stream() |> Stream.Map producer)
    stream()
