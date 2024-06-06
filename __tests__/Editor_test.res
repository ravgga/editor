open Ava
open Editor

type tag = P | S | B | H2 | H3

let viewer: viewer<tag, string> = {
  draw: (id, p) =>
    switch p {
    | Fragment(f) => f
    | Wrapper(t, list) =>
      let tn = switch t {
      | P => "p"
      | S => "section"
      | B => "b"
      | H2 => "h2"
      | H3 => "h3"
      }
      "<" ++
      tn ++
      " data-id='" ++
      id ++
      "'>" ++
      List.reduce(list, "", (s, e) => s ++ e) ++
      "</" ++
      tn ++ ">"
    },
}

let source = {
  get: id =>
    switch id {
    | "hello" => Fragment("Hello, Rava.")
    | "doc1" => Index(list{"sec1.1", "sec1.2", "sec1.3"})
    | "sec1.1" => Block(S, list{"para1.1.1", "para1.1.2"})
    | "sec1.2" => Block(S, list{"para1.2.1", "para1.2.2", "para1.2.3"})
    | "sec1.3" => Block(S, list{"para1.3.1"})
    | "para1.1.1" => Block(P, list{"f1.1.1.1", "f1.1.1.2", "f1.1.1.3"})
    | "para1.1.2" => Block(P, list{"f1.1.2.1"})
    | "para1.2.1" => Block(P, list{"f1.2.1.1", "f1.2.1.2"})
    | "para1.2.2" => Block(P, list{"f1.2.2.1", "f1.2.2.2"})
    | "para1.2.3" => Block(P, list{"f1.2.3.1", "f1.2.3.2"})
    | "para1.3.1" => Block(P, list{"f1.3.1.1", "f1.3.1.2"})
    | "f1.1.1.1" => Fragment("Hello, ")
    | "f1.1.1.2" => Block(B, list{"f1.1.1.2.1"})
    | "f1.1.1.2.1" => Fragment("World")
    | "f1.1.1.3" => Fragment("!")
    | "f1.1.2.1" => Fragment("Lorem ipsum dolor sit amet.")
    | id => Fragment("[" ++ id ++ "]")
    // | _ => Fragment("~Unknown~")
    },
}

let view = id => {
  let list = Editor.editor(source, viewer, id)
  List.reduce(list, "", (s, e) => s == "" ? e : s ++ "|" ++ e)
}

test("simplest root", t => {
  t->Assert.deepEqual(view("hello"), "Hello, Rava.")
})

test("section detail", t => {
  t->Assert.deepEqual(
    view("sec1.1"),
    "<section data-id='sec1.1'><p data-id='para1.1.1'>Hello, <b data-id='f1.1.1.2'>World</b>!</p><p data-id='para1.1.2'>Lorem ipsum dolor sit amet.</p></section>",
  )
})

test("document with index", t => {
  t->Assert.deepEqual(
    view("doc1"),
    "<section data-id='sec1.1'><p data-id='para1.1.1'>Hello, <b data-id='f1.1.1.2'>World</b>!</p><p data-id='para1.1.2'>Lorem ipsum dolor sit amet.</p></section>|<section data-id='sec1.2'><p data-id='para1.2.1'>[f1.2.1.1][f1.2.1.2]</p><p data-id='para1.2.2'>[f1.2.2.1][f1.2.2.2]</p><p data-id='para1.2.3'>[f1.2.3.1][f1.2.3.2]</p></section>|<section data-id='sec1.3'><p data-id='para1.3.1'>[f1.3.1.1][f1.3.1.2]</p></section>",
  )
})
