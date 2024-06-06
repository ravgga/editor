/* CONTENT TYPES */
type fragment = string
type index = List.t<string>
type content<'b> =
  | Index(index)
  | Block('b, index)
  | Fragment(fragment)

type source<'b> = {
  /* CREATE: put(parent_id, content) => new_child_id */
  // put: (string, content<'b>) => string,
  /* READ: get(content_id) */
  get: string => content<'b>,
  /* UPDATE: post(content_id, new_content) */
  // post: (string, content<'b>) => unit,
  /* DELETE: delete(content_id) */
  // delete: string => unit,
}

/* VIEW TYPES */
type rec primitive<'s, 'v> =
  | Wrapper('s, List.t<'v>)
  | Fragment(fragment)

type viewer<'s, 'v> = {
  draw: (string, primitive<'s, 'v>) => 'v
}

type editor<'s, 'v> = (source<'s>, viewer<'s, 'v>, string) => List.t<'v>
let editor: editor<'s, 'v> = (source, viewer, root) => {
  let rec expand = (acc: List.t<'v>, list) =>
    switch list {
    | list{} => List.reverse(acc)
    | list{id, ...tail} =>
      switch source.get(id) {
      | Index(children) => expand(expand(acc, children), tail)
      | Block(b, children) => {
          let p = viewer.draw(id, Wrapper(b, expand(list{}, children)))
          expand(list{p, ...acc}, tail)
        }
      | Fragment(f) => expand(list{viewer.draw(id, Fragment(f)), ...acc}, tail)
      }
    }

  List.reverse(expand(list{}, list{root}))
}
