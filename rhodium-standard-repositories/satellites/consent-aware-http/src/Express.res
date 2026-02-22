// Express.js bindings

type req = {
  headers: Js.Dict.t<string>,
  path: string,
}

type res

@send external status: (res, int) => res = "status"
@send external header: (res, string, string) => res = "header"
@send external json: (res, 'a) => unit = "json"

type next = unit => unit
type middleware = (req, res, next) => promise<unit>
