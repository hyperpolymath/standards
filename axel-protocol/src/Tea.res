// SPDX-License-Identifier: PMPL-1.0-or-later
// Minimal TEA implementation for STAMP
// Compatible with full rescript-tea architecture

module Cmd = {
  type t<'msg> = unit
  let none = ()
  let msg = (_msg: 'msg) => ()
}

module Sub = {
  type t<'msg> = unit
  let none = ()
}

module Html = {
  type node
  type attribute

  @val external document: 'a = "document"
  @send external getElementById: ('a, string) => Js.Nullable.t<Dom.element> = "getElementById"
  @set external setInnerHTML: (Dom.element, string) => unit = "innerHTML"

  let noNode: node = Obj.magic("")

  let text = (str: string): node => Obj.magic(str)

  let tag = (tagName: string, _attrs: array<attribute>, children: array<node>): node => {
    // Use raw JavaScript to concatenate children
    let childrenHtml: string = %raw(`
      children.map(c => c).join('')
    `)
    Obj.magic(`<${tagName}>${childrenHtml}</${tagName}>`)
  }

  let div = (attrs, children) => tag("div", attrs, children)
  let p = (attrs, children) => tag("p", attrs, children)
  let h2 = (attrs, children) => tag("h2", attrs, children)
  let h3 = (attrs, children) => tag("h3", attrs, children)
  let h4 = (attrs, children) => tag("h4", attrs, children)
  let pre = (attrs, children) => tag("pre", attrs, children)
  let code = (attrs, children) => tag("code", attrs, children)
  let button = (attrs, children) => tag("button", attrs, children)
  let section = (attrs, children) => tag("section", attrs, children)

  let class' = (name: string): attribute => Obj.magic(`class="${name}"`)
  let id = (name: string): attribute => Obj.magic(`id="${name}"`)
  let onClick = (_handler: 'msg): attribute => Obj.magic("")
}

module App = {
  type program<'model, 'msg> = {
    init: unit => ('model, Cmd.t<'msg>),
    update: ('model, 'msg) => ('model, Cmd.t<'msg>),
    view: 'model => Html.node,
    subscriptions: 'model => Sub.t<'msg>,
  }

  let standardProgram = (program: program<'model, 'msg>) => {
    let (model, _cmd) = program.init()
    let html = program.view(model)

    // Mount to DOM
    switch Html.document->Html.getElementById("tea-app")->Js.Nullable.toOption {
    | Some(el) => el->Html.setInnerHTML(Obj.magic(html))
    | None => Js.log("TEA mount point #tea-app not found")
    }
  }
}
