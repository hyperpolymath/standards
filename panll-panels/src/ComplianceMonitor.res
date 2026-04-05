// SPDX-License-Identifier: PMPL-1.0-or-later
// Compliance Monitor panel — Hypatia findings heatmap + trend.

type finding = {
  repo: string,
  severity: string,
  count: int,
}

type state = {
  loading: bool,
  findings: array<finding>,
  error: option<string>,
}

type action =
  | LoadStarted
  | FindingsLoaded(array<finding>)
  | LoadFailed(string)

let initial: state = {loading: false, findings: [], error: None}

let reduce = (state: state, action: action): state =>
  switch action {
  | LoadStarted => {...state, loading: true, error: None}
  | FindingsLoaded(rows) => {...state, loading: false, findings: rows}
  | LoadFailed(msg) => {...state, loading: false, error: Some(msg)}
  }

let refresh = async (dispatch: action => unit) => {
  dispatch(LoadStarted)
  switch await VcldbClient.openFindings() {
  | Ok(result) =>
    let findings = result.rows->Belt.Array.map(row => {
      let repo = row["semantic"]->Js.Dict.get("repo")
        ->Belt.Option.flatMap(Js.Json.decodeString)
        ->Belt.Option.getWithDefault("?")
      let sev = row["semantic"]->Js.Dict.get("severity")
        ->Belt.Option.flatMap(Js.Json.decodeString)
        ->Belt.Option.getWithDefault("?")
      let count = row["semantic"]->Js.Dict.get("count")
        ->Belt.Option.flatMap(Js.Json.decodeNumber)
        ->Belt.Option.mapWithDefault(0, f => f->Belt.Float.toInt)
      {repo, severity: sev, count}
    })
    dispatch(FindingsLoaded(findings))
  | Error(msg) => dispatch(LoadFailed(msg))
  }
}

let severityColour = (sev: string): string =>
  switch sev {
  | "critical" => "#d00"
  | "high" => "#f60"
  | "medium" => "#fc0"
  | "low" => "#6c0"
  | _ => "#ccc"
  }

@react.component
let make = () => {
  let (state, dispatch) = React.useReducer(reduce, initial)

  React.useEffect0(() => {
    refresh(dispatch)->ignore
    let id = Js.Global.setInterval(() => refresh(dispatch)->ignore, 30000)
    Some(() => Js.Global.clearInterval(id))
  })

  <div className="panel panel-compliance-monitor">
    <h2> {React.string("Compliance Monitor")} </h2>
    {switch state.error {
    | Some(msg) => <div className="error"> {React.string(msg)} </div>
    | None => React.null
    }}
    {state.loading
      ? <div className="spinner"> {React.string("Loading...")} </div>
      : <div className="heatmap">
          {state.findings
          ->Belt.Array.map(f =>
            <div
              key={f.repo ++ "-" ++ f.severity}
              className="cell"
              style={ReactDOM.Style.make(~backgroundColor=severityColour(f.severity), ())}>
              <div className="cell-repo"> {React.string(f.repo)} </div>
              <div className="cell-count"> {React.int(f.count)} </div>
            </div>
          )
          ->React.array}
        </div>}
  </div>
}
