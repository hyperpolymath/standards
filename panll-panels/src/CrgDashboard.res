// SPDX-License-Identifier: PMPL-1.0-or-later
// CRG Dashboard panel — grade distribution, transitions, promotion queue.

type state = {
  loading: bool,
  distribution: array<(string, int)>,  // (grade, count)
  error: option<string>,
}

type action =
  | LoadStarted
  | DistributionLoaded(array<(string, int)>)
  | LoadFailed(string)

let initial: state = {
  loading: false,
  distribution: [],
  error: None,
}

let reduce = (state: state, action: action): state =>
  switch action {
  | LoadStarted => {...state, loading: true, error: None}
  | DistributionLoaded(rows) => {...state, loading: false, distribution: rows}
  | LoadFailed(msg) => {...state, loading: false, error: Some(msg)}
  }

let refresh = async (dispatch: action => unit) => {
  dispatch(LoadStarted)
  switch await VcldbClient.gradeDistribution() {
  | Ok(result) =>
    let rows = result.rows->Belt.Array.map(row => {
      let g = row["semantic"]->Js.Dict.get("grade")->Belt.Option.flatMap(Js.Json.decodeString)
      let c = row["semantic"]->Js.Dict.get("count")->Belt.Option.flatMap(Js.Json.decodeNumber)
      (g->Belt.Option.getWithDefault("?"), c->Belt.Option.mapWithDefault(0, f => f->Belt.Float.toInt))
    })
    dispatch(DistributionLoaded(rows))
  | Error(msg) => dispatch(LoadFailed(msg))
  }
}

@react.component
let make = () => {
  let (state, dispatch) = React.useReducer(reduce, initial)

  React.useEffect0(() => {
    refresh(dispatch)->ignore
    let id = Js.Global.setInterval(() => refresh(dispatch)->ignore, 60000)
    Some(() => Js.Global.clearInterval(id))
  })

  <div className="panel panel-crg-dashboard">
    <h2> {React.string("CRG Dashboard")} </h2>
    {switch state.error {
    | Some(msg) => <div className="error"> {React.string(msg)} </div>
    | None => React.null
    }}
    {state.loading
      ? <div className="spinner"> {React.string("Loading...")} </div>
      : <table className="grade-matrix">
          <thead>
            <tr>
              <th> {React.string("Grade")} </th>
              <th> {React.string("Count")} </th>
            </tr>
          </thead>
          <tbody>
            {state.distribution
            ->Belt.Array.map(((grade, count)) =>
              <tr key={grade}>
                <td> {React.string(grade)} </td>
                <td> {React.int(count)} </td>
              </tr>
            )
            ->React.array}
          </tbody>
        </table>}
  </div>
}
