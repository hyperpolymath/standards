// SPDX-License-Identifier: PMPL-1.0-or-later
// Proof Verification Hub panel — proof table with trust level & staleness.

type proof = {
  file: string,
  prover: string,
  theoremName: string,
  trustLevel: string,
  verifiedAt: string,
}

type state = {
  loading: bool,
  proofs: array<proof>,
  error: option<string>,
}

type action =
  | LoadStarted
  | ProofsLoaded(array<proof>)
  | LoadFailed(string)

let initial: state = {loading: false, proofs: [], error: None}

let reduce = (state: state, action: action): state =>
  switch action {
  | LoadStarted => {...state, loading: true, error: None}
  | ProofsLoaded(rows) => {...state, loading: false, proofs: rows}
  | LoadFailed(msg) => {...state, loading: false, error: Some(msg)}
  }

let pick = (semantic: Js.Dict.t<Js.Json.t>, key: string, fallback: string): string =>
  semantic
  ->Js.Dict.get(key)
  ->Belt.Option.flatMap(Js.Json.decodeString)
  ->Belt.Option.getWithDefault(fallback)

let refresh = async (dispatch: action => unit) => {
  dispatch(LoadStarted)
  switch await VcldbClient.currentProofs() {
  | Ok(result) =>
    let proofs = result.rows->Belt.Array.map(row => {
      let s = row["semantic"]
      let t = row["temporal"]
      {
        file: pick(s, "file", "?"),
        prover: pick(s, "prover", "?"),
        theoremName: pick(s, "theorem_name", "?"),
        trustLevel: pick(s, "trust_level", "unreviewed"),
        verifiedAt: pick(t, "verified_at", "never"),
      }
    })
    dispatch(ProofsLoaded(proofs))
  | Error(msg) => dispatch(LoadFailed(msg))
  }
}

let trustBadge = (level: string): string =>
  switch level {
  | "proven" => "✓ proven"
  | "tested" => "● tested"
  | "reviewed" => "◉ reviewed"
  | "postulate" => "▲ postulate"
  | "axiom" => "★ axiom"
  | "admitted" => "✗ admitted"
  | _ => "? " ++ level
  }

@react.component
let make = () => {
  let (state, dispatch) = React.useReducer(reduce, initial)

  React.useEffect0(() => {
    refresh(dispatch)->ignore
    let id = Js.Global.setInterval(() => refresh(dispatch)->ignore, 120000)
    Some(() => Js.Global.clearInterval(id))
  })

  <div className="panel panel-proof-hub">
    <h2> {React.string("Proof Verification Hub")} </h2>
    {switch state.error {
    | Some(msg) => <div className="error"> {React.string(msg)} </div>
    | None => React.null
    }}
    {state.loading
      ? <div className="spinner"> {React.string("Loading...")} </div>
      : <table className="proof-table">
          <thead>
            <tr>
              <th> {React.string("File")} </th>
              <th> {React.string("Prover")} </th>
              <th> {React.string("Theorem")} </th>
              <th> {React.string("Trust")} </th>
              <th> {React.string("Verified")} </th>
            </tr>
          </thead>
          <tbody>
            {state.proofs
            ->Belt.Array.mapWithIndex((i, p) =>
              <tr key={Belt.Int.toString(i)}>
                <td> {React.string(p.file)} </td>
                <td> {React.string(p.prover)} </td>
                <td> {React.string(p.theoremName)} </td>
                <td> {React.string(trustBadge(p.trustLevel))} </td>
                <td> {React.string(p.verifiedAt)} </td>
              </tr>
            )
            ->React.array}
          </tbody>
        </table>}
  </div>
}
