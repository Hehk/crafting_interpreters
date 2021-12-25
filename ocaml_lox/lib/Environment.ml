open Base

type 'a env = (string, 'a, String.comparator_witness) Map.t

(* Rather than a default list, i don't want an explicit nil. It adds extra logic *)
(* for a state that should never be possible *)
type 'a t = Environment of 'a env * 'a t option

let rec get env key =
  let (Environment (currentEnv, parent)) = env in
  match Map.find currentEnv key with
  | Some value -> Some value
  | None -> ( match parent with Some env -> get env key | None -> None)

let getInCurrentEnvironment env key =
  let (Environment (currentEnv, _parent)) = env in
  Map.find currentEnv key

let add env key data =
  let (Environment (currentEnv, parent)) = env in
  let env = Map.set currentEnv ~key ~data in
  Environment (env, parent)

let child env = Environment (Map.empty (module String), Some env)

let empty = Environment (Map.empty (module String), None)
