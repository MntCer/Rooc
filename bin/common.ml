type node_id = Node of int
let int_of_node (Node i) = i

type 'a identified = { node: 'a; id: node_id }