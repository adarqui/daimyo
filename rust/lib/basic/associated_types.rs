// https://doc.rust-lang.org/book/first-edition/associated-types.html



use std;
use std::fmt::Display;
use std::fmt::Result;



trait Graph {
  type N: std::fmt::Display;
  type E;

  fn has_edge(&self, &Self::N, &Self::N) -> bool;
  fn edges(&self, &Self::N) -> Vec<Self::E>;
}



struct MyNode;
struct MyEdge;
struct MyGraph;



impl Display for MyNode {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> Result {
    write!(f, "MyNode")
  }
}



impl Graph for MyGraph {
  type N = MyNode;
  type E = MyEdge;

  fn has_edge(&self, n1: &MyNode, n2: &MyNode) -> bool {
    true
  }

  fn edges(&self, n: &MyNode) -> Vec<MyEdge> {
    Vec::new()
  }
}



fn basic_associated_types() {
  let obj = Box::new(MyGraph) as Box<Graph<N=MyNode, E=MyEdge>>;
}
