package prim



import (
  "github.com/adarqui/daimyo/go/prim"
  "testing"
)



func TestTrue(t *testing.T) {
  if v := prim.True(); v != true {
    t.Fatalf("expected true, got %v\n", v)
  }
}



func TestFalse(t *testing.T) {
  if v := prim.False(); v != false {
    t.Fatalf("expected false, got %v\n", v)
  }
}
