package path



import (
  "github.com/adarqui/daimyo/go/path"
  "testing"
)



func TestBasename(t *testing.T) {
  if v, err := path.Basename("/some/path/to/basename"); v != "basename" || err != nil {
    t.Fatalf("expected \"basename\", got %s. expected nil err = %v\n", v, err)
  }
  if v, err := path.Basename("/some/path/to/"); v != "to" || err != nil {
    t.Fatalf("expected \"to\", got \"%s\". expected nil err, got %v\n", v, err)
  }
  if v, err := path.Basename(""); v != "" || err == nil {
    t.Fatalf("expected \"\", got \"%s\". expected err, got %v\n", v, err)
  }
}
