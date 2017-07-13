package path



import (
  "errors"
)



func Basename(name string) (string, error) {
  i := len(name) - 1
  // Remove trailing slashes
  for ; i > 0 && name[i] == '/'; i-- {
    name = name[:i]
  }
  // Remove leading directory name
  for i--; i >= 0; i-- {
    if name[i] == '/' {
      name = name[i+1:]
      break
    }
  }

  if name == "" {
    return "", errors.New("no basename")
  } else {
    return name, nil
  }
}
