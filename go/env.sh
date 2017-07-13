export GOPATH=/github.go

mkdir -p /github.go/src/github.com/adarqui

if [ ! -s /github.go/src/github.com/adarqui/daimyo ]; then
  ln -s /github/daimyo /github.go/src/github.com/adarqui/daimyo
fi
