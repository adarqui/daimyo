package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	accum := 0
	reader := bufio.NewReader(os.Stdin)
	_, _ = reader.ReadString('\n')
	line2, _ := reader.ReadString('\n')
	tokens := strings.Split(strings.Trim(line2, "\n"), " ")
	for _, tok := range tokens {
		n, _ := strconv.Atoi(tok)
		accum += n
	}
	fmt.Printf("%d\n", accum)
}
