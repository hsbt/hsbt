package main

import (
	"fmt"
	"strings"
	"code.google.com/p/go-tour/wc"
)

func WordCount(s string) map[string]int {
	fmt.Println(strings.Fields("f o o b  arbuzz"))
	return map[string]int{"x": 1}
}

func main() {
	wc.Test(WordCount)
}
