package main

import "code.google.com/p/go-tour/pic"

func Pic(dx, dy int) [][]uint8 {
	y := make([][]uint8, dy)
	for _, value := range y {
		value = make([]uint8, dx)
	}
	return y
}

func main() {
	pic.Show(Pic)
}
