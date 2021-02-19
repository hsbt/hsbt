package main

import (
	"fmt"
)

func Sqrt(x float64) float64 {
	z := float64(1);
	z1 := float64(1);
	delta := 0.001
	i := 0
	for ;i < 10; i++ {
		z1 = z - (z * z - x)/(2 * x)
		if z1 - z < delta {
			break
		}
		z = z1
	}
	fmt.Println(i)
	return z
}

func main() {
	fmt.Println(Sqrt(2))
}
