package main

import "fmt"
import "time"

func sum(a []int, c chan int){
	sum := 0
	for _, v := range a {
		if v == 7 { time.Sleep(1000 * time.Millisecond) }
		sum += v
	}
	fmt.Println(sum)
	c <- sum
}

func main(){
	a := []int{7, 2, 8, -9, 4, 0}

	c := make(chan int)
	go sum(a[:len(a)/2], c)
	go sum(a[len(a)/2:], c)
	x, y := <-c, <-c

	fmt.Println(x, y, x+y)
}
