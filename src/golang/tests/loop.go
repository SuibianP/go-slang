package main

func main() {
	var sum int = 0
	var i int = 0
	for i < 1000 {
		sum = sum + i
		i = i + 1
	}
	display(sum, "")
}
