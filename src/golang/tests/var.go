package main

var g = 1

func f(g int) {
  g = g + 42
  display(g, "")
}

func main() {
  f(g)
  f(0)
  display(g, "")
}