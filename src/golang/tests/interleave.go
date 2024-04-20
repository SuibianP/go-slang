package main

var i = 0;

func loop() {
  for 1 {
    i = i + 1
    display(i, "")
  }
}

func main() {
  go loop()
  go loop()
}