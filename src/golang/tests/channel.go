package main

func main() {
  var c = make(chan string)
  go func (s string) {
    c <- s
  }("sss")

  display(<-c, "")
}