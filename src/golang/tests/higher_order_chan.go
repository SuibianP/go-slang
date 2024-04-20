package main

func main() {
  var c1 = make(chan string)
  var c2 = make(chan chan string)
  go func (s string) {
    c2 <- c1
  }("sss")
  go func () {
    var c = <-c2
    c <- "str"
  }()
  display(<-c1, "")
}
`