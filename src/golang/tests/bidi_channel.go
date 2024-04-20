package main

var c = make(chan string)

func g() {
    c <- "hello"
    display(<-c, "")
}

func f() {
    display(<-c, "")
}

func main() {
    go g()
    go f()
    c <- "world"
}