package main
func test(str string) {
  return func() {return str + "42"}
}
func main() {
  display(test("222")(), test("333")())
}