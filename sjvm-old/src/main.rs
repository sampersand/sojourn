use sjvm::*;


fn main() {
	SojournVm::read_from(std::fs::File::open("../parser-ruby/test.sjc").unwrap())
		.unwrap()
		.run();
}
