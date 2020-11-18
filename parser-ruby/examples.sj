struct person { name, favcolor }

fn greet(person) {
	print("Hello, " + person.name + "!")

	if person.favcolor == "green" {
		print("I like green too!")
	}
}

samp = person("samp", "green")
greet(samp)

fn fizzBuzz (max) {
	i = 1;

	while i < max {
		if (i % 3) == 0 {
			print("Fizz")
		}

		if (i % 5) == 0 {
			print("Buzz")
		}

		if ((i % 3) * (i % 5)) != 0 {
			print(i)
		}

		i += 1

		print("\n")
	}
}

fizzBuzz(100)
