const defaultCode = `print("Welcome to the Monkey Playground!");

let multiplier = fn(factor) {
	return fn(x) {
		x * factor;
	};
};

let double = multiplier(2);
let triple = multiplier(3);

print("The double of 5 is: ", double(5));
print("The triple of 5 is: ", triple(5));
`;

const fibonacci = `let fibonacci = fn(x) {
	if (x == 0 || x == 1) {
		return x;
	}
	return fibonacci(x - 1) + fibonacci(x - 2);
};

print(fibonacci(10));
`;

const variables = `let version = 1;
let name = "Monkey programming language";
let myArray = [1, 2, 3, 4, 5];
let coolBooleanLiteral = true;

let awesomeValue = 10 / 2 * 5 + 30;
let arrayWithValues = [1 + 1, 2 * 2, 3];

print(awesomeValue);
print(arrayWithValues);
`;

const ifElse = `if (10 > 5) {
	print("foo");
} else {
	print("bar");
}

let value = if (true || false) {
	10;
} else {
	20;
};

print(value);
`;

const maps = `let people = [{
	"age": 24,
	"name": "Anna"
}, {
	"age": 63,
	"name": "Bob"
}, {
	"age": 16,
	"name": "Chris"
}];

let getName = fn(person) {
	person["name"];
};

let map = fn(arr, f) {
	let iter = fn(arr, accumulated) {
		if (len(arr) == 0) {
			accumulated;
		} else {
			iter(rest(arr), push(accumulated, f(first(arr))));
		}
	};

	iter(arr, []);
};

let peopleNames = map(people, getName);
print(peopleNames);
`;

export { defaultCode, fibonacci, variables, ifElse, maps };
