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
	if (x == 0) {
		0;
	} else {
		if (x == 1) {
			return 1;
		} else {
			fibonacci(x - 1) + fibonacci(x - 2);
		}
	}
};

print(fibonacci(10));
`;

export { defaultCode, fibonacci };
