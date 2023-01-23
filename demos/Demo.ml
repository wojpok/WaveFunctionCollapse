
module Pixel8Wfc = Wfc.Make(Pixel8)
module StringWfc = Wfc.Make(
	struct 
		type t = char 
		let compare = Char.compare

		let stringify = function
		| Some x -> Char.escaped x
		| None -> "_"

		let newline = "\n"
end)



let demo1 = let open Pixel8 in [
	[White;Yellow;Yellow;Yellow;Yellow;Yellow;White;White;White;White;White;White;White;White;White;White;];
	[White;Yellow;White;White;White;Yellow;White;White;Yellow;Yellow;Yellow;Yellow;Yellow;Yellow;Yellow;White;];
	[White;Yellow;White;White;Red;Red;Red;White;Yellow;White;White;White;White;White;Yellow;White;];
	[Red;Red;Red;White;Red;White;Red;Yellow;Yellow;White;White;White;White;White;Yellow;White;];
	[Red;White;Red;White;Red;Red;Red;White;Yellow;White;White;White;White;White;Yellow;White;];
	[Red;Red;Red;White;White;White;White;White;Yellow;White;Red;Red;Red;White;Yellow;White;];
	[White;Yellow;White;White;White;White;White;White;Yellow;Yellow;Red;White;Red;Yellow;Yellow;White;];
	[White;Yellow;White;White;White;White;White;White;Yellow;White;Red;Red;Red;White;White;White;];
	[White;Yellow;White;White;Red;Red;Red;White;Yellow;White;White;White;White;White;White;White;];
	[White;Yellow;Yellow;Yellow;Red;White;Red;Yellow;Yellow;White;White;White;White;White;White;White;];
	[White;White;Yellow;White;Red;Red;Red;White;Yellow;White;Red;Red;Red;White;White;White;];
	[White;White;Yellow;White;White;White;White;White;Yellow;Yellow;Red;White;Red;White;White;White;];
	[White;White;Yellow;White;White;White;White;White;White;White;Red;Red;Red;White;White;White;];
	[White;Red;Red;Red;White;White;White;White;White;White;White;Yellow;White;White;White;White;];
	[White;Red;White;Red;Yellow;Yellow;Yellow;Yellow;Yellow;Yellow;Yellow;Yellow;White;White;White;White;];
	[White;Red;Red;Red;White;White;White;White;White;White;White;White;White;White;White;White;];
]

let test_map = let open Pixel8 in [
	[White;White;White;White;White;White;White;White;White;White;White;White;White;White;White;White;];
	[White;White;White;White;White;White;Black;Black;Black;Black;Black;Black;Black;White;White;White;];
	[White;White;White;White;White;White;Black;White;White;White;White;White;Black;White;White;White;];
	[White;White;White;White;White;White;Black;Black;Black;White;White;White;Black;White;White;White;];
	[White;White;White;White;White;White;White;White;Black;White;White;White;Black;White;White;White;];
	[White;White;White;Black;Black;Black;White;White;Black;White;White;White;Black;White;White;White;];
	[White;White;White;Black;White;Black;White;White;Black;White;White;White;Black;White;White;White;];
	[White;White;White;Black;White;Black;Black;Black;Black;White;White;White;Black;White;White;White;];
	[White;White;White;Black;White;White;White;White;White;White;White;White;Black;White;White;White;];
	[White;White;White;Black;White;White;White;White;White;White;White;White;Black;White;White;White;];
	[White;White;White;Black;Black;Black;Black;Black;White;White;White;White;Black;White;White;White;];
	[White;White;White;White;White;White;White;Black;White;White;White;White;Black;White;White;White;];
	[White;White;White;White;Black;Black;White;Black;White;Black;Black;Black;Black;White;White;White;];
	[White;White;White;White;Black;White;White;Black;White;White;White;White;White;White;White;White;];
	[White;White;White;White;Black;Black;Black;Black;White;White;White;White;White;White;White;White;];
	[White;White;White;White;White;White;White;White;White;White;White;White;White;White;White;White;];
]

let arr = let open Pixel8 in [
	[White;White;White;White;White;White;White;White;White;White;White;White;White;White;White;White;];
	[White;White;White;Black;Black;Black;Black;Black;White;White;White;White;White;White;White;White;];
	[White;White;White;Yellow;Yellow;Yellow;Yellow;Yellow;White;White;White;White;White;White;White;White;];
	[White;White;White;Red;Red;Red;Red;Red;White;White;White;White;White;White;White;White;];
	[White;White;White;White;White;White;White;White;White;White;White;White;Yellow;Black;Red;White;];
	[White;White;White;White;White;White;White;White;White;White;White;White;Yellow;Black;Red;White;];
	[White;White;White;White;White;White;White;White;White;White;White;White;Yellow;Black;Red;White;];
	[White;White;White;White;White;White;Black;Red;Yellow;White;White;White;Yellow;Black;Red;White;];
	[Red;Red;Red;Red;White;White;Black;Red;Yellow;White;White;White;Yellow;Black;Red;White;];
	[Black;Black;Black;Black;White;White;Black;Red;Yellow;White;White;White;Yellow;Black;Red;White;];
	[Yellow;Yellow;Yellow;Yellow;White;White;Black;Red;Yellow;White;White;White;White;White;White;White;];
	[White;White;White;White;White;White;Black;Red;Yellow;White;White;White;White;White;White;White;];
	[White;White;White;White;White;White;Black;Red;Yellow;White;White;White;White;White;White;White;];
	[White;White;White;White;White;White;White;White;White;White;White;White;Yellow;Red;Black;White;];
	[White;White;White;White;White;White;White;White;White;White;White;White;Yellow;Red;Black;White;];
	[White;White;White;White;White;White;White;White;White;White;White;White;Yellow;Red;Black;White;];
]


let lorem = "aaaaaaaaaabcdddddccccddcbbbcccccddcbaaabababcdd"

let string_demo x = String.to_seq x |> List.of_seq

let test_string x = StringWfc.wfc1 (true, 2, (true, false), 0, [100]) @@ string_demo x

