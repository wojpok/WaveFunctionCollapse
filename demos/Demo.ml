
(* Modules *)
module Pixel8Wfc = Wfc.Make(Pixel8)

module StringWfc = Wfc.Make (
	struct 
		type t = char 
		let compare = Char.compare

		let stringify = function
		| Some x -> Char.escaped x
		| None -> "_"

		let newline = "\n"
	end
)

(* Maps *)


let pixel_hatches = let open Pixel8 in [
	[White;White;White;White;Cyan;White;White;White;White;White;Cyan;White;White;White;Cyan;White;];
	[White;White;White;White;Cyan;White;White;White;White;White;Cyan;White;White;White;Cyan;White;];
	[White;White;White;White;Cyan;White;White;White;White;White;Cyan;White;White;White;Cyan;White;];
	[White;White;White;White;Cyan;White;White;White;White;White;Cyan;White;White;White;Cyan;White;];
	[Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;];
	[White;White;White;White;Cyan;White;White;White;White;White;Cyan;White;White;White;Cyan;White;];
	[White;White;White;White;Cyan;White;White;White;White;White;Cyan;White;White;White;Cyan;White;];
	[Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;];
	[White;White;White;White;Cyan;White;White;White;White;White;Cyan;White;White;White;Cyan;White;];
	[White;White;White;White;Cyan;White;White;White;White;White;Cyan;White;White;White;Cyan;White;];
	[White;White;White;White;Cyan;White;White;White;White;White;Cyan;White;White;White;Cyan;White;];
	[White;White;White;White;Cyan;White;White;White;White;White;Cyan;White;White;White;Cyan;White;];
	[Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;Cyan;];
	[White;White;White;White;Cyan;White;White;White;White;White;Cyan;White;White;White;Cyan;White;];
	[White;White;White;White;Cyan;White;White;White;White;White;Cyan;White;White;White;Cyan;White;];
	[White;White;White;White;Cyan;White;White;White;White;White;Cyan;White;White;White;Cyan;White;];
]

let pixel_flowers = let open Pixel8 in [
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

let pixel_lines = let open Pixel8 in [
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

let pixel_flags = let open Pixel8 in [
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

let list_of_string x = String.to_seq x |> List.of_seq

(* Strings *)

let string_lorem = list_of_string
"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Mi proin sed libero enim sed faucibus turpis in eu. Ullamcorper velit sed ullamcorper morbi tincidunt. Netus et malesuada fames ac turpis egestas. Pretium fusce id velit ut. Eu non diam phasellus vestibulum lorem sed risus ultricies tristique. Tortor condimentum lacinia quis vel. Amet tellus cras adipiscing enim eu turpis egestas. Bibendum est ultricies integer quis auctor elit sed. Mi proin sed libero enim sed faucibus turpis. Bibendum ut tristique et egestas. Porta lorem mollis aliquam ut porttitor leo a diam sollicitudin. Sem fringilla ut morbi tincidunt augue interdum velit.
Feugiat sed lectus vestibulum mattis ullamcorper velit sed. Quam elementum pulvinar etiam non quam lacus suspendisse. Ipsum faucibus vitae aliquet nec ullamcorper sit. Non sodales neque sodales ut. Viverra tellus in hac habitasse platea. Bibendum enim facilisis gravida neque convallis a cras. Massa sapien faucibus et molestie ac feugiat. Consequat mauris nunc congue nisi vitae suscipit tellus mauris. Dolor sit amet consectetur adipiscing. Eu mi bibendum neque egestas congue. Pretium quam vulputate dignissim suspendisse in est ante in nibh. Aliquet risus feugiat in ante metus dictum at. Facilisi nullam vehicula ipsum a arcu cursus vitae congue mauris. Turpis egestas pretium aenean pharetra magna ac placerat vestibulum lectus. Non curabitur gravida arcu ac tortor. Habitant morbi tristique senectus et netus et malesuada. Libero justo laoreet sit amet cursus.
Vulputate sapien nec sagittis aliquam malesuada bibendum. Augue neque gravida in fermentum et sollicitudin ac orci. Magna sit amet purus gravida quis blandit turpis. Sed turpis tincidunt id aliquet risus feugiat in. Sed vulputate mi sit amet mauris commodo quis. Sed arcu non odio euismod lacinia at quis. Adipiscing bibendum est ultricies integer quis. Tristique senectus et netus et malesuada fames ac turpis. Dolor sed viverra ipsum nunc aliquet. Sapien pellentesque habitant morbi tristique senectus et netus et. At urna condimentum mattis pellentesque id nibh tortor. Fusce id velit ut tortor pretium viverra suspendisse. Sed felis eget velit aliquet sagittis id consectetur. Enim lobortis scelerisque fermentum dui faucibus in ornare quam. Lorem ipsum dolor sit amet consectetur adipiscing elit duis tristique. Ut eu sem integer vitae justo eget magna. Ut diam quam nulla porttitor massa id.
Morbi leo urna molestie at elementum eu. Consectetur adipiscing elit ut aliquam purus sit amet luctus. Urna molestie at elementum eu facilisis sed odio morbi quis. Risus nullam eget felis eget nunc lobortis. Nunc vel risus commodo viverra maecenas accumsan. Blandit volutpat maecenas volutpat blandit aliquam. Risus feugiat in ante metus dictum. Purus sit amet luctus venenatis lectus magna. Viverra maecenas accumsan lacus vel. Massa sapien faucibus et molestie ac feugiat sed lectus vestibulum. Purus faucibus ornare suspendisse sed nisi lacus sed. Lectus magna fringilla urna porttitor rhoncus dolor. Mauris commodo quis imperdiet massa tincidunt. Volutpat sed cras ornare arcu.
At erat pellentesque adipiscing commodo. Massa sapien faucibus et molestie ac feugiat sed lectus. Tempor nec feugiat nisl pretium fusce id velit ut tortor. Facilisis magna etiam tempor orci eu. Gravida dictum fusce ut placerat orci nulla pellentesque. Cras adipiscing enim eu turpis egestas pretium aenean. Malesuada bibendum arcu vitae elementum curabitur vitae nunc. A diam maecenas sed enim ut. Mattis molestie a iaculis at erat pellentesque adipiscing commodo elit. Egestas congue quisque egestas diam in arcu."

let string_pan_tadeusz = list_of_string
"Litwo! Ojczyzno moja! ty jesteś jak zdrowie.
Ile cię trzeba cenić, ten tylko się dowie,
Kto cię stracił. Dziś piękność twą w całej ozdobie
Widzę i opisuję, bo tęsknię po tobie.
Panno Święta, co Jasnej bronisz Częstochowy
I w Ostrej świecisz Bramie! Ty, co gród zamkowy
Nowogródzki ochraniasz z jego wiernym ludem!
Jak mnie dziecko do zdrowia powróciłaś cudem
(Gdy od płaczącej matki pod Twoję opiekę
Ofiarowany, martwą podniosłem powiekę
I zaraz mogłem pieszo do Twych świątyń progu
Iść za wrócone życie podziękować Bogu),
Tak nas powrócisz cudem na Ojczyzny łono.
Tymczasem przenoś moję duszę utęsknioną
Do tych pagórków leśnych, do tych łąk zielonych,
Szeroko nad błękitnym Niemnem rozciągnionych;
Do tych pól malowanych zbożem rozmaitem,
Wyzłacanych pszenicą, posrebrzanych żytem;
Gdzie bursztynowy świerzop, gryka jak śnieg biała,
Gdzie panieńskim rumieńcem dzięcielina pała,
A wszystko przepasane, jakby wstęgą, miedzą
Zieloną, na niej z rzadka ciche grusze siedzą."

let string_napkin_speech = list_of_string
"Suppose that you were sitting down at this table. 
The napkins are in front of you, which napkin would you take? 
The one on your left? Or the one on your right? 
The one on your left side? Or the one on your right side? 
Usually you would take the one on your left side. That is correct too.
But in a larger sense on society, that is wrong. Perhaps I could even substitute society with the Universe. 
The correct answer is that It is determined by the one who takes his or her own napkin first. Yes? 
If the first one takes the napkin to their right, then there's no choice but for others to also take the right napkin. 
The same goes for the left. Everyone else will take the napkin to their left, because they have no other option. This is society 
Who are the ones that determine the price of land first? There must have been someone who determined the value of money, first. 
The size of the rails on a train track? The magnitude of electricity? Laws and Regulations? 
Who was the first to determine these things? Did we all do it, because this is a Republic? 
Or was it Arbitrary? NO! The one who took the napkin first determined all of these things! 
The rules of this world are determined by that same principle of right or left?! 
In a Society like this table, a state of equilibrium, once one makes the first move, everyone must follow! 
In every era, this World has been operating by this napkin principle. And the one who takes the napkin first must be someone who is respected by all. 
It's not that anyone can fulfill this role Those that are despotic or unworthy will be scorned. And those are the losers. 
In the case of this table, the eldest or the Master of the party will take the napkin first. 
Because everyone respects those individuals"


let show_pixel_demo_maps () = 
	let show_entry name map = let _ =
		print_endline "";
		print_endline ("    " ^ name);
		print_endline "";
		Pixel8Wfc.show2 map
	in () in let _ =
	show_entry "pixel_hatches" pixel_hatches;
	show_entry "pixel_flowers" pixel_flowers;
	show_entry "pixel_lines"   pixel_lines;
	show_entry "pixel_flags"   pixel_flags 
	in ()

let generate_pixel_map map = 
	Pixel8Wfc.wfc2 (true, 1, (true, true), Float.to_int @@ Unix.time (), [16; 16]) map 
	|> Pixel8Wfc.show2_opt

let show_string_demo_text () = 
	let show_entry name text = let _ =
		print_endline "";
		print_endline ("    " ^ name);
		print_endline "";
		text |> List.to_seq |> Seq.take 70 |> List.of_seq |> StringWfc.show1
	in () in let _ =
	show_entry "string_lorem"         string_lorem;
	show_entry "string_pan_tadeusz"   string_pan_tadeusz;
	show_entry "string_napkin_speech" string_napkin_speech
	in ()

let generate_string map = 
	StringWfc.wfc1 (true, 1, (false, false), Float.to_int @@ Unix.time (), [60]) map 
	|> StringWfc.show1_opt





