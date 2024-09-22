open%client Js_of_ocaml

type%client e_state = Healthy | Infected | Berserker | Mean

type%client direction = {
	mutable x : float;
	mutable y : float;
}

type%client creetSize = {
	mutable width : float;
	mutable height : float;
}
  
type%client creet = {
	mutable x : float;
	mutable y : float;
	mutable img: string;
	mutable direction: direction;
	mutable speed: float;
	size: creetSize;
	mutable state: e_state; 
	creet_elt: Dom_html.imageElement Js.t;
	id: int;
}

type%client  quadtreeNode = {
	mutable contains : creet list;
	mutable childNo : quadtreeNode option;
	mutable childNe : quadtreeNode option;
	mutable childSo : quadtreeNode option;
	mutable childSe : quadtreeNode option;
	mutable startX : int;
	mutable startY : int;
	mutable width : int;
	mutable height : int;
}

type%client  quadtree = {
	mutable root : quadtreeNode option;
	mutable maxDepth : int;
}