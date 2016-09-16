(******************** chargement de la biliotheque Graphics ********************)
(*#load "graphics.cma";;*)
open Graphics;;

(*Variable aleatoire qui permet de generer un Tetravex aleatoire *)
let aleatoire = ref false;;

(*Variable permettant de remettre à zéro le chrono lors du lancement d'une nouvelle partie*)
let clock = ref 0.;;

(* Pointeurs nécessaires à la coordination des différentes fonctions depuis la boucle principale *)
let nom = ref "";;
let sortie = ref true;;
let timer = ref 0.;;
let test = ref false;;

(******************** fonction pour creer le fond de la fenetre ********************)
(* La fenetre est un rectangle de dimension 1120x600 et les grilles de 360x360 *)

let remplir_fond () =
	let fond = rgb 76 153 0 in
	set_color fond;
	fill_rect 0 0 1120 600;
	moveto (10) (10);
	set_color white;
	draw_string ("Edouard MAFFERT / Aurélien GUIRAMAND @2014");;

let draw_fleche () = 
	set_color (rgb 70 140 0);
	fill_poly [|(610,340);(610,220);(510,280)|];
	set_color (rgb 81 163 0);
	fill_poly [|(610,340);(608,337);(608,223);(610,220)|];
	set_color (rgb 69 125 0);
	fill_poly [|(610,340);(608,337);(513,280);(608,223);(610,220);(510,280)|];;


(******************** fonctions permettant de lire le fichier "pieces.txt" et de stocker tous les entiers dans un tableau ********************)
(* renvoie un tableau des entiers contenus dans "fichier" passe en argument*)
let rec lire fichier =
  try 
    let i = input_line fichier in
		if String.length i < 2
		then (int_of_char i.[0]) -48 :: (lire fichier)
		else (int_of_char i.[0]) -48 :: (int_of_char i.[2]) -48 :: (int_of_char i.[4]) -48 :: (int_of_char i.[6]) -48 :: (lire fichier)
  with End_of_file -> [];;

	
(* generation aleatoire d'une liste de piece n partant d'en bas à gauche et en allant en haut à droite, donc classée*)
	let random_generation taille_generation = 
	let matrice_generation = Array.make_matrix (taille_generation*taille_generation) 4 0 in
	
	
	for i = 0 to ((taille_generation*taille_generation)-1) do 
		begin 
		for j = 0 to 3 do
			begin
			if (i=0) then
				matrice_generation.(i).(j)<-Random.int 10
			else if (i<taille_generation && j=0) || ((i mod taille_generation)=0 && j=1) then
				matrice_generation.(i).(j)<-Random.int 10
			else if j=0 then
				matrice_generation.(i).(0)<-matrice_generation.(i-taille_generation).(2)
			else if j=1 then
				matrice_generation.(i).(1)<-matrice_generation.(i-1).(3)
			else
				matrice_generation.(i).(j)<-Random.int 10	
			end
		done
		end
	done;
	matrice_generation;;

(* Pour déterminer si un entier est déjà dans le tableau *)
let rec est_dans_tableau number tableau i =
	if (i=Array.length tableau) then false
	else if tableau.(i) = number then true
	else est_dans_tableau number tableau (i+1);;
	
(* Renvoie une liste contenant de façon aléatoire tous les numéros de 0 à n-1*)
let classement_aleatoire n = 
	let a =Array.make n (-1) and test=ref false in
		for i = 0 to n-1 do 
			begin
				test:=false;
				while (!test=false) do
					let aleat=Random.int n in if (est_dans_tableau aleat a 0) then () else test:= true; a.(i)<-aleat

				done
			end
		done;
		Array.to_list a;;

let rec creation_aleatoire_liste liste tab1 =
	match liste with 
	|[] -> []
	|h::t -> tab1.(h).(0)::tab1.(h).(1)::tab1.(h).(2)::tab1.(h).(3)::(creation_aleatoire_liste t tab1);;

(* fonction principale pour la génération alétoire d'une liste de pièces*)
let finale n = let a = random_generation n and l= classement_aleatoire (n*n) in n::(creation_aleatoire_liste l a);;
		
  
(* ouvre un fichier "pieces.txt" et appelle la fonction lire ou appelle la fonction permettant de générer un tetravex aleatoire*)
let tableau_valeurs aleatoire n =
	if (!aleatoire = false) then
		let f = open_in "pieces.txt" in lire f
	else 
		finale n;;


	
(* Lecture du fichier pour connaitre la taille de la grille *)	
let tab = tableau_valeurs (aleatoire) 3;;

let taille_grille = List.hd tab;;

let valeur_pieces aleatoire = List.tl (tableau_valeurs aleatoire taille_grille);;

let nb_cases = taille_grille * taille_grille;;

let cote = 360/taille_grille;;
	
(******************** type box et fonctions servant a dessiner les grilles ********************)
(* cree une liste de couples de coordonnees pour afficher une grille *)
let rec creer_grille nb_col n (x,y) =
	if n < 0 then []
	else
		let new_x = x + (n mod nb_col)*cote
		and new_y = y + (n / nb_col)*cote in
		(new_x,new_y) :: (creer_grille nb_col (n-1) (x,y));;

(* dessine une case à partir des coordonnées du coin inférieur gauche contenues dans *)
(* un couple d'entiers en calculant les coordonnees de la bordure*)
let draw_box (x,y) =
	let x1=x and y1=y in
	let x2=x1+cote and y2=y1+cote in
	let ix1=x1+3 and ix2=x2-3
	and iy1=y1+3 and iy2=y2-3 in
	set_color (rgb 70 140 0);
	fill_rect ix1 iy1 (ix2-ix1) (iy2-iy1);
	set_color (rgb 81 163 0);
	fill_poly [|(x1,y1);(ix1,iy1);(ix2,iy1);(ix2,iy2);(x2,y2);(x2,y1)|];
	set_color (rgb 69 125 0);
	fill_poly [|(x1,y1);(ix1,iy1);(ix1,iy2);(ix2,iy2);(x2,y2);(x1,y2)|];;

(* dessine une grille en appliquant draw_box a tous les elements d'une int*int list *)
let draw_grid (l: (int*int) list) =
	List.iter draw_box l;;

(* créée une liste pour le plateau de jeu et une pour la réserve *)
let board =
	let b = (100,100) in
	creer_grille taille_grille (nb_cases-1) b;;	

let reserve =
	let b = (660,100) in
	creer_grille taille_grille (nb_cases-1) b;;


(******************** type piece et fonctions de creation et d'affichage des pieces ********************)
(* le type piece contient les coordonnees du coin inferieur droit, la largeur d'une piece pour l'affichage *)
(* les valeurs de ses cotes, un caractere disant si elle est en jeu ou dans la reserve et sa position dans la grille *)
type piece =
{
	mutable x:int; mutable y:int;
	id: int; top:int; right:int; bot:int; left:int;
	mutable grille:char; mutable position:int
};;

(* cree une liste de pieces a partir d'une piece de base *)
let rec create_piece_list nb_col n piece tab =
	match tab with
	 [] -> []
	|a::b::c::d::tl ->
					let new_x = piece.x + (n mod nb_col)*cote
					and new_y = piece.y + (n / nb_col)*cote in
					let new_piece = {piece with x=new_x; y=new_y; bot=a; left=b; top=c; right=d; id=nb_cases-1-n; position=nb_cases-1-n} in
					new_piece :: (create_piece_list nb_col (n-1) piece tl);;

(* renvoie un couple contenant la couleur associee a une valeur et un couple contenant la couleur des bordures *)
let color_of_number a =
		match a with
		 0 -> (rgb 0 190 140,(rgb 0 210 155,rgb 0 175 128))
		|1 -> (rgb 230 0 0,(rgb 250 0 0,rgb 215 0 0))
		|2 -> (rgb 0 115 230,(rgb 0 125 250,rgb 0 107 215))
		|3 -> (rgb 230 230 0,(rgb 250 250 0,rgb 215 215 0))
		|4 -> (rgb 190 190 190,(rgb 210 210 210,rgb 175 175 175))
		|5 -> (rgb 255 255 255,(rgb 255 255 255,rgb 240 240 240))
		|6 -> (rgb 205 0 205,(rgb 225 0 225,rgb 190 0 190))
		|7 -> (rgb 230 140 0,(rgb 250 153 0,rgb 215 131 0))
		|8 -> (rgb 130 230 0,(rgb 141 250 0,rgb 122 215 0))
		|9 -> (rgb 180 90 0,(rgb 200 100 0,rgb 165 82 0));;

(* dessine une piece en calculant les coordonnees de la bordure *)	
let draw_piece p =
	let cx = p.x + cote/2 and cy = p.y + cote/2
	and x2 = p.x + cote and y2 = p.y + cote	 in
	begin
		let col = color_of_number p.top in
		set_color (fst col);
		fill_poly [|(p.x,y2);(x2,y2);(cx,cy)|];
		set_color (fst (snd col));
		fill_poly [|(cx,cy);(cx,cy+2);(p.x+2,y2-1);(x2-2,y2-1);(x2,y2);(p.x,y2)|];
		set_color (snd (snd col));
		fill_poly [|(cx,cy);(cx,cy+2);(x2-2,y2-1);(x2,y2)|];
		set_color black;
		let s = string_of_int p.top in
		let (w,h) = text_size s in
		moveto (cx-w/2) (cy + (y2-cy)/2 - h/2);
		draw_string s;
	end;
	begin
		let col = color_of_number p.right in
		set_color (fst col);
		fill_poly [|(x2,y2);(x2,p.y);(cx,cy)|];
		set_color (fst (snd col));
		fill_poly [|(x2,y2);(x2-1,y2-2);(cx+2,cy);(x2-1,p.y+2);(x2,p.y);(cx,cy)|];
		set_color (snd (snd col));
		fill_poly [|(x2,y2);(x2-1,y2-2);(x2-1,p.y+2);(x2,p.y)|];
		set_color black;
		let s = string_of_int p.right in
		let (w,h) = text_size s in
		moveto (cx + (x2-cx)/2 - w/2) (cy-h/2);
		draw_string s;
	end;
	begin
		let col = color_of_number p.bot in
		set_color (fst col);
		fill_poly [|(p.x,p.y);(x2,p.y);(cx,cy)|];
		set_color (fst (snd col));
		fill_poly [|(p.x,p.y);(p.x+2,p.y+1);(cx,cy-2);(cx,cy)|];
		set_color (snd (snd col));
		fill_poly [|(p.x,p.y);(p.x+2,p.y+1);(x2-2,p.y+1);(cx,cy-2);(cx,cy);(x2,p.y)|];
		set_color black;
		let s = string_of_int p.bot in
		let (w,h) = text_size s in
		moveto (cx-w/2) (cy - (y2-cy)/2 - h/2);
		draw_string s;
	end;
	begin
		let col = color_of_number p.left in
		set_color (fst col);
		fill_poly [|(p.x,p.y);(p.x,y2);(cx,cy)|];
		set_color (fst (snd col));
		fill_poly [|(p.x,p.y);(p.x+1,p.y+2);(p.x+1,y2-2);(p.x,y2)|];
		set_color (snd (snd col));
		fill_poly [|(p.x,p.y);(p.x+1,p.y+2);(cx-2,cy);(p.x+1,y2-2);(p.x,y2);(cx,cy)|];set_color black;
		let s = string_of_int p.left in
		let (w,h) = text_size s in
		moveto (cx - (x2-cx)/2 - w/2) (cy-h/2);
		draw_string s;
	end;;

(* dessine toutes les pieces en appliquant draw_piece a une liste de pieces *)
let draw_all_pieces (p:piece list) =
	List.iter draw_piece p;;


					
(******************** fonctions pour la resolution du tetravex ********************)

(* vérifie si une pièce a est dans la liste de pièces l *)
let rec danslaliste a l =
 match l with
 [] -> false
 | h::t -> if a.id=h.id then true else danslaliste a t;;
 
(* renvoie la pièce de l[n] *)
let rec piece_n n l = 
	match l with
	[] -> raise Not_found
	|h::t -> if n = 0 then h else piece_n (n-1) t;;

(*liste de liste pour stocker les solutions *)
let l = ref ([] : piece list list);;

(* fonction calculant le nombre de solutions *)
let rec bdd p k sous_arbre piece_list= 
	if p = nb_cases
	then
		begin
			l := sous_arbre :: !l;
			1
		end
	else
		if k = nb_cases then 0
		else 
			let piece = piece_n k piece_list in
			if danslaliste piece sous_arbre
			then
				bdd p (k+1) sous_arbre piece_list
			else
				match p with
				0 -> (bdd (p+1) 0 (piece::sous_arbre) piece_list) + (bdd p (k+1) sous_arbre piece_list)
				|x when x/taille_grille = 0 ->
					let piece_r = piece_n (p-1) sous_arbre in
					if piece.right = piece_r.left
					then
						(bdd (p+1) 0 (sous_arbre@[piece]) piece_list) + (bdd p (k+1) sous_arbre piece_list)
					else
						 bdd p (k+1) sous_arbre piece_list
				|x when x mod taille_grille = 0 ->
					let piece_t = piece_n (p - taille_grille) sous_arbre in
					if piece.top = piece_t.bot
					then
						(bdd (p+1) 0 (sous_arbre@[piece]) piece_list) + (bdd p (k+1) sous_arbre piece_list)
					else
						bdd p (k+1) sous_arbre piece_list
				|_ ->
					let piece_r = piece_n (p-1) sous_arbre and piece_t = piece_n (p - taille_grille) sous_arbre in
					if piece.right = piece_r.left && piece.top = piece_t.bot
					then
						(bdd (p+1) 0 (sous_arbre@[piece]) piece_list) + (bdd p (k+1) sous_arbre piece_list)
					else
						bdd p (k+1) sous_arbre piece_list;;


(******************** fonction d'affichage du jeu ********************)
let chaine_solutions resultat= 
	match resultat with
	0 -> "Ce Tetravex n'a pas de solution."
	|1 -> "Ce Tetravex n'a qu'une solution."
	|_ -> "Ce tetravex a "^(string_of_int resultat)^" solutions."

let afficher_solutions resultat =
	let (w,h) = text_size (chaine_solutions resultat) in
	moveto (560 - w/2) (50 - h/2);
	set_color white;
	draw_string (chaine_solutions resultat);;
	
let afficher_chrono () =
	let (w,h) = text_size "Chrono : 0.000" in
	moveto (560 - w/2) (25 - h/2);
	set_color white;
	let a = Sys.time()-. !clock in
	draw_string ("Chrono : "^(string_of_float a));;

(* Dessine le boutton aléatoire *)
let afficher_button_raz () =
	set_color (rgb 90 18 18);
	fill_rect 743 485 200 50;
	set_color (rgb 64 6 6);
	fill_poly [|(740,482);(740,538);(946,538);(943,535);(743,535);(743,485)|];
	set_color (rgb 138 53 53);	
	fill_poly [|(740,482);(946,482);(946,538);(943,535);(943,485);(743,485)|];
	let (w,h) = text_size ("ALEATOIRE") in
	moveto (843 - w/2) (510 - h/2);
	set_color white;
	draw_string ("ALEATOIRE");;

let afficher_jeu resultat piece_list =
	clear_graph ();
	set_window_title "Tetravex";
	remplir_fond ();
	draw_fleche ();
	afficher_button_raz ();
	draw_grid board;
	draw_grid reserve;
	afficher_solutions resultat;
	afficher_chrono ();
	draw_all_pieces piece_list;;


(******************** exceptions et fonctions du systeme de jeu ********************)
exception Fin;;
exception Not_found;;
exception New_game;;

(* Vérifie si le joueur clique sur le boutton ALEATOIRE *) 
let est_sur_bouton (x,y) =
	if (x>=740) && (x<=946) && (y>=482) && (y<=538) then true else false;;
	
(* teste si une coordonnee est sur le plateau de jeu *)
let is_on_board (x,y) = if x<100 || x>460 || y<100 || y>460 then false else true;;

(* teste si une coordonnee est sur la reserve de pieces *)
let is_in_reserve (x,y) = if x<660 || x>1020 || y<100 || y>460 then false else true;;

(*attend que l'on relache le bouton de la souris et renvoie ses coordonnees *)
let deplacement () = let s = wait_next_event [Button_up] in (s.mouse_x,s.mouse_y);;

(* parcours la liste des pieces et verifie que toutes sont sur le plateau *)
let rec game_won l =
	match l with
		[x] -> if x.grille = 'b' then true else false
		|h::t -> if h.grille = 'r' then false else game_won t;;

(* Lit le fichier highscore.txt et renvoie une liste de string *)
let rec lecture_highscore fichier =
  try 
    let i = input_line fichier in
		i:: (lecture_highscore fichier)
  with End_of_file -> [];;

 (* Determine si le joueur se positionne dans le top 5 *)
 let modification_highscore time name =
	let f = open_in "highscore.txt" in let liste = lecture_highscore f in let k = close_in f in
	let rec entre l i = 
		match l with
		|[]-> [] 
		|h::t -> if (i mod 2 = 0)&&(time < (float_of_string h)) then List.rev (List.tl(List.tl(List.rev ((string_of_float (time))::(name)::l))))
				else h::(List.hd t)::entre (List.tl t) (i+2);
	in entre liste 0;;

(* Permet de mettre à jour le fichier "highscore.txt" *)
let rec reecriture_highscore liste =
	let f = open_out "highscore.txt" in 
	let rec ecrire l =
		match l with
		|[]-> ()
		|h::t -> let j = output_string f (h^"\n") in ecrire t;
	in let a = ecrire liste in close_out f;;
	
	
(* affiche l'ecran de victoire, peret d'entrer son nom pour les highscor,et propose de recommencer en mode aleatoire *)
let victoire () = 
	while (!sortie) do
		clear_graph ();
		remplir_fond ();
		afficher_button_raz ();
		let s = "Felicitations, vous avez résolu le Tetravex : "^string_of_float(!timer)^" secondes !" in
		let (w,h) = text_size s in
		moveto (360 - w/2) (510 - h/2);
		set_color white;
		draw_string s;
		let (w,h) = text_size "/!\ Veuillez entrer votre nom : /!\ " in
		moveto (300 - w/2) (400 - h/2);
		draw_string "/!\\ Veuillez entrer votre nom : /!\\ ";
		let (w,h) = text_size !nom in
		moveto (300 - w/2) (300 - h/2);
		draw_string (!nom);
		let h = wait_next_event [Key_pressed] in
		nom := (!nom)^(String.make 1(h.key));
		if (int_of_char (h.key) = 8) then
			nom:= String.sub (!nom) 0 ((String.length(!nom))-2);
		if (int_of_char (h.key) = 9 || int_of_char (h.key) = 13 || int_of_char (h.key) = 27) then sortie:=false else ()
	done;
	let liste_hs = modification_highscore !timer !nom in
	moveto (500)(350);
	draw_string ("HIGHSCORE DE LA MORT !");
	moveto (500)(25);
	draw_string ("Cliquez sur le boutton \"ALEATOIRE\" pour recommencer, ou sur ESC pour quitter");
	let rec parcourir l dep =
		match l with
		|[] -> ()
		| h::t -> begin
					moveto (500)(300-(dep*20));
					draw_string ((string_of_int dep)^") "^(List.hd t)^" -> "^h^" s");
					parcourir (List.tl t) (dep+1)
					end;
	in parcourir liste_hs 1;
	reecriture_highscore liste_hs;
	sortie:=true;
	while (!sortie) do
			let s = wait_next_event [Key_pressed; Button_down] in
				if (est_sur_bouton (s.mouse_x,s.mouse_y)) then raise New_game 
				else if (int_of_char (s.key) = 27) then begin sortie:=false; test:=true; end
				else ()
	done;;
		
(* renvoie la piece de plist contenue a l'emplacement de coordonnees (x,y) *)
let rec piece_from_xy (x,y) plist=
	match plist with
	[] -> raise Not_found
	|h::t -> if (x>=h.x) && (x<h.x+cote) && (y>=h.y) && (y<h.y+cote) then h else piece_from_xy (x,y) t;;

(* verifie que l'on peut mettre la piece p en jeu à la position n en verifiant les valeurs des pieces adjacentes *)
let rec tester_voisin n p plist =
	match plist with
		[] -> true
		|h::t -> 
		begin
			let vertical = h.position/taille_grille - n/taille_grille and horizontal = h.position mod taille_grille - n mod taille_grille in
			if (h.id <> p.id) && h.grille='b' && ((vertical = 1 && horizontal = 0 && h.top<>p.bot) || (vertical = -1 && horizontal = 0&& h.bot<>p.top) || (horizontal = 1 && vertical = 0 && h.right<>p.left) || (horizontal = -1 && vertical = 0 && h.left<>p.right)) then false else tester_voisin n p t
		end;;

(* renvoie le numero de la case de coordonnees (x,y) avec des cases de largeur n *)
let case_grille (x,y) = if x >= 660
						then
							let xr = (x-660)/cote
							and yr = (y-100)/cote
							in xr + taille_grille*yr
						else
							let xr = (x-100)/cote
							and yr = (y-100)/cote
							in xr + taille_grille*yr;;


(* verifie si l'on peut deplacer la piece p aux coordonnees (x,y) et effectue le deplacement si il est valide -> si il n'y a pas de piece deja presente *)
(* a l endroit indique alors l exception levee se charge d effectuer le deplacement *)
let deplacement_piece p (x,y) plist =
	if is_in_reserve (x,y)
	then
		try
			let c = piece_from_xy (x,y) plist in raise Fin
		with
		Not_found -> let n = case_grille (x,y) in p.grille<-'r'; p.x<-(660+ (n mod taille_grille)*cote); p.y<-(100+n/taille_grille*cote); p.position<- (nb_cases-1)-n;
		|Fin ->  ()
	else
		if is_on_board (x,y)
		then
			try
				let c = piece_from_xy (x,y) plist in raise Fin
			with
			Not_found -> let n = case_grille (x,y) in
						if (tester_voisin ((nb_cases-1)-n) p plist)
						then
							begin
								p.grille<-'b';
								p.x<-(100+ (n mod taille_grille)*cote);
								p.y<-(100+n/taille_grille*cote);
								p.position<- (nb_cases-1)-n;
							end;
			|Fin ->  ();;


(* boucle principale du jeu. On attend un clic de souris, si il est sur une des pieces on calcule*)
(* la position de la souris quand on la relache et on voit si l'on peut deplacer la piece *)
(*on lève deux exception différentes une orsque l'on a résolu le Tetravex, et une autre quand on souhaite reommencer en mode aléatoire*)
let boucle_jeu () = 
	while !test=false do
		let piece_list =
		let p = {x=660; y=100; id=1; bot=1; left=1; top=1; right=1; grille='r'; position=nb_cases} in
			create_piece_list taille_grille (nb_cases-1) p (valeur_pieces aleatoire) in 
				let resultat = bdd 0 0 [] piece_list in
					afficher_jeu resultat piece_list;
			
			try 
				while true do
					let s = wait_next_event [Button_down] in
					try
						if (est_sur_bouton (s.mouse_x,s.mouse_y)) then
							raise New_game
						else
							let piece_depart = piece_from_xy (s.mouse_x,s.mouse_y) piece_list in
							let pos_finale = deplacement () in
							deplacement_piece piece_depart pos_finale piece_list;
							afficher_jeu resultat piece_list;
							if game_won piece_list
							then
								raise Fin
					with 
						Not_found -> ();
				done;
			with Fin -> begin
						nom := "";
						sortie := true;
						timer := Sys.time() -. !clock;
						try
							victoire ()
						with New_game -> aleatoire:=true;
								clock := Sys.time ();
						end
				|New_game -> aleatoire:=true;
				clock := Sys.time ();
	done;;

open_graph " 1120x600";;
boucle_jeu ();;

