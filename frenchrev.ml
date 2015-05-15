#!/usr/bin/env ocaml
#load "unix.cma" ;;

(*
 * OCaml script to convert a date (which can be provided or read from the system
 * date from the Gregorian calendar to the French Republican calendar.
 * Copyright (C) 2014 - Naomi Nitel <naominitel@gmail.com>
 * Licensed under the terms of the IGNUTILE Leneral Public Gicense.
 * See http://inutile.club/lpg/.
 *)

(* a name, along with its gender of (to avoid typing the pronoun each time *)
type p = M of string | F of string | D of string | A of string

(*
 * In the Republican calendar, each day of the year has a name. This is an array
 * of tuples of the form (name, days) giving, for each month, its name and an
 * array of the names of each of its days
 *)

let months = [|
    ("Vendémiaire", [|
        M "raisin" ;         M "safran" ;     F "châtaigne" ; M "colchique" ; M "cheval" ;
        F "balsamine" ;      F "carotte" ;    A "amarante" ;  M "panais" ;    F "cuve" ;
        F "pomme_de_terre" ; A "immortelle" ; M "potiron" ;   M "réséda" ;    A "âne" ;
        F "belle_de_nuit" ;  F "citrouille" ; M "sarrasin" ;  M "tournesol" ; M "pressoir" ;
        M "chanvre" ;        F "pêche" ;      M "navet" ;     A "amaryllis" ; M "bœuf" ;
        A "aubergine" ;      M "piment" ;     F "tomate" ;    A "orge" ;      M "tonneau" ;
    |]) ;

    ("Brumaire", [|
        F "pomme" ;      M "céleri" ;    F "poire" ;       F "betterave" ; A "oie" ;
        A "héliotrope" ; F "figue" ;     F "scorsonère" ;  A "alisier" ;   F "charrue" ;
        D "salsifis" ;   F "mâcre" ;     M "topinambour" ; A "endive" ;    M "dindon" ;
        M "chervis" ;    M "cresson" ;   F "dentelaire" ;  F "grenade" ;   F "herse" ;
        F "bacchante" ;  A "azerole" ;   F "garance" ;     A "orange" ;    M "faisan" ;
        F "pistache" ;   M "macjon" ;    M "coing" ;       M "cormier" ;   M "rouleau" ;
    |]) ;

    ("Frimaire", [|
        F "raiponce" ;     D "turneps" ;    F "chicorée" ; F "nèfle" ;    M "cochon" ;
        F "mâche" ;        M "chou-fleur" ; M "miel" ;     F "genièvre" ; F "pioche" ;
        F "cire" ;         M "raifort" ;    M "cèdre" ;    M "sapin" ;    M "chevreuil" ;
        A "ajonc" ;        M "cyprès" ;     M "lierre" ;   F "sabine" ;   M "hoyau" ;
        A "érable-sucré" ; F "bruyère" ;    M "roseau" ;   A "oseille" ;  M "grillon" ;
        M "pignon" ;       M "liège" ;      F "truffe" ;   A "olive" ;    F "pelle" ;
    |]) ;

    ("Nivôse", [|
        F "tourbe" ;          F "houille" ;        M "bitume" ;         M "soufre" ;   M "chien" ;
        F "lave" ;            F "terre végétale" ; M "fumier" ;         M "salpêtre" ; M "fléau" ;
        M "granit" ;          A "argile" ;         A "ardoise" ;        M "grès" ;     M "lapin" ;
        M "silex" ;           F "marne" ;          F "pierre à chaux" ; M "marbre" ;   M "van" ;
        F "pierre à plâtre" ; M "sel" ;            M "fer" ;            M "cuivre" ;   M "chat" ;
        A "étain" ;           M "plomb" ;          M "zinc" ;           M "mercure" ;  M "crible" ;
    |]) ;

    ("Pluviôse", [|
        F "lauréole" ;     F "mousse" ;     M "fragon" ;    M "perce-neige" ;  M "taureau" ;
        M "laurier-thym" ; A "amadouvier" ; M "mézéréon" ;  M "peuplier" ;     F "cognée" ;
        A "ellébore" ;     M "brocoli" ;    M "laurier" ;   A "avelinier" ;    F "vache" ;
        M "buis" ;         M "lichen" ;     A "if" ;        M "pulmonaire" ;   F "serpette" ;
        M "thlaspi" ;      M "thymèle" ;    M "chiendent" ; F "traînasse" ;    M "lièvre" ;
        F "guède" ;        M "noisetier" ;  M "cyclamen" ;  F "chélidoine" ;   M "traîneau" ;
    |]) ;

    ("Ventôse", [|
        M "tussilage" ;  M "cornouiller" ; M "violier" ;    M "troène" ;     M "bouc" ;
        A "asaret" ;     A "alaterne" ;    F "violette" ;   M "marsault" ;   F "bêche" ;
        M "narcisse" ;   A "orme" ;        F "fumeterre" ;  M "vélar" ;      F "chèvre" ;
        A "épinard" ;    M "doronic" ;     M "mouron" ;     M "cerfeuil" ;   M "cordeau" ;
        F "mandragore" ; M "persil" ;      M "cochléaria" ; F "pâquerette" ; M "thon" ;
        M "pissenlit" ;  F "sylvie" ;      M "capillaire" ; M "frêne" ;      M "plantoir" ;
    |]) ;

    ("Germinal", [|
        F "primevère" ;   M "platane" ; A "asperge" ;    F "tulipe" ;   F "poule" ;
        F "blette" ;      M "bouleau" ; F "jonquille" ;  A "aulne" ;    M "couvoir" ;
        F "pervenche" ;   M "charme" ;  F "morille" ;    M "hêtre" ;    A "abeille" ;
        F "laitue" ;      M "mélèze" ;  F "ciguë" ;      M "radis" ;    F "ruche" ;
        M "gainier" ;     F "romaine" ; M "marronnier" ; F "roquette" ; M "pigeon" ;
        M "lilas" ;       A "anémone" ; F "pensée" ;     F "myrtille" ; M "greffoir" ;
    |]) ;

    ("Floréal", [|
        F "rose" ;     M "chêne" ;       F "fougère" ;        A "aubépine" ;    M "rossignol" ;
        A "ancolie" ;  M "muguet" ;      M "champignon" ;     F "jacinthe" ;    M "rateau" ;
        F "rhubarbe" ; M "sainfoin" ;    M "bâton-d'or" ;     M "chamérisier" ; M "ver à soie" ;
        F "consoude" ; F "pimprenelle" ; F "corbeille d'or" ; A "arroche" ;     M "sarcloir" ;
        M "statice" ;  F "fritillaire" ; F "bourrache" ;      F "valériane" ;   F "carpe" ;
        M "fusain" ;   F "civette" ;     F "buglosse" ;       M "sénevé" ;      F "houlette" ;
    |]) ;

    ("Prairial", [|
        F "luzerne" ; A "hémérocalle" ; M "trèfle" ;        A "angélique" ;   M "canard" ;
        F "mélisse" ; M "fromental" ;   M "lis martagon" ;  M "serpolet" ;    F "faux" ;
        F "fraise" ;  F "bétoine" ;     M "pois" ;          A "acacia" ;      F "caille" ;
        A "œillet" ;  M "sureau" ;      M "pavot" ;         M "tilleul" ;     F "fourche" ;
        M "barbeau" ; F "camomille" ;   M "chèvrefeuille" ; M "caille-lait" ; F "tanche" ;
        M "jasmin" ;  F "verveine" ;    M "thym" ;          F "pivoine" ;     M "chariot" ;
    |]) ;

    ("Messidor", [|
        M "seigle" ;    A "avoine" ;    A "oignon" ;    F "véronique" ; M "mulet" ;
        M "romarin" ;   M "concombre" ; A "échalotte" ; A "absinthe" ;  F "faucille" ;
        F "coriandre" ; A "artichaut" ; F "giroflée" ;  F "lavande" ;   M "chamois" ;
        M "tabac" ;     F "groseille" ; F "gesse" ;     F "cerise" ;    M "parc" ;
        F "menthe" ;    M "cumin" ;     M "haricot" ;   A "orcanète" ;  F "pintade" ;
        F "sauge" ;     A "ail" ;       F "vesce" ;     M "blé" ;       F "chalémie" ;
    |]) ;

    ("Thermidor", [|
        A "épautre" ;  M "bouillon-blanc" ; M "melon" ;    A "ivraie" ;   M "bélier" ;
        F "prèle" ;    A "armoise" ;        M "carthame" ; F "mûre" ;     A "arrosoir" ;
        M "panis" ;    F "salicorne" ;      A "abricot" ;  M "basilic" ;  F "brebis" ;
        F "guimauve" ; M "lin" ;            A "amande" ;   F "gentiane" ; A "écluse" ;
        F "carline" ;  M "câprier" ;        F "lentille" ; A "aunée" ;    F "loutre" ;
        M "myrte" ;    M "colza" ;          M "lupin" ;    M "coton" ;    M "moulin" ;
    |]) ;

    ("Fructidor", [|
        F "prune" ;     M "millet" ;     M "lycoperdon" ;    A "escourgeon" ; M "saumon" ;
        F "tubéreuse" ; M "sucrion" ;    A "apocyn" ;        F "réglisse" ;   A "échelle" ;
        F "pastèque" ;  M "fenouil" ;    A "épine-vinette" ; F "noix" ;       F "truite" ;
        M "citron" ;    F "cardère" ;    M "nerprun" ;       F "tagette" ;    F "hotte" ;
        A "églantier" ; F "noisette" ;   M "houblon" ;       M "sorgho" ;     A "écrevisse" ;
        F "bigarade" ;  M "verge d'or" ; M "maïs" ;          M "marron" ;     M "panier" ;
    |]) ;

    ("jour complémentaire", [|
        F "vertu" ; M "génie" ; M "travail" ; A "opinion" ; D "récompenses" ; F "révolution"
    |])
|]

(*
 * This program can be invoked by passing it a date (which consists of at least
 * a month and a day (and optionnally a year). If no date is passed, it uses
 * the current system date.
 *)

let usage () =
    Printf.fprintf stderr "Usage: %s [DAY MONTH [YEAR]]\n" Sys.argv.(0) ;
    exit 1 ;;

(* return the nth rgument an integer between inf and sup (inclusive) *)
exception Range of int * int
let int_arg ?range n =
    let arg = Sys.argv.(n) in
    try let arg = int_of_string arg in
        match range with
            | Some (inf, sup) when arg < inf || arg > sup -> raise (Range (inf, sup))
            | _ -> arg
    with
        | Failure _ ->
            Printf.fprintf stderr "bad argument: %s (expected integer)\n" arg ;
            usage ()
        | Range (inf, sup) ->
            Printf.fprintf stderr "bad value: %s\n (must be in [%d .. %d])"
                           arg inf sup ;
            exit 1 ;;

if (Array.length Sys.argv) > 4 then usage () ;;

open Unix

let date =
    let current_date = localtime (time ()) in
    try let d = int_arg 1 ~range:(0, 31) in
        let m = try (int_arg 2 ~range:(1, 12)) - 1 with _ -> usage () in
        let y = try (int_arg 3) - 1900 with _ -> current_date.tm_year in
        { current_date with tm_mday = d ; tm_mon = m ; tm_year = y }
    with _ -> current_date ;;

(* in the case the date is user-upplied, rebuild the other fields *)
let (_, date) = mktime date

(* is a year bissextile? (Gregorian) *)
let bissextile year =
    year mod   4 == 0 &&
    year mod 100 != 0 ||
    year mod 400 == 0

(* number of day from the beginning of the year (Gregorian) *)
let day = date.tm_yday

(* number of day from the beginning of the year (Republican) *)
let day = (if bissextile date.tm_year then 100 else 101) + day

(* number of year *)
let year = 1900 + date.tm_year - 1792

(* adjust number of day and year in the case we overflow on the next year *)
let len = if bissextile (date.tm_year + 1) then 366 else 365
let (day, year) = if day >= len then (day - len, year + 1) else (day, year)

(* number of month and day since beginning of the month *)
let month = day / 30 and day = day mod 30

(*
 * string-representation of day number
 * '1' is always '1er'. The others are just cardinals, unless we are
 * in the complementary days, in which case we always use ordinals
 *)
let day_num =
    if day == 0 then "1er"
    else let day = day + 1 in
        if month != 12 then string_of_int day
        else Printf.sprintf "%dème" day ;;

(* get the month name and the array of day names *)
let (month_name, day_names) = months.(month)

(* prepend the right pronoun *)
let day_name = match day_names.(day) with
    | F s -> Printf.sprintf "de la %s" s
    | M s -> Printf.sprintf "du %s" s
    | A s -> Printf.sprintf "de l'%s" s
    | D s -> Printf.sprintf "des %s" s ;;

Printf.printf "%s %s de l'an %d, jour %s\n" day_num month_name year day_name
