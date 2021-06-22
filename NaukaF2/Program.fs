// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

let rec znajdz(x:int)( dane:int list)=
    match dane with
    |[]->[]
    |h::t->if x=h then h::(znajdz x t) else znajdz x t

let rec znajdz2(x:int)( dane:int list)=
    match dane with
    |[]->false
    |h::t->if x=h then true else znajdz2 x t
let rec znajdz3(x:int)( dane:int list)(ind:int)=
    match dane with
    |[]-> -1
    |h::t->if x=h then ind else znajdz3 x t ind+1

let rec znajdz4(x:int)( dane:int list)(ind:int)=
    match dane with
    |[]->[]
    |h::t->if x=h then [ind]::znajdz4 x t (ind+1)  else znajdz4 x t (ind+1)
let rec zlicz(x:int)( dane:int list)=
    match dane with
    |[]->0
    |h::t->if x=h then 1 + zlicz x t else zlicz x t

let rec suma(dane:int list)=
    match dane with
    |[]->0
    |h::t-> h+suma t
let  maks(dane :int list)=
    dane |> List.fold(fun acc elem->if acc>elem then acc else elem) (List.head dane)

let rec wstaw(w:int)(l:int list)=
    match l with
    |[]->[w]
    |h::t->if h<w then h::wstaw w t else w::h::t

let rec sortuj(dane:int list)=
    match dane with
    |[]->[]
    |h::t->wstaw h (sortuj t)

[<EntryPoint>]
let main argv =
    let listaLiczb=[4;2;5;2;6;2;6;7;4;3;6;9;5;2]
    let listaLiczb2=[4;2;5]
    printfn "%A" <| (znajdz 2 listaLiczb)
    printfn "%A" <| (znajdz2 2 listaLiczb)
    printfn "%A" <| (zlicz 2 listaLiczb)
    printfn "%A" <| (znajdz4 2 listaLiczb 0)
    printfn "%A" <| (suma listaLiczb2)
    printfn "%A" <| (List.sum<| listaLiczb2)
    printfn "%A" <| (maks listaLiczb2)
    printfn "%A" <| (sortuj listaLiczb)

    0 // return an integer exit code