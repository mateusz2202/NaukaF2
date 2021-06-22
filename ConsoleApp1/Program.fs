// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

type objetosc =
    | LITR of float
    | GALON of float
    | KORZEC of float
    | KONEW of float

let sortuj(l:objetosc list)=
    List.sortBy(fun x->match x with
                       | LITR n->n
                       | GALON n->n
                       | KORZEC n->n
                       | KONEW n-> n
                       ) l
let sumaLiczb(l:float list)=
    l |> List.fold(fun x y-> x+y) 0.0 
let sumaW(l:objetosc list)=
    let z1=List.choose(fun x->match x with
                              | LITR l -> Some l                           
                              | _->None
                             ) l
    let z2=List.choose(fun x->match x with
                              | GALON l -> Some l                           
                              | _->None
                            ) l
    let z3=List.choose(fun x->match x with
                              | KORZEC l -> Some l                           
                              | _->None
                             ) l
    let z4=List.choose(fun x->match x with
                              | KONEW l -> Some l                           
                              | _->None
                            ) l
    [sumaLiczb z1; sumaLiczb z2;sumaLiczb z3; sumaLiczb z4]

let sprawdzCzySz(dane:float list)=
    List.filter(
                let rec isSz i v= if i / 3.0 <= v then Math.Pow(i,3.0)=v || isSz (i+1.0) v else false
                fun x -> isSz 0.0 x
                ) dane
   
let wy = List.filter (
                        let rec isKw i v = if i / 2 <= v then (i*i = v || isKw (i+1) v) else false
                        fun x -> isKw 0 x
                      ) 
type Owoc(s,w)=
    inherit Object()
    let mutable smak:string=s
    let mutable waga:float=w
    member this.Smak
                with get () = smak
                and set (value:string) = smak <- value
    member this.Waga
                with get()=waga

type Gruszka(s,w)=
    inherit Owoc(s,w)
    [<DefaultValue>] val mutable odmiana:string

let rec silinia(n:int)=
    match n with
    |0 ->1
    |1 ->1
    |_ -> n*(silinia(n-1))

let seq=Seq.initInfinite(fun i->(float(silinia(i))/Math.Sqrt((float(i)+1.0))) )

let zwez(s:seq<float>)=
    Seq.filter(fun x->x<1e10) s

    
[<EntryPoint>]
let main argv =
    let listaO=[
        LITR 4.3;
        GALON 2.1;
        KORZEC 4.2;
        KONEW 1.3;
        LITR 6.5;
        GALON 3.2;
        KORZEC 4.1;
        KONEW 5.6;
    ]
    //printfn "%A" listaO
    //printfn "%A" <| sortuj listaO
    //printfn "%A" <| sumaW listaO
    let listR = [1.4;2.5;3.2;11.7;8.0;27.0;36.0]
    printfn "%A" <| sprawdzCzySz listR 
    printfn "%A" <| seq 
    printfn "%A" <| zwez seq 
    let wedata = [1 ; 3; 5; 9; 12; 16; 20; 25; 30; 36; 48] 
    
     
    printfn "Wy: %A" <| wy wedata
    


    0 // return an integer exit code