// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

type ksiazka={tytul:string; autor:string; rok:int}

let szukajWydane(l:ksiazka list)=
    List.filter(fun x->x.rok>=1970 && x.rok<=1980) l

type dlugosc=
    |METR of float
    |PIEDZE of float
    |LOKCIE of float
    |CWIERCI of float

let kon(d:dlugosc):dlugosc=
    match d with
    |CWIERCI c->METR (c*0.1489)
    |S->S
let zlicz(l:dlugosc list)=
    let z1=List.choose(fun x->match x with
                              |METR m->Some m
                              |_->None) l
    let z2=List.choose(fun x->match x with
                              |PIEDZE m->Some m
                              |_->None) l
    let z3=List.choose(fun x->match x with
                              |LOKCIE m->Some m
                              |_->None) l
    let z4=List.choose(fun x->match x with
                              |CWIERCI m->Some m
                              |_->None) l
    [List.sum z1;List.sum z2;List.sum z3;List.sum z4]

let szukaj4Potegi(l:float list)=
    List.filter(
                let rec isK i v=if float(i)/4.0 < v then Math.Pow(float(i),4.0)=v || isK (float(i+1.0)) v else false    
                fun x->isK 0.0 x
                )l

type Warzywo(k,c)=
    inherit Object()
    let mutable kategoria:string=k
    let mutable cena:float=c    
    member this.Kategoira with get()=kategoria
    member this.Cena with get() = cena and set(value:float)= cena <- value

type Pietruszka(k,c)=
    inherit Warzywo(k,c)
    [<DefaultValue>]val mutable dataZbioru:string
let rec silinia(n:int)=
    match n with
    |0 ->1
    |1 ->1
    |_ -> n*(silinia(n-1))
let seq=Seq.initInfinite(fun k->(Math.Sqrt(float(k))+Math.Pow(float(k),2.0))/float(silinia(k)))    

[<EntryPoint>]
let main argv =
    //let cb = (fun a b -> let d = a * b in d-4 * a ) 1 5 
    //let xb = cb - 6                  
    //printfn "%A" xb
    //let z =ref 45
    //Seq.iter(fun x-> z := !z - x)[1..9]
    //printfn "%A" !z
    //let listaKsiazek = [
    //                    {tytul = "Ksiazka1";autor = "JanKowalski1";rok = 1969};
    //                    {tytul = "Ksiazka2";autor = "JanKowalski2";rok = 1970};
    //                    {tytul = "Ksiazka3";autor = "JanKowalski3";rok = 1972};
    //                    {tytul = "Ksiazka4";autor = "JanKowalski4";rok = 1975};
    //                    {tytul = "Ksiazka5";autor = "JanKowalski5";rok = 1980};
    //                    {tytul = "Ksiazka6";autor = "JanKowalski6";rok = 1985};
    //                    {tytul = "Ksiazka7";autor = "JanKowalski7";rok = 1999};
    //                    ]
    //printfn "%A" <| szukajWydane listaKsiazek
    //let listaD=[
    //    METR 4.3;
    //    PIEDZE 2.1;
    //    LOKCIE 4.2;
    //    CWIERCI 1.3;
    //    METR 6.5;
    //    PIEDZE 3.2;
    //    LOKCIE 4.1;
    //    CWIERCI 5.6;
    //]
    //printfn "%A" <| zlicz listaD
    let listR = [1.4;2.5;3.2;11.7;16.0;81.0;36.0]
    printfn "%A" <| szukaj4Potegi listR
    printfn "%A" <| seq 
    
    0 // return an integer exit code