
open System
open System.IO

open System.Xml.Linq
open System.Globalization

// Helpers
let cdate str = DateTime.ParseExact(str, "yyyy-MM-dd", CultureInfo.CurrentCulture)

let xn = XName.Get
let xns ns n = XName.Get(n, $"http://www.gnucash.org/XML/{ns}")

let get ns n (xelement: XElement) = xelement.Descendants (xns ns n) |> Seq.head |> fun x -> x.Value
let optGet ns n (xelement: XElement) = xelement.Descendants (xns ns n) |> Seq.tryHead |> Option.map (fun x -> x.Value)


let scaled (value: string) =
  let n = value.IndexOf '/'
  if n <> -1
    then let p1 = value.Substring(0, n) |> decimal
         let p2 = value.Substring(n+1) |> decimal
         p1 / p2
    else decimal value

let parse1 (inputFn: string) =
  let xml = XDocument.Load (inputFn, LoadOptions.SetLineInfo)

  // need to rejoin accounts up to full size
  let accounts0 = xml.Descendants (xns "gnc" "account")
                    |> Seq.map (fun account -> let guid = get "act" "id" account
                                               let name = get "act" "name" account
                                               let parent = optGet "act" "parent" account
                                               guid, (name, parent))
                    |> Map.ofSeq
  let rec collectAccount acc hierarchy =
    let (name, parent) = accounts0 |> Map.find acc
    match parent with
      | None -> hierarchy
      | Some p -> match hierarchy with "" -> collectAccount p name | h -> collectAccount p $"{name}:{h}"
  let accounts = accounts0 |> Map.map (fun k _ -> collectAccount k "" |> fun s -> s.Replace(" ", ""))

  let txns = xml.Descendants (xns "gnc" "transaction")
               |> Seq.map (fun txn -> let commodity = get "cmdty" "id" txn
                                      let date = get "trn" "date-posted" txn |> fun x -> x.Substring(0, 10) |> cdate
                                      let payee = get "trn" "description" txn
                                      let narrative = optGet "slot" "value" txn
                                      let postings = txn.Descendants (xns "trn" "split")
                                                       |> Seq.map (fun split -> let account = get "split" "account" split |> fun k -> Map.find k accounts
                                                                                let value = get "split" "value" split
                                                                                let value2 = match value.[0] with | '-' -> None | _ -> Some (scaled value, commodity)
                                                                                account, value2)
                                                       |> Seq.toList
                                      date, payee, narrative, postings)
               |> Seq.toList

  // print it
  let p ((date: DateTime), payee, narrative, postings) =
    let l1 = $"""{date.ToString("yyyy-MM-dd")} {payee} | {Option.defaultValue "(no narrative)" narrative}"""
    let l2 = postings |> List.find (fun (_, x) -> Option.isSome x)
                      |> function (account, Some (value, commodity)) -> $"  {account}  {value} {commodity}"
    let l3 = postings |> List.find (fun (_, x) -> Option.isNone x)
                  |> function (account, _) -> $"  {account}"
    [l1;l2;l3;""]

  txns |> List.collect p

[<EntryPoint>]
let main argv =

  let f (inputFn: string) =
    Console.WriteLine(inputFn)
    let outputFn = Path.ChangeExtension(inputFn, "fledge")
    let parsed = parse1 inputFn
    File.WriteAllLines(outputFn, parsed)

  argv |> Array.iter f

  printfn "Completed."
  0 // return an integer exit code