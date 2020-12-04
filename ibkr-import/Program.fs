open System.IO
open System.Net.Http
open System.Xml.Linq
open System.Threading

open Argu
open System
open System.Globalization

let client = new HttpClient()

// Helpers
let xn = XName.Get
let negate x = -x
let EST = TimeZoneInfo.FindSystemTimeZoneById("Eastern Standard Time")
let HKT = TimeZoneInfo.FindSystemTimeZoneById("China Standard Time")
let est2hkt dt = TimeZoneInfo.ConvertTime(TimeZoneInfo.ConvertTimeToUtc(dt, EST), HKT);;
let cdate str = DateTime.ParseExact(str, "yyyy-MM-dd;HH:mm:ss", CultureInfo.CurrentCulture) |> est2hkt

// Remote query interface to IBKR
let requestUrl token queryId =
  sprintf @"https://gdcdyn.interactivebrokers.com/Universal/servlet/FlexStatementService.SendRequest?t=%s&q=%s&v=3" token queryId

let responseUrl token reponseId =
  sprintf @"https://gdcdyn.interactivebrokers.com/Universal/servlet/FlexStatementService.GetStatement?t=%s&q=%s&v=3" token reponseId

let readUrlAsyncXml (url : string) =
  async {
    let! response = client.GetStringAsync(url) |> Async.AwaitTask
    return response |> XDocument.Parse
  }

let getFlexStatement token queryId =
  printf "Requesting report for IBKR..."
  let acceptedRequestXml = requestUrl token queryId |> readUrlAsyncXml |> Async.RunSynchronously
  let fresponse = acceptedRequestXml.Element(xn "FlexStatementResponse")
  let success = fresponse.Element(xn "Status").Value
  printfn "Response received."
  if success <> "Success" then
    printfn "%A" acceptedRequestXml
    failwith "Bad request from InteractiveBrokers."
  else
    // proceed to get the full response
    printf "Waiting 10s for remote report generation to complete..."
    Thread.Sleep(10000)
    let confirmsUrl = fresponse.Element(xn "ReferenceCode").Value |> responseUrl token
    let rep = Async.RunSynchronously (readUrlAsyncXml confirmsUrl)
    printfn "collected report."
    rep

// Processing and output
type OrderElement =
  {
    orderid: string
    symbol: string
    currency : string
    description: string
    buysell: string
    quantity: decimal
    price: decimal
    multiplier: int

    commission: decimal
    commissionCurrency: string

    executionDate: DateTime
    assetCategory : string
  }

let createTradeFromElement (node : XElement) =
  let a nm = xn nm |> node.Attribute |> fun a -> a.Value
  {
    orderid = a "ibOrderID"
    symbol = a "symbol" |> fun s -> s.Replace (' ', '_')
    currency = a "currency"
    description = a "description"
    buysell = a "buySell"
    quantity = a "quantity" |> decimal
    price = a "tradePrice" |> decimal
    multiplier = a "multiplier" |> int

    commission = a "ibCommission" |> decimal |> negate
    commissionCurrency = a "ibCommissionCurrency"

    executionDate = a "dateTime" |> cdate
    assetCategory = a "assetCategory"
  }

let parseFlexStatement (fnIn : string) =
  printf "Processing statment..."
  let xml = XDocument.Load fnIn
  let statement = xml.Element(xn "FlexQueryResponse").Element(xn "FlexStatements").Element(xn "FlexStatement")
  let trades = statement.Element(xn "Trades").Elements(xn "Trade") |> Seq.map createTradeFromElement |> Seq.toList
  let parsed = trades |> List.filter (fun a -> a.buysell = "BUY" || a.buysell = "SELL")

  let merge x y = { x with quantity = x.quantity + y.quantity
                           price = (x.price * x.quantity + y.price * y.quantity) / (x.quantity + y.quantity)
                           commission = x.commission + y.commission }
  let fold1 xs = List.fold merge (List.head xs) (List.tail xs)
  let grouped = parsed |> List.groupBy (fun x -> x.orderid)
                       |> List.map (snd >> fold1)
                       |> List.sortBy (fun x -> x.executionDate)

  printfn "completed."
  grouped

let ppBlossom fnOut statement = ()
// CLI

type OutputMode = Blossom
type Arguments =
  | [<Unique>] Fetch of token:string * query:string * output:string
  | [<Unique>] Parse of mode:OutputMode * source:string * output:string
  with
    interface IArgParserTemplate with
      member s.Usage =
        match s with
          | Fetch _ -> "Get report from IBKR"
          | Parse _ -> "Parse a report xml to an output file"

[<EntryPoint>]
let main argv =
  let parser = ArgumentParser.Create<Arguments>()
  let results = parser.Parse argv

  // find the mode (Fetch takes priority)
  let fetch = results.TryGetResult Fetch
  let parse = results.TryGetResult Parse

  match (fetch, parse) with
   | (Some f, None) ->
        let t, q, fn = f
        let statement = getFlexStatement t q
        statement.Save fn
   | (None, Some p) ->
        let md, fn1, fn2 = p
        match md with
          | Blossom -> parseFlexStatement fn1 |> ppBlossom fn2

   | (Some _, Some _) -> printfn "Only 1 option can be specified"
   | _ -> printfn "Unrecognised command line combination"

  0