open System.IO
open System.Xml.Linq

open Argu
open System
open System.Globalization

// Helpers
let xn = XName.Get
let negate x = -x
let EST = TimeZoneInfo.FindSystemTimeZoneById("Eastern Standard Time")
let HKT = TimeZoneInfo.FindSystemTimeZoneById("China Standard Time")
let est2hkt dt = TimeZoneInfo.ConvertTime(TimeZoneInfo.ConvertTimeToUtc(dt, EST), HKT);;

let cdatetime str = DateTime.ParseExact(str, "yyyy-MM-dd;HH:mm:ss", CultureInfo.CurrentCulture)
let cdate str = DateTime.ParseExact(str, "yyyy-MM-dd", CultureInfo.CurrentCulture)

let cdate2 (str: String) = 
  match str.Length with 
    | 0  -> None
    | 10 -> cdate str |> Some 
    | _  -> cdatetime str |> Some

let merge (m1 : Map<'a, 'b>) (m2 : Map<'a, 'b>) = Map.fold (fun s k v -> Map.add k v s) m2 m1

(*
  <!-- example nodes for parsing assistance -->

  <Order accountId="Uxxxxxx" acctAlias="" model="" currency="HKD" fxRateToBase="1" assetCategory="STK"
    symbol="1972" description="SWIRE PROPERTIES LTD" conid="99662733" securityID="HK0000063609" securityIDType="ISIN" cusip=""
    isin="HK0000063609" listingExchange="SEHK" underlyingConid="" underlyingSymbol="" underlyingSecurityID="" underlyingListingExchange=""
    issuer="" multiplier="1" strike="" expiry="" tradeID="" putCall="" reportDate="2017-07-14" principalAdjustFactor=""
    dateTime="2017-07-13;23:48:30" tradeDate="2017-07-14" settleDateTarget="2017-07-18" transactionType="" exchange=""
    quantity="2400" tradePrice="26.45" tradeMoney="63480" proceeds="-63480" taxes="-67.71396" ibCommission="-50.784"
    ibCommissionCurrency="HKD" netCash="-63598.49796" closePrice="26.65" openCloseIndicator="O" notes="O;P" cost="63598.49796"
    fifoPnlRealized="0" fxPnl="0" mtmPnl="480" origTradePrice="" origTradeDate="" origTradeID="" origOrderID="" clearingFirmID=""
    transactionID="" buySell="BUY" ibOrderID="xx" ibExecID="" brokerageOrderID="" orderReference="" volatilityOrderLink=""
    exchOrderId="" extExecID="" orderTime="2017-07-13;23:48:30" openDateTime="" holdingPeriodDateTime="" whenRealized=""
    whenReopened="" levelOfDetail="ORDER" changeInPrice="" changeInQuantity="" orderType="LMT" traderID="" isAPIOrder="" accruedInt="0" />

  <!-- or for traded assets, SymbolSummary can have necessary fields, depending upon the relevant activity in the file -->
  <SecurityInfo currency="HKD" assetCategory="STK" symbol="1972" description="SWIRE PROPERTIES LTD"
    conid="99662733" securityID="HK0000063609" securityIDType="ISIN" cusip="" isin="HK0000063609" listingExchange="SEHK" underlyingConid=""
    underlyingSymbol="" underlyingSecurityID="" underlyingListingExchange="" issuer="" multiplier="1" strike="" expiry="" putCall=""
    principalAdjustFactor="" maturity="" issueDate="" underlyingCategory="" subCategory="COMMON" settlementPolicyMethod="" code="" />

  <!-- levelOfDetail = "SUMMARY" -->
  <OpenPosition accountId="Uxxxxxx" acctAlias="" model="" currency="HKD" fxRateToBase="1" assetCategory="STK" symbol="1972"
    description="SWIRE PROPERTIES LTD" conid="99662733" securityID="HK0000063609" securityIDType="ISIN" cusip="" isin="HK0000063609"
    listingExchange="SEHK" underlyingConid="" underlyingSymbol="" underlyingSecurityID="" underlyingListingExchange="" issuer=""
    multiplier="1" strike="" expiry="" putCall="" principalAdjustFactor="" reportDate="2017-07-14" position="2400" markPrice="26.65"
    positionValue="63960" openPrice="26.49937415" costBasisPrice="26.49937415" costBasisMoney="63598.49796" percentOfNAV="71.34"
    fifoPnlUnrealized="361.50204" side="Long" levelOfDetail="SUMMARY" openDateTime="" holdingPeriodDateTime="" vestingDate="" code=""
    originatingOrderID="" originatingTransactionID="" accruedInt="" />

  <ConversionRate reportDate="2021-01-07" fromCurrency="ILS" toCurrency="HKD" rate="2.4384" />

  <InterestAccrualsCurrency accountId="Uxxxxxx" acctAlias="" model="" currency="JPY"
  fromDate="2021-01-21" toDate="2021-01-21" startingAccrualBalance="-xxxx" interestAccrued="-xxxx"
  accrualReversal="0" fxTranslation="0" endingAccrualBalance="-xxxx" />

  <ChangeInDividendAccrual accountId="Uxxxxxx" acctAlias="" model="" currency="HKD" fxRateToBase="1" assetCategory="STK"
  symbol="315" description="SMARTONE TELECOMMUNICATIONS" conid="4116485" securityID="BMG8219Z1059" securityIDType="ISIN" cusip=""
  isin="BMG8219Z1059" listingExchange="SEHK" underlyingConid="" underlyingSymbol="" underlyingSecurityID=""
  underlyingListingExchange="" issuer="" multiplier="1" strike="" expiry="" putCall="" principalAdjustFactor=""
  reportDate="2021-03-18" date="2021-03-18" exDate="2021-03-05" payDate="2021-03-18" quantity="15000" tax="0" fee="0"
  grossRate="0.145" grossAmount="-2175" netAmount="-2175" code="Re" fromAcct="" toAcct="" levelOfDetail="DETAIL" />
*)

// Processing and output
type Order =
  {
    conid: string
    quantity: decimal
    price: decimal
    commission: decimal
    commissionCurrency: string
    taxes: decimal
    reportDate: DateTime
    opening: bool
  }

type Security =
  {
    conid: string
    symbol: string
    category: string
    currency: string
    description: string
    isin: string
    multiplier: int
    strike: decimal option
    expiry: DateTime option
    putcall: string option
    underlyingConid: string option
  }

type OpenPosition =
  {
    levelOfDetail: string
    conid: string
    reportDate: DateTime
    position: decimal
    price: decimal
    openPrice: decimal
    originatingOrderId: string
    openDateTime: DateTime option
  }

type FXRate =
  {
    reportDate: DateTime
    accy: string
    dccy: string
    rate: decimal
  }

type InterestAccrual =
  {
    accrualDate: DateTime
    currency: string
    accrual: decimal
    reversal: decimal
  }

type DividendAccrual =
  {
    conid: string
    symbol: string
    currency: string
    quantity: decimal
    gross: decimal
    net: decimal
    tax: decimal
    fee: decimal
    isPosting: bool
    exDate: DateTime
    payDate: DateTime option
  }

type Statement =
  {
    orders: Order list
    securities: Map<string, Security>
    positions: OpenPosition list
    legacy: OpenPosition list
    fxrates: FXRate list
    interest: InterestAccrual list
    dividends: DividendAccrual list
  }
    with
      static member Empty =
        {
          orders = []
          securities = Map.empty
          positions = []
          legacy = []
          fxrates = []
          interest = []
          dividends = []
        }

let mergeStatements s1 s2 =
  {
    orders = s1.orders @ s2.orders
    securities = merge s1.securities s2.securities
    positions = s1.positions @ s2.positions
    legacy = s1.legacy @ s2.legacy
    fxrates = s1.fxrates @ s2.fxrates
    interest = s1.interest @ s2.interest
    dividends = s1.dividends @ s2.dividends
  }

let bind_attribute_lookup (node: XElement) = fun nm -> xn nm |> node.Attribute |> fun a -> a.Value

let createOrder (node: XElement) =
  let a = bind_attribute_lookup node
  {
    conid = a "conid"
    quantity = a "quantity" |> decimal
    price = a "tradePrice" |> decimal
    commission = a "ibCommission" |> decimal |> negate
    commissionCurrency = a "ibCommissionCurrency"
    taxes = a "taxes" |> decimal |> negate
    reportDate = a "reportDate" |> cdate
    opening = a "openCloseIndicator" |> fun x -> x = "O"
  }

let createSecurity (node: XElement) =
  let a = bind_attribute_lookup node
  {
    conid = a "conid"
    symbol = a "symbol" |> fun s -> s.Replace(' ', '_')
    category = a "assetCategory"
    currency = a "currency"
    description = a "description"
    isin = a "isin"
    multiplier = a "multiplier" |> int

    // these vary more depending upon symbol
    strike = a "strike" |> fun v -> if v = "" then None else Some (decimal v)
    expiry = a "expiry" |> fun v -> if v = "" then None else Some (cdate v)
    putcall = a "putCall" |> fun v -> if v = "" then None else Some v
    underlyingConid = a "underlyingConid" |> fun v -> if v = "" then None else Some v
  }

let createOpenPosition (node: XElement) =
  let a = bind_attribute_lookup node
  {
    levelOfDetail = a "levelOfDetail"
    conid = a "conid"
    reportDate = a "reportDate" |> cdate
    position = a "position" |> decimal
    price = a "markPrice" |> decimal
    openPrice = a "openPrice" |> decimal
    originatingOrderId = a "originatingOrderID"
    openDateTime = a "openDateTime" |> cdate2
  }

let createFX (node: XElement) =
  let a = bind_attribute_lookup node
  {
    reportDate = a "reportDate" |> cdate
    accy = a "fromCurrency"
    dccy = a "toCurrency"
    rate = a "rate" |> decimal
  }

let createInterest (node: XElement) =
  let a = bind_attribute_lookup node

  let ix =
    { accrualDate = a "toDate" |> cdate
      currency = a "currency"
      accrual = a "interestAccrued" |> decimal
      reversal = a "accrualReversal" |> decimal }

  if ix.currency = "BASE_SUMMARY"
    then None
    else Some ix

let createDividendAccrual (node: XElement) =
  let a = bind_attribute_lookup node
  let d = {
    conid = a "conid"
    symbol = a "symbol"
    currency = a "currency"
    quantity = a "quantity" |> decimal
    gross = a "grossAmount" |> decimal
    net = a "netAmount" |> decimal
    tax = a "tax" |> decimal
    fee = a "fee" |> decimal
    isPosting = a "code" = "Po"
    exDate = a "exDate" |> cdate
    payDate = a "payDate" |> fun ds -> if ds <> "" then Some (cdate ds) else None
  }
  if (a "levelOfDetail" = "DETAIL" && d.isPosting)
    then Some d
    else None

let parseSymbology (snIn : string) =
  let xml = XDocument.Load (snIn, LoadOptions.SetLineInfo)
  xml

let parseFlexStatement (fnIn : string) =
  printfn "%s" fnIn
  let xml = XDocument.Load (fnIn, LoadOptions.SetLineInfo)

  let orders = xml.Descendants(xn "Order") |> Seq.map createOrder |> Seq.toList
  // securities are copied each day, need to look at the history for 9984 due to the split
  // this does not handle the date nor things like that yet
  let securities1 =
    xml.Descendants(xn "SymbolSummary")
      |> Seq.map createSecurity
      |> Seq.distinct
      |> Seq.map (fun s -> s.conid, s)
      |> Map.ofSeq

  let securities2 =
    xml.Descendants(xn "SecurityInfo")
      |> Seq.map createSecurity
      |> Seq.distinct
      |> Seq.map (fun s -> s.conid, s)
      |> Map.ofSeq

  let securities = merge securities1 securities2
  let openPositions0 = xml.Descendants(xn "OpenPosition") |> Seq.map createOpenPosition |> Seq.toList
  let openPositions_summary, openPositions_lot = openPositions0 |> List.partition (fun x -> x.levelOfDetail = "SUMMARY")
  let openPositions_legacy = openPositions_lot |> List.filter (fun a -> a.originatingOrderId = "")
  let fxrates = xml.Descendants(xn "ConversionRate") |> Seq.map createFX |> Seq.toList
  let interest = xml.Descendants(xn "InterestAccrualsCurrency") |> Seq.choose createInterest |> Seq.toList
  let dividends = xml.Descendants(xn "ChangeInDividendAccrual") |> Seq.choose createDividendAccrual |> Seq.toList

  {
    orders = orders
    securities = securities
    positions = openPositions_summary
    legacy = openPositions_legacy
    fxrates = fxrates
    interest = interest
    dividends = dividends
  }

let ppBlossom (fnOut: string) (statement : Statement) =
  // create an extra import line for extras, etc
  let imports = [
    $"import {Path.GetFileNameWithoutExtension(fnOut)}_legacy.fledge"
    $"import {Path.GetFileNameWithoutExtension(fnOut)}_extras.fledge"
    $"import {Path.GetFileNameWithoutExtension(fnOut)}_prices.fledge"
    $"import {Path.GetFileNameWithoutExtension(fnOut)}_financing.fledge"
    ""
  ]

  // commodities
  let writeSecurity (security: Security) =
    let klass, multiplier, mtm =
      match security.category with
        | "STK"   -> "Equity", None, false
        | "FUT"   -> "Future", Some security.multiplier, true
        | "OPT"   -> "Option", Some security.multiplier, false
        | "FOP"   -> "Option", Some security.multiplier, false
        | "FSOPT" -> "Option", Some security.multiplier, true
        | "CASH"  -> "Currency", None, false
        | x -> failwith $"Unexpected security category: {x}"

    let l1 = $"commodity {security.symbol}"
    let l2 = $"  name {security.description}"
    let l4 = $"  class {klass}"
    let l5 = $"  externalid ibkr.conid {security.conid}"

    let p1 = [l1; l2; l4; l5]
    let p2 = match (multiplier, mtm) with
               | None, false -> p1
               | Some a, false -> p1 @ [$"  multiplier {a}"]
               | Some a, true  -> p1 @ [$"  multiplier {a}"; "  mtm"]
               | _ -> failwith "Unexpected handling of security multiplier / mtm flags"
    p2 @ [""]
  let securities = statement.securities |> Map.toList |> List.map snd |> List.collect writeSecurity

  // transactions
  let writeOrder (order : Order) =
    let security = statement.securities |> Map.find order.conid
    let physicalAccount = $"""Asset:InteractiveBrokers:Trading:{security.category}"""
    let l1 = $"""{order.reportDate.ToString("yyyy-MM-dd")} trade {physicalAccount} {order.quantity} {security.symbol} @ {order.price} {security.currency}"""
    let l2 = $"""  settlement      Asset:InteractiveBrokers:Cash"""
    let p1 = [l1; l2]

    let p2 = match order.commission with
               | x when x = 0M -> p1
               | x -> p1 @ [$"  expense         Expense:InteractiveBrokers:Commission  {order.commission} {order.commissionCurrency}"]

    let p3 = match order.taxes with
               | x when x = 0M -> p2
               | x -> p2 @ [$"  expense         Expense:InteractiveBrokers:Taxes  {order.taxes} {security.currency}"]

    p3 @ [""]

  let orders = statement.orders |> List.collect writeOrder

  // interest accruals
  let writeInterest (interest: InterestAccrual) =
    let p1 =
      if interest.accrual <> 0M
        then  let l1 = $"""{interest.accrualDate.ToString("yyyy-MM-dd")} Interactive Brokers | Credit / debit interest accrual"""
              let l2 = $"""  Payable:InteractiveBrokers:Interest    {interest.accrual} {interest.currency}"""
              let l3 = $"""  Income:Interest"""
              [l1; l2; l3; ""]
        else []
    let p2 =
      if interest.reversal <> 0M
        then  let l1 = $"""{interest.accrualDate.ToString("yyyy-MM-dd")} Interactive Brokers | Credit / debit interest posting"""
              let l2 = $"""  Payable:InteractiveBrokers:Interest    {interest.reversal} {interest.currency}"""
              let l3 = $"""  Asset:InteractiveBrokers:Cash"""
              [l1; l2; l3; ""]
        else []
    p1 @ p2
  let interest = statement.interest |> List.collect writeInterest

  let writeDividend (dividend: DividendAccrual) =
    let physicalAccount = $"""Asset:InteractiveBrokers:Trading:STK"""
    let grossPerUnit = (dividend.gross / dividend.quantity) * if dividend.isPosting then 1.0M else -1.0M
    let l1 = $"""{dividend.exDate.ToString("yyyy-MM-dd")} dividend {physicalAccount} {dividend.quantity} {dividend.symbol} @ {grossPerUnit} {dividend.currency}"""
    let l2 = $"""  settlement      Asset:InteractiveBrokers:Cash"""
    let l3 = $"""  income          Income:Dividends"""
    let l4 = $"""  receivable      Receivable:Dividends"""
    let p1 = [l1; l2; l3; l4]
    let p2 = match dividend.payDate with
                | Some pd -> p1 @ [$"""  paydate         {pd.ToString("yyyy-MM-dd")}"""]
                | _ -> p1
    p2 @ [""]

  let dividends = statement.dividends |> List.collect writeDividend

  let writePriceSeries header (hs : (DateTime * decimal) list) =
    let hsm = hs |> Map.ofList
    // reindex to a monday start and 5 day groups on 1 line
    let firstMonday = hs |> List.minBy fst
                         |> fst
                         |> function dt -> let g = (int)dt.DayOfWeek - (int)DayOfWeek.Monday
                                           dt.AddDays(-(float)g)
    let lastDay = hs |> List.maxBy fst |> fst
    let schedule = firstMonday |> List.unfold (fun d -> if d <= lastDay then Some (d, d.AddDays(1.0)) else None)
                               |> List.filter (fun d -> d.DayOfWeek <> DayOfWeek.Saturday && d.DayOfWeek <> DayOfWeek.Sunday)
    let reindexed = schedule |> List.map (fun dt -> dt, hsm |> Map.tryFind dt)
    let width = reindexed |> List.map (fun (_, b) -> match b with Some c -> $"{c}" |> String.length | _ -> 3)  // since '...' is length 3
                          |> List.max
    let hs2 = reindexed |> List.chunkBySize 5
                        |> List.map (fun xs -> let left = xs |> List.head |> fst
                                               let right = xs |> List.map snd
                                               left, right)
    let ls = hs2 |> List.map (fun (d, ps) -> let ps2 = ps |> List.map (Option.map (fun o -> $"{o}") >> Option.defaultValue "..."
                                                                                                    >> sprintf "%-*s" width)
                                                          |> String.concat " "
                                             $"""  {d.ToString("yyyy-MM-dd")} {ps2}""")
    header :: ls @ [""]

  // prices
  let writePrice conid (hs : (DateTime * decimal) list) =
    let security = statement.securities |> Map.find conid
    let l0 = $"""prices {security.symbol} {security.currency}"""
    writePriceSeries l0 hs

  let prices = statement.positions |> List.groupBy (fun x -> x.conid)
                                   |> List.collect (fun (conid, ps) -> let hs = ps |> List.map (fun p -> p.reportDate, p.price)
                                                                                   |> List.distinct
                                                                       writePrice conid hs)

  // write fx rates
  let writeFx accy dccy (hs : (DateTime * decimal) list) =
    let l0 = $"prices {accy}.{dccy} {dccy}"
    writePriceSeries l0 hs

  let fxrates =
    let pairs = statement.fxrates |> List.groupBy (fun x -> x.accy, x.dccy)
                                  |> List.map (fun ((accy, dccy), fxs) -> let xs = fxs |> List.map (fun p -> p.reportDate, p.rate)
                                                                                       |> List.distinct
                                                                          (accy, dccy), xs)
    let s0 = pairs |> List.collect (fun ((a, d), v) -> writeFx a d v)
    let mpairs = Map.ofList pairs

    let inverses = ["HKD", "JPY"]
    let s1 = inverses |> List.collect (fun (a, d) -> let p1 = mpairs |> Map.find (d,a)
                                                     writeFx a d (p1 |> List.map (fun (dt,v) -> (dt, 1.0M/v))))
    let indirects = ["HKD", "AUD", "JPY"; "HKD", "KRW", "JPY"]
    let s2 = indirects |> List.collect (fun (a, d1, d2) -> let p1 = mpairs |> Map.find (d1, a)
                                                           let p2 = mpairs |> Map.find (d2, a)
                                                           let m1 = p1 |> Map.ofList
                                                           let m2 = p2 |> Map.ofList
                                                           let ds = List.map fst p1 @ List.map fst p2 |> List.distinct |> List.sort
                                                           let vs = ds |> List.choose (fun dt -> let v1 = Map.tryFind dt m1
                                                                                                 let v2 = Map.tryFind dt m2
                                                                                                 match v1, v2 with
                                                                                                   | Some w1, Some w2 -> Some (dt, w1 / w2)
                                                                                                   | _ -> None)

                                                           writeFx d1 d2 vs)

    s0 @ s1 @ s2

  // write legacy transactions, temporarily. this picks up open positions without an "originatingOrderID"
  let legacy = 
    statement.legacy |> List.distinctBy (fun x -> (x.conid, x.position, x.openPrice, x.openDateTime))
                     |> List.map (fun x -> {conid = x.conid
                                            quantity = x.position
                                            price = x.price
                                            commission = 0.0m
                                            commissionCurrency = ""
                                            taxes = 0.0m
                                            reportDate = Option.get x.openDateTime
                                            opening = true})
                     |> List.sortBy _.reportDate
                     |> List.collect writeOrder


  // split outputs, as getting... big
  let stub = Path.Join(Path.GetDirectoryName(fnOut), Path.GetFileNameWithoutExtension(fnOut))
  let fnLegacy = $"{stub}_legacy.fledge"
  let fnPrices = $"{stub}_prices.fledge"
  let fnFinancing = $"{stub}_financing.fledge"

  File.WriteAllLines(fnLegacy, legacy)
  File.WriteAllLines(fnOut, imports @ securities @ orders) // temporaryily disable: @ dividends)
  File.WriteAllLines(fnPrices, prices @ fxrates)
  File.WriteAllLines(fnFinancing, interest)

let resolveFiles inputs = 
  let resolve1 (input: string) = 
    let directory = Path.GetDirectoryName(input)
    let pattern = Path.GetFileName(input)
    Directory.GetFiles(directory, pattern) |> List.ofArray
  inputs |> List.collect resolve1 

// CLI

type OutputMode = Blossom | Beancount
type Arguments =
  | [<Mandatory>] Mode of OutputMode
  | [<Mandatory>] Output of string
  | Symbology of string
  | Input of string
  with
   interface IArgParserTemplate with
       member s.Usage =
         match s with
           | Mode _      -> "specify the file format for output"
           | Symbology _ -> "symbology filename"
           | Output _    -> "output filename (stub)"
           | Input _     -> "input filename"

[<EntryPoint>]
let main argv =
  let parser = ArgumentParser.Create<Arguments>()
  let results = parser.Parse argv

  let mode = results.GetResult Mode
  let output = results.GetResult Output
  let symbologyIn = results.TryGetResult Symbology
  let inputs = results.GetResults Input

  let inputs2 = resolveFiles inputs
  let parsed = inputs2 |> List.map parseFlexStatement
                       |> List.fold mergeStatements Statement.Empty

  // use symbology here to enrich
  // let symbology = symbologyIn |> parseSymbology

  // print result
  match mode with
    | Blossom -> ppBlossom output parsed
    | Beancount -> failwith "Not implemented"

  printfn "Completed."
  0