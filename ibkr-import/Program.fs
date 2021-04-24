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
    conid: string
    reportDate: DateTime
    position: decimal
    price: decimal
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

type Statement =
  {
    orders: Order list
    securities: Map<string, Security>
    positions: OpenPosition list
    fxrates: FXRate list
    interest: InterestAccrual list
  }
    with
      static member Empty =
        {
          orders = []
          securities = Map.empty
          positions = []
          fxrates = []
          interest = []
        }

let mergeStatements s1 s2 =
  {
    orders = s1.orders @ s2.orders
    securities = merge s1.securities s2.securities
    positions = s1.positions @ s2.positions
    fxrates = s1.fxrates @ s2.fxrates
    interest = s1.interest @ s2.interest
  }

let createOrder (node: XElement) =
  let a nm = xn nm |> node.Attribute |> fun a -> a.Value
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
  let a nm = xn nm |> node.Attribute |> fun a -> a.Value
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
  let a nm = xn nm |> node.Attribute |> fun a -> a.Value
  {
    conid = a "conid"
    reportDate = a "reportDate" |> cdate
    position = a "position" |> decimal
    price = a "markPrice" |> decimal
  }

let createFX (node: XElement) =
  let a nm = xn nm |> node.Attribute |> fun a -> a.Value
  {
    reportDate = a "reportDate" |> cdate
    accy = a "fromCurrency"
    dccy = a "toCurrency"
    rate = a "rate" |> decimal
  }

let createInterest (node: XElement) =
    let a nm =
        xn nm |> node.Attribute |> fun a -> a.Value

    let ix =
      { accrualDate = a "toDate" |> cdate
        currency = a "currency"
        accrual = a "interestAccrued" |> decimal
        reversal = a "accrualReversal" |> decimal }

    if ix.currency = "BASE_SUMMARY"
      then None
      else Some ix

let parseSymbology (snIn : string) =
  let xml = XDocument.Load (snIn, LoadOptions.SetLineInfo)
  xml

let parseFlexStatement (fnIn : string) =
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
  let openPositions = xml.Descendants(xn "OpenPosition") |> Seq.map createOpenPosition |> Seq.toList |> List.distinct
  let fxrates = xml.Descendants(xn "ConversionRate") |> Seq.map createFX |> Seq.toList
  let interest = xml.Descendants(xn "InterestAccrualsCurrency") |> Seq.choose createInterest |> Seq.toList

  {
    orders = orders
    securities = securities
    positions = openPositions
    fxrates = fxrates
    interest = interest
  }

let ppBlossom fnOut (statement : Statement) =
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

    let p4 = match order.opening with
               | false -> p3 @ ["  cg              Income:CapitalGains"]
               | true  -> p3

    p4 @ [""]

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

  // prices
  let writePrice conid (hs : (DateTime * decimal) list) =
    let security = statement.securities |> Map.find conid
    let l0 = $"""prices {security.symbol} {security.currency}"""
    let ls = hs |> List.map (fun (d, p) -> $"""  {d.ToString("yyyy-MM-dd")} {p}""")
    l0 :: ls @ [""]

  let prices = statement.positions |> List.groupBy (fun x -> x.conid)
                                   |> List.collect (fun (conid, ps) -> let hs = ps |> List.map (fun p -> p.reportDate, p.price)
                                                                                   |> List.distinct
                                                                       writePrice conid hs)

  // write fx rates
  let writeFx accy dccy (hs : (DateTime * decimal) list) =
    let l0 = $"prices {accy} {dccy}"
    let ls = hs |> List.map (fun (d, p) -> $"""  {d.ToString("yyyy-MM-dd")} {p}""")
    l0 :: ls @ [""]

  let fxrates = statement.fxrates |> List.groupBy (fun x -> x.accy, x.dccy)
                                  |> List.collect (fun ((accy, dccy), fxs) -> let xs = fxs |> List.map (fun p -> p.reportDate, p.rate)
                                                                                           |> List.distinct
                                                                              writeFx accy dccy xs)

  File.WriteAllLines(fnOut, securities @ orders @ interest @ prices @ fxrates)

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
           | Output _    -> "output filename"
           | Input _     -> "input filename"

[<EntryPoint>]
let main argv =
  let parser = ArgumentParser.Create<Arguments>()
  let results = parser.Parse argv

  let mode = results.GetResult Mode
  let output = results.GetResult Output
  let symbologyIn = results.TryGetResult Symbology
  let inputs = results.GetResults Input

  let parsed = inputs |> List.map parseFlexStatement
                      |> List.fold mergeStatements Statement.Empty

  // use symbology here to enrich
  // let symbology = symbologyIn |> parseSymbology

  // print result
  match mode with
    | Blossom -> ppBlossom output parsed
    | Beancount -> failwith "Not implemented"

  printfn "Completed."
  0