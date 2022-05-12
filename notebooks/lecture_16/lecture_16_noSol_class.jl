### A Pluto.jl notebook ###
# v0.18.1

using Markdown
using InteractiveUtils

# ╔═╡ 6199926c-0c4a-4991-b4c3-ebed35217375
begin
	using PlutoUI, Printf 
	using Dates, DataFrames, DataFramesMeta, Chain
	using Statistics
	using Plots
	using TimesDates
end

# ╔═╡ b0b9984b-45f5-49a2-a5ce-86281745d623
begin

	
		
	# The following code is by Paul Soederlind
	# https://sites.google.com/site/paulsoderlindecon/home
		
		#------------------------------------------------------------------------------
	"""
	    printmat([fh::IO],x...;colNames=[],rowNames=[],
	             width=10,prec=3,NoPrinting=false,StringFmt="",cell00="")
	
	Print all elements of a matrix (or several) with predefined formatting. It can also handle
	OffsetArrays. StringFmt = "csv" prints using a csv format.
	
	# Input
	- `fh::IO`:            (optional) file handle. If not supplied, prints to screen
	- `x::Array(s)`:       (of numbers, dates, strings, ...) to print
	- `colNames::Array`:   of strings with column headers
	- `rowNames::Array`:   of strings with row labels
	- `width::Int`:        (keyword) scalar, minimum width of printed cells
	- `prec::Int`:         (keyword) scalar, precision of printed cells
	- `NoPrinting::Bool`:  (keyword) bool, true: no printing, just return formatted string [false]
	- `StringFmt::String`: (keyword) string, "", "csv"
	- `cell00::String`:    (keyword) string, for row 0, column 0
	
	# Output
	- str         (if NoPrinting) string, (otherwise nothing)
	
	# Examples
	```
	x = [11 12;21 22]
	printmat(x)
	```
	```
	x = [1 "ab"; Date(2018,10,7) 3.14]
	printmat(x,width=20,colNames=["col 1","col 2"])
	```
	```
	printmat([11,12],[21,22])
	```
	Can also call as
	```
	opt = Dict(:rowNames=>["1";"4"],:width=>10,:prec=>3,:NoPrinting=>false,:StringFmt=>"")
	printmat(x;colNames=["a","b"],opt...)     #notice ; and ...
	```
	(not all keywords are needed)
	
	# Requires
	- fmtNumPs
	
	# Notice
	- The prefixN and suffixN could potentially be made function inputs. This would allow
	a fairly flexible way to format tables.
	
	
	Paul.Soderlind@unisg.ch
	
	"""
	function printmat(fh::IO,x...;colNames=[],rowNames=[],
	                  width=10,prec=3,NoPrinting=false,StringFmt="",cell00="")
	
	  isempty(x) && return nothing                         #do nothing is isempty(x)
	
	  typeTestQ = any(!=(eltype(x[1])),[eltype(z) for z in x])  #test if eltype(x[i]) differs
	  if typeTestQ                                      #create matrix from tuple created by x...
	    x = hcat(Matrix{Any}(hcat(x[1])),x[2:end]...)   #preserving types of x[i]
	  else
	    x = hcat(x...)
	  end
	
	  (m,n) = (size(x,1),size(x,2))
	
	  (length(rowNames) == 1 < m) && (rowNames = [string(rowNames[1],i) for i = 1:m])  #"ri"
	  (length(colNames) == 1 < n) && (colNames = [string(colNames[1],i) for i = 1:n])  #"ci"
	
	  if StringFmt == "csv"
	    (prefixN,suffixN)   = (fill("",n),vcat(fill(",",n-1),""))  #prefix and suffix for column 1:n
	    (prefixC0,suffixC0) = ("",",")                             #prefix and suffix for column 0
	  else
	    (prefixN,suffixN) = (fill("",n),fill("",n))
	    (prefixC0,suffixC0) = ("","")
	  end
	
	  if length(rowNames) == 0                         #width of column 0 (cell00 and rowNames)
	    col0Width = 0
	  else
	    col0Width = maximum(length,vcat(cell00,rowNames)) + length(prefixC0) + length(suffixC0)
	  end
	
	  colWidth = [width + length(prefixN[j]) + length(suffixN[j]) for j=1:n]  #widths of column 1:n
	
	  iob = IOBuffer()
	
	  if !isempty(colNames)                                #print (cell00,colNames), if any
	    !isempty(cell00) ?  txt0 = string(prefixC0,cell00,suffixC0) : txt0 = ""
	    print(iob,rpad(txt0,col0Width))
	    for j = 1:n                                #loop over columns
	      print(iob,lpad(string(prefixN[j],colNames[j],suffixN[j]),colWidth[j]))
	    end
	    print(iob,"\n")
	  end
	                                                       #print rowNames and x
	  (i0,j0) = (1 - first(axes(x,1)),1 - first(axes(x,2)))   #i+i0,j+j0 give traditional indices
	  for i in axes(x,1)                           #loop over rows
	    !isempty(rowNames) && print(iob,rpad(string(prefixC0,rowNames[i+i0],suffixC0),col0Width))
	    for j in axes(x,2)                         #loop over columns
	      print(iob,fmtNumPs(x[i,j],width,prec,"right",prefix=prefixN[j+j0],suffix=suffixN[j+j0]))
	    end
	    print(iob,"\n")
	  end
	  str = String(take!(iob))
	
	  if NoPrinting                              #no printing, just return str
	    return str
	  else                                       #print, return nothing
	    print(fh,str,"\n")
	    return nothing
	  end
	
	end
	                        #when fh is not supplied: printing to screen
	printmat(x...;colNames=[],rowNames=[],width=10,prec=3,NoPrinting=false,StringFmt="",cell00="") =
	    printmat(stdout::IO,x...;colNames,rowNames,width,prec,NoPrinting,StringFmt,cell00)
	#------------------------------------------------------------------------------
	
	
	#------------------------------------------------------------------------------
	"""
	    printlnPs([fh::IO],z...;width=10,prec=3)
	
	Subsitute for println, with predefined formatting.
	
	
	# Input
	- `fh::IO`:    (optional) file handle. If not supplied, prints to screen
	- `z::String`: string, numbers and arrays to print
	
	Paul.Soderlind@unisg.ch
	
	"""
	function printlnPs(fh::IO,z...;width=10,prec=3)
	
	  for x in z                              #loop over inputs in z...
	    if isa(x,AbstractArray)
	      iob = IOBuffer()
	      for i = 1:length(x)
	        print(iob,fmtNumPs(x[i],width,prec,"right"))
	      end
	      print(fh,String(take!(iob)))
	    else
	      print(fh,fmtNumPs(x,width,prec,"right"))
	    end
	  end
	
	  print(fh,"\n")
	
	end
	                      #when fh is not supplied: printing to screen
	printlnPs(z...;width=10,prec=3) = printlnPs(stdout::IO,z...;width,prec)
	#------------------------------------------------------------------------------
	
	
	#------------------------------------------------------------------------------
	"""
	    fmtNumPs(z,width=10,prec=2,justify="right";prefix="",suffix="")
	
	Create a formatted string of a float (eg, "%10.4f"), nothing (""),
	while other values are passed through. Strings are right (or left) justified
	and can optionally be given prefix and suffix (eg, ",")
	
	# Notice
	- With prec > 0 and isa(z,Integer), then the string is padded with 1+prec spaces
	to align with the printing of floats with the same prec.
	
	# Requires
	- Printf (for 1.6-), fmtNumPsC (for < 1.6)
	
	"""
	function fmtNumPs(z,width=10,prec=2,justify="right";prefix="",suffix="")
	
	  isa(z,Bool) && (z = convert(Int,z))             #Bool -> Int
	
	  if isa(z,AbstractFloat)                         #example: 101.0234, prec=3
	    if VERSION < v"1.6-"
	      fmt    = "%$(width).$(prec)f"
	      zRound = round(z,digits=prec)
	      strLR  = fmtNumPsC(fmt,zRound)                #C fallback solution
	    else
	      fmt   = Printf.Format("%$(width).$(prec)f")
	      strLR = Printf.format(fmt,z)
	    end
	  elseif isa(z,Nothing)
	    strLR = ""
	  elseif isa(z,Integer) && prec > 0               #integer followed by (1+prec spaces)
	    strLR = string(z," "^(1+prec))
	  else                                            #Int, String, Date, Missing, etc
	    strLR = string(z)
	  end
	
	  strLR = string(prefix,strLR,suffix)
	
	  if justify == "left"                            #justification
	    strLR = rpad(strLR,width+length(prefix)+length(suffix))
	  else
	    strLR = lpad(strLR,width+length(prefix)+length(suffix))
	  end
	
	  return strLR
	
	end
	#------------------------------------------------------------------------------
	
	
	#------------------------------------------------------------------------------
	"""
	    fmtNumPsC(fmt,z)
	
	c fallback solution for formatting of floating point number. Used if VERSION < v"1.6-"
	"""
	function fmtNumPsC(fmt,z)                           #c fallback solution
	  if ismissing(z) || isnan(z) || isinf(z)    #asprintf does not work for these cases
	    str = string(z)
	  else
	    strp = Ref{Ptr{Cchar}}(0)
	    len = ccall(:asprintf,Cint,(Ptr{Ptr{Cchar}},Cstring,Cdouble...),strp,fmt,z)
	    str = unsafe_string(strp[],len)
	    Libc.free(strp[])
	  end
	  return str
	end
	#------------------------------------------------------------------------------
	
	
	#------------------------------------------------------------------------------
	function printblue(x...)
	  foreach(z->printstyled(z,color=:blue,bold=true),x)
	  print("\n")
	end
	function printred(x...)
	  foreach(z->printstyled(z,color=:red,bold=true),x)
	  print("\n")
	end
	function printmagenta(x...)
	  foreach(z->printstyled(z,color=:magenta,bold=true),x)
	  print("\n")
	end
	function printyellow(x...)
	  foreach(z->printstyled(z,color=:yellow,bold=true),x)
	  print("\n")
	end
	#------------------------------------------------------------------------------
	
		
	using Logging
	global_logger(NullLogger())
		
	display("")
	
end

# ╔═╡ 6c003a32-ddb4-462b-bc73-3bbf633f184f
using AlpacaMarkets

# ╔═╡ 886b6ade-b8df-494c-be5f-13aae328b848
md"""
## FINC 672: Tick-by-Tick Stock Quotes and Trades
"""

# ╔═╡ 26a704bf-6b13-45f2-9211-023fbf2e3cb6
md"""
_Thanks to [Dean Markwick](https://dm13450.github.io/about/) for making the AlpacMarkets Julia API publicly available._
"""

# ╔═╡ 1897543d-5c47-41ee-9f13-a099df5d9ed1
md"""
- In this notebook, we will learn to work with real-time stock data. Specifically, we will use data from (Alpaca)[https://alpaca.markets/] on stock trades and quotes.
- We will connect to IEX using [AlpacaMarkets.jl](https://github.com/dm13450/AlpacaMarkets.jl) API in Julia.
- First, you need to register a free account [here](https://alpaca.markets/).
- After your registration is complete, you need to get your `API KEY` and `Secret KEY`."""

# ╔═╡ 89d72808-d09e-421a-ae7b-5491e8fae0ee
md"""
- Step 1: Go to Alpaca website https://alpaca.markets/ and login.
- Step 2: Once you login, Go to paper account.
- Step 3: Your API Key and API Secret will be located on the right side of the screen. Click on View.
"""

# ╔═╡ dff6f3d9-f4fc-4964-a075-c1c11a953667
LocalResource("./AlpacaAPIKeys.png")

# ╔═╡ 931659d2-cae1-43d3-87d9-b67edb7c16d0
md"""
- Step 4: Click on “Generate New Key”.
"""

# ╔═╡ c8cba87a-6ead-4f6a-b29b-bc3bcc3e75f0
LocalResource("./AlpacaAPIKeys_02.png")

# ╔═╡ 50fb3be3-1674-47a3-9102-0c6fb24241de
md"""
- Step 5: Once you generate API Key, an API Key ID and Secret Key will be generated. Please store both of these as they are confidential. DO NOT share API keys and secrets with other people.
  - __API Secret will be generated only once when you generate the key. In case you forget API Secret, both key ID and Secret will have to be regenerated.__
"""

# ╔═╡ 81f0904b-3da6-4981-971b-b5e11e301eaa
md"""
- Next, we load `AlpacaMarkets.jl` and authenticate us as users.
"""

# ╔═╡ 34545087-b68d-4271-9771-4686b770c619
AlpacaMarkets.auth("PKN64AVCL7JD1YF2Q5XN","I9vzYuasn91liH1GkQZNITJ1SQzpQXgliZUtUcUy") #KEY, SECRET

# ╔═╡ 4a235440-1810-4b18-85f9-377f9f5f6876
md"""
# Stock Quote Data
"""

# ╔═╡ 80b29628-2214-49ab-8eb7-31e99677fb1b
md"""
- Let's get _1 second_ of streaming historical quotes on Apple stock (APPL).
"""

# ╔═╡ 8560c709-61bd-4d7b-a7cd-78c9861b54cc
begin
  aapl = AlpacaMarkets.get_stock_quotes("AAPL", 
  DateTime("2022-01-27T15:00:00"),
  DateTime("2022-01-27T15:00:01"))
end

# ╔═╡ 96ec6830-4c58-48b5-8538-236dc2a3c599
md"""
- In the `ax` and `bx` columns (ask exchange and bid exchange) we can see what venue was offering that price at a given time.
- In the `ap` and `bp` columns we see the ask and bid quotes, respectively.
"""

# ╔═╡ 022c457f-83c1-4059-8730-6f788b1ec479
md"""
| Exchange Code 	|          Name of Exchange         	|
|:-------------:	|:---------------------------------:	|
| A             	| NYSE American (AMEX)              	|
| B             	| NASDAQ OMX BX                     	|
| C             	| National Stock Exchange           	|
| D             	| FINRA ADF                         	|
| E             	| Market Independent                	|
| H             	| MIAX                              	|
| I             	| International Securities Exchange 	|
| J             	| Cboe EDGA                         	|
| K             	| Cboe EDGX                         	|
| L             	| Long Term Stock Exchange          	|
| M             	| Chicago Stock Exchange            	|
| N             	| New York Stock Exchange           	|
| P             	| NYSE Arca                         	|
| Q             	| NASDAQ OMX                        	|
| S             	| NASDAQ Small Cap                  	|
| T             	| NASDAQ Int                        	|
| U             	| Members Exchange                  	|
| V             	| IEX                               	|
| W             	| CBOE                              	|
| X             	| NASDAQ OMX PSX                    	|
| Y             	| Cboe BYX                          	|
| Z             	| Cboe BZX                          	|
"""

# ╔═╡ 1dadf403-6dff-48d7-bb24-eae5c1147d7d
md"""
- Each feed/exchange uses its own set of codes to identify trade and quote conditions, so the same condition may have a different code depending on the originator of the data.
  - More information at [Link](https://alpaca.markets/docs/market-data/).
"""

# ╔═╡ 0fbd9fc7-2e7b-4559-a34b-17528e507ebe
md"""
- __Quote conditions__
"""

# ╔═╡ a099bcf5-76df-468b-82df-cf29a914be9c
md"""
| Code |              Value             |
|:----:|:------------------------------:|
| A    | Manual Ask Automated Bid       |
| B    | Manual Bid Automated Ask       |
| F    | Fast Trading                   |
| H    | Manual Bid And Ask             |
| I    | Order Imbalance                |
| L    | Closed Quote                   |
| N    | Non Firm Quote                 |
| O    | Opening Quote Automated        |
| R    | Regular Two Sided Open         |
| U    | Manual Bid And Ask Non Firm    |
| Y    | No Offer No Bid One Sided Open |
| X    | Order Influx                   |
| Z    | No Open No Resume              |
"""

# ╔═╡ 6978ce4e-0880-4d37-b894-caa63c5ff91f
md"""
- To work with this data, we need to convert the timestamp into a Julia DateTime object.
- First, we define two functions to extract the date and time components from the `t` column which has entries like `2022-01-27T15:00:00.007566848Z`.
"""

# ╔═╡ bb9d0cfe-7f25-464e-bcd6-09d732c36eac
begin

	function convert_t_timestamp(x)
		ts = first(x, 23)

		if endswith(ts,"Z")
			ts = chop(ts)
		end
		DateTime(ts)
	end

	function convert_t_time(x)
		ts = split(x,"T")[2]
		ts = first(ts,12)
			if endswith(ts,"Z")
				ts = chop(ts)
			end
		Time(ts)
	end
	
end

# ╔═╡ e861bcff-9e3f-453a-989f-1fbd342bccf8
md"""
- The timestaps are up to nanosecond precision. However, Julia’s default DateTime type only allows up millisecond precision. To work with nanosecond data, we use the [TimeDate.jl](https://github.com/JeffreySarnoff/TimesDates.jl) package.
"""

# ╔═╡ 3a4c94b5-6fd3-4253-839b-e898ab9025be
aapl2 = @chain aapl begin
	transform(:t => (x-> convert_t_timestamp.(x)) => :TimeStamp,
			  :t => (x-> TimeDate.(string.(chop.(x)))) => :TimeStamp_nano)
end

# ╔═╡ 3bf77a4e-5019-4ad6-9e05-60d2dff701e6
md"""
- Let's now plot the bid and ask prices.
"""

# ╔═╡ 435f564e-c550-4a54-bb7d-86ed3d3017bc
begin
	ticks = minimum( aapl2.TimeStamp):Millisecond(250):maximum(aapl2.TimeStamp)
	tick_labels = Dates.format.(ticks, "HH:MM:SS.sss")

	plot(aapl2.TimeStamp, aapl2.ap, label="Ask Price", seriestype=:steppre,
		xticks=(ticks, tick_labels))
	plot!(aapl2.TimeStamp, aapl2.bp, label="Bid Price", seriestype=:steppre)
end

# ╔═╡ 04c1af80-11cd-4a97-8d8c-5e35fd233d24
md"""
# Stock Trade Data
"""

# ╔═╡ 77339769-19c5-4299-bacc-72719804b1e5
md"""
- Let's look at the same one-second period, but use trades instead of quotes.
"""

# ╔═╡ e398ea5c-575e-4aa5-ad3b-2dd3cc0c1164
begin
	aaplTrades = AlpacaMarkets.get_stock_trades("AAPL", 
	DateTime("2022-01-27T15:00:00"), DateTime("2022-01-27T15:00:01") )
	
end

# ╔═╡ b16c32a6-cf18-4854-bd7d-020e324b4ff1
md"""
- Column `c` shows the condition code which describes the type of trade. For the first two trades we see:
  - @ : Is a regular trade
  - I: is an odd lot trade
- The `x` column dictates where the trade happened, so the venue that executed the trade. 
- The `z` column tells us what tape the trade was recorded on. 
  - There are three possible tapes, A, B, and C.
"""

# ╔═╡ b9229901-5355-4479-836f-90310ce17a28
md"""
- __Trade conditions__
"""

# ╔═╡ 8c47c245-27e3-4e9b-88bf-10a9b807c451
md"""
| Code |             Value            | Code |                       Value                       |
|:----:|:----------------------------:|:----:|:-------------------------------------------------:|
| @    | Regular Sale                 | R    | Seller                                            |
| A    | Acquisition                  | S    | Split Trade                                       |
| B    | Bunched Trade                | T    | Form T                                            |
| C    | Cash Sale                    | U    | Extended trading hours (Sold Out of Sequence)     |
| D    | Distribution                 | V    | Contingent Trade                                  |
| E    | Placeholder                  | W    | Average Price Trade                               |
| F    | Intermarket Sweep            | X    | Cross Trade                                       |
| G    | Bunched Sold Trade           | Y    | Yellow Flag Regular Trade                         |
| H    | Price Variation Trade        | Z    | Sold (out of sequence)                            |
| I    | Odd Lot Trade                | 1    | Stopped Stock (Regular Trade)                     |
| K    | Rule 155 Trade (AMEX)        | 4    | Derivatively priced                               |
| L    | Sold Last                    | 5    | Re-Opening Prints                                 |
| M    | Market Center Official Close | 6    | Closing Prints                                    |
| N    | Next Day                     | 7    | Qualified Contingent Trade (QCT)                  |
| O    | Opening Prints               | 8    | Placeholder For 611 Exempt                        |
| P    | Prior Reference Price        | 9    | Corrected Consolidated Close (per listing market) |
| Q    | Market Center Official Open  |      |                                                   |
"""

# ╔═╡ 8f7c8ce2-ce16-45fa-8244-3422b1abd03b
md"""
- Again, we convert the timestamp and plot it against the prices. In doing so, We also select only the unique trade ids (`i`) so that each trade is represented once.
"""

# ╔═╡ 03fcdb25-0b2d-4022-8945-f7a66df81419
aaplTrades2 = @chain aaplTrades begin
	unique(_,:i)
	transform(:t => (x-> convert_t_timestamp.(x)) => :TimeStamp,
			  :t => (x-> TimeDate.(string.(chop.(x)))) => :TimeStamp_nano)

end

# ╔═╡ 3760c655-d8a9-4893-a17a-0978ee1a4f40
md"""
- Let's creat the plot next.
"""

# ╔═╡ a345db06-db99-4262-9b18-b4f4ce229515
begin
	plotTrades = plot(aapl2.TimeStamp, aapl2.ap, label="Ask Price", seriestype=:steppre, xticks=(ticks, tick_labels))
	
	plot!(plotTrades, aapl2.TimeStamp, aapl2.bp, label="Bid Price",
	seriestype=:steppre, xticks=(ticks, tick_labels))

	plot!(plotTrades, aaplTrades2.TimeStamp, aaplTrades2.p, label="Trades", 
		seriestype=:scatter)
end

# ╔═╡ b861419a-6e7a-4708-91d1-f160015241c5
md"""
- The trades line up with the prices at the same time and we can see the series of trades that appears to move the price higher between 500 and 750 milliseconds past 15:00.
- Note that all this occured in the second between 15:00:00 and 15:00:01.
  - 344 price updates
  - 384 trades
"""

# ╔═╡ 612c2a18-5038-4b79-b768-fa93ad073cac
md"""
# Equity Venue Analysis
"""

# ╔═╡ 17fc303a-0b4a-4c03-9f6e-c8919e993557
md"""
- Let's look at where stocks are traded and evaluate stock trading venues by
  - How long did they have the best price?
  - How much volume did they have at this best price?
- To do this, let's use Apple quotes over a window of one hour.
"""

# ╔═╡ f9a3339a-4858-4fde-9967-427ea6fc933c
aaplVenue = AlpacaMarkets.get_stock_quotes("AAPL", DateTime("2022-01-27T15:00:00"), DateTime("2022-01-27T16:00:00"))

# ╔═╡ ddbf5a8f-083c-4796-9f92-150e050d963d
md"""
- We will use the TimeDate package to create an object with the correct resolution up to the nanosecond as reported by Alpaca Markets. Then, we calculate how long that price was the best bid or offer using the `diff` function.
"""

# ╔═╡ 837cf9b4-92ba-4539-802d-e3e37e83182d
function get_ns(x)
    getfield(x, :value)
end

# ╔═╡ 7242e288-5eb3-42ab-8a7b-9b7cfdaa361e
begin
	aaplVenue2 = @chain aaplVenue begin
		transform(:t => (x-> convert_t_timestamp.(x)) => :TimeStamp, 
			      :t => (x-> TimeDate.(string.(chop.(x)))) => :TimeStamp_nano)
		transform(:TimeStamp_nano => (x-> [diff(x); NaN]) => :TimeDelta)
	end
	aaplVenue2 = aaplVenue2[1:(end-1), :]
	transform!(aaplVenue2, :TimeDelta => (x->get_ns.(x)) => :ns)
end

# ╔═╡ 9fba2de3-878b-4bcc-95b0-66aa868b925e
md"""
- Next, for each venue, as well as for bid and ask prices, we group by exchange and calculate the following:
  - Number of times it was the best bid and best offer.
  - The average number of shares available at the best bid/ask price.
  - The length of time the quote the best bid or offer.
- This gives us three different values to evaluate of each venue.
"""

# ╔═╡ 16689d26-739c-4581-b210-01998ace0d06
begin
	venue_bids = @chain aaplVenue2 begin
		groupby(:bx)
		combine(:c  => (x->length(x)) => :n_best_bid,
				:as => (x->mean(x))   => :ave_size_bid,
				:ns => (x->mean(x)*1e-9) => :avg_time_best_bid)
		rename(["venue", "n_best_bid", "avg_size_bid", "avg_time_best_bid"])
	end

	venue_asks = @chain aaplVenue2 begin
		groupby(:ax)
		combine(:c  => (x->length(x)) => :n_best_bid,
				:as => (x->mean(x)) => :ave_size_ask,
				:ns => (x->mean(x)*1e-9) => :avg_time_best_ask)
		rename(["venue", "n_best_ask", "avg_size_ask", "avg_time_best_ask"])
	end
	
	venue = leftjoin(venue_bids, venue_asks, on = "venue")
	venue = leftjoin(venue, rename!(AlpacaMarkets.STOCK_EXCHANGES, ["Name", "venue"]), on = "venue")
	
end

# ╔═╡ 6f145c34-a0e9-47ec-99ac-f7cf0d11b4d2
first(venue[!,["Name", "n_best_ask", "avg_size_ask", "avg_time_best_ask"]], 4)

# ╔═╡ 191ddc32-d1f1-44c0-a1d8-2db523997791
md"""
- We can visualize this using a quadrant plot.
"""

# ╔═╡ 0cb113ad-75f0-433d-85f2-e242a18e24fd
begin
	plot(log.(venue.n_best_bid), venue.avg_size_bid, seriestype = :scatter, 
    label = :none, group = venue.venue, 
    series_annotations = text.(venue.Name, :bottom, pointsize=8),
    xlabel = "log (Number of Times Best Bid)",
    ylabel = "Average Bid Size")
	hline!([mean(venue.avg_size_bid)], label=:none, color=:black)
	vline!([mean(log.(venue.n_best_bid))], label=:none, color=:black)
end

# ╔═╡ 5ef1d64f-7854-4dd5-aa4b-d34c2aafdb89
begin
	plot(log.(venue.n_best_ask), venue.avg_time_best_ask, seriestype = :scatter, 
    label = :none, group = venue.venue, 
    series_annotations = text.(venue.Name, :bottom, pointsize=8),
    xlabel = "log (Number of Times Best Ask)",
    ylabel = "Average Time Best Ask (seconds)")
	hline!([mean(venue.avg_time_best_ask)], label=:none, color=:black)
	vline!([mean(log.(venue.n_best_ask))], label=:none, color=:black)
end

# ╔═╡ 581546d5-3e82-4a73-8b3a-b7cc4cd3633d
md"""
- There appear to be two clusters of exchanges and those to the right would score higher in terms of our metrics.  
  - The first plot suggests that the difference between IEX and Members Exchange is about 0.5 in terms of the average bid size. 
"""

# ╔═╡ e589c45b-4e77-4630-8cf4-88c7cf666dc1
md"""
# The Lee-Ready Algorithm
"""

# ╔═╡ b814e8cd-31a7-4d9a-8c61-6f757141fc51
md"""
- Let's consider again the trades we have plotted at the beginning.
"""

# ╔═╡ 0dcbae87-d41a-4926-ac88-462b1c6fb1f4
plotTrades

# ╔═╡ fbe5916a-5788-44a1-b822-233dec173825
md"""
- The trades data from Alpaca Markets have no information on whether the trade is a buy or a sell. Can we infer the sign of the trade?
- Looking at the graph, it looks as if most trades are at the ask price. This suggestst that these are likely buys. Similarly, the trades at the bid are likely sells. 
"""

# ╔═╡ 05838fce-a50e-4b0a-bde1-42bbdfa9daea
md"""
- One commonly-used method of assigning the trade direction (buy or sell) to trades is the [Lee-Ready Algorithm](https://www.jstor.org/stable/2328845). 
- Basically, this algorithm  looks at where the trade occurs relative to the quoted mid-price at the time of the trade. 
  - If the trade is above the mid-price then it is likely that the trade was a buy and vice versa, if it was below it was likely a sell.
"""

# ╔═╡ 28244c87-0ad7-4717-af40-b01bf632e3db
md"""
- To use the `Lee-Ready` algorithm we need to join the trades with the closest prices.
"""

# ╔═╡ 7bff09b3-a8eb-4c96-adc1-be5a89b6babb
begin
	aaplTrades3 = copy(aaplTrades2)
	
	tradeTimes = aaplTrades3.TimeStamp_nano
	quoteTimes = aapl2.TimeStamp_nano
	
	quoteInds = searchsortedlast.([quoteTimes], tradeTimes)
	#See: https://docs.julialang.org/en/v1/base/sort/#Base.Sort.searchsortedlast
	
	aaplTrades3[!, "ap"] = aapl2.ap[quoteInds]
	aaplTrades3[!, "bp"] = aapl2.bp[quoteInds]
	
	transform!(aaplTrades3, [:ap, :bp] => ((a,b)-> (a .+ b) ./ 2) => :Mid)
end

# ╔═╡ 078763d8-8263-41fe-a5da-4e825357148c
aaplTrades3[1:4, ["t", "TimeStamp_nano", "p", "ap", "bp", "Mid"]]

# ╔═╡ 677a8ed3-eb0f-4d08-9379-41df5c9a0bd5
md"""
- Next, we check the sign of the difference between the traded price and the mid-price to classify it as a buy or sell.
"""

# ╔═╡ 8296b9a4-38bf-4381-b10f-9c32cf8b540e
function classify_trade(x)
	if x==0
		return "Unknown"
	elseif x==1
		return "Buy"
	else
		return "Sell"
	end

# ╔═╡ ba17bd98-556c-4033-b8f9-3978f8aedd9e
begin

end

# ╔═╡ f988520b-a61e-44ac-8b59-8840ca035389


# ╔═╡ 77de41e2-4433-44dc-87c5-255b905edd29
md"""
- The results suggest that the Lee-Ready algorithm performs fairly well on average, but we also notice that not all the trades, are classified---some are unknown. In particular, the algorithm seems to struggle around periods where the market starts moving and the mid is volatile.
"""

# ╔═╡ 00000000-0000-0000-0000-000000000001
PLUTO_PROJECT_TOML_CONTENTS = """
[deps]
AlpacaMarkets = "02dc6022-739e-4478-acf8-ef45d2bc67b8"
Chain = "8be319e6-bccf-4806-a6f7-6fae938471bc"
DataFrames = "a93c6f00-e57d-5684-b7b6-d8193f3e46c0"
DataFramesMeta = "1313f7d8-7da2-5740-9ea0-a2ca25f37964"
Dates = "ade2ca70-3891-5945-98fb-dc099432e06a"
Logging = "56ddb016-857b-54e1-b83d-db4d58db5568"
Plots = "91a5bcdd-55d7-5caf-9e0b-520d859cae80"
PlutoUI = "7f904dfe-b85e-4ff6-b463-dae2292396a8"
Printf = "de0858da-6303-5e67-8744-51eddeeeb8d7"
Statistics = "10745b16-79ce-11e8-11f9-7d13ad32a3b2"
TimesDates = "bdfc003b-8df8-5c39-adcd-3a9087f5df4a"

[compat]
AlpacaMarkets = "~0.1.0"
Chain = "~0.4.10"
DataFrames = "~1.3.2"
DataFramesMeta = "~0.10.0"
Plots = "~1.27.4"
PlutoUI = "~0.7.38"
TimesDates = "~0.3.1"
"""

# ╔═╡ 00000000-0000-0000-0000-000000000002
PLUTO_MANIFEST_TOML_CONTENTS = """
# This file is machine-generated - editing it directly is not advised

[[AbstractPlutoDingetjes]]
deps = ["Pkg"]
git-tree-sha1 = "8eaf9f1b4921132a4cff3f36a1d9ba923b14a481"
uuid = "6e696c72-6542-2067-7265-42206c756150"
version = "1.1.4"

[[Adapt]]
deps = ["LinearAlgebra"]
git-tree-sha1 = "af92965fb30777147966f58acb05da51c5616b5f"
uuid = "79e6a3ab-5dfb-504d-930d-738a2a938a0e"
version = "3.3.3"

[[AlpacaMarkets]]
deps = ["DataFrames", "Dates", "HTTP", "JSON", "Test"]
git-tree-sha1 = "5cfd2edf97bfe0478897cc87af40be4c389b8dc7"
uuid = "02dc6022-739e-4478-acf8-ef45d2bc67b8"
version = "0.1.0"

[[ArgTools]]
uuid = "0dad84c5-d112-42e6-8d28-ef12dabb789f"

[[Artifacts]]
uuid = "56f22d72-fd6d-98f1-02f0-08ddc0907c33"

[[Base64]]
uuid = "2a0f44e3-6c83-55bd-87e4-b1978d98bd5f"

[[Bzip2_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "19a35467a82e236ff51bc17a3a44b69ef35185a2"
uuid = "6e34b625-4abd-537c-b88f-471c36dfa7a0"
version = "1.0.8+0"

[[Cairo_jll]]
deps = ["Artifacts", "Bzip2_jll", "Fontconfig_jll", "FreeType2_jll", "Glib_jll", "JLLWrappers", "LZO_jll", "Libdl", "Pixman_jll", "Pkg", "Xorg_libXext_jll", "Xorg_libXrender_jll", "Zlib_jll", "libpng_jll"]
git-tree-sha1 = "4b859a208b2397a7a623a03449e4636bdb17bcf2"
uuid = "83423d85-b0ee-5818-9007-b63ccbeb887a"
version = "1.16.1+1"

[[Chain]]
git-tree-sha1 = "339237319ef4712e6e5df7758d0bccddf5c237d9"
uuid = "8be319e6-bccf-4806-a6f7-6fae938471bc"
version = "0.4.10"

[[ChainRulesCore]]
deps = ["Compat", "LinearAlgebra", "SparseArrays"]
git-tree-sha1 = "9950387274246d08af38f6eef8cb5480862a435f"
uuid = "d360d2e6-b24c-11e9-a2a3-2a2ae2dbcce4"
version = "1.14.0"

[[ChangesOfVariables]]
deps = ["ChainRulesCore", "LinearAlgebra", "Test"]
git-tree-sha1 = "bf98fa45a0a4cee295de98d4c1462be26345b9a1"
uuid = "9e997f8a-9a97-42d5-a9f1-ce6bfc15e2c0"
version = "0.1.2"

[[ColorSchemes]]
deps = ["ColorTypes", "Colors", "FixedPointNumbers", "Random"]
git-tree-sha1 = "12fc73e5e0af68ad3137b886e3f7c1eacfca2640"
uuid = "35d6a980-a343-548e-a6ea-1d62b119f2f4"
version = "3.17.1"

[[ColorTypes]]
deps = ["FixedPointNumbers", "Random"]
git-tree-sha1 = "024fe24d83e4a5bf5fc80501a314ce0d1aa35597"
uuid = "3da002f7-5984-5a60-b8a6-cbb66c0b333f"
version = "0.11.0"

[[Colors]]
deps = ["ColorTypes", "FixedPointNumbers", "Reexport"]
git-tree-sha1 = "417b0ed7b8b838aa6ca0a87aadf1bb9eb111ce40"
uuid = "5ae59095-9a9b-59fe-a467-6f913c188581"
version = "0.12.8"

[[Compat]]
deps = ["Base64", "Dates", "DelimitedFiles", "Distributed", "InteractiveUtils", "LibGit2", "Libdl", "LinearAlgebra", "Markdown", "Mmap", "Pkg", "Printf", "REPL", "Random", "SHA", "Serialization", "SharedArrays", "Sockets", "SparseArrays", "Statistics", "Test", "UUIDs", "Unicode"]
git-tree-sha1 = "96b0bc6c52df76506efc8a441c6cf1adcb1babc4"
uuid = "34da2185-b29b-5c13-b0c7-acf172513d20"
version = "3.42.0"

[[CompilerSupportLibraries_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "e66e0078-7015-5450-92f7-15fbd957f2ae"

[[CompoundPeriods]]
deps = ["Dates"]
git-tree-sha1 = "5879c1c39fea46fb9eb5b8c9323f08f9fb5c4de5"
uuid = "a216cea6-0a8c-5945-ab87-5ade47210022"
version = "0.5.1"

[[Contour]]
deps = ["StaticArrays"]
git-tree-sha1 = "9f02045d934dc030edad45944ea80dbd1f0ebea7"
uuid = "d38c429a-6771-53c6-b99e-75d170b6e991"
version = "0.5.7"

[[Crayons]]
git-tree-sha1 = "249fe38abf76d48563e2f4556bebd215aa317e15"
uuid = "a8cc5b0e-0ffa-5ad4-8c14-923d3ee1735f"
version = "4.1.1"

[[DataAPI]]
git-tree-sha1 = "cc70b17275652eb47bc9e5f81635981f13cea5c8"
uuid = "9a962f9c-6df0-11e9-0e5d-c546b8b5ee8a"
version = "1.9.0"

[[DataFrames]]
deps = ["Compat", "DataAPI", "Future", "InvertedIndices", "IteratorInterfaceExtensions", "LinearAlgebra", "Markdown", "Missings", "PooledArrays", "PrettyTables", "Printf", "REPL", "Reexport", "SortingAlgorithms", "Statistics", "TableTraits", "Tables", "Unicode"]
git-tree-sha1 = "ae02104e835f219b8930c7664b8012c93475c340"
uuid = "a93c6f00-e57d-5684-b7b6-d8193f3e46c0"
version = "1.3.2"

[[DataFramesMeta]]
deps = ["Chain", "DataFrames", "MacroTools", "OrderedCollections", "Reexport"]
git-tree-sha1 = "ab4768d2cc6ab000cd0cec78e8e1ea6b03c7c3e2"
uuid = "1313f7d8-7da2-5740-9ea0-a2ca25f37964"
version = "0.10.0"

[[DataStructures]]
deps = ["Compat", "InteractiveUtils", "OrderedCollections"]
git-tree-sha1 = "3daef5523dd2e769dad2365274f760ff5f282c7d"
uuid = "864edb3b-99cc-5e75-8d2d-829cb0a9cfe8"
version = "0.18.11"

[[DataValueInterfaces]]
git-tree-sha1 = "bfc1187b79289637fa0ef6d4436ebdfe6905cbd6"
uuid = "e2d170a0-9d28-54be-80f0-106bbe20a464"
version = "1.0.0"

[[Dates]]
deps = ["Printf"]
uuid = "ade2ca70-3891-5945-98fb-dc099432e06a"

[[DelimitedFiles]]
deps = ["Mmap"]
uuid = "8bb1440f-4735-579b-a4ab-409b98df4dab"

[[Distributed]]
deps = ["Random", "Serialization", "Sockets"]
uuid = "8ba89e20-285c-5b6f-9357-94700520ee1b"

[[DocStringExtensions]]
deps = ["LibGit2"]
git-tree-sha1 = "b19534d1895d702889b219c382a6e18010797f0b"
uuid = "ffbed154-4ef7-542d-bbb7-c09d3a79fcae"
version = "0.8.6"

[[Downloads]]
deps = ["ArgTools", "LibCURL", "NetworkOptions"]
uuid = "f43a241f-c20a-4ad4-852c-f6b1247861c6"

[[EarCut_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "3f3a2501fa7236e9b911e0f7a588c657e822bb6d"
uuid = "5ae413db-bbd1-5e63-b57d-d24a61df00f5"
version = "2.2.3+0"

[[Expat_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "bad72f730e9e91c08d9427d5e8db95478a3c323d"
uuid = "2e619515-83b5-522b-bb60-26c02a35a201"
version = "2.4.8+0"

[[ExprTools]]
git-tree-sha1 = "56559bbef6ca5ea0c0818fa5c90320398a6fbf8d"
uuid = "e2ba6199-217a-4e67-a87a-7c52f15ade04"
version = "0.1.8"

[[FFMPEG]]
deps = ["FFMPEG_jll"]
git-tree-sha1 = "b57e3acbe22f8484b4b5ff66a7499717fe1a9cc8"
uuid = "c87230d0-a227-11e9-1b43-d7ebe4e7570a"
version = "0.4.1"

[[FFMPEG_jll]]
deps = ["Artifacts", "Bzip2_jll", "FreeType2_jll", "FriBidi_jll", "JLLWrappers", "LAME_jll", "Libdl", "Ogg_jll", "OpenSSL_jll", "Opus_jll", "Pkg", "Zlib_jll", "libass_jll", "libfdk_aac_jll", "libvorbis_jll", "x264_jll", "x265_jll"]
git-tree-sha1 = "d8a578692e3077ac998b50c0217dfd67f21d1e5f"
uuid = "b22a6f82-2f65-5046-a5b2-351ab43fb4e5"
version = "4.4.0+0"

[[FixedPointNumbers]]
deps = ["Statistics"]
git-tree-sha1 = "335bfdceacc84c5cdf16aadc768aa5ddfc5383cc"
uuid = "53c48c17-4a7d-5ca2-90c5-79b7896eea93"
version = "0.8.4"

[[Fontconfig_jll]]
deps = ["Artifacts", "Bzip2_jll", "Expat_jll", "FreeType2_jll", "JLLWrappers", "Libdl", "Libuuid_jll", "Pkg", "Zlib_jll"]
git-tree-sha1 = "21efd19106a55620a188615da6d3d06cd7f6ee03"
uuid = "a3f928ae-7b40-5064-980b-68af3947d34b"
version = "2.13.93+0"

[[Formatting]]
deps = ["Printf"]
git-tree-sha1 = "8339d61043228fdd3eb658d86c926cb282ae72a8"
uuid = "59287772-0a20-5a39-b81b-1366585eb4c0"
version = "0.4.2"

[[FreeType2_jll]]
deps = ["Artifacts", "Bzip2_jll", "JLLWrappers", "Libdl", "Pkg", "Zlib_jll"]
git-tree-sha1 = "87eb71354d8ec1a96d4a7636bd57a7347dde3ef9"
uuid = "d7e528f0-a631-5988-bf34-fe36492bcfd7"
version = "2.10.4+0"

[[FriBidi_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "aa31987c2ba8704e23c6c8ba8a4f769d5d7e4f91"
uuid = "559328eb-81f9-559d-9380-de523a88c83c"
version = "1.0.10+0"

[[Future]]
deps = ["Random"]
uuid = "9fa8497b-333b-5362-9e8d-4d0656e87820"

[[GLFW_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Libglvnd_jll", "Pkg", "Xorg_libXcursor_jll", "Xorg_libXi_jll", "Xorg_libXinerama_jll", "Xorg_libXrandr_jll"]
git-tree-sha1 = "51d2dfe8e590fbd74e7a842cf6d13d8a2f45dc01"
uuid = "0656b61e-2033-5cc2-a64a-77c0f6c09b89"
version = "3.3.6+0"

[[GR]]
deps = ["Base64", "DelimitedFiles", "GR_jll", "HTTP", "JSON", "Libdl", "LinearAlgebra", "Pkg", "Printf", "Random", "RelocatableFolders", "Serialization", "Sockets", "Test", "UUIDs"]
git-tree-sha1 = "af237c08bda486b74318c8070adb96efa6952530"
uuid = "28b8d3ca-fb5f-59d9-8090-bfdbd6d07a71"
version = "0.64.2"

[[GR_jll]]
deps = ["Artifacts", "Bzip2_jll", "Cairo_jll", "FFMPEG_jll", "Fontconfig_jll", "GLFW_jll", "JLLWrappers", "JpegTurbo_jll", "Libdl", "Libtiff_jll", "Pixman_jll", "Pkg", "Qt5Base_jll", "Zlib_jll", "libpng_jll"]
git-tree-sha1 = "cd6efcf9dc746b06709df14e462f0a3fe0786b1e"
uuid = "d2c73de3-f751-5644-a686-071e5b155ba9"
version = "0.64.2+0"

[[GeometryBasics]]
deps = ["EarCut_jll", "IterTools", "LinearAlgebra", "StaticArrays", "StructArrays", "Tables"]
git-tree-sha1 = "83ea630384a13fc4f002b77690bc0afeb4255ac9"
uuid = "5c1252a2-5f33-56bf-86c9-59e7332b4326"
version = "0.4.2"

[[Gettext_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "JLLWrappers", "Libdl", "Libiconv_jll", "Pkg", "XML2_jll"]
git-tree-sha1 = "9b02998aba7bf074d14de89f9d37ca24a1a0b046"
uuid = "78b55507-aeef-58d4-861c-77aaff3498b1"
version = "0.21.0+0"

[[Glib_jll]]
deps = ["Artifacts", "Gettext_jll", "JLLWrappers", "Libdl", "Libffi_jll", "Libiconv_jll", "Libmount_jll", "PCRE_jll", "Pkg", "Zlib_jll"]
git-tree-sha1 = "a32d672ac2c967f3deb8a81d828afc739c838a06"
uuid = "7746bdde-850d-59dc-9ae8-88ece973131d"
version = "2.68.3+2"

[[Graphite2_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "344bf40dcab1073aca04aa0df4fb092f920e4011"
uuid = "3b182d85-2403-5c21-9c21-1e1f0cc25472"
version = "1.3.14+0"

[[Grisu]]
git-tree-sha1 = "53bb909d1151e57e2484c3d1b53e19552b887fb2"
uuid = "42e2da0e-8278-4e71-bc24-59509adca0fe"
version = "1.0.2"

[[HTTP]]
deps = ["Base64", "Dates", "IniFile", "Logging", "MbedTLS", "NetworkOptions", "Sockets", "URIs"]
git-tree-sha1 = "0fa77022fe4b511826b39c894c90daf5fce3334a"
uuid = "cd3eb016-35fb-5094-929b-558a96fad6f3"
version = "0.9.17"

[[HarfBuzz_jll]]
deps = ["Artifacts", "Cairo_jll", "Fontconfig_jll", "FreeType2_jll", "Glib_jll", "Graphite2_jll", "JLLWrappers", "Libdl", "Libffi_jll", "Pkg"]
git-tree-sha1 = "129acf094d168394e80ee1dc4bc06ec835e510a3"
uuid = "2e76f6c2-a576-52d4-95c1-20adfe4de566"
version = "2.8.1+1"

[[Hyperscript]]
deps = ["Test"]
git-tree-sha1 = "8d511d5b81240fc8e6802386302675bdf47737b9"
uuid = "47d2ed2b-36de-50cf-bf87-49c2cf4b8b91"
version = "0.0.4"

[[HypertextLiteral]]
git-tree-sha1 = "2b078b5a615c6c0396c77810d92ee8c6f470d238"
uuid = "ac1192a8-f4b3-4bfe-ba22-af5b92cd3ab2"
version = "0.9.3"

[[IOCapture]]
deps = ["Logging", "Random"]
git-tree-sha1 = "f7be53659ab06ddc986428d3a9dcc95f6fa6705a"
uuid = "b5f81e59-6552-4d32-b1f0-c071b021bf89"
version = "0.2.2"

[[IniFile]]
git-tree-sha1 = "f550e6e32074c939295eb5ea6de31849ac2c9625"
uuid = "83e8ac13-25f8-5344-8a64-a9f2b223428f"
version = "0.5.1"

[[InlineStrings]]
deps = ["Parsers"]
git-tree-sha1 = "61feba885fac3a407465726d0c330b3055df897f"
uuid = "842dd82b-1e85-43dc-bf29-5d0ee9dffc48"
version = "1.1.2"

[[InteractiveUtils]]
deps = ["Markdown"]
uuid = "b77e0a4c-d291-57a0-90e8-8db25a27a240"

[[InverseFunctions]]
deps = ["Test"]
git-tree-sha1 = "91b5dcf362c5add98049e6c29ee756910b03051d"
uuid = "3587e190-3f89-42d0-90ee-14403ec27112"
version = "0.1.3"

[[InvertedIndices]]
git-tree-sha1 = "bee5f1ef5bf65df56bdd2e40447590b272a5471f"
uuid = "41ab1584-1d38-5bbf-9106-f11c6c58b48f"
version = "1.1.0"

[[IrrationalConstants]]
git-tree-sha1 = "7fd44fd4ff43fc60815f8e764c0f352b83c49151"
uuid = "92d709cd-6900-40b7-9082-c6be49f344b6"
version = "0.1.1"

[[IterTools]]
git-tree-sha1 = "fa6287a4469f5e048d763df38279ee729fbd44e5"
uuid = "c8e1da08-722c-5040-9ed9-7db0dc04731e"
version = "1.4.0"

[[IteratorInterfaceExtensions]]
git-tree-sha1 = "a3f24677c21f5bbe9d2a714f95dcd58337fb2856"
uuid = "82899510-4779-5014-852e-03e436cf321d"
version = "1.0.0"

[[JLLWrappers]]
deps = ["Preferences"]
git-tree-sha1 = "abc9885a7ca2052a736a600f7fa66209f96506e1"
uuid = "692b3bcd-3c85-4b1f-b108-f13ce0eb3210"
version = "1.4.1"

[[JSON]]
deps = ["Dates", "Mmap", "Parsers", "Unicode"]
git-tree-sha1 = "3c837543ddb02250ef42f4738347454f95079d4e"
uuid = "682c06a0-de6a-54ab-a142-c8b1cf79cde6"
version = "0.21.3"

[[JpegTurbo_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "b53380851c6e6664204efb2e62cd24fa5c47e4ba"
uuid = "aacddb02-875f-59d6-b918-886e6ef4fbf8"
version = "2.1.2+0"

[[LAME_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "f6250b16881adf048549549fba48b1161acdac8c"
uuid = "c1c5ebd0-6772-5130-a774-d5fcae4a789d"
version = "3.100.1+0"

[[LERC_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "bf36f528eec6634efc60d7ec062008f171071434"
uuid = "88015f11-f218-50d7-93a8-a6af411a945d"
version = "3.0.0+1"

[[LZO_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "e5b909bcf985c5e2605737d2ce278ed791b89be6"
uuid = "dd4b983a-f0e5-5f8d-a1b7-129d4a5fb1ac"
version = "2.10.1+0"

[[LaTeXStrings]]
git-tree-sha1 = "f2355693d6778a178ade15952b7ac47a4ff97996"
uuid = "b964fa9f-0449-5b57-a5c2-d3ea65f4040f"
version = "1.3.0"

[[Latexify]]
deps = ["Formatting", "InteractiveUtils", "LaTeXStrings", "MacroTools", "Markdown", "Printf", "Requires"]
git-tree-sha1 = "6f14549f7760d84b2db7a9b10b88cd3cc3025730"
uuid = "23fbe1c1-3f47-55db-b15f-69d7ec21a316"
version = "0.15.14"

[[LazyArtifacts]]
deps = ["Artifacts", "Pkg"]
uuid = "4af54fe1-eca0-43a8-85a7-787d91b784e3"

[[LibCURL]]
deps = ["LibCURL_jll", "MozillaCACerts_jll"]
uuid = "b27032c2-a3e7-50c8-80cd-2d36dbcbfd21"

[[LibCURL_jll]]
deps = ["Artifacts", "LibSSH2_jll", "Libdl", "MbedTLS_jll", "Zlib_jll", "nghttp2_jll"]
uuid = "deac9b47-8bc7-5906-a0fe-35ac56dc84c0"

[[LibGit2]]
deps = ["Base64", "NetworkOptions", "Printf", "SHA"]
uuid = "76f85450-5226-5b5a-8eaa-529ad045b433"

[[LibSSH2_jll]]
deps = ["Artifacts", "Libdl", "MbedTLS_jll"]
uuid = "29816b5a-b9ab-546f-933c-edad1886dfa8"

[[Libdl]]
uuid = "8f399da3-3557-5675-b5ff-fb832c97cbdb"

[[Libffi_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "0b4a5d71f3e5200a7dff793393e09dfc2d874290"
uuid = "e9f186c6-92d2-5b65-8a66-fee21dc1b490"
version = "3.2.2+1"

[[Libgcrypt_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Libgpg_error_jll", "Pkg"]
git-tree-sha1 = "64613c82a59c120435c067c2b809fc61cf5166ae"
uuid = "d4300ac3-e22c-5743-9152-c294e39db1e4"
version = "1.8.7+0"

[[Libglvnd_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libX11_jll", "Xorg_libXext_jll"]
git-tree-sha1 = "7739f837d6447403596a75d19ed01fd08d6f56bf"
uuid = "7e76a0d4-f3c7-5321-8279-8d96eeed0f29"
version = "1.3.0+3"

[[Libgpg_error_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "c333716e46366857753e273ce6a69ee0945a6db9"
uuid = "7add5ba3-2f88-524e-9cd5-f83b8a55f7b8"
version = "1.42.0+0"

[[Libiconv_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "42b62845d70a619f063a7da093d995ec8e15e778"
uuid = "94ce4f54-9a6c-5748-9c1c-f9c7231a4531"
version = "1.16.1+1"

[[Libmount_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "9c30530bf0effd46e15e0fdcf2b8636e78cbbd73"
uuid = "4b2f31a3-9ecc-558c-b454-b3730dcb73e9"
version = "2.35.0+0"

[[Libtiff_jll]]
deps = ["Artifacts", "JLLWrappers", "JpegTurbo_jll", "LERC_jll", "Libdl", "Pkg", "Zlib_jll", "Zstd_jll"]
git-tree-sha1 = "c9551dd26e31ab17b86cbd00c2ede019c08758eb"
uuid = "89763e89-9b03-5906-acba-b20f662cd828"
version = "4.3.0+1"

[[Libuuid_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "7f3efec06033682db852f8b3bc3c1d2b0a0ab066"
uuid = "38a345b3-de98-5d2b-a5d3-14cd9215e700"
version = "2.36.0+0"

[[LinearAlgebra]]
deps = ["Libdl"]
uuid = "37e2e46d-f89d-539d-b4ee-838fcccc9c8e"

[[LogExpFunctions]]
deps = ["ChainRulesCore", "ChangesOfVariables", "DocStringExtensions", "InverseFunctions", "IrrationalConstants", "LinearAlgebra"]
git-tree-sha1 = "58f25e56b706f95125dcb796f39e1fb01d913a71"
uuid = "2ab3a3ac-af41-5b50-aa03-7779005ae688"
version = "0.3.10"

[[Logging]]
uuid = "56ddb016-857b-54e1-b83d-db4d58db5568"

[[MacroTools]]
deps = ["Markdown", "Random"]
git-tree-sha1 = "3d3e902b31198a27340d0bf00d6ac452866021cf"
uuid = "1914dd2f-81c6-5fcd-8719-6d5c9610ff09"
version = "0.5.9"

[[Markdown]]
deps = ["Base64"]
uuid = "d6f4376e-aef5-505a-96c1-9c027394607a"

[[MbedTLS]]
deps = ["Dates", "MbedTLS_jll", "Random", "Sockets"]
git-tree-sha1 = "1c38e51c3d08ef2278062ebceade0e46cefc96fe"
uuid = "739be429-bea8-5141-9913-cc70e7f3736d"
version = "1.0.3"

[[MbedTLS_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "c8ffd9c3-330d-5841-b78e-0817d7145fa1"

[[Measures]]
git-tree-sha1 = "e498ddeee6f9fdb4551ce855a46f54dbd900245f"
uuid = "442fdcdd-2543-5da2-b0f3-8c86c306513e"
version = "0.3.1"

[[Missings]]
deps = ["DataAPI"]
git-tree-sha1 = "bf210ce90b6c9eed32d25dbcae1ebc565df2687f"
uuid = "e1d29d7a-bbdc-5cf2-9ac0-f12de2c33e28"
version = "1.0.2"

[[Mmap]]
uuid = "a63ad114-7e13-5084-954f-fe012c677804"

[[Mocking]]
deps = ["Compat", "ExprTools"]
git-tree-sha1 = "29714d0a7a8083bba8427a4fbfb00a540c681ce7"
uuid = "78c3b35d-d492-501b-9361-3d52fe80e533"
version = "0.7.3"

[[MozillaCACerts_jll]]
uuid = "14a3606d-f60d-562e-9121-12d972cd8159"

[[NaNMath]]
git-tree-sha1 = "737a5957f387b17e74d4ad2f440eb330b39a62c5"
uuid = "77ba4419-2d1f-58cd-9bb1-8ffee604a2e3"
version = "1.0.0"

[[NetworkOptions]]
uuid = "ca575930-c2e3-43a9-ace4-1e988b2c1908"

[[Ogg_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "887579a3eb005446d514ab7aeac5d1d027658b8f"
uuid = "e7412a2a-1a6e-54c0-be00-318e2571c051"
version = "1.3.5+1"

[[OpenSSL_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "ab05aa4cc89736e95915b01e7279e61b1bfe33b8"
uuid = "458c3c95-2e84-50aa-8efc-19380b2a3a95"
version = "1.1.14+0"

[[Opus_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "51a08fb14ec28da2ec7a927c4337e4332c2a4720"
uuid = "91d4177d-7536-5919-b921-800302f37372"
version = "1.3.2+0"

[[OrderedCollections]]
git-tree-sha1 = "85f8e6578bf1f9ee0d11e7bb1b1456435479d47c"
uuid = "bac558e1-5e72-5ebc-8fee-abe8a469f55d"
version = "1.4.1"

[[PCRE_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "b2a7af664e098055a7529ad1a900ded962bca488"
uuid = "2f80f16e-611a-54ab-bc61-aa92de5b98fc"
version = "8.44.0+0"

[[Parsers]]
deps = ["Dates"]
git-tree-sha1 = "85b5da0fa43588c75bb1ff986493443f821c70b7"
uuid = "69de0a69-1ddd-5017-9359-2bf0b02dc9f0"
version = "2.2.3"

[[Pixman_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "b4f5d02549a10e20780a24fce72bea96b6329e29"
uuid = "30392449-352a-5448-841d-b1acce4e97dc"
version = "0.40.1+0"

[[Pkg]]
deps = ["Artifacts", "Dates", "Downloads", "LibGit2", "Libdl", "Logging", "Markdown", "Printf", "REPL", "Random", "SHA", "Serialization", "TOML", "Tar", "UUIDs", "p7zip_jll"]
uuid = "44cfe95a-1eb2-52ea-b672-e2afdf69b78f"

[[PlotThemes]]
deps = ["PlotUtils", "Requires", "Statistics"]
git-tree-sha1 = "a3a964ce9dc7898193536002a6dd892b1b5a6f1d"
uuid = "ccf2f8ad-2431-5c83-bf29-c5338b663b6a"
version = "2.0.1"

[[PlotUtils]]
deps = ["ColorSchemes", "Colors", "Dates", "Printf", "Random", "Reexport", "Statistics"]
git-tree-sha1 = "bb16469fd5224100e422f0b027d26c5a25de1200"
uuid = "995b91a9-d308-5afd-9ec6-746e21dbc043"
version = "1.2.0"

[[Plots]]
deps = ["Base64", "Contour", "Dates", "Downloads", "FFMPEG", "FixedPointNumbers", "GR", "GeometryBasics", "JSON", "Latexify", "LinearAlgebra", "Measures", "NaNMath", "Pkg", "PlotThemes", "PlotUtils", "Printf", "REPL", "Random", "RecipesBase", "RecipesPipeline", "Reexport", "Requires", "Scratch", "Showoff", "SparseArrays", "Statistics", "StatsBase", "UUIDs", "UnicodeFun", "Unzip"]
git-tree-sha1 = "edec0846433f1c1941032385588fd57380b62b59"
uuid = "91a5bcdd-55d7-5caf-9e0b-520d859cae80"
version = "1.27.4"

[[PlutoUI]]
deps = ["AbstractPlutoDingetjes", "Base64", "ColorTypes", "Dates", "Hyperscript", "HypertextLiteral", "IOCapture", "InteractiveUtils", "JSON", "Logging", "Markdown", "Random", "Reexport", "UUIDs"]
git-tree-sha1 = "670e559e5c8e191ded66fa9ea89c97f10376bb4c"
uuid = "7f904dfe-b85e-4ff6-b463-dae2292396a8"
version = "0.7.38"

[[PooledArrays]]
deps = ["DataAPI", "Future"]
git-tree-sha1 = "db3a23166af8aebf4db5ef87ac5b00d36eb771e2"
uuid = "2dfb63ee-cc39-5dd5-95bd-886bf059d720"
version = "1.4.0"

[[Preferences]]
deps = ["TOML"]
git-tree-sha1 = "d3538e7f8a790dc8903519090857ef8e1283eecd"
uuid = "21216c6a-2e73-6563-6e65-726566657250"
version = "1.2.5"

[[PrettyTables]]
deps = ["Crayons", "Formatting", "Markdown", "Reexport", "Tables"]
git-tree-sha1 = "dfb54c4e414caa595a1f2ed759b160f5a3ddcba5"
uuid = "08abe8d2-0d0c-5749-adfa-8a2ac140af0d"
version = "1.3.1"

[[Printf]]
deps = ["Unicode"]
uuid = "de0858da-6303-5e67-8744-51eddeeeb8d7"

[[Qt5Base_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "Fontconfig_jll", "Glib_jll", "JLLWrappers", "Libdl", "Libglvnd_jll", "OpenSSL_jll", "Pkg", "Xorg_libXext_jll", "Xorg_libxcb_jll", "Xorg_xcb_util_image_jll", "Xorg_xcb_util_keysyms_jll", "Xorg_xcb_util_renderutil_jll", "Xorg_xcb_util_wm_jll", "Zlib_jll", "xkbcommon_jll"]
git-tree-sha1 = "c6c0f690d0cc7caddb74cef7aa847b824a16b256"
uuid = "ea2cea3b-5b76-57ae-a6ef-0a8af62496e1"
version = "5.15.3+1"

[[REPL]]
deps = ["InteractiveUtils", "Markdown", "Sockets", "Unicode"]
uuid = "3fa0cd96-eef1-5676-8a61-b3b8758bbffb"

[[Random]]
deps = ["Serialization"]
uuid = "9a3f8284-a2c9-5f02-9a11-845980a1fd5c"

[[RecipesBase]]
git-tree-sha1 = "6bf3f380ff52ce0832ddd3a2a7b9538ed1bcca7d"
uuid = "3cdcf5f2-1ef4-517c-9805-6587b60abb01"
version = "1.2.1"

[[RecipesPipeline]]
deps = ["Dates", "NaNMath", "PlotUtils", "RecipesBase"]
git-tree-sha1 = "dc1e451e15d90347a7decc4221842a022b011714"
uuid = "01d81517-befc-4cb6-b9ec-a95719d0359c"
version = "0.5.2"

[[Reexport]]
git-tree-sha1 = "45e428421666073eab6f2da5c9d310d99bb12f9b"
uuid = "189a3867-3050-52da-a836-e630ba90ab69"
version = "1.2.2"

[[RelocatableFolders]]
deps = ["SHA", "Scratch"]
git-tree-sha1 = "cdbd3b1338c72ce29d9584fdbe9e9b70eeb5adca"
uuid = "05181044-ff0b-4ac5-8273-598c1e38db00"
version = "0.1.3"

[[Requires]]
deps = ["UUIDs"]
git-tree-sha1 = "838a3a4188e2ded87a4f9f184b4b0d78a1e91cb7"
uuid = "ae029012-a4dd-5104-9daa-d747884805df"
version = "1.3.0"

[[SHA]]
uuid = "ea8e919c-243c-51af-8825-aaa63cd721ce"

[[Scratch]]
deps = ["Dates"]
git-tree-sha1 = "0b4b7f1393cff97c33891da2a0bf69c6ed241fda"
uuid = "6c6a2e73-6563-6170-7368-637461726353"
version = "1.1.0"

[[Serialization]]
uuid = "9e88b42a-f829-5b0c-bbe9-9e923198166b"

[[SharedArrays]]
deps = ["Distributed", "Mmap", "Random", "Serialization"]
uuid = "1a1011a3-84de-559e-8e89-a11a2f7dc383"

[[Showoff]]
deps = ["Dates", "Grisu"]
git-tree-sha1 = "91eddf657aca81df9ae6ceb20b959ae5653ad1de"
uuid = "992d4aef-0814-514b-bc4d-f2e9a6c4116f"
version = "1.0.3"

[[Sockets]]
uuid = "6462fe0b-24de-5631-8697-dd941f90decc"

[[SortingAlgorithms]]
deps = ["DataStructures"]
git-tree-sha1 = "b3363d7460f7d098ca0912c69b082f75625d7508"
uuid = "a2af1166-a08f-5f64-846c-94a0d3cef48c"
version = "1.0.1"

[[SparseArrays]]
deps = ["LinearAlgebra", "Random"]
uuid = "2f01184e-e22b-5df5-ae63-d93ebab69eaf"

[[StaticArrays]]
deps = ["LinearAlgebra", "Random", "Statistics"]
git-tree-sha1 = "4f6ec5d99a28e1a749559ef7dd518663c5eca3d5"
uuid = "90137ffa-7385-5640-81b9-e52037218182"
version = "1.4.3"

[[Statistics]]
deps = ["LinearAlgebra", "SparseArrays"]
uuid = "10745b16-79ce-11e8-11f9-7d13ad32a3b2"

[[StatsAPI]]
deps = ["LinearAlgebra"]
git-tree-sha1 = "c3d8ba7f3fa0625b062b82853a7d5229cb728b6b"
uuid = "82ae8749-77ed-4fe6-ae5f-f523153014b0"
version = "1.2.1"

[[StatsBase]]
deps = ["DataAPI", "DataStructures", "LinearAlgebra", "LogExpFunctions", "Missings", "Printf", "Random", "SortingAlgorithms", "SparseArrays", "Statistics", "StatsAPI"]
git-tree-sha1 = "8977b17906b0a1cc74ab2e3a05faa16cf08a8291"
uuid = "2913bbd2-ae8a-5f71-8c99-4fb6c76f3a91"
version = "0.33.16"

[[StructArrays]]
deps = ["Adapt", "DataAPI", "StaticArrays", "Tables"]
git-tree-sha1 = "57617b34fa34f91d536eb265df67c2d4519b8b98"
uuid = "09ab397b-f2b6-538f-b94a-2f83cf4a842a"
version = "0.6.5"

[[TOML]]
deps = ["Dates"]
uuid = "fa267f1f-6049-4f14-aa54-33bafae1ed76"

[[TableTraits]]
deps = ["IteratorInterfaceExtensions"]
git-tree-sha1 = "c06b2f539df1c6efa794486abfb6ed2022561a39"
uuid = "3783bdb8-4a98-5b6b-af9a-565f29a5fe9c"
version = "1.0.1"

[[Tables]]
deps = ["DataAPI", "DataValueInterfaces", "IteratorInterfaceExtensions", "LinearAlgebra", "OrderedCollections", "TableTraits", "Test"]
git-tree-sha1 = "5ce79ce186cc678bbb5c5681ca3379d1ddae11a1"
uuid = "bd369af6-aec1-5ad0-b16a-f7cc5008161c"
version = "1.7.0"

[[Tar]]
deps = ["ArgTools", "SHA"]
uuid = "a4e569a6-e804-4fa4-b0f3-eef7a1d5b13e"

[[Test]]
deps = ["InteractiveUtils", "Logging", "Random", "Serialization"]
uuid = "8dfed614-e22c-5e08-85e1-65c5234f0b40"

[[TimeZones]]
deps = ["Dates", "Downloads", "InlineStrings", "LazyArtifacts", "Mocking", "Printf", "RecipesBase", "Serialization", "Unicode"]
git-tree-sha1 = "2d4b6de8676b34525ac518de36006dc2e89c7e2e"
uuid = "f269a46b-ccf7-5d73-abea-4c690281aa53"
version = "1.7.2"

[[TimesDates]]
deps = ["CompoundPeriods", "Dates", "TimeZones"]
git-tree-sha1 = "4ca99fd8145f6ae574b6f98e1233148e7b91ac30"
uuid = "bdfc003b-8df8-5c39-adcd-3a9087f5df4a"
version = "0.3.1"

[[URIs]]
git-tree-sha1 = "97bbe755a53fe859669cd907f2d96aee8d2c1355"
uuid = "5c2747f8-b7ea-4ff2-ba2e-563bfd36b1d4"
version = "1.3.0"

[[UUIDs]]
deps = ["Random", "SHA"]
uuid = "cf7118a7-6976-5b1a-9a39-7adc72f591a4"

[[Unicode]]
uuid = "4ec0a83e-493e-50e2-b9ac-8f72acf5a8f5"

[[UnicodeFun]]
deps = ["REPL"]
git-tree-sha1 = "53915e50200959667e78a92a418594b428dffddf"
uuid = "1cfade01-22cf-5700-b092-accc4b62d6e1"
version = "0.4.1"

[[Unzip]]
git-tree-sha1 = "34db80951901073501137bdbc3d5a8e7bbd06670"
uuid = "41fe7b60-77ed-43a1-b4f0-825fd5a5650d"
version = "0.1.2"

[[Wayland_jll]]
deps = ["Artifacts", "Expat_jll", "JLLWrappers", "Libdl", "Libffi_jll", "Pkg", "XML2_jll"]
git-tree-sha1 = "3e61f0b86f90dacb0bc0e73a0c5a83f6a8636e23"
uuid = "a2964d1f-97da-50d4-b82a-358c7fce9d89"
version = "1.19.0+0"

[[Wayland_protocols_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "4528479aa01ee1b3b4cd0e6faef0e04cf16466da"
uuid = "2381bf8a-dfd0-557d-9999-79630e7b1b91"
version = "1.25.0+0"

[[XML2_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Libiconv_jll", "Pkg", "Zlib_jll"]
git-tree-sha1 = "1acf5bdf07aa0907e0a37d3718bb88d4b687b74a"
uuid = "02c8fc9c-b97f-50b9-bbe4-9be30ff0a78a"
version = "2.9.12+0"

[[XSLT_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Libgcrypt_jll", "Libgpg_error_jll", "Libiconv_jll", "Pkg", "XML2_jll", "Zlib_jll"]
git-tree-sha1 = "91844873c4085240b95e795f692c4cec4d805f8a"
uuid = "aed1982a-8fda-507f-9586-7b0439959a61"
version = "1.1.34+0"

[[Xorg_libX11_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libxcb_jll", "Xorg_xtrans_jll"]
git-tree-sha1 = "5be649d550f3f4b95308bf0183b82e2582876527"
uuid = "4f6342f7-b3d2-589e-9d20-edeb45f2b2bc"
version = "1.6.9+4"

[[Xorg_libXau_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "4e490d5c960c314f33885790ed410ff3a94ce67e"
uuid = "0c0b7dd1-d40b-584c-a123-a41640f87eec"
version = "1.0.9+4"

[[Xorg_libXcursor_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libXfixes_jll", "Xorg_libXrender_jll"]
git-tree-sha1 = "12e0eb3bc634fa2080c1c37fccf56f7c22989afd"
uuid = "935fb764-8cf2-53bf-bb30-45bb1f8bf724"
version = "1.2.0+4"

[[Xorg_libXdmcp_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "4fe47bd2247248125c428978740e18a681372dd4"
uuid = "a3789734-cfe1-5b06-b2d0-1dd0d9d62d05"
version = "1.1.3+4"

[[Xorg_libXext_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libX11_jll"]
git-tree-sha1 = "b7c0aa8c376b31e4852b360222848637f481f8c3"
uuid = "1082639a-0dae-5f34-9b06-72781eeb8cb3"
version = "1.3.4+4"

[[Xorg_libXfixes_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libX11_jll"]
git-tree-sha1 = "0e0dc7431e7a0587559f9294aeec269471c991a4"
uuid = "d091e8ba-531a-589c-9de9-94069b037ed8"
version = "5.0.3+4"

[[Xorg_libXi_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libXext_jll", "Xorg_libXfixes_jll"]
git-tree-sha1 = "89b52bc2160aadc84d707093930ef0bffa641246"
uuid = "a51aa0fd-4e3c-5386-b890-e753decda492"
version = "1.7.10+4"

[[Xorg_libXinerama_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libXext_jll"]
git-tree-sha1 = "26be8b1c342929259317d8b9f7b53bf2bb73b123"
uuid = "d1454406-59df-5ea1-beac-c340f2130bc3"
version = "1.1.4+4"

[[Xorg_libXrandr_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libXext_jll", "Xorg_libXrender_jll"]
git-tree-sha1 = "34cea83cb726fb58f325887bf0612c6b3fb17631"
uuid = "ec84b674-ba8e-5d96-8ba1-2a689ba10484"
version = "1.5.2+4"

[[Xorg_libXrender_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libX11_jll"]
git-tree-sha1 = "19560f30fd49f4d4efbe7002a1037f8c43d43b96"
uuid = "ea2f1a96-1ddc-540d-b46f-429655e07cfa"
version = "0.9.10+4"

[[Xorg_libpthread_stubs_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "6783737e45d3c59a4a4c4091f5f88cdcf0908cbb"
uuid = "14d82f49-176c-5ed1-bb49-ad3f5cbd8c74"
version = "0.1.0+3"

[[Xorg_libxcb_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "XSLT_jll", "Xorg_libXau_jll", "Xorg_libXdmcp_jll", "Xorg_libpthread_stubs_jll"]
git-tree-sha1 = "daf17f441228e7a3833846cd048892861cff16d6"
uuid = "c7cfdc94-dc32-55de-ac96-5a1b8d977c5b"
version = "1.13.0+3"

[[Xorg_libxkbfile_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libX11_jll"]
git-tree-sha1 = "926af861744212db0eb001d9e40b5d16292080b2"
uuid = "cc61e674-0454-545c-8b26-ed2c68acab7a"
version = "1.1.0+4"

[[Xorg_xcb_util_image_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_xcb_util_jll"]
git-tree-sha1 = "0fab0a40349ba1cba2c1da699243396ff8e94b97"
uuid = "12413925-8142-5f55-bb0e-6d7ca50bb09b"
version = "0.4.0+1"

[[Xorg_xcb_util_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libxcb_jll"]
git-tree-sha1 = "e7fd7b2881fa2eaa72717420894d3938177862d1"
uuid = "2def613f-5ad1-5310-b15b-b15d46f528f5"
version = "0.4.0+1"

[[Xorg_xcb_util_keysyms_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_xcb_util_jll"]
git-tree-sha1 = "d1151e2c45a544f32441a567d1690e701ec89b00"
uuid = "975044d2-76e6-5fbe-bf08-97ce7c6574c7"
version = "0.4.0+1"

[[Xorg_xcb_util_renderutil_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_xcb_util_jll"]
git-tree-sha1 = "dfd7a8f38d4613b6a575253b3174dd991ca6183e"
uuid = "0d47668e-0667-5a69-a72c-f761630bfb7e"
version = "0.3.9+1"

[[Xorg_xcb_util_wm_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_xcb_util_jll"]
git-tree-sha1 = "e78d10aab01a4a154142c5006ed44fd9e8e31b67"
uuid = "c22f9ab0-d5fe-5066-847c-f4bb1cd4e361"
version = "0.4.1+1"

[[Xorg_xkbcomp_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libxkbfile_jll"]
git-tree-sha1 = "4bcbf660f6c2e714f87e960a171b119d06ee163b"
uuid = "35661453-b289-5fab-8a00-3d9160c6a3a4"
version = "1.4.2+4"

[[Xorg_xkeyboard_config_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_xkbcomp_jll"]
git-tree-sha1 = "5c8424f8a67c3f2209646d4425f3d415fee5931d"
uuid = "33bec58e-1273-512f-9401-5d533626f822"
version = "2.27.0+4"

[[Xorg_xtrans_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "79c31e7844f6ecf779705fbc12146eb190b7d845"
uuid = "c5fb5394-a638-5e4d-96e5-b29de1b5cf10"
version = "1.4.0+3"

[[Zlib_jll]]
deps = ["Libdl"]
uuid = "83775a58-1f1d-513f-b197-d71354ab007a"

[[Zstd_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "e45044cd873ded54b6a5bac0eb5c971392cf1927"
uuid = "3161d3a3-bdf6-5164-811a-617609db77b4"
version = "1.5.2+0"

[[libass_jll]]
deps = ["Artifacts", "Bzip2_jll", "FreeType2_jll", "FriBidi_jll", "HarfBuzz_jll", "JLLWrappers", "Libdl", "Pkg", "Zlib_jll"]
git-tree-sha1 = "5982a94fcba20f02f42ace44b9894ee2b140fe47"
uuid = "0ac62f75-1d6f-5e53-bd7c-93b484bb37c0"
version = "0.15.1+0"

[[libfdk_aac_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "daacc84a041563f965be61859a36e17c4e4fcd55"
uuid = "f638f0a6-7fb0-5443-88ba-1cc74229b280"
version = "2.0.2+0"

[[libpng_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Zlib_jll"]
git-tree-sha1 = "94d180a6d2b5e55e447e2d27a29ed04fe79eb30c"
uuid = "b53b4c65-9356-5827-b1ea-8c7a1a84506f"
version = "1.6.38+0"

[[libvorbis_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Ogg_jll", "Pkg"]
git-tree-sha1 = "b910cb81ef3fe6e78bf6acee440bda86fd6ae00c"
uuid = "f27f6e37-5d2b-51aa-960f-b287f2bc3b7a"
version = "1.3.7+1"

[[nghttp2_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "8e850ede-7688-5339-a07c-302acd2aaf8d"

[[p7zip_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "3f19e933-33d8-53b3-aaab-bd5110c3b7a0"

[[x264_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "4fea590b89e6ec504593146bf8b988b2c00922b2"
uuid = "1270edf5-f2f9-52d2-97e9-ab00b5d0237a"
version = "2021.5.5+0"

[[x265_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "ee567a171cce03570d77ad3a43e90218e38937a9"
uuid = "dfaa095f-4041-5dcd-9319-2fabd8486b76"
version = "3.5.0+0"

[[xkbcommon_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Wayland_jll", "Wayland_protocols_jll", "Xorg_libxcb_jll", "Xorg_xkeyboard_config_jll"]
git-tree-sha1 = "ece2350174195bb31de1a63bea3a41ae1aa593b6"
uuid = "d8fb68d0-12a3-5cfd-a85a-d49703b185fd"
version = "0.9.1+5"
"""

# ╔═╡ Cell order:
# ╟─6199926c-0c4a-4991-b4c3-ebed35217375
# ╟─b0b9984b-45f5-49a2-a5ce-86281745d623
# ╟─886b6ade-b8df-494c-be5f-13aae328b848
# ╟─26a704bf-6b13-45f2-9211-023fbf2e3cb6
# ╟─1897543d-5c47-41ee-9f13-a099df5d9ed1
# ╟─89d72808-d09e-421a-ae7b-5491e8fae0ee
# ╟─dff6f3d9-f4fc-4964-a075-c1c11a953667
# ╟─931659d2-cae1-43d3-87d9-b67edb7c16d0
# ╟─c8cba87a-6ead-4f6a-b29b-bc3bcc3e75f0
# ╟─50fb3be3-1674-47a3-9102-0c6fb24241de
# ╟─81f0904b-3da6-4981-971b-b5e11e301eaa
# ╠═6c003a32-ddb4-462b-bc73-3bbf633f184f
# ╠═34545087-b68d-4271-9771-4686b770c619
# ╟─4a235440-1810-4b18-85f9-377f9f5f6876
# ╟─80b29628-2214-49ab-8eb7-31e99677fb1b
# ╠═8560c709-61bd-4d7b-a7cd-78c9861b54cc
# ╟─96ec6830-4c58-48b5-8538-236dc2a3c599
# ╟─022c457f-83c1-4059-8730-6f788b1ec479
# ╟─1dadf403-6dff-48d7-bb24-eae5c1147d7d
# ╟─0fbd9fc7-2e7b-4559-a34b-17528e507ebe
# ╟─a099bcf5-76df-468b-82df-cf29a914be9c
# ╟─6978ce4e-0880-4d37-b894-caa63c5ff91f
# ╠═bb9d0cfe-7f25-464e-bcd6-09d732c36eac
# ╟─e861bcff-9e3f-453a-989f-1fbd342bccf8
# ╠═3a4c94b5-6fd3-4253-839b-e898ab9025be
# ╟─3bf77a4e-5019-4ad6-9e05-60d2dff701e6
# ╠═435f564e-c550-4a54-bb7d-86ed3d3017bc
# ╟─04c1af80-11cd-4a97-8d8c-5e35fd233d24
# ╟─77339769-19c5-4299-bacc-72719804b1e5
# ╠═e398ea5c-575e-4aa5-ad3b-2dd3cc0c1164
# ╟─b16c32a6-cf18-4854-bd7d-020e324b4ff1
# ╟─b9229901-5355-4479-836f-90310ce17a28
# ╟─8c47c245-27e3-4e9b-88bf-10a9b807c451
# ╟─8f7c8ce2-ce16-45fa-8244-3422b1abd03b
# ╠═03fcdb25-0b2d-4022-8945-f7a66df81419
# ╟─3760c655-d8a9-4893-a17a-0978ee1a4f40
# ╠═a345db06-db99-4262-9b18-b4f4ce229515
# ╟─b861419a-6e7a-4708-91d1-f160015241c5
# ╟─612c2a18-5038-4b79-b768-fa93ad073cac
# ╟─17fc303a-0b4a-4c03-9f6e-c8919e993557
# ╠═f9a3339a-4858-4fde-9967-427ea6fc933c
# ╟─ddbf5a8f-083c-4796-9f92-150e050d963d
# ╠═837cf9b4-92ba-4539-802d-e3e37e83182d
# ╠═7242e288-5eb3-42ab-8a7b-9b7cfdaa361e
# ╟─9fba2de3-878b-4bcc-95b0-66aa868b925e
# ╠═16689d26-739c-4581-b210-01998ace0d06
# ╠═6f145c34-a0e9-47ec-99ac-f7cf0d11b4d2
# ╟─191ddc32-d1f1-44c0-a1d8-2db523997791
# ╠═0cb113ad-75f0-433d-85f2-e242a18e24fd
# ╠═5ef1d64f-7854-4dd5-aa4b-d34c2aafdb89
# ╟─581546d5-3e82-4a73-8b3a-b7cc4cd3633d
# ╟─e589c45b-4e77-4630-8cf4-88c7cf666dc1
# ╟─b814e8cd-31a7-4d9a-8c61-6f757141fc51
# ╠═0dcbae87-d41a-4926-ac88-462b1c6fb1f4
# ╟─fbe5916a-5788-44a1-b822-233dec173825
# ╟─05838fce-a50e-4b0a-bde1-42bbdfa9daea
# ╟─28244c87-0ad7-4717-af40-b01bf632e3db
# ╠═7bff09b3-a8eb-4c96-adc1-be5a89b6babb
# ╠═078763d8-8263-41fe-a5da-4e825357148c
# ╟─677a8ed3-eb0f-4d08-9379-41df5c9a0bd5
# ╠═8296b9a4-38bf-4381-b10f-9c32cf8b540e
# ╠═ba17bd98-556c-4033-b8f9-3978f8aedd9e
# ╠═f988520b-a61e-44ac-8b59-8840ca035389
# ╟─77de41e2-4433-44dc-87c5-255b905edd29
# ╟─00000000-0000-0000-0000-000000000001
# ╟─00000000-0000-0000-0000-000000000002
