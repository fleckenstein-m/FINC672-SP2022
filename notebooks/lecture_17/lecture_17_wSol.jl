### A Pluto.jl notebook ###
# v0.19.0

using Markdown
using InteractiveUtils

# ╔═╡ 6199926c-0c4a-4991-b4c3-ebed35217375
using Plots, Statistics, StatsBase, StatsModels, DataFrames, Chain, Dates, DataFramesMeta

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

# ╔═╡ cc9ed5ee-ef24-457a-806d-8bed26617b13
using CoinbasePro

# ╔═╡ 886b6ade-b8df-494c-be5f-13aae328b848
md"""
## FINC 672: Bitcoin
"""

# ╔═╡ 26a704bf-6b13-45f2-9211-023fbf2e3cb6
md"""
_Thanks to [Dean Markwick](https://dm13450.github.io/about/) for making the CoinbasePro Julia API publicly available._
"""

# ╔═╡ 1897543d-5c47-41ee-9f13-a099df5d9ed1
md"""
- In this notebook, we will learn the basic of limit order books. For this purpose, we will use [Coinbase](https://www.coinbase.com/) which is an easy-to-use platform for accessing Bitcoin data.
- We will connect to Coinbase using the [CoinbasePro](https://github.com/dm13450/CoinbasePro.jl) API in Julia.
- Using this API we can access
  - `products` lists the available currency pairs provided by Coinbase.
  - `book` provides order book information at 3 different levels.
  - `ticker` a snapshot of the last trade, best bid/offer and volume over the last 24 hours.
  - `trades` historical trades of a currency.
  - `candles` historical open-high-low-close (OHLC) market data.
  - `stats` OHLC and volume data from the last 24 hours.
"""

# ╔═╡ 4a235440-1810-4b18-85f9-377f9f5f6876
md"""
# The Limit Order Book
"""

# ╔═╡ 825c7258-ade5-48ad-83c9-acc362101715
md"""
- The limit order book (LOB) is the collection of orders at which people are willing to buy and sell assets (stocks, bond, Bitcoin, etc.). 
  - Basically, an order in the order book is executed when the market price reaches the limit order price. By contrast, a market order typically is executed right-away at the current market price.
"""

# ╔═╡ 16386405-896d-4cea-a733-41e298da1258
md"""
- In the order book, data are categorized into different groups ("levels").
  - Level 1: The best price to buy or sell at (also referred to the "top level" of the book).
  - Level 2: Aggregated levels, 50 levels at which people are willing to buy and sell.
  - Level 3: The full order book.
"""

# ╔═╡ 912ee012-981c-4003-abca-5aa0b3e7f673
md"""
- We use the `book` function to select the level and currency pair to query from Coinbase.
- Let's select USD Bitcoin (`btc-usd`) and get the currently best bid and ask prices.
"""

# ╔═╡ 451ecc21-b276-4009-ac12-561dd858e507
begin
	l1 = CoinbasePro.book("btc-usd", 1)
	first(l1, 5)
end

# ╔═╡ a4b4cb2a-03b4-4498-9978-d786a2663718
md"""
- As expected, there is just one row---the top level.
- If we were to place a market order of a size less than the size (see `size_ask` above), this is the price our order would be filled at. 
- However, if our trade size was larger, our order would be partially filled using the next levels of the order book until our full size was filled.
- Thus, let's take a look at the second level (`level 2`).
"""

# ╔═╡ 62eeb08c-9c02-47e0-9e44-8dda89fe7f3c
begin
	bookData = CoinbasePro.book("btc-usd", 2)
	first(bookData, 15)
end

# ╔═╡ 8d5c0e6b-ae4a-4f77-a0ff-8de784934136
md"""
- Note that the data are aggregated, i.e. there might be multiple orders at one price rather than just a single large order.
"""

# ╔═╡ d71efea4-1a1e-43b2-ba62-84411b8688b1
md"""
- Finally, let's take a look at `level 3`.
"""

# ╔═╡ ec0eb792-f467-43ac-8f31-91d542b08e67
begin
	l3 = CoinbasePro.book("btc-usd", 3)
	first(l3, 5)
end

# ╔═╡ ab3fef5d-14d7-4528-ad4b-a395f844e0b9
nrow(l3)

# ╔═╡ 653dc5ac-245a-4bcd-bfaa-69567275a388
md"""
- The full order books shows all orders currently outstanding. Note that there over 60000 orders currently in the order book.
"""

# ╔═╡ a08832c4-58e8-48fd-a4a1-205399b7a1fb
md"""
# Sweeping the Book

"""

# ╔═╡ d3cde55d-0e81-4637-a705-31255148e7b2
md"""
- Suppose we want to trade a large quantity of Bitcoin. In that case, what is the average price if our trade were filled at different levels of the order book?
- This matters because a large order might not be filled at the best bid/ask prices.
- To calculate the average price, we fix an order size and add up the amount at each level and calculate the cumulative average price at each level. Let's do this calculation on both the bid and ask side to see how they differ.
"""

# ╔═╡ 8d6cbbed-e8bd-4b43-859d-d86e1a70e7ff
begin
askSize = cumsum(bookData[!, "size_ask"])
askCost = map(n -> sum(bookData[!, "size_ask"][1:n] .* bookData[!, "price_ask"][1:n]) / askSize[n], 1:nrow(bookData))

bidSize = cumsum(bookData[!, "size_bid"])
bidCost = map(n -> sum(bookData[!, "size_bid"][1:n] .* bookData[!, "price_bid"][1:n]) / bidSize[n], 1:nrow(bookData));
end

# ╔═╡ 2ed85d07-b706-4ba9-83c2-3e3e0249c4a5
df = DataFrame(Size=askSize,Cost=askCost)

# ╔═╡ 417a13a8-0594-43fe-97d8-b49b7288992e
bookData[!, "price_bid"][1]

# ╔═╡ 1bdd1856-8f8b-490b-a6f6-224538fa7356
md"""
- Next, let's plot the total amount and the total cost of trading.
"""

# ╔═╡ 8e1246c0-3bf3-4bd0-b7e2-a87dc818831e
begin
	plot(askSize, (1e4 .* ((askCost ./ bookData[!, "price_ask"][1]) .- 1)), label="Ask", xlabel="Number of Bitcoins", ylabel="Cost (bps)")
plot!(bidSize, abs.(1e4 .* ((bidCost ./ bookData[!, "price_bid"][1]) .- 1)), label="Bid", xlim=(0,1), ylim=(0,1))
end

# ╔═╡ f6d356b2-4b9a-4552-9d4a-7840ff1c253c
md"""
- We can improve this plot. In particular, the vertical axis is in basis points of the total amount traded. Let's convert both axis to dollars.
- To do this, we use the midprice of BTC-USD, which we get using the ticker function from the CoinbasePro API.
"""

# ╔═╡ ddffd1c1-75b0-463f-89bb-a1ddfb337f7e
tickerStats = CoinbasePro.ticker("BTC-USD")

# ╔═╡ 9c95c0b9-63b1-46c1-9457-3561c665b127
mid = (tickerStats.ask + tickerStats.bid)/2

# ╔═╡ 2940c863-1067-493d-8a96-e00aa7bd5d08
begin
	askBps = 1e4 .* ((askCost ./ bookData[!, "price_ask"][1]) .- 1)
	bidBps = abs.(1e4 .* ((bidCost ./ bookData[!, "price_bid"][1]) .- 1))

	plot(askSize .* mid , (askBps /1e4) .* askSize .* mid, label="Ask", xlabel="Dollars")
	plot!(bidSize .* mid , (bidBps/1e4) .* bidSize .* mid, label="Bid", ylabel = "Cost (dollars)", xlim=(0,10000), ylim=(0,1))
end

# ╔═╡ 64728fae-6674-4877-a19e-26767b96d7c4
md"""
- We can interpret this as follows. Suppose we bought \$1,000 of bitcoin, then the plots shows the intercept at around $0.22. Thus, it costs twenty-two cents to get the $1,000 order executed.
"""

# ╔═╡ 9165b298-c580-420f-b2e5-8190ccb9ce6f
md"""
# Trade history

"""

# ╔═╡ 0287028d-b219-4c01-b712-e7dc0db214d7
md"""
- We can get the last 1000 trades.
"""

# ╔═╡ 0f655a55-7da4-4f99-80e4-d0460c8d083d
begin
	trad, pages = CoinbasePro.trades("BTC-USD")
	first(trad, 5)
end

# ╔═╡ 49b20294-14ce-408f-91e6-773fc7d15816
md"""
- The timestamp contains the number of nanoseconds. Julia's `DateTime` does not support nanoseconds. 
- Notice that the timestamp looks like 2021-01-01T12:00:00.123456. We'll split on the `.` to get the datetime up to seconds and the nanoseconds.
"""

# ╔═╡ 2af218ec-0514-431a-a79b-736a83ada996
trades = @chain trad begin
	 transform(:time => ByRow(x-> split(string(x),".")) => [:time_2, :time_nano]) 
	 transform(:time_2 => ByRow(x->DateTime(x)) => :time)
	 select(Not([:time_2, :time_nano]))
	sort(:time)
 end

# ╔═╡ b9c2b715-f99c-4d2b-8d42-10c1e68e1f75
plot(trades.time, trades.price, group=trades.side, seriestype=:scatter, xlabel="Time", ylabel="Price")

# ╔═╡ fc510d58-17b4-4067-bd6b-967a966bc35b
begin
	trades_g = groupby(trades, :side)
	@combine(trades_g, AvgPrice = mean(:price), 
	                   N = length(:price), 
	                   TotalNotional = sum(:size),
	                   AvgWPrice = mean(:price, Weights(:size)))
end

# ╔═╡ 45fd2d0e-2e10-43c6-9964-125648dba4ff
md"""
# Price Impact
"""

# ╔═╡ 724bf604-9c31-4307-a6a6-9d4e4038aa7d
md"""
- Using the trades we can build up a (very) simple model of price impact. Basically, price impact measures how much we move the market by trading. With each trade we look at the absolute price difference of the next traded price. To look at this, we first need to aggregate trades that occur at the same time and in the same direction.
"""

# ╔═╡ 37d58842-b064-42ce-a39e-d1c75a4781a3
trades2 = @chain trades begin
	groupby([:time, :side])
	@combine(AvgPrice = mean(:price, Weights(:size)),                          TotalSize = sum(:size), N = length(:price))
end

# ╔═╡ 11ce1c61-1ac8-4156-8f01-edbc98b8a88d
begin
	plot(log.(trades2[2:end, :TotalSize] .+ 1), abs.(diff(log.(trades2.AvgPrice))), seriestype=:scatter, label=:none, xlabel="Trade Size", ylabel="Impact")
end

# ╔═╡ a719d880-0847-4925-81e3-b9dc77407a6c
begin
	trades3 = copy(trades2)
	trades3.Impact = [NaN; diff(log.(trades3.AvgPrice))]
	trades3.Sign = [x == "sell" ? -1 : 1 for x in trades3.side]
	trades3.SignedImpact = trades3.Impact .* trades3.Sign
	first(trades3, 5)
end

# ╔═╡ 903dfdf7-ec30-4c22-a8da-2cbc8adb359d
begin
	using CategoricalArrays

	trades4 = trades3[2:end, :]
	
	trades4.SizeBucket = cut(trades4.TotalSize, quantile(trades4.TotalSize, 0:0.1:1), extend=true)
	
	
end

# ╔═╡ 8918ea92-5792-4f4d-b640-e827410cf004
quantileSummary = @chain trades4 begin
	groupby([:SizeBucket, :side])
	@combine(AvgSize = mean(:TotalSize), MeanImpact = mean(:Impact), MeanAbsImpact = mean(abs.(:Impact)))
end

# ╔═╡ 561bdf9a-77d3-426d-a788-264f9e6ba520
plot(log.(quantileSummary.AvgSize), log.(quantileSummary.MeanAbsImpact), group=quantileSummary.side, seriestype=:scatter)

# ╔═╡ 9b460614-9c7b-4224-a40f-470b9bd195e8
md"""
# Trade Sign Correlation

"""

# ╔═╡ 122ff387-ee13-4bf8-b323-279d9620d1f6
md"""
- The distribution of trade signs also gives insight into the nature of markets. To look into whether buys are more or less likely followed by buys (or sells), we first aggregate those trades that occurred on the same timestamp and in the same direction.
"""

# ╔═╡ f8f91e89-2637-40bc-baec-9651ace19609
aggTrades = @chain trades begin
	groupby([:time, :side])
	@combine( N=length(:size), TotalSize = sum(:size))
end

# ╔═╡ c311670b-4127-4426-a179-e314db3e3078
md"""
- For each of the aggregated timestamp trades we assign buys as 1 and sells as -1. Then by looking at the autocorrelation between the trades we can come up with an explanation of how likely a buy is followed by another buy.
"""

# ╔═╡ 80d45c3e-6c44-426e-88bf-85760d1a1ffb
begin
	sides = aggTrades.side
	sidesI = zeros(length(sides))
	sidesI[sides .== "buy"] .= 1
	sidesI[sides .== "sell"] .= -1
	
	ac = autocor(sidesI);
end

# ╔═╡ adad8237-7b4d-404c-9976-83fa6cc67fc8
md"""
- As we are looking at a possible power law relationship, we take the log of both the autocorrelation and the lag to see if there is a straight line. 
"""

# ╔═╡ 2859fbaf-e284-4156-ba27-b4981c96041f
begin
	lags = eachindex(ac)
	posInds = findall(ac .> 0)
	
	acPlot = plot(lags, ac, seriestype=:scatter, label=:none, xlabel="Lag", ylabel="Correlation")
	acLogPlot = plot(log.(lags[posInds]), log.(ac[posInds]), seriestype=:scatter, label=:none, xlabel = "log(Lag)", ylabel="log(Correlation)")
	plot(acPlot, acLogPlot)
end

# ╔═╡ 431ab113-2f60-4c1f-8997-73805833bac9
begin
	using GLM
	modelFrame = DataFrame(LogAC = log.(ac[posInds]), LogLag = log.(lags[posInds]))

	m = lm(@formula(LogAC ~ LogLag), modelFrame)
end

# ╔═╡ 9f469b4e-4687-4632-a4a1-32a571cc4bda
md"""
- We model the relationship as $\tau^\gamma$, where $\tau$ is the lag. We also remove some of the outliers to stop them influencing the result.
"""

# ╔═╡ 0bebe9bd-3314-4697-a95c-cde9d80d7966
plot!(acLogPlot, log.(0:30), coef(m)[1] .+ coef(m)[2] .* log.(0:30), label="Power Law", xlabel = "log(Lag)", ylabel="log(Correlation)")

# ╔═╡ 10120e92-bf32-4ddd-bffd-7f9a840381ab
md"""
- We see the log model fits well and we arrive at a γ value of around -0.78 which seems sensible and comparable to their value of -0.57 for some stocks.
"""

# ╔═╡ 90a153cb-5482-47cf-9315-3e8b25c32350
md"""
# Trade Size Distribution
"""

# ╔═╡ c9755bcf-97b7-4176-a91e-70ccf0671ed6
md"""
- We calculate the empirical distribution and plot that on both a linear and log scale. 
"""

# ╔═╡ 242de54b-d628-4798-86fe-bfced022a8c2
begin
	sizes = aggTrades.TotalSize
	uSizes = unique(aggTrades.TotalSize)
	
	empF = ecdf(sizes)
	
	tradesSizePlot = plot((uSizes), (1 .- empF(uSizes)), seriestype=:scatter, label="P(V > x)", xlabel="Trade Size", ylabel="Probability")
	tradesSizeLogPlot = plot(log.(uSizes), log.(1 .- empF(uSizes)), seriestype=:scatter, label="P(V > x)", xlabel = "log(Trade Size)", ylabel="log(Probability)")
	
	plot(tradesSizePlot, tradesSizeLogPlot)
end

# ╔═╡ 7384e361-1b07-4645-b9de-74989a837ac6
md"""
- The log-log plot indicates that there is some power law behaviour in the tail of the distribution. Estimating this power lower can be achieved in a number of ways, and we use the Hill estimator to calculate the tail parameter. In short, this a method for estimating the tail of a distribution when there are heavy tails. For our purposes, we take this equation as how we will come up with an α value.
"""

# ╔═╡ 5d4a0ddd-9eb9-4459-8eea-e9451a503fe7
function hill_estimator(sizes, k)
    sizes_sort = sort(sizes)
    N = length(sizes_sort)
    res = log.(sizes_sort[(N-k+1):N] / sizes_sort[N-k])
    k*(1/sum(res))
end

# ╔═╡ ab83b86d-6774-4c50-ba6d-7f8b934787b6
md"""
- For this estimator we need to chose a threshold k and take all the values above that to calculate the parameter
"""

# ╔═╡ 36f3d960-9ea8-4cb1-8efb-a6ab3941cde7
begin
	alphak = [hill_estimator(sizes, k) for k in 1:(length(sizes)-1)]
	plot(1:(length(sizes)-1), alphak, seriestype= :scatter, xlabel="k", ylabel="Alpha", label=:none)
end

# ╔═╡ 7c2d1dc4-31b3-449c-b4a3-92b096e7f72a
last(alphak, 5)

# ╔═╡ 9cbed260-c673-4e46-93e0-644e5dced80a
md"""
- It looks like the value is converging to a value of around 0.2. Clearly, this results is not conclusive since our dataset is too small.
"""

# ╔═╡ 00000000-0000-0000-0000-000000000001
PLUTO_PROJECT_TOML_CONTENTS = """
[deps]
CategoricalArrays = "324d7699-5711-5eae-9e2f-1d82baa6b597"
Chain = "8be319e6-bccf-4806-a6f7-6fae938471bc"
CoinbasePro = "3632ec16-99db-4259-aa88-30b9105699f8"
DataFrames = "a93c6f00-e57d-5684-b7b6-d8193f3e46c0"
DataFramesMeta = "1313f7d8-7da2-5740-9ea0-a2ca25f37964"
Dates = "ade2ca70-3891-5945-98fb-dc099432e06a"
GLM = "38e38edf-8417-5370-95a0-9cbb8c7f171a"
Logging = "56ddb016-857b-54e1-b83d-db4d58db5568"
Plots = "91a5bcdd-55d7-5caf-9e0b-520d859cae80"
Statistics = "10745b16-79ce-11e8-11f9-7d13ad32a3b2"
StatsBase = "2913bbd2-ae8a-5f71-8c99-4fb6c76f3a91"
StatsModels = "3eaba693-59b7-5ba5-a881-562e759f1c8d"

[compat]
CategoricalArrays = "~0.10.4"
Chain = "~0.4.10"
CoinbasePro = "~0.1.3"
DataFrames = "~1.2.2"
DataFramesMeta = "~0.10.0"
GLM = "~1.6.1"
Plots = "~1.27.0"
StatsBase = "~0.33.16"
StatsModels = "~0.6.29"
"""

# ╔═╡ 00000000-0000-0000-0000-000000000002
PLUTO_MANIFEST_TOML_CONTENTS = """
# This file is machine-generated - editing it directly is not advised

[[Adapt]]
deps = ["LinearAlgebra"]
git-tree-sha1 = "af92965fb30777147966f58acb05da51c5616b5f"
uuid = "79e6a3ab-5dfb-504d-930d-738a2a938a0e"
version = "3.3.3"

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

[[Calculus]]
deps = ["LinearAlgebra"]
git-tree-sha1 = "f641eb0a4f00c343bbc32346e1217b86f3ce9dad"
uuid = "49dc2e85-a5d0-5ad3-a950-438e2897f1b9"
version = "0.5.1"

[[CategoricalArrays]]
deps = ["DataAPI", "Future", "Missings", "Printf", "Requires", "Statistics", "Unicode"]
git-tree-sha1 = "5196120341b6dfe3ee5f33cf97392a05d6fe80d0"
uuid = "324d7699-5711-5eae-9e2f-1d82baa6b597"
version = "0.10.4"

[[Chain]]
git-tree-sha1 = "339237319ef4712e6e5df7758d0bccddf5c237d9"
uuid = "8be319e6-bccf-4806-a6f7-6fae938471bc"
version = "0.4.10"

[[ChainRulesCore]]
deps = ["Compat", "LinearAlgebra", "SparseArrays"]
git-tree-sha1 = "c9a6160317d1abe9c44b3beb367fd448117679ca"
uuid = "d360d2e6-b24c-11e9-a2a3-2a2ae2dbcce4"
version = "1.13.0"

[[ChangesOfVariables]]
deps = ["ChainRulesCore", "LinearAlgebra", "Test"]
git-tree-sha1 = "bf98fa45a0a4cee295de98d4c1462be26345b9a1"
uuid = "9e997f8a-9a97-42d5-a9f1-ce6bfc15e2c0"
version = "0.1.2"

[[CoinbasePro]]
deps = ["DataFrames", "Dates", "HTTP", "JSON", "TimesDates"]
git-tree-sha1 = "fe630d2db2602fe63ce38916fa019c556c6f6e11"
uuid = "3632ec16-99db-4259-aa88-30b9105699f8"
version = "0.1.3"

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
git-tree-sha1 = "8466629faf92fbe90840d146f19c6162db666b6c"
uuid = "a216cea6-0a8c-5945-ab87-5ade47210022"
version = "0.4.3"

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
git-tree-sha1 = "d785f42445b63fc86caa08bb9a9351008be9b765"
uuid = "a93c6f00-e57d-5684-b7b6-d8193f3e46c0"
version = "1.2.2"

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

[[DensityInterface]]
deps = ["InverseFunctions", "Test"]
git-tree-sha1 = "80c3e8639e3353e5d2912fb3a1916b8455e2494b"
uuid = "b429d917-457f-4dbc-8f4c-0cc954292b1d"
version = "0.4.0"

[[Distributed]]
deps = ["Random", "Serialization", "Sockets"]
uuid = "8ba89e20-285c-5b6f-9357-94700520ee1b"

[[Distributions]]
deps = ["ChainRulesCore", "DensityInterface", "FillArrays", "LinearAlgebra", "PDMats", "Printf", "QuadGK", "Random", "SparseArrays", "SpecialFunctions", "Statistics", "StatsBase", "StatsFuns", "Test"]
git-tree-sha1 = "9d3c0c762d4666db9187f363a76b47f7346e673b"
uuid = "31c24e10-a181-5473-b8eb-7969acd0382f"
version = "0.25.49"

[[DocStringExtensions]]
deps = ["LibGit2"]
git-tree-sha1 = "b19534d1895d702889b219c382a6e18010797f0b"
uuid = "ffbed154-4ef7-542d-bbb7-c09d3a79fcae"
version = "0.8.6"

[[Downloads]]
deps = ["ArgTools", "LibCURL", "NetworkOptions"]
uuid = "f43a241f-c20a-4ad4-852c-f6b1247861c6"

[[DualNumbers]]
deps = ["Calculus", "NaNMath", "SpecialFunctions"]
git-tree-sha1 = "90b158083179a6ccbce2c7eb1446d5bf9d7ae571"
uuid = "fa6b7ba4-c1ee-5f82-b5fc-ecf0adba8f74"
version = "0.6.7"

[[EarCut_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "3f3a2501fa7236e9b911e0f7a588c657e822bb6d"
uuid = "5ae413db-bbd1-5e63-b57d-d24a61df00f5"
version = "2.2.3+0"

[[Expat_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "ae13fcbc7ab8f16b0856729b050ef0c446aa3492"
uuid = "2e619515-83b5-522b-bb60-26c02a35a201"
version = "2.4.4+0"

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

[[FillArrays]]
deps = ["LinearAlgebra", "Random", "SparseArrays", "Statistics"]
git-tree-sha1 = "0dbc5b9683245f905993b51d2814202d75b34f1a"
uuid = "1a297f60-69ca-5386-bcde-b61e274b549b"
version = "0.13.1"

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

[[GLM]]
deps = ["Distributions", "LinearAlgebra", "Printf", "Reexport", "SparseArrays", "SpecialFunctions", "Statistics", "StatsBase", "StatsFuns", "StatsModels"]
git-tree-sha1 = "fb764dacfa30f948d52a6a4269ae293a479bbc62"
uuid = "38e38edf-8417-5370-95a0-9cbb8c7f171a"
version = "1.6.1"

[[GR]]
deps = ["Base64", "DelimitedFiles", "GR_jll", "HTTP", "JSON", "Libdl", "LinearAlgebra", "Pkg", "Printf", "Random", "RelocatableFolders", "Serialization", "Sockets", "Test", "UUIDs"]
git-tree-sha1 = "9f836fb62492f4b0f0d3b06f55983f2704ed0883"
uuid = "28b8d3ca-fb5f-59d9-8090-bfdbd6d07a71"
version = "0.64.0"

[[GR_jll]]
deps = ["Artifacts", "Bzip2_jll", "Cairo_jll", "FFMPEG_jll", "Fontconfig_jll", "GLFW_jll", "JLLWrappers", "JpegTurbo_jll", "Libdl", "Libtiff_jll", "Pixman_jll", "Pkg", "Qt5Base_jll", "Zlib_jll", "libpng_jll"]
git-tree-sha1 = "a6c850d77ad5118ad3be4bd188919ce97fffac47"
uuid = "d2c73de3-f751-5644-a686-071e5b155ba9"
version = "0.64.0+0"

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

[[HypergeometricFunctions]]
deps = ["DualNumbers", "LinearAlgebra", "SpecialFunctions", "Test"]
git-tree-sha1 = "65e4589030ef3c44d3b90bdc5aac462b4bb05567"
uuid = "34004b35-14d8-5ef3-9330-4cdb6864b03a"
version = "0.3.8"

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
git-tree-sha1 = "4f00cc36fede3c04b8acf9b2e2763decfdcecfa6"
uuid = "23fbe1c1-3f47-55db-b15f-69d7ec21a316"
version = "0.15.13"

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
git-tree-sha1 = "56ad13e26b7093472eba53b418eba15ad830d6b5"
uuid = "2ab3a3ac-af41-5b50-aa03-7779005ae688"
version = "0.3.9"

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

[[OpenLibm_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "05823500-19ac-5b8b-9628-191a04bc5112"

[[OpenSSL_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "648107615c15d4e09f7eca16307bc821c1f718d8"
uuid = "458c3c95-2e84-50aa-8efc-19380b2a3a95"
version = "1.1.13+0"

[[OpenSpecFun_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "13652491f6856acfd2db29360e1bbcd4565d04f1"
uuid = "efe28fd5-8261-553b-a9e1-b2916fc3738e"
version = "0.5.5+0"

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

[[PDMats]]
deps = ["LinearAlgebra", "SparseArrays", "SuiteSparse"]
git-tree-sha1 = "e8185b83b9fc56eb6456200e873ce598ebc7f262"
uuid = "90014a1f-27ba-587c-ab20-58faa44d9150"
version = "0.11.7"

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
git-tree-sha1 = "6f1b25e8ea06279b5689263cc538f51331d7ca17"
uuid = "995b91a9-d308-5afd-9ec6-746e21dbc043"
version = "1.1.3"

[[Plots]]
deps = ["Base64", "Contour", "Dates", "Downloads", "FFMPEG", "FixedPointNumbers", "GR", "GeometryBasics", "JSON", "Latexify", "LinearAlgebra", "Measures", "NaNMath", "Pkg", "PlotThemes", "PlotUtils", "Printf", "REPL", "Random", "RecipesBase", "RecipesPipeline", "Reexport", "Requires", "Scratch", "Showoff", "SparseArrays", "Statistics", "StatsBase", "UUIDs", "UnicodeFun", "Unzip"]
git-tree-sha1 = "9213b4c18b57b7020ee20f33a4ba49eb7bef85e0"
uuid = "91a5bcdd-55d7-5caf-9e0b-520d859cae80"
version = "1.27.0"

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

[[QuadGK]]
deps = ["DataStructures", "LinearAlgebra"]
git-tree-sha1 = "78aadffb3efd2155af139781b8a8df1ef279ea39"
uuid = "1fd47b50-473d-5c70-9696-f719f8f3bcdc"
version = "2.4.2"

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
git-tree-sha1 = "995a812c6f7edea7527bb570f0ac39d0fb15663c"
uuid = "01d81517-befc-4cb6-b9ec-a95719d0359c"
version = "0.5.1"

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

[[Rmath]]
deps = ["Random", "Rmath_jll"]
git-tree-sha1 = "bf3188feca147ce108c76ad82c2792c57abe7b1f"
uuid = "79098fc4-a85e-5d69-aa6a-4863f24498fa"
version = "0.7.0"

[[Rmath_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "68db32dff12bb6127bac73c209881191bf0efbb7"
uuid = "f50d1b31-88e8-58de-be2c-1cc44531875f"
version = "0.3.0+0"

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

[[ShiftedArrays]]
git-tree-sha1 = "22395afdcf37d6709a5a0766cc4a5ca52cb85ea0"
uuid = "1277b4bf-5013-50f5-be3d-901d8477a67a"
version = "1.0.0"

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

[[SpecialFunctions]]
deps = ["ChainRulesCore", "IrrationalConstants", "LogExpFunctions", "OpenLibm_jll", "OpenSpecFun_jll"]
git-tree-sha1 = "5ba658aeecaaf96923dce0da9e703bd1fe7666f9"
uuid = "276daf66-3868-5448-9aa4-cd146d93841b"
version = "2.1.4"

[[StaticArrays]]
deps = ["LinearAlgebra", "Random", "Statistics"]
git-tree-sha1 = "74fb527333e72ada2dd9ef77d98e4991fb185f04"
uuid = "90137ffa-7385-5640-81b9-e52037218182"
version = "1.4.1"

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

[[StatsFuns]]
deps = ["ChainRulesCore", "HypergeometricFunctions", "InverseFunctions", "IrrationalConstants", "LogExpFunctions", "Reexport", "Rmath", "SpecialFunctions"]
git-tree-sha1 = "25405d7016a47cf2bd6cd91e66f4de437fd54a07"
uuid = "4c63d2b9-4356-54db-8cca-17b64c39e42c"
version = "0.9.16"

[[StatsModels]]
deps = ["DataAPI", "DataStructures", "LinearAlgebra", "Printf", "REPL", "ShiftedArrays", "SparseArrays", "StatsBase", "StatsFuns", "Tables"]
git-tree-sha1 = "03c99c7ef267c8526953cafe3c4239656693b8ab"
uuid = "3eaba693-59b7-5ba5-a881-562e759f1c8d"
version = "0.6.29"

[[StructArrays]]
deps = ["Adapt", "DataAPI", "StaticArrays", "Tables"]
git-tree-sha1 = "57617b34fa34f91d536eb265df67c2d4519b8b98"
uuid = "09ab397b-f2b6-538f-b94a-2f83cf4a842a"
version = "0.6.5"

[[SuiteSparse]]
deps = ["Libdl", "LinearAlgebra", "Serialization", "SparseArrays"]
uuid = "4607b0f0-06f3-5cda-b6b1-a6196a1729e9"

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
git-tree-sha1 = "b56fad6f36724a4261db450baa69074037846289"
uuid = "bdfc003b-8df8-5c39-adcd-3a9087f5df4a"
version = "0.2.6"

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
# ╠═6199926c-0c4a-4991-b4c3-ebed35217375
# ╟─b0b9984b-45f5-49a2-a5ce-86281745d623
# ╟─886b6ade-b8df-494c-be5f-13aae328b848
# ╟─26a704bf-6b13-45f2-9211-023fbf2e3cb6
# ╟─1897543d-5c47-41ee-9f13-a099df5d9ed1
# ╠═cc9ed5ee-ef24-457a-806d-8bed26617b13
# ╟─4a235440-1810-4b18-85f9-377f9f5f6876
# ╟─825c7258-ade5-48ad-83c9-acc362101715
# ╟─16386405-896d-4cea-a733-41e298da1258
# ╟─912ee012-981c-4003-abca-5aa0b3e7f673
# ╠═451ecc21-b276-4009-ac12-561dd858e507
# ╟─a4b4cb2a-03b4-4498-9978-d786a2663718
# ╠═62eeb08c-9c02-47e0-9e44-8dda89fe7f3c
# ╟─8d5c0e6b-ae4a-4f77-a0ff-8de784934136
# ╟─d71efea4-1a1e-43b2-ba62-84411b8688b1
# ╠═ec0eb792-f467-43ac-8f31-91d542b08e67
# ╠═ab3fef5d-14d7-4528-ad4b-a395f844e0b9
# ╟─653dc5ac-245a-4bcd-bfaa-69567275a388
# ╟─a08832c4-58e8-48fd-a4a1-205399b7a1fb
# ╟─d3cde55d-0e81-4637-a705-31255148e7b2
# ╠═8d6cbbed-e8bd-4b43-859d-d86e1a70e7ff
# ╠═2ed85d07-b706-4ba9-83c2-3e3e0249c4a5
# ╠═417a13a8-0594-43fe-97d8-b49b7288992e
# ╟─1bdd1856-8f8b-490b-a6f6-224538fa7356
# ╠═8e1246c0-3bf3-4bd0-b7e2-a87dc818831e
# ╟─f6d356b2-4b9a-4552-9d4a-7840ff1c253c
# ╠═ddffd1c1-75b0-463f-89bb-a1ddfb337f7e
# ╠═9c95c0b9-63b1-46c1-9457-3561c665b127
# ╠═2940c863-1067-493d-8a96-e00aa7bd5d08
# ╟─64728fae-6674-4877-a19e-26767b96d7c4
# ╟─9165b298-c580-420f-b2e5-8190ccb9ce6f
# ╟─0287028d-b219-4c01-b712-e7dc0db214d7
# ╠═0f655a55-7da4-4f99-80e4-d0460c8d083d
# ╟─49b20294-14ce-408f-91e6-773fc7d15816
# ╠═2af218ec-0514-431a-a79b-736a83ada996
# ╠═b9c2b715-f99c-4d2b-8d42-10c1e68e1f75
# ╠═fc510d58-17b4-4067-bd6b-967a966bc35b
# ╟─45fd2d0e-2e10-43c6-9964-125648dba4ff
# ╟─724bf604-9c31-4307-a6a6-9d4e4038aa7d
# ╠═37d58842-b064-42ce-a39e-d1c75a4781a3
# ╠═11ce1c61-1ac8-4156-8f01-edbc98b8a88d
# ╠═a719d880-0847-4925-81e3-b9dc77407a6c
# ╠═903dfdf7-ec30-4c22-a8da-2cbc8adb359d
# ╠═8918ea92-5792-4f4d-b640-e827410cf004
# ╠═561bdf9a-77d3-426d-a788-264f9e6ba520
# ╟─9b460614-9c7b-4224-a40f-470b9bd195e8
# ╟─122ff387-ee13-4bf8-b323-279d9620d1f6
# ╠═f8f91e89-2637-40bc-baec-9651ace19609
# ╟─c311670b-4127-4426-a179-e314db3e3078
# ╠═80d45c3e-6c44-426e-88bf-85760d1a1ffb
# ╟─adad8237-7b4d-404c-9976-83fa6cc67fc8
# ╠═2859fbaf-e284-4156-ba27-b4981c96041f
# ╟─9f469b4e-4687-4632-a4a1-32a571cc4bda
# ╠═431ab113-2f60-4c1f-8997-73805833bac9
# ╠═0bebe9bd-3314-4697-a95c-cde9d80d7966
# ╟─10120e92-bf32-4ddd-bffd-7f9a840381ab
# ╟─90a153cb-5482-47cf-9315-3e8b25c32350
# ╟─c9755bcf-97b7-4176-a91e-70ccf0671ed6
# ╠═242de54b-d628-4798-86fe-bfced022a8c2
# ╟─7384e361-1b07-4645-b9de-74989a837ac6
# ╠═5d4a0ddd-9eb9-4459-8eea-e9451a503fe7
# ╟─ab83b86d-6774-4c50-ba6d-7f8b934787b6
# ╠═36f3d960-9ea8-4cb1-8efb-a6ab3941cde7
# ╠═7c2d1dc4-31b3-449c-b4a3-92b096e7f72a
# ╟─9cbed260-c673-4e46-93e0-644e5dced80a
# ╟─00000000-0000-0000-0000-000000000001
# ╟─00000000-0000-0000-0000-000000000002
