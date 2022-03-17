### A Pluto.jl notebook ###
# v0.18.1

using Markdown
using InteractiveUtils

# ╔═╡ 2a5055b4-2137-447d-bfd2-a26cc4b14735
using Chain, CSV, DataFrames, Dates, ShiftedArrays, Statistics

# ╔═╡ e796c131-5a67-4a99-8df2-b7ea52a03056
begin
	using PlutoUI, Printf
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

# ╔═╡ a548314b-511d-4319-8f7b-95a3158ba90c
md"""
# FINC 672: Introduction to Portfolio Mathematics
"""

# ╔═╡ bd352f2e-3b7f-4b5b-ae43-fd244447347a
md"""
## Return Calculations

The return of holding the asset between $t-1$ and $t$ is

$$R_t = (P_t+D_t)/P_{t-1} - 1,$$

where $P_t$ is the price (measured after dividends) and $D_t$ is the dividend.

We can calculate the returns by a loop or by a more compact notation, see below.

"""

# ╔═╡ 6334d495-fa91-4428-a203-4d8234324ece
md"""
Let's look at an example where we have a stock with three price observations.
"""

# ╔═╡ 6c19fbf2-342d-48ff-97d7-983bb1ae1122
let
	
		P = [100,108,109]                     #prices (after dividends) for t=1,2,3
		D = [0,2,0]                           #dividends, could also use [0;2;0]
	
		R = zeros(length(P))                  #where to store the results
		
		for t = 2:length(P)                   #P[2] is the 2nd element of P  
			R[t] = (P[t] + D[t])/P[t-1] - 1
		end
		
		popfirst!(R)                          #get rid of R[1] since we have no return there

	with_terminal() do 
		printmat(R*100,colNames=["return, %"],rowNames=2:3,cell00="period",width=15)
	end
	
end

# ╔═╡ 75007c57-94e8-49fc-8bbb-3d39d6207fa2
md"""
# Cumulating Returns
Net returns can be cumulated into a portfolio value as

$$V_t = V_{t-1}(1+R_t),$$

where we need a starting value (initial investment) for the portfolio (a common choice is to normalise to $V_0=1$).

With log returns, $r_t=\log(1+R_t)$, we instead do

$$\ln V_t = \ln V_{t-1} + r_t$$

If the return series is an excess return, add the riskfree rate to convert it to get net returns - and then cumulate as described above.
"""

# ╔═╡ c1a5ff0e-ce5e-4390-bed7-af5be140a145
let
	R    = [20,-35,25]/100                #returns for t=1,2,3
	V   = cumprod(1.0 .+ R)              #V(t) = V(t-1)*(1+R(t)), starting at 1 in t=0
	lnV = cumsum(log.(1.0 .+ R))         #lnV(t) = lnV(t-1) + r(t) 
	expLnV = exp.(lnV)
		
	#Display results
	with_terminal() do
		printmat(R,V,lnV,expLnV,colNames=["R","V","lnV","ExpLnV"],rowNames=1:3,cell00="period")
	end
end

# ╔═╡ e6c198f4-5abd-468c-841c-8a7b032f1dc2
md"""
# Portfolio Return: Definition, Expected Value and Variance
We form a portfolio by combining $n$ assets: $w$ is the vector of $n$ portfolio weights, $R$ is a vector of returns, $\mu$ a vector of expected expected (average) returns and $\Sigma$ the $n \times n$ covariance matrix.

The portfolio return, the expected portfolio return and the portfolio variance can be computed as:

$$R_p = w'R,$$

$$\text{E}R_p = w'\mu$$ and

$$\text{Var}(R_p) = w'\Sigma w$$

The covariance of two portfolios (with weights $v$ and $w$, respectively) can be computed as

$$\text{Cov}(R_q,R_p) = v'\Sigma w$$.
"""

# ╔═╡ e89ecb62-4c0f-4266-863b-529694cca1c0
let
	w = [0.8,0.2]
	R = [10,5]/100          #returns of asset 1 and 2
	Rp = w'R

	with_terminal() do 
		printred("Portfolio weights:")
		printmat(w,rowNames=["asset 1","asset 2"])

		printred("Returns:")
		printmat(R,rowNames=["asset 1","asset 2"])

		printred("Portfolio return: ")
		printlnPs(Rp)
	end
end

# ╔═╡ 3369be71-2a11-4dc8-8d1b-d2b1d7749c5f
let
	μ = [9,6]/100                    #\mu[TAB] to get μ
	Σ = [256 96;                     #\Sigma[TAB]
		 96 144]/100^2

	with_terminal() do
		printblue("expected returns*100: ")
		printmat(μ*100,rowNames=["asset 1","asset 2"])

		printblue("covariance matrix*100^2:")
		printmat(Σ*100^2,rowNames=["asset 1","asset 2"],colNames=["asset 1","asset 2"])
	end
	
end

# ╔═╡ 4dece1db-3305-4b94-8a1b-f5c833d67444
let
	w = [0.8,0.2]
	R = [10,5]/100 #returns of asset 1 and 2
	μ = [9,6]/100                    #\mu[TAB] to get μ
	Σ = [256 96;                     #\Sigma[TAB]
		 96 144]/100^2
	ERp   = w'μ
	VarRp = w'Σ*w

	with_terminal() do
		printlnPs("Expected portfolio return: ",ERp)
		printlnPs("Portfolio variance and std:",VarRp,sqrt(VarRp))
	end
		
end

# ╔═╡ e5318e00-c074-4074-8eb4-3245ceceb4c4
let
	w = [0.8,0.2]
	μ = [9,6]/100  
	Σb = [256 -96;                #another covariance matrix
          -96 144]/100^2

	with_terminal() do
		printlnPs("Portfolio std if the assets were negatively correlated: ",sqrt(w'Σb*w))
	end
end

# ╔═╡ c537a2e6-114f-4a86-9323-cc7e7b94ceb3
md"""
# CRSP dataset
"""

# ╔═╡ 98083989-a006-4cf3-afd1-0b3495e1d5b6
md"""
To illustrate the concepts, we will be working with the CRSP dataset from WRDS.

> From CANVAS, download the csv file **CRSP_monthly.csv** from the dataset section.
"""

# ╔═╡ e3529bed-ac89-4e5f-a1e2-e2a7d4c7b073
md"""
**Let's first read the data into a dataframe.**
"""

# ╔═╡ 2a18e0ef-4332-4a11-af9f-3efea3f8f69d
 CRSP = DataFrame(CSV.File("CRSP_monthly.csv", ntasks=1))

# ╔═╡ ad26e54b-85b1-4631-b1ed-531873ba712a
md"""
**Let's describe the data to get a sense of the variables and their types**
"""

# ╔═╡ aee2e708-6b13-4609-b60c-a33cbd741023
describe(CRSP)

# ╔═╡ d99160b7-9421-404d-bef4-55f634d6e586
md"""
**Let's consider a set of stocks in the Dow Jones Index**
"""

# ╔═╡ 22073910-f676-4e8b-8b80-38fed57bf399
md"""
Ticker | Company Name
:------|:-------------
AAPL | 	Apple Inc
AMGN | 	Amgen Inc
AXP  | 	American Express Co
BA   | 	Boeing Co/The
CAT  | 	Caterpillar Inc
CRM  | 	salesforce.com Inc
CSCO | 	Cisco Systems Inc/Delaware
CVX  | 	Chevron Corp
DIS  | 	Walt Disney Co/The
DOW  | 	Dow Inc
GS   | 	Goldman Sachs Group Inc/The
HD   | 	Home Depot Inc/The
HON  | 	Honeywell International Inc
IBM  | 	International Business Machines Corp
INTC | 	Intel Corp
JNJ  | 	Johnson & Johnson
JPM  | 	JPMorgan Chase & Co
KO   | 	Coca-Cola Co/The
MCD  | 	McDonald's Corp
MMM  | 	3M Co
MRK  | 	Merck & Co Inc
MSFT | 	Microsoft Corp
NKE  | 	NIKE Inc
PG   | 	Procter & Gamble Co/The
TRV  | 	Travelers Cos Inc/The
UNH  | 	UnitedHealth Group Inc
V    | 	Visa Inc
VZ   | 	Verizon Communications Inc
WBA  | 	Walgreens Boots Alliance Inc
WMT  | 	Walmart Inc
"""

# ╔═╡ 8980a963-b6c0-48cd-9316-b96df2acf4c6
md"""
**Let's work with a single stock first. We will pick AAPL.**
"""

# ╔═╡ 6a25c898-54b3-4afc-b771-e13c7cda66eb
md"""
#### Apple (AAPL)
"""

# ╔═╡ 456211a1-3828-458c-a597-cb35a88c35ca
md"""
**How can we deal with missing data?**
"""

# ╔═╡ 590799eb-b5e0-4ee0-bdb5-ae4bd01f5576
md"""
If we try to filter out AAPL from the dataset, we get an error message.
>AAPL = filter(:TICKER => (x-> x=="AAPL"),CRSP)

This is because :TICKER has missing data, and we need to give instructions to the `filter` function how `missing` values ought to be dealt with.
"""

# ╔═╡ d86c3764-81fb-4dcc-af04-094356a25216
md"""
**Let's see how to do this.**
"""

# ╔═╡ 6cb63947-6490-4a02-8b74-55eb5e2efa96
let
	df = @chain CRSP begin
		filter(:TICKER => (x-> ismissing(x) ? false : x=="AAPL"),_)
		describe(_)
	end
end

# ╔═╡ a65fa0ae-1ceb-451e-ab37-08c0d23a508e
md"""
**Next, let's convert the `:date` column to a type `Date`**
"""

# ╔═╡ 523e0191-237e-4bdb-b9a3-53079b9e92d5
begin
	df = @chain CRSP begin
		filter(:TICKER => (x-> ismissing(x) ? false : x=="AAPL"),_)
		transform(:date => ByRow(x->Date(string(x),dateformat"yyyymmdd")) => :date)
		select(:date, :TICKER, :PERMCO, :PERMNO, :PRC, :DIVAMT)
	end
end

# ╔═╡ b8051d63-ed26-4e21-8a98-f564a3b8a967
md"""
**Notice that there is a strange duplicate. How can we find and deal with this?**
"""

# ╔═╡ a0cf0361-422a-4bcd-8b9e-943dc00044b3
@chain df begin
	groupby(:date)
	combine(_) do sdf
           if nrow(sdf) == 1 
               DataFrame()
           else
               sdf
           end
       end
end

# ╔═╡ 6b720ade-5e4c-4f58-8fcd-f609df29df7b
md"""
**Let's remove this duplicate and get unique rows.**
"""

# ╔═╡ 0cf85da2-a538-402d-9785-094425260bdb
@chain df begin
	groupby(:date)
	combine(_) do sdf
           if nrow(sdf) == 2 
               DataFrame(sdf[2,:])
           else
               sdf
           end
       end
end

# ╔═╡ 8a869ecb-a6ed-499d-873b-131c9bc2e0b1
md"""
**Let's now create a new dataframe with all the changes we want to make.**
"""

# ╔═╡ 446e0db1-8107-4724-b774-561c5d37f87a
begin
	AAPL = @chain CRSP begin
		filter(:TICKER => (x-> ismissing(x) ? false : x=="AAPL"),_)
		transform(:date => ByRow(x->Date(string(x),dateformat"yyyymmdd")) => :date)
		select(:date, :TICKER, :PERMCO, :PERMNO, :PRC, :DIVAMT)

		groupby(:date)
		combine(_) do sdf
           if nrow(sdf) == 2 
               DataFrame(sdf[2,:])
           else
               sdf
           end
        end

	    sort(:date)
	end
end

# ╔═╡ 45868894-a317-4370-9631-6ef3c31661e3
md"""
**Next, let's calculate monthly returns for AAPL/**
"""

# ╔═╡ 7b666951-b303-4100-8dd2-feb8e2d22b14
md"""
**To do this, we need to add the lagged price.**
>The `ShiftedArrays` package allows us to do this.
"""

# ╔═╡ 83a3993e-073f-4e84-acfe-61bfc76dde3a
@chain AAPL begin
	sort(:date)
	groupby(:TICKER)
	transform(:PRC => (x-> lag(x)) => :PRC_L)
	select(:date, :TICKER, :PERMCO, :PERMNO, :PRC, :PRC_L, :DIVAMT)
end

# ╔═╡ 4805e8a6-34d6-49f0-8706-88c916e4689f
md"""
**As the next step, we need two functions. One to calculate arithmetic returns, and another to calculate log returns.**
"""

# ╔═╡ 9b816236-f9de-4aad-aea0-b5f8fbfc6b11
function GetRx(Px_t,Px_tminus,div)
	divAmt = 0.0
	if !ismissing(div)
		divAmt = div
	end
		
	if any(ismissing.([Px_t,Px_tminus]))
		return missing
	else
		return (Px_t + divAmt - Px_tminus)/Px_tminus
	end
end

# ╔═╡ d300be65-f494-4181-9924-e69cc6c04f09
function GetLogRx(Px_t,Px_tminus,div)
	divAmt = 0.0
	if !ismissing(div)
		divAmt = div
	end
		
	if any(ismissing.([Px_t,Px_tminus]))
		return missing
	else
		return log((Px_t + divAmt)) - log(Px_tminus)
	end
end

# ╔═╡ 2bd1674d-2254-45a8-9f02-5caa5bd0bd1c
md"""
**Let's now create a new dataframe with the returns for AAPL.**
"""

# ╔═╡ a23a366d-fa73-4672-b59c-e14b2b817ce8
AAPL_Rx = @chain AAPL begin
	sort(:date)
	groupby(:TICKER)
	transform(:PRC => (x-> lag(x)) => :PRC_L)
	select(:date, :TICKER, :PERMCO, :PERMNO, :PRC, :PRC_L, :DIVAMT)

	transform([:PRC, :PRC_L, :DIVAMT] => ByRow((Px_t,Px_tminus,div) -> GetRx(Px_t,Px_tminus,div)) => :Rx,
			  [:PRC, :PRC_L, :DIVAMT] => ByRow((Px_t,Px_tminus,div) -> GetLogRx(Px_t,Px_tminus,div)) => :LogRx)
	
	dropmissing(:Rx)
end

# ╔═╡ 5baa1a9f-7ad5-4bfc-b468-9e0b40438548
md"""
**Now, we can calculate the cumulative return over the period from January 2000 to December 2020 from investing $1 in AAPL.**
"""

# ╔═╡ 0c821308-45ca-4317-80b3-9e87c6840465
@chain AAPL_Rx begin
	combine(:Rx => (rx -> prod(1 .+ rx)) => :V_T)
end

# ╔═╡ 8c58cbc6-cfbd-43de-9e68-bbaf447213fa
@chain AAPL_Rx begin
	combine(:LogRx => (rx -> exp(sum(rx))) => :V_T)
end

# ╔═╡ a488a679-6dc0-4a33-b327-9d0f6e3b9eb2
md"""
**Next, let's work with a portfolio of stocks.**
"""

# ╔═╡ 76adf9cb-db2c-4b76-9614-be12bb9c5764
md"""
Let's pick 5 stocks: AAPL, BA, DIS, GS and JNJ.
"""

# ╔═╡ 99c0ee6b-e040-4261-992c-0f5a0eb8158c
Portfolio = @chain CRSP begin
	dropmissing(:TICKER)
	filter(:TICKER => (x-> x ∈ ["AAPL","BA","DIS","GS","JNJ"]),_)
	transform(:date => ByRow(x->Date(string(x),dateformat"yyyymmdd")) => :date)
	select(:date, :TICKER, :PERMCO, :PERMNO, :PRC, :DIVAMT)

	groupby([:date,:TICKER])
	combine(_) do sdf
       if nrow(sdf) == 2 
            DataFrame(sdf[2,:])
       else
            sdf
       end
    end
	
    sort([:TICKER,:date])
	groupby([:TICKER])
	transform(:PRC => (x-> lag(x)) => :PRC_L)
	
	transform([:PRC, :PRC_L, :DIVAMT] => ByRow((Px_t,Px_tminus,div) -> GetRx(Px_t,Px_tminus,div)) => :Rx,
			  [:PRC, :PRC_L, :DIVAMT] => ByRow((Px_t,Px_tminus,div) -> GetLogRx(Px_t,Px_tminus,div)) => :LogRx)
	dropmissing(:Rx)

	select(:date, :TICKER, :PERMCO, :PERMNO, :PRC, :PRC_L, :DIVAMT, :Rx, :LogRx)
	
end

# ╔═╡ 0eefd0af-f5dd-4b87-9be0-391d86cf8bf9
md"""
**To form a portfolio, it is more convenient to work with the data in *wide format*. This means that we want to put the five stocks in columns (one for each stock).**
"""

# ╔═╡ da35b9a0-5ac8-4608-8dc5-2c08901cc2e3
#https://dataframes.juliadata.org/stable/lib/functions/#DataFrames.unstack
@chain Portfolio begin
	select(:date,:TICKER,:Rx) 
	unstack(:date,:TICKER,:Rx)
end

# ╔═╡ 6572bd2f-76e8-4735-9dff-d371fe7f69bd
md"""
**Next, let's pick a vector of portfolio weights.**
"""

# ╔═╡ e5c2b68f-4f1e-4bd1-8240-ac52d268304b
w = [0.2,0.2,0.2,0.2,0.2]


# ╔═╡ 16cc7074-d786-4bb5-a735-187465e815e2
md"""
**Then, we calculate the portfolio return in each month.**
"""

# ╔═╡ 87099910-dfde-4fee-8910-1d6bd25d5170
PortfRx = @chain Portfolio begin
	select(:date,:TICKER,:Rx) 
	unstack(:date,:TICKER,:Rx)
	
	transform([:AAPL, :BA, :DIS, :GS, :JNJ] => ByRow((a,b,d,g,j) -> w'Array([a,b,d,g,j])) => :PortfRx)
end

# ╔═╡ c63e96e7-0ea0-4daf-a230-26b258f12e79
md"""
**Let's get a sense of the stock returns by using `describe`.**
"""

# ╔═╡ 07e767e6-d6fc-4f52-8b9d-72ca7c633955
describe(PortfRx)

# ╔═╡ fb69d701-e623-443e-b6f0-c75180b9be1b
md"""
**Let's create a table with summary statistics that we are interested in:**
average return, standard deviation of returns, minimum, median, and the maximum of monthly returns.
"""

# ╔═╡ cb78be8c-f039-434e-acbb-9375f3174588
md"""
**To create this table, it is easier to work with the data in `long` format.**
That is we have one column for the TICKER and one column for the return for each date. In this format, it is easier to group the data and to apply a data transformation.
"""

# ╔═╡ cdda5183-46c6-4283-a66f-168ed5790a77
#https://dataframes.juliadata.org/stable/lib/functions/#DataFrames.stack
@chain PortfRx begin
	stack([:AAPL,:BA,:DIS,:GS,:JNJ,:PortfRx],[:date], variable_name=:TICKER, value_name=:Rx)
end

# ╔═╡ c799f1ad-b82b-4421-bb71-d07cb7b6afd7
md"""
**Let's now make the final table with the summary statistics for the returns.**
"""

# ╔═╡ b1a76909-a53f-492a-bec2-5415d90d5de4
PortfSummary = @chain PortfRx begin
	stack([:AAPL,:BA,:DIS,:GS,:JNJ,:PortfRx],[:date], variable_name=:TICKER, value_name=:Rx)

	groupby(:TICKER)
	combine(:Rx => (x-> (Mean=mean(x), StdDev=std(x), Min=minimum(x), Med=median(x), Max=maximum(x) )) => AsTable)
end

# ╔═╡ 4cfeda6b-6b12-4ffa-9379-15c00b53a49d
md"""
**Next, let's calculate the covariance matrix of returns.**
"""

# ╔═╡ 80865386-79cf-45b0-836d-e55a1ca64174
cov(Matrix(PortfRx[:,Not(:date)]))

# ╔═╡ aa21f05c-1e78-4919-86fa-57ae89d4d633
md"""
**Lastly, let's use what we have learned about portfolio mathematics at the beginning of this lecuture and apply it to our portfolio of stocks.**
"""

# ╔═╡ fa05b130-4ab5-49ea-8a8c-f6d9fcfd1656
let
	assets = filter(:TICKER => (x->x!="PortfRx"), PortfSummary)
	μ = assets.Mean                    #\mu[TAB] to get μ
	Σ = cov(Matrix(PortfRx[:,Not([:date,:PortfRx])]))                     #\Sigma[TAB]

	ERp   = w'μ
	VarRp = w'Σ*w
	
	with_terminal() do
		printyellow("expected returns*100: ")
		printmat(μ*100,rowNames=assets.TICKER)

		printyellow("covariance matrix*100^2:")
		printmat(Σ*100^2,rowNames=assets.TICKER,colNames=assets.TICKER)

		printlnPs("Expected portfolio return: ",ERp)
		printlnPs("Portfolio variance and std:",VarRp,sqrt(VarRp))
		
	end

end

# ╔═╡ 00000000-0000-0000-0000-000000000001
PLUTO_PROJECT_TOML_CONTENTS = """
[deps]
CSV = "336ed68f-0bac-5ca0-87d4-7b16caf5d00b"
Chain = "8be319e6-bccf-4806-a6f7-6fae938471bc"
DataFrames = "a93c6f00-e57d-5684-b7b6-d8193f3e46c0"
Dates = "ade2ca70-3891-5945-98fb-dc099432e06a"
Logging = "56ddb016-857b-54e1-b83d-db4d58db5568"
PlutoUI = "7f904dfe-b85e-4ff6-b463-dae2292396a8"
Printf = "de0858da-6303-5e67-8744-51eddeeeb8d7"
ShiftedArrays = "1277b4bf-5013-50f5-be3d-901d8477a67a"
Statistics = "10745b16-79ce-11e8-11f9-7d13ad32a3b2"

[compat]
CSV = "~0.10.2"
Chain = "~0.4.10"
DataFrames = "~1.3.2"
PlutoUI = "~0.7.35"
ShiftedArrays = "~1.0.0"
"""

# ╔═╡ 00000000-0000-0000-0000-000000000002
PLUTO_MANIFEST_TOML_CONTENTS = """
# This file is machine-generated - editing it directly is not advised

[[AbstractPlutoDingetjes]]
deps = ["Pkg"]
git-tree-sha1 = "8eaf9f1b4921132a4cff3f36a1d9ba923b14a481"
uuid = "6e696c72-6542-2067-7265-42206c756150"
version = "1.1.4"

[[ArgTools]]
uuid = "0dad84c5-d112-42e6-8d28-ef12dabb789f"

[[Artifacts]]
uuid = "56f22d72-fd6d-98f1-02f0-08ddc0907c33"

[[Base64]]
uuid = "2a0f44e3-6c83-55bd-87e4-b1978d98bd5f"

[[CSV]]
deps = ["CodecZlib", "Dates", "FilePathsBase", "InlineStrings", "Mmap", "Parsers", "PooledArrays", "SentinelArrays", "Tables", "Unicode", "WeakRefStrings"]
git-tree-sha1 = "9519274b50500b8029973d241d32cfbf0b127d97"
uuid = "336ed68f-0bac-5ca0-87d4-7b16caf5d00b"
version = "0.10.2"

[[Chain]]
git-tree-sha1 = "339237319ef4712e6e5df7758d0bccddf5c237d9"
uuid = "8be319e6-bccf-4806-a6f7-6fae938471bc"
version = "0.4.10"

[[CodecZlib]]
deps = ["TranscodingStreams", "Zlib_jll"]
git-tree-sha1 = "ded953804d019afa9a3f98981d99b33e3db7b6da"
uuid = "944b1d66-785c-5afd-91f1-9de20f533193"
version = "0.7.0"

[[ColorTypes]]
deps = ["FixedPointNumbers", "Random"]
git-tree-sha1 = "024fe24d83e4a5bf5fc80501a314ce0d1aa35597"
uuid = "3da002f7-5984-5a60-b8a6-cbb66c0b333f"
version = "0.11.0"

[[Compat]]
deps = ["Base64", "Dates", "DelimitedFiles", "Distributed", "InteractiveUtils", "LibGit2", "Libdl", "LinearAlgebra", "Markdown", "Mmap", "Pkg", "Printf", "REPL", "Random", "SHA", "Serialization", "SharedArrays", "Sockets", "SparseArrays", "Statistics", "Test", "UUIDs", "Unicode"]
git-tree-sha1 = "44c37b4636bc54afac5c574d2d02b625349d6582"
uuid = "34da2185-b29b-5c13-b0c7-acf172513d20"
version = "3.41.0"

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

[[Downloads]]
deps = ["ArgTools", "LibCURL", "NetworkOptions"]
uuid = "f43a241f-c20a-4ad4-852c-f6b1247861c6"

[[FilePathsBase]]
deps = ["Compat", "Dates", "Mmap", "Printf", "Test", "UUIDs"]
git-tree-sha1 = "04d13bfa8ef11720c24e4d840c0033d145537df7"
uuid = "48062228-2e41-5def-b9a4-89aafe57970f"
version = "0.9.17"

[[FixedPointNumbers]]
deps = ["Statistics"]
git-tree-sha1 = "335bfdceacc84c5cdf16aadc768aa5ddfc5383cc"
uuid = "53c48c17-4a7d-5ca2-90c5-79b7896eea93"
version = "0.8.4"

[[Formatting]]
deps = ["Printf"]
git-tree-sha1 = "8339d61043228fdd3eb658d86c926cb282ae72a8"
uuid = "59287772-0a20-5a39-b81b-1366585eb4c0"
version = "0.4.2"

[[Future]]
deps = ["Random"]
uuid = "9fa8497b-333b-5362-9e8d-4d0656e87820"

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

[[InlineStrings]]
deps = ["Parsers"]
git-tree-sha1 = "61feba885fac3a407465726d0c330b3055df897f"
uuid = "842dd82b-1e85-43dc-bf29-5d0ee9dffc48"
version = "1.1.2"

[[InteractiveUtils]]
deps = ["Markdown"]
uuid = "b77e0a4c-d291-57a0-90e8-8db25a27a240"

[[InvertedIndices]]
git-tree-sha1 = "bee5f1ef5bf65df56bdd2e40447590b272a5471f"
uuid = "41ab1584-1d38-5bbf-9106-f11c6c58b48f"
version = "1.1.0"

[[IteratorInterfaceExtensions]]
git-tree-sha1 = "a3f24677c21f5bbe9d2a714f95dcd58337fb2856"
uuid = "82899510-4779-5014-852e-03e436cf321d"
version = "1.0.0"

[[JSON]]
deps = ["Dates", "Mmap", "Parsers", "Unicode"]
git-tree-sha1 = "3c837543ddb02250ef42f4738347454f95079d4e"
uuid = "682c06a0-de6a-54ab-a142-c8b1cf79cde6"
version = "0.21.3"

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

[[LinearAlgebra]]
deps = ["Libdl"]
uuid = "37e2e46d-f89d-539d-b4ee-838fcccc9c8e"

[[Logging]]
uuid = "56ddb016-857b-54e1-b83d-db4d58db5568"

[[Markdown]]
deps = ["Base64"]
uuid = "d6f4376e-aef5-505a-96c1-9c027394607a"

[[MbedTLS_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "c8ffd9c3-330d-5841-b78e-0817d7145fa1"

[[Missings]]
deps = ["DataAPI"]
git-tree-sha1 = "bf210ce90b6c9eed32d25dbcae1ebc565df2687f"
uuid = "e1d29d7a-bbdc-5cf2-9ac0-f12de2c33e28"
version = "1.0.2"

[[Mmap]]
uuid = "a63ad114-7e13-5084-954f-fe012c677804"

[[MozillaCACerts_jll]]
uuid = "14a3606d-f60d-562e-9121-12d972cd8159"

[[NetworkOptions]]
uuid = "ca575930-c2e3-43a9-ace4-1e988b2c1908"

[[OrderedCollections]]
git-tree-sha1 = "85f8e6578bf1f9ee0d11e7bb1b1456435479d47c"
uuid = "bac558e1-5e72-5ebc-8fee-abe8a469f55d"
version = "1.4.1"

[[Parsers]]
deps = ["Dates"]
git-tree-sha1 = "13468f237353112a01b2d6b32f3d0f80219944aa"
uuid = "69de0a69-1ddd-5017-9359-2bf0b02dc9f0"
version = "2.2.2"

[[Pkg]]
deps = ["Artifacts", "Dates", "Downloads", "LibGit2", "Libdl", "Logging", "Markdown", "Printf", "REPL", "Random", "SHA", "Serialization", "TOML", "Tar", "UUIDs", "p7zip_jll"]
uuid = "44cfe95a-1eb2-52ea-b672-e2afdf69b78f"

[[PlutoUI]]
deps = ["AbstractPlutoDingetjes", "Base64", "ColorTypes", "Dates", "Hyperscript", "HypertextLiteral", "IOCapture", "InteractiveUtils", "JSON", "Logging", "Markdown", "Random", "Reexport", "UUIDs"]
git-tree-sha1 = "85bf3e4bd279e405f91489ce518dedb1e32119cb"
uuid = "7f904dfe-b85e-4ff6-b463-dae2292396a8"
version = "0.7.35"

[[PooledArrays]]
deps = ["DataAPI", "Future"]
git-tree-sha1 = "db3a23166af8aebf4db5ef87ac5b00d36eb771e2"
uuid = "2dfb63ee-cc39-5dd5-95bd-886bf059d720"
version = "1.4.0"

[[PrettyTables]]
deps = ["Crayons", "Formatting", "Markdown", "Reexport", "Tables"]
git-tree-sha1 = "dfb54c4e414caa595a1f2ed759b160f5a3ddcba5"
uuid = "08abe8d2-0d0c-5749-adfa-8a2ac140af0d"
version = "1.3.1"

[[Printf]]
deps = ["Unicode"]
uuid = "de0858da-6303-5e67-8744-51eddeeeb8d7"

[[REPL]]
deps = ["InteractiveUtils", "Markdown", "Sockets", "Unicode"]
uuid = "3fa0cd96-eef1-5676-8a61-b3b8758bbffb"

[[Random]]
deps = ["Serialization"]
uuid = "9a3f8284-a2c9-5f02-9a11-845980a1fd5c"

[[Reexport]]
git-tree-sha1 = "45e428421666073eab6f2da5c9d310d99bb12f9b"
uuid = "189a3867-3050-52da-a836-e630ba90ab69"
version = "1.2.2"

[[SHA]]
uuid = "ea8e919c-243c-51af-8825-aaa63cd721ce"

[[SentinelArrays]]
deps = ["Dates", "Random"]
git-tree-sha1 = "6a2f7d70512d205ca8c7ee31bfa9f142fe74310c"
uuid = "91c51154-3ec4-41a3-a24f-3f23e20d615c"
version = "1.3.12"

[[Serialization]]
uuid = "9e88b42a-f829-5b0c-bbe9-9e923198166b"

[[SharedArrays]]
deps = ["Distributed", "Mmap", "Random", "Serialization"]
uuid = "1a1011a3-84de-559e-8e89-a11a2f7dc383"

[[ShiftedArrays]]
git-tree-sha1 = "22395afdcf37d6709a5a0766cc4a5ca52cb85ea0"
uuid = "1277b4bf-5013-50f5-be3d-901d8477a67a"
version = "1.0.0"

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

[[Statistics]]
deps = ["LinearAlgebra", "SparseArrays"]
uuid = "10745b16-79ce-11e8-11f9-7d13ad32a3b2"

[[TOML]]
deps = ["Dates"]
uuid = "fa267f1f-6049-4f14-aa54-33bafae1ed76"

[[TableTraits]]
deps = ["IteratorInterfaceExtensions"]
git-tree-sha1 = "c06b2f539df1c6efa794486abfb6ed2022561a39"
uuid = "3783bdb8-4a98-5b6b-af9a-565f29a5fe9c"
version = "1.0.1"

[[Tables]]
deps = ["DataAPI", "DataValueInterfaces", "IteratorInterfaceExtensions", "LinearAlgebra", "TableTraits", "Test"]
git-tree-sha1 = "bb1064c9a84c52e277f1096cf41434b675cd368b"
uuid = "bd369af6-aec1-5ad0-b16a-f7cc5008161c"
version = "1.6.1"

[[Tar]]
deps = ["ArgTools", "SHA"]
uuid = "a4e569a6-e804-4fa4-b0f3-eef7a1d5b13e"

[[Test]]
deps = ["InteractiveUtils", "Logging", "Random", "Serialization"]
uuid = "8dfed614-e22c-5e08-85e1-65c5234f0b40"

[[TranscodingStreams]]
deps = ["Random", "Test"]
git-tree-sha1 = "216b95ea110b5972db65aa90f88d8d89dcb8851c"
uuid = "3bb67fe8-82b1-5028-8e26-92a6c54297fa"
version = "0.9.6"

[[UUIDs]]
deps = ["Random", "SHA"]
uuid = "cf7118a7-6976-5b1a-9a39-7adc72f591a4"

[[Unicode]]
uuid = "4ec0a83e-493e-50e2-b9ac-8f72acf5a8f5"

[[WeakRefStrings]]
deps = ["DataAPI", "InlineStrings", "Parsers"]
git-tree-sha1 = "c69f9da3ff2f4f02e811c3323c22e5dfcb584cfa"
uuid = "ea10d353-3f73-51f8-a26c-33c1cb351aa5"
version = "1.4.1"

[[Zlib_jll]]
deps = ["Libdl"]
uuid = "83775a58-1f1d-513f-b197-d71354ab007a"

[[nghttp2_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "8e850ede-7688-5339-a07c-302acd2aaf8d"

[[p7zip_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "3f19e933-33d8-53b3-aaab-bd5110c3b7a0"
"""

# ╔═╡ Cell order:
# ╟─e796c131-5a67-4a99-8df2-b7ea52a03056
# ╠═2a5055b4-2137-447d-bfd2-a26cc4b14735
# ╟─a548314b-511d-4319-8f7b-95a3158ba90c
# ╟─bd352f2e-3b7f-4b5b-ae43-fd244447347a
# ╟─6334d495-fa91-4428-a203-4d8234324ece
# ╠═6c19fbf2-342d-48ff-97d7-983bb1ae1122
# ╟─75007c57-94e8-49fc-8bbb-3d39d6207fa2
# ╠═c1a5ff0e-ce5e-4390-bed7-af5be140a145
# ╟─e6c198f4-5abd-468c-841c-8a7b032f1dc2
# ╠═e89ecb62-4c0f-4266-863b-529694cca1c0
# ╠═3369be71-2a11-4dc8-8d1b-d2b1d7749c5f
# ╠═4dece1db-3305-4b94-8a1b-f5c833d67444
# ╠═e5318e00-c074-4074-8eb4-3245ceceb4c4
# ╟─c537a2e6-114f-4a86-9323-cc7e7b94ceb3
# ╟─98083989-a006-4cf3-afd1-0b3495e1d5b6
# ╟─e3529bed-ac89-4e5f-a1e2-e2a7d4c7b073
# ╠═2a18e0ef-4332-4a11-af9f-3efea3f8f69d
# ╟─ad26e54b-85b1-4631-b1ed-531873ba712a
# ╠═aee2e708-6b13-4609-b60c-a33cbd741023
# ╟─d99160b7-9421-404d-bef4-55f634d6e586
# ╟─22073910-f676-4e8b-8b80-38fed57bf399
# ╟─8980a963-b6c0-48cd-9316-b96df2acf4c6
# ╟─6a25c898-54b3-4afc-b771-e13c7cda66eb
# ╟─456211a1-3828-458c-a597-cb35a88c35ca
# ╟─590799eb-b5e0-4ee0-bdb5-ae4bd01f5576
# ╟─d86c3764-81fb-4dcc-af04-094356a25216
# ╠═6cb63947-6490-4a02-8b74-55eb5e2efa96
# ╟─a65fa0ae-1ceb-451e-ab37-08c0d23a508e
# ╠═523e0191-237e-4bdb-b9a3-53079b9e92d5
# ╟─b8051d63-ed26-4e21-8a98-f564a3b8a967
# ╠═a0cf0361-422a-4bcd-8b9e-943dc00044b3
# ╟─6b720ade-5e4c-4f58-8fcd-f609df29df7b
# ╠═0cf85da2-a538-402d-9785-094425260bdb
# ╟─8a869ecb-a6ed-499d-873b-131c9bc2e0b1
# ╠═446e0db1-8107-4724-b774-561c5d37f87a
# ╟─45868894-a317-4370-9631-6ef3c31661e3
# ╟─7b666951-b303-4100-8dd2-feb8e2d22b14
# ╠═83a3993e-073f-4e84-acfe-61bfc76dde3a
# ╟─4805e8a6-34d6-49f0-8706-88c916e4689f
# ╠═9b816236-f9de-4aad-aea0-b5f8fbfc6b11
# ╠═d300be65-f494-4181-9924-e69cc6c04f09
# ╟─2bd1674d-2254-45a8-9f02-5caa5bd0bd1c
# ╠═a23a366d-fa73-4672-b59c-e14b2b817ce8
# ╟─5baa1a9f-7ad5-4bfc-b468-9e0b40438548
# ╠═0c821308-45ca-4317-80b3-9e87c6840465
# ╠═8c58cbc6-cfbd-43de-9e68-bbaf447213fa
# ╟─a488a679-6dc0-4a33-b327-9d0f6e3b9eb2
# ╟─76adf9cb-db2c-4b76-9614-be12bb9c5764
# ╠═99c0ee6b-e040-4261-992c-0f5a0eb8158c
# ╟─0eefd0af-f5dd-4b87-9be0-391d86cf8bf9
# ╠═da35b9a0-5ac8-4608-8dc5-2c08901cc2e3
# ╟─6572bd2f-76e8-4735-9dff-d371fe7f69bd
# ╠═e5c2b68f-4f1e-4bd1-8240-ac52d268304b
# ╟─16cc7074-d786-4bb5-a735-187465e815e2
# ╠═87099910-dfde-4fee-8910-1d6bd25d5170
# ╟─c63e96e7-0ea0-4daf-a230-26b258f12e79
# ╠═07e767e6-d6fc-4f52-8b9d-72ca7c633955
# ╟─fb69d701-e623-443e-b6f0-c75180b9be1b
# ╟─cb78be8c-f039-434e-acbb-9375f3174588
# ╠═cdda5183-46c6-4283-a66f-168ed5790a77
# ╟─c799f1ad-b82b-4421-bb71-d07cb7b6afd7
# ╠═b1a76909-a53f-492a-bec2-5415d90d5de4
# ╟─4cfeda6b-6b12-4ffa-9379-15c00b53a49d
# ╠═80865386-79cf-45b0-836d-e55a1ca64174
# ╟─aa21f05c-1e78-4919-86fa-57ae89d4d633
# ╠═fa05b130-4ab5-49ea-8a8c-f6d9fcfd1656
# ╟─00000000-0000-0000-0000-000000000001
# ╟─00000000-0000-0000-0000-000000000002
