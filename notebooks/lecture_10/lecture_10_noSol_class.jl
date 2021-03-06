### A Pluto.jl notebook ###
# v0.18.1

using Markdown
using InteractiveUtils

# ╔═╡ 3d731294-71d0-4b34-85e2-96d29bd8a7ca
begin
	using CSV, Chain, DataFrames, Dates, LinearAlgebra, Plots, LaTeXStrings, PlutoUI, Printf, ShiftedArrays, Statistics
	
	gr(size=(480,320)) #for plotting
end

# ╔═╡ c43df4a3-a1d8-433e-9a1c-f7c0984be879
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
#helper functions
#round to digits, e.g. 6 digits then prec=1e-6
roundmult(val, prec) = (inv_prec = 1 / prec; round(val * inv_prec) / inv_prec)
	
using Logging
global_logger(NullLogger())
	
display("")
	
end

# ╔═╡ 49fdad82-e563-4e4c-88a1-33166e9ddee4
begin
	using JuMP
	import Ipopt
end

# ╔═╡ 81404cf5-bd49-4dda-8471-fc1c8417199b
begin
	using Convex
	import SCS
end

# ╔═╡ 5141ad80-2374-11ec-2455-c7ff63842559
md"""
## FINC 672: Mean-Variance Analysis
"""

# ╔═╡ 5fc44a1a-2c5c-4cdf-b2da-4cbc17c8f8a3
TableOfContents(aside=true, depth=1)

# ╔═╡ 72c219a7-1c04-4678-988c-8f17674d9d75
md"""
# Case of Three Assets
- Suppose we estimate the average returns and the covariance matrix of returns for three assets (A, B, and C).
- Let the riskfree rate be $r_f=0.03$.
"""

# ╔═╡ 652dc6ae-6f9d-4ebd-9251-00821a8ee09d
begin
	μ = [0.115, 0.095, 0.06]
	Σ = [166 34 58;
	     34  64 4;
	     58  4  100]/100^2
	Rf = 0.03

	assetNames = ["A","B","C"]

	with_terminal() do
		printred("expected returns:")
		printmat(μ,rowNames=assetNames)

		printred("covariance matrix:")
		printmat(Σ,colNames=assetNames,rowNames=assetNames)
	end
	
end

# ╔═╡ d558b927-2cea-45ef-b50b-0f53640e49d4
md"""
# Mean-Variance Frontier of Risky Assets
"""

# ╔═╡ fae8140d-9eea-432b-a858-ec12d3728ef2
md"""
- We define a function to implement the approach discussed in `lecture_10_Reading.pdf` available on Canvas.
"""

# ╔═╡ 3fca5d0e-c342-4520-867c-917116495ac2
"""
    MVCalc(μstar,μ,Σ)

Calculate the std and weights of a portfolio (with mean return μstar) on MVF of risky assets.

- Remark
  - Only (λ,δ) and thus (w,stdRp) depend on μstar. We could therefore speed up the computations a bit by doing the loop over different μstar values inside the function (and thus not recalculate Σ_1,a,b,c).
  - See p. 60 of lecture notes `lecture_10_Reading.pdf` available on Canvas.
"""
function MVCalc(μstar, μ, Σ)
	n = length(μ)
	Σ_1 = inv(Σ)
	a = μ'Σ_1*μ
	b = μ'Σ_1*ones(n)
	c = ones(n)'Σ_1*ones(n)
	λ = (c*μstar - b)/(a*c-b^2)
	δ = (a-b*μstar)/(a*c-b^2)
	w = Σ_1 * (μ*λ .+ δ)
	StdRp = sqrt(w'Σ*w)
	return StdRp, w
end


# ╔═╡ b16ecff8-2dc1-4827-b33c-5eb0af4a6961
md"""
- Let's pick a range of values for $\mu$ and plot the mean-variance frontier.
"""

# ╔═╡ 95a13699-95b4-4f7b-baaa-80627f33841f
begin
	μ_MV = collect(0.01:0.001:0.20)
	σ_MV = Array{Float64}(undef,length(μ_MV))
	for (i,mu) = enumerate(μ_MV)
		σ_MV[i] = MVCalc(mu,μ,Σ)[1]
	end

	p_MV = plot(σ_MV,μ_MV, color=:blue, xlim=(0,0.20), ylim=(0,0.15), xlabel=L"\sigma", ylabel=L"\mu", label="MV Frontier", legend=:topleft)
end

# ╔═╡ 63c6b02c-4674-45b4-a96a-2545959d9d7c
md"""
- Let's add the individual assets.
"""

# ╔═╡ c40dc723-1169-4826-941d-ba162d2bf7b8
begin
	#Add Asset A
	p_MV2 = scatter!(p_MV, [sqrt.(Σ[1,1])], [μ[1]], markershape=:cross, markersize=7, markerstrokewidth=15, markercolor=:red, label="")
	annotate!(p_MV2,[sqrt.(Σ[1,1])+0.01],[μ[1]],"A")
	
	#Add Asset B
	scatter!(p_MV2, [sqrt.(Σ[2,2])], [μ[2]], markershape=:cross, markersize=7, markerstrokewidth=15, markercolor=:red, label="")
	annotate!(p_MV2,[sqrt.(Σ[2,2])+0.01],[μ[2]],"B")
	
	#Add Asset C
	scatter!(p_MV2, [sqrt.(Σ[3,3])], [μ[3]], markershape=:cross, markersize=7, markerstrokewidth=15, markercolor=:red, label="")
	annotate!(p_MV2,[sqrt.(Σ[3,3])+0.01],[μ[3]],"C")
	
end

# ╔═╡ 0e91775f-b061-445c-adcd-d453f98c594f
MVCalc(0.12, μ, Σ)

# ╔═╡ ef313c73-14bd-42d9-a1d4-c35709dec26c
md"""
- Let's add other portfolios.
Portfolio weights

Portfolio weights	|  B     |   1     |   2     |   3
:---|:-------|:--------|:--------|:-------
A   |  0.00  |   0.72  |   0.02  |   0.05
B   |  1.00  |   0.08  |   0.63  |   0.15
C   |  0.00  |   0.20  |   0.35  |   0.80

"""

# ╔═╡ 69f5983a-5e47-4331-9518-6c8c30340b24
md"""
- Let's make a table with the means and standard deviations of Portfolio 1, 2, and 3.
"""

# ╔═╡ 212685c3-9049-42e9-b535-6e515f19153b
begin
	df_MV = DataFrame(P1=[0.72, 0.08, 0.20], P2=[0.02, 0.63, 0.35], P3=[0.05, 0.15, 0.80])
	df_MV_μ = combine(df_MV, [:P1, :P2, :P3] .=> (x->x'μ) , renamecols=false)
	df_MV_σ = combine(df_MV, [:P1, :P2, :P3] .=> ( x-> sqrt(x'Σ*x) ), renamecols=false)
	df_MV_μσ = vcat(df_MV_μ, df_MV_σ)
	
end

# ╔═╡ 77a08244-548c-4a7a-be3e-cdc8c6e29622
md"""
- Now, we add the portfolios to the plots.
"""

# ╔═╡ f3f52640-d079-4d6c-8c2a-63f89c53b061
begin
	#add other portfolios
	#1
	p_MV3 = scatter!(p_MV2,[df_MV_μσ[1,:P1]], [df_MV_μσ[2,:P1]], markershape=:star,markersize=3, label="")
	annotate!(p_MV3,[df_MV_μσ[2,:P1]+0.005],[df_MV_μσ[1,:P1]],("1",12,:green))
	#2
	scatter!(p_MV3,[df_MV_μσ[1,:P2]], [df_MV_μσ[2,:P2]], markershape=:star,markersize=3, label="")
	annotate!(p_MV3,[df_MV_μσ[2,:P2]+0.005],[df_MV_μσ[1,:P2]],("2",12,:green))
	#3
	scatter!(p_MV3,[df_MV_μσ[1,:P3]], [df_MV_μσ[2,:P3]], markershape=:star,markersize=3, label="")
	annotate!(p_MV3,[df_MV_μσ[2,:P3]+0.005],[df_MV_μσ[1,:P3]],("3",12,:green))
end

# ╔═╡ fa8d6069-e8da-4bba-8475-d20c5c6dc090
md"""
# Mean-Variance Frontier of Riskfree and Risky Assets
"""

# ╔═╡ 67c26862-f6b3-48cc-a884-36adb554c378
md"""
- Next, we add the riskfree asset to the set of risky assets.
"""

# ╔═╡ 33fe5282-db49-4732-9c80-cc1537c120c5
md"""
- We define a function to implement the approach discussed in `lecture_10_Reading.pdf` available on Canvas.
"""

# ╔═╡ ecf84238-b03d-4036-8fd3-88bcb394c925

"""
    MVCalcRf(μstar,μ,Σ,Rf)

Calculate the std and portfolio weights of a portfolio (with a given mean, μstar) on MVF of (risky assets,riskfree). See p. 62 of lecture notes `lecture_10_Reading.pdf` available on Canvas.
"""



# ╔═╡ 92a880c2-91c3-4db4-bcb4-a376572396c9
function MVCalcRf(μstar,μ,Σ,Rf)
	μe = μ .- Rf
	Σ_1 = inv(Σ)
	w = (μstar-Rf)/(μe'Σ_1*μe) * Σ_1 * μe
	StdRp = sqrt(w'Σ*w)
	return StdRp, w
end

# ╔═╡ 2e082d7a-7fec-44e9-8697-78a1328e7d3d
md"""
- Let's plot the efficient frontier with a riskfree asset.
"""

# ╔═╡ e1ac8d53-000e-44d7-8090-334468a6d2ad
begin
	μ_MVRf = collect(Rf:0.001:0.20)
	σ_MVRf = Array{Float64}(undef, length(μ_MVRf))
	w_MVRf = Array{Float64}(undef, length(μ_MVRf))
	for (i,mu) in enumerate(μ_MVRf)
		σ_MVRf[i] = MVCalcRf(mu,μ,Σ,Rf)[1] 
	end
	
	p_MVRf = plot(σ_MVRf,μ_MVRf, color=:blue, xlim=(0,0.20), ylim=(0,0.15), xlabel=L"\sigma", ylabel = L"\mu", label="MV/Rf Frontier", legend=:topleft )
end

# ╔═╡ aa126d99-3fba-49bc-91c0-bde7cdfeb3bb
md"""
- Let's add the mean-variance frontier of (only) risky assets.
"""

# ╔═╡ a0782ccf-5fb4-4ca3-9e3e-c8da69909847
begin
	p_MVRf2 = p_MVRf
	plot!(p_MVRf2, σ_MV, μ_MV, label="MV Frontier", color=:red)
end

# ╔═╡ 74e6c915-5dca-4c84-a890-0b829ba6cd92
md"""
# The Tangency Portfolio
"""

# ╔═╡ 5be9cfce-89e0-4ad7-9aef-dd3252829c18
md"""
- Let's next calculate the tangency portfolio.
- We define a function to implement the approach discussed in `lecture_10_Reading.pdf` available on Canvas.
"""

# ╔═╡ fe44f447-72db-4c05-ba03-6c26e1573bd6

"""
    MVTangencyP(μ,Σ,Rf)

Calculate the tangency portfolio. See p. 65 of lecture notes `lecture_10_Reading.pdf` available on Canvas.
"""


# ╔═╡ e45ff2ab-eb7d-40af-887c-b9f56cbcf6a6

function MVTangencyP(μ,Σ,Rf)
	n = length(μ)
	μe = μ .- Rf
	Σ_1 = inv(Σ)
	w = Σ_1 *μe/(ones(n)'Σ_1*μe)
	muT = w'μ + (1-sum(w))*Rf
	StdT = sqrt(w'Σ*w)
	return w, muT, StdT
end

# ╔═╡ 3652b806-8991-47d4-ad82-e8affde497f6
md"""
- Let's collect the tangency portfolio weights, and the expected return and standard deviation of the returns of the tangency portfolio.
"""

# ╔═╡ fc304ff1-e4ff-4047-a8d3-353816202724
begin
	wT, muT, StdT = MVTangencyP(μ,Σ,Rf)
	df_T = DataFrame(wA=wT[1], wB=wT[2], wC=wT[3], μ=muT , σ=StdT)
end

# ╔═╡ 0b30896b-d6da-4391-a2dc-e9e0bcbe2fa2
md"""
- Let's add the tangency portfolio to the mean-variance frontier.
"""

# ╔═╡ 83a1fb1d-221b-4f26-8e65-e319ea9b899d
let
	p_MVRf2T = p_MVRf2
	scatter!(p_MVRf2T, [df_T.σ], [df_T.μ], label="")
end

# ╔═╡ 3f73b5cf-fe07-4bd5-8306-e319024f4bb9
md"""
# Mean Variance Analysis using CRSP Data
"""

# ╔═╡ dff92924-af65-444c-9ac6-62c0449f7c41
md"""
- Let's pick six stocks from CRSP: AAPL BA CAT GS PG WMT.
"""

# ╔═╡ e0bffa07-2db9-4ec1-a116-474d71050df6
md"""
Ticker | Company Name
:------|:-------------
AAPL | 	Apple Inc
AXP	 |  American Express Co
CAT  | 	Caterpillar Inc
GS   | 	Goldman Sachs Group Inc/The
MRK	 |  Merck & Co Inc
WMT  | 	Walmart Inc
"""

# ╔═╡ 296b85d2-2dac-4cf4-aa86-b33f0ca13650
StockTicker = ["AAPL","AXP","CAT","GS","MRK","WMT"]

# ╔═╡ 322112e3-3aee-4e70-8203-6a1227a448de
md"""
- Let's load the CRSP data into a dataframe.
"""

# ╔═╡ 624bfb0a-3336-448d-982a-511f9743202c
CRSP = DataFrame(CSV.File("CRSP_monthly.csv",ntasks=1))

# ╔═╡ e2e22821-c6c1-4e98-a5f4-725595c3f250
md"""
- In addition to the stock returns, we need an estimate of the riskfree interest rate.
- We will use the widely-used Fama-French dataset available on the [website of Kenneth French](https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html).
- The monthly dataset is available on Canvas under the datasets section. Download it from there (file name `F-F_Research_Data_Factors.csv`).
"""

# ╔═╡ b3ac63b0-aebd-4fff-9e98-82d0f4bbb8dc
FF = DataFrame(CSV.File("F-F_Research_Data_Factors.CSV", header=4, skipto=5))

# ╔═╡ d751ef72-7ab1-45f0-8375-7831175f075f
md"""
- Next, let's estimate the riskfree interest rate.
"""

# ╔═╡ 7465eeed-9af0-4504-9154-be22c8160a4b
RF = @chain FF begin
	transform(:Date => ByRow(x-> lastdayofmonth(Date(string(x),dateformat"yyyymm"))) , :RF => (x-> x./100), renamecols=false)
	select(:Date,:RF)
end

# ╔═╡ f07348bf-2ec1-4a83-9f1d-6af7787bd42d
md"""
- Next, let's define function to calculate stock returns (simple returns and log returns).
"""

# ╔═╡ a53c4108-7ee7-4547-afee-dfaacf87d944
function GetRx(Px_t,Px_tminus, div)
	divAmt = 0.0
	if !ismissing(div)
		divAmt = div
	end

	if any(ismissing.([Px_t, Px_tminus]))
		return missing
	else
		return (Px_t + divAmt - Px_tminus)/Px_tminus
	end
end

# ╔═╡ 3d053364-5a63-41b1-b654-9d99e00cc6c3
function GetLogRx(Px_t,Px_tminus, div)
	divAmt = 0.0
	if !ismissing(div)
		divAmt = div
	end

	if any(ismissing.([Px_t, Px_tminus]))
		return missing
	else
		return log((Px_t+divAmt)) - log(Px_tminus)
	end
end

# ╔═╡ db15e7a8-6912-49ae-81b4-c22abc40aac5
md"""
- Next, we filter the CRSP dataset to our set of stocks and calculate returns. We also convert the dates to Julia dates.
"""

# ╔═╡ 5deea522-f60b-4242-8c9f-26ee7c243677
df = @chain CRSP begin

	#Filter
	dropmissing(:TICKER)
	filter(:TICKER => (x-> x∈StockTicker),_)
	transform(:date => ByRow(x->Date(string(x),dateformat"yyyymmdd")) => :date)
	select!(:date, :TICKER, :PERMCO, :PERMNO, :PRC, :DIVAMT)
	
	#Delete duplicates
	groupby([:date, :TICKER])
	combine(_) do sdf
		if nrow(sdf) == 2
			DataFrame(sdf[2,:])
		else
			sdf
		end
	end

	#Get returns
	sort([:TICKER,:date])
	groupby([:TICKER])
	transform(:PRC => (x->lag(x)) => :PRC_L)

	transform( [:PRC, :PRC_L, :DIVAMT] => ByRow( (Px_t, Px_tminus, div)->GetRx(Px_t, Px_tminus, div) ) => :Rx)
	transform( [:PRC, :PRC_L, :DIVAMT] => ByRow( (Px_t, Px_tminus, div)->GetLogRx(Px_t, Px_tminus, div)) => :LogRx)
	dropmissing(:Rx)

	#order columns
	select(:date, :TICKER, :PERMCO, :PERMNO, :PRC, :PRC_L, :DIVAMT, :Rx, :LogRx)

end

# ╔═╡ b26bf12b-f377-4228-9ac6-9af67eaf6880
md"""
- To work with the data, we unstack it and put it into wide-format (i.e. we have the stocks in the columns and a single dates column).
"""

# ╔═╡ 3f2eb8f8-b30f-487d-a915-bcfa6ebd0954
#https://dataframes.juliadata.org/stable/lib/functions/#DataFrames.unstack
Stocks = @chain df begin
	select(:date,:TICKER,:Rx)
	unstack(:date,:TICKER,:Rx)
end

# ╔═╡ b4a689de-eb5a-42a0-b7f1-274bb81884f4
md"""
- Now, we can estimate expected returns and the covariance matrix of stock returns.
"""

# ╔═╡ 201bdaf1-bcb4-4e9e-8b38-fba457527ba6
μ_Stocks = let
	df = combine(Stocks, StockTicker .=> (x->mean(x)), renamecols=false)
	Array(df[1,:])
end

# ╔═╡ 1af038cd-1f76-4473-9abb-ca25efd5ef5b
Σ_Stocks = cov( Matrix( Stocks[:,Not(:date)] ) )

# ╔═╡ b10c5e71-2cd7-46ff-9a49-c749312becc9
md"""
- We are now all set to calculate the mean-variance frontier for our set of stocks and also to get the tangency portfolio.
"""

# ╔═╡ 9583fcde-ddc0-4463-b9e1-7c8f69921f17
begin
	Rf_Stocks = @chain RF begin
		filter(:Date => (x-> (year(x)>=2000 && year(x)<=2020)),_)
		combine(:RF => mean, renamecols=false)
		_.RF[1]
	end
		
	with_terminal() do
		printred("riskfree rate:")
		printmat(Rf_Stocks,rowNames="RF")
		printred("expected returns:")
		printmat(μ_Stocks,rowNames=StockTicker)
		printred("covariance matrix:")
		printmat(Σ_Stocks,colNames=StockTicker,rowNames=StockTicker)
	end
end

# ╔═╡ 63e089e7-19da-4b22-b4f8-93c7dcf3fffd
md"""
- Let's plot the mean-variance frontier for our set of stocks.
"""

# ╔═╡ 41b4f432-2218-427b-b9b0-c882e3e4c0f0
begin
	μ_MVStocks = collect(0.0010:0.0001:0.03)
	σ_MVStocks = Array{Float64}(undef, length(μ_MVStocks))
	for (i,mu) in enumerate(μ_MVStocks)
		σ_MVStocks[i] = MVCalc(mu, μ_Stocks, Σ_Stocks)[1]
	end

	p_MVStocks = plot(σ_MVStocks, μ_MVStocks, color=:blue, xlim=(0,0.15), ylim=(0,0.03), xlabel=L"\sigma", ylabel=L"\mu", label="MV Frontier", legend=:topleft)
end

# ╔═╡ 0d2c744c-cfbc-4ee7-b50f-56fa5f2faef6
md"""
- Let's add the individual assets.
"""

# ╔═╡ 68c13c22-274b-4e9b-ba45-6cd7054b1642
begin
	p_MVStocks2 = plot(σ_MVStocks,μ_MVStocks, color=:blue, xlim=(0,0.15), ylim=(0,0.03), xlabel=L"\sigma", ylabel=L"\mu", label="MV Frontier", legend=:topleft)	
	for i=1:length(StockTicker)
		scatter!(p_MVStocks2,[sqrt.((Σ_Stocks[i,i]))],[μ_Stocks[i]], markershape=:cross,markersize=4, markerstrokewidth=15, markercolor=:red,label="")
		annotate!(p_MVStocks2,[sqrt.((Σ_Stocks[i,i]))+0.01],[μ_Stocks[i]],(StockTicker[i],10))
	end
	p_MVStocks2
end

# ╔═╡ 66b6a673-8a3f-46e6-ad02-43451c6b75f2
md"""
- Let's visualize the mean-variance frontier with a riskfree asset.
"""

# ╔═╡ 28059fe1-7051-4846-83c4-4dc6a3a0217c
begin
	μ_MVRfStocks = collect(Rf_Stocks:0.0001:0.10)
	σ_MVRfStocks = Array{Float64}(undef,length(μ_MVRfStocks))
	w_MVRfStocks = Array{Float64}(undef,length(μ_MVRfStocks))
	for (i,mu) in enumerate(μ_MVRfStocks)
		σ_MVRfStocks[i] = MVCalcRf(mu,μ_Stocks,Σ_Stocks,Rf_Stocks)[1]
	end
	p_MVRfStocks = plot(σ_MVRfStocks,μ_MVRfStocks, color=:blue, xlim=(0,0.20), ylim=(0,0.05), xlabel=L"\sigma", ylabel=L"\mu", label="MV/Rf Frontier", legend=:topleft)
end

# ╔═╡ f652f235-ea75-4498-b1a7-e2d8d6a9839a
md"""
- We also add the mean-variance frontier of (only) risky assets.
"""

# ╔═╡ dbc7a54a-2010-4ece-9f5c-830ca732bc2e
begin
	p_MVRfStocks2 = p_MVRfStocks
	plot!(p_MVRfStocks2,σ_MVStocks,μ_MVStocks, label="MV Frontier", color=:red)
end

# ╔═╡ 46e17e2e-6b1f-459f-8586-409f84a1e70d
md"""
- Next, we calculate the tangency portfolio.
"""

# ╔═╡ 1f75311f-7311-4b1a-bdf7-b904645f3433
begin
	wTStocks,muTStocks,stdTStocks = MVTangencyP(μ_Stocks,Σ_Stocks,Rf_Stocks)
	df_TStocks = DataFrame(Ticker=StockTicker, PortfolioWeights=wTStocks)
end

# ╔═╡ ed6bd06e-91ca-4904-b419-9224c506ba06
with_terminal() do
		printred("Expected Return Tangency Portfolio:")
		printmat(muTStocks,rowNames="μ")
		printred("Standard Deviation Tangency Portfolio:")
		printmat(stdTStocks,rowNames="σ")
		printred("Portfolio Weights:")
		printmat(wTStocks,colNames="w",rowNames=StockTicker)
	end

# ╔═╡ 7bc6b7af-3734-4bbd-9fd1-918b809ac824
md"""
# Portfolio Optimization using Julia Libraries
"""

# ╔═╡ 368f33fe-3efb-48ed-ab41-67c6f9afe51c
md"""
- We will now consider how we can add investment constraints to a portfolio choice problem (e.g. we cannot invest more than a certain fraction of the portfolio in a single stock).
- In general, as we add constraints  these types of optimizations become more complex and we will learnhow we can use an optimization algorithm in setting up the portfolio choice problem.
- Specifically, we will use the JuMP Package ([Link](https://jump.dev/JuMP.jl/stable/)) and Ipopt ([Link](https://github.com/jump-dev/Ipopt.jl)).
"""

# ╔═╡ a7ba3e82-7035-4eb0-8d0c-dfad466628d9
md"""

Suppose we are considering investing \$1000 in the six stocks "AAPL", "AXP", "CAT", "GS", "MRK", and "WMT" from the previous examples.

We will use the \$1000 initial money to buy shares of the three stocks, hold these for one month, and sell the shares off at the prevailing market prices at the end of the month.

Our goal is to invest in such a way that the expected end-of-month return is at least \$10 or 1%. Furthermore, given the target return we want to minimize the risk “risk” of not achieving our target return.

We make the following assumptions:
1. We can trade any continuum of shares.
2. No short-selling is allowed.
3. There are no transaction costs.

We model this problem by taking decision variables $x_{i}, i=1,2,3,4,6$ denoting
the dollars invested in each of the 6 stocks.

Let us denote by $\tilde{r}_{i}$ the random variable corresponding to the
monthly return (increase in the stock price) per dollar for stock $i$.

Then, the return (or profit) on $x_{i}$ dollars invested in stock $i$ is
$\tilde{r}_{i} x_{i},$ and the total (random) return on our investment is
$\sum_{i=1}^{6} \tilde{r}_{i} x_{i}.$ The expected return on our investment is
then $\mathbb{E}\left[\sum_{i=1}^{6} \tilde{r}_{i} x_{i}\right]=\sum_{i=1}^{6} \overline{r}_{i} x_{i},$
where $\overline{r}_{i}$ is the expected value of the $\tilde{r}_{i}.$

Our goal is to minimize the variance of the return of the investment portfolio. 
This variance is given by:

```math
\operatorname{Var}\left[\sum_{i=1}^{6} \tilde{r}_{i} x_{i}\right] = \sum_{i=1}^{6} \sum_{j=1}^{6} x_{i} x_{j} \sigma_{i j}
```

where $\sigma_{i j}$ is the covariance of the return of stock $i$ with stock $j$.

We can also write this equation as:

```math
\operatorname{Var}\left[\sum_{i=1}^{6} \tilde{r}_{i} x_{i}\right] =x^{T} Q x
```

Where $Q$ is the covariance matrix for the random vector $\tilde{r}$.

Finally, we can write the model as:

```math
\begin{aligned}
\min x^{T} Q x \\
\text { s.t. } \sum_{i=1}^{6} x_{i} \leq 1000.00 \\
\overline{r}^{T} x \geq 10.00 \\
x \geq 0
\end{aligned}
```

let's now use JuMP to solve the portfolio optimization problem.
"""

# ╔═╡ 530de26b-4ef0-4985-b2b5-c173848554bf
md"""
- We import the JuMP and the Ipopt packages.
"""

# ╔═╡ 20e2e63b-a815-48e1-8c9b-fc58c37fc7f4
md"""
- Next, we set up the constrained optimization problem as follows:
"""

# ╔═╡ 3f1f9312-5aa7-4659-a9cd-a6087325db6f
begin
	portfolio = Model(Ipopt.Optimizer)
	set_silent(portfolio)
	@variable(portfolio, x[1:6] >= 0)
	@objective(portfolio, Min, x' * Σ_Stocks * x)
	@constraint(portfolio, sum(x) <= 1000)
	@constraint(portfolio, sum(μ_Stocks[i] * x[i] for i in 1:6) >= 10)
	optimize!(portfolio)
end

# ╔═╡ 032fa9cd-59a0-4a8d-a11f-035185b4a68d
objective_value(portfolio)

# ╔═╡ 2a1d3f42-4a6a-4463-a7e8-1d42faeef67e
md"""
- Let's get the portfolio weights in the optimal solution.
"""

# ╔═╡ cf0f4d47-b2a5-4068-9bb4-8a7d92b220a5
df_JuMP = DataFrame(Ticker=StockTicker, PortfolioWeight=roundmult.(value.(x)/sum(value.(x)),1e-4), 		PortfolioAmount=roundmult.(value.(x),1e-2))

# ╔═╡ 66f01e97-7b13-430c-ac17-9a790d54b9bc
md"""
#  Portfolio Optimization with Constraints

In this problem, we will find the unconstrained portfolio allocation where we 
introduce the weighting parameter $\lambda \;(0 \leq \lambda \leq$ 1) and minimize $\lambda * \text{risk} - (1-\lambda)* \text{expected return}$. By varying the values of $\lambda$, we trace out the efficient frontier.

We will use a convex optimization package [Convex.jl](https://jump.dev/Convex.jl/stable/) and [SCS](https://github.com/jump-dev/SCS.jl) (splitting conic solver) which is a numerical optimization package for solving large-scale convex cone problems.

Suppose that we know the mean returns $\mu \in \mathbf{R}^n$ of each asset and the covariance $\Sigma \in \mathbf{R}^{n \times n}$ between the assets. Our objective is to find a portfolio allocation that minimizes the *risk* (which we measure as the variance $w^T \Sigma w$) and maximizes the *expected return* ($w^T \mu$) of the portfolio of the simulataneously. We require $w \in \mathbf{R}^n$ and $\sum_i w_i = 1$.

This problem can be written as

$$\begin{array}{ll}
    \text{minimize}   & \lambda*w^T \Sigma w - (1-\lambda)*w^T \mu \\
    \text{subject to} & \sum_i w_i = 1
\end{array}$$
 where $w \in \mathbf{R}^n$ is the vector containing weights allocated to each asset.
"""

# ╔═╡ 2f70bd24-6b2f-4dab-895d-3a58759e42bc
md"""
- We import the Convex and the SCS packages.
"""

# ╔═╡ 069eb48b-ef05-45e3-9415-db5e083d59aa
md"""
- Next, we set up the optimization problem and solve it.
"""

# ╔═╡ deab3f39-c1df-40ca-b8be-4f069fd9bd96
begin
	N = 101
	λ_vals = range(0.01, stop=0.99, length=N)

	n = length(StockTicker)
	w = Variable(n)
	ret = dot(w, μ_Stocks)
	risk = quadform(w, Σ_Stocks)

	MeanVarA = zeros(N,2)
	for i in 1:N
		λ = λ_vals[i]

		p = minimize(λ * risk - (1-λ) * ret, sum(w)==1)
		solve!(p, SCS.Optimizer; silent_solver=true)
		MeanVarA[i, :] = [evaluate(ret), evaluate(risk)]
		
	end
end

# ╔═╡ a6de58d1-40a3-445a-8e4d-9629ef44b5ad
md"""
- Next, let's solve with the bounds $0\le w_i \le 1$.
"""

# ╔═╡ 683643a3-fbb9-4971-ad24-38df63a1e8b4
begin
	w_lower = 0
	w_upper = 1

	MeanVarB = zeros(N,2)
	for i in 1:N
		λ = λ_vals[i]
		p = minimize(λ * risk - (1-λ) * ret,
			sum(w) == 1,
			w_lower <= w,
			w <= w_upper)
		
		solve!(p, SCS.Optimizer; silent_solver=true)

		MeanVarB[i,:] = [evaluate(ret) evaluate(risk)]
		
	end
	
end

# ╔═╡ 9a9c7a6c-1ec2-41b7-917a-fc5fb6455302
md"""
- Let's visualize the differences in the unconstrained and the constrained solutions.
"""

# ╔═╡ f6c08155-4fa8-453e-b030-f3fd1a848762
let
	pOpt = plot(
		sqrt.( [ MeanVarA[:,2] MeanVarB[:,2] ] ),
		[MeanVarA[:,1] MeanVarB[:,1]],
		xlim = (0,0.15),
		ylim = (0,0.02),
		title="Mean Variance Frontier",
		xlabel="Standard Deviation",
		ylabel="Expected Return",
		label=["no bounds w" "with 0<w<1"],
		legend = :topleft
		)

		for i=1:length(StockTicker)
			scatter!(pOpt, [sqrt.((Σ_Stocks[i,i]))], [μ_Stocks[i]],
				markershape=:cross, markersize=4, markerstrokewidth=15,
				markercolor=:red, label="")
			annotate!(pOpt, [sqrt.((Σ_Stocks[i,i]))+0.01],[μ_Stocks[i]],
				(StockTicker[i],10))
			
		end

		pOpt
end

# ╔═╡ c68a311d-a336-48ec-80da-4d26556d8041
md"""
We now instead impose a restriction on  $\sum_i |w_i| - 1$, allowing for varying degrees of "leverage".
"""

# ╔═╡ bd01048f-4c5b-4bfb-b0c3-d6eb09aa6cd6
begin
	Lmax = 0.5

	MeanVarC = zeros(N,2)
	for i in 1:N

		λ = λ_vals[i]
		
		p = minimize(
			λ * risk - (1-λ) * ret,
			sum(w) == 1,
			(norm(w,1)-1) <= Lmax)

		solve!(p, SCS.Optimizer; silent_solver=true)

		MeanVarC[i,:] = [evaluate(ret) evaluate(risk)]
		
	end
end

# ╔═╡ 1d9b10c4-f5e8-4992-b7f2-8f10cce049e2
md"""
- Let's add the optimal solution with leverage to the previous graph.
"""

# ╔═╡ 6b8170eb-9f1f-4e65-8681-fc6f7018bfd5
let
	pOpt = plot(
		sqrt.( [ MeanVarA[:,2] MeanVarB[:,2] MeanVarC[:,2]] ),
		[MeanVarA[:,1] MeanVarB[:,1] MeanVarC[:,1]],
		xlim = (0,0.15),
		ylim = (0,0.02),
		title="Mean Variance Frontier",
		xlabel="Standard Deviation",
		ylabel="Expected Return",
		label=["no bounds w" "with 0<w<1" "restriction on sum(|w|)"],
		legend = :topleft
		)

		for i=1:length(StockTicker)
			scatter!(pOpt, [sqrt.((Σ_Stocks[i,i]))], [μ_Stocks[i]],
				markershape=:cross, markersize=4, markerstrokewidth=15,
				markercolor=:red, label="")
			annotate!(pOpt, [sqrt.((Σ_Stocks[i,i]))+0.01],[μ_Stocks[i]],
				(StockTicker[i],10))
			
		end

		pOpt
end

# ╔═╡ 10f0edd8-157e-43e5-a64a-40fdcf1d9dd4
md"""
#  Portfolio Optimization with Target Return

Let's see how we can set up portfolio choice problem with a target expected return and bounds on the portfolio shares of the stocks. Specifically, in this problem, we will find the portfolio allocation that minimizes risk while achieving a given expected return $R_\text{target}$.

Suppose that we know the mean returns $\mu \in \mathbf{R}^n$ and the covariance $\Sigma \in \mathbf{R}^{n \times n}$ of the $n$ assets. We would like to find a portfolio allocation $w \in \mathbf{R}^n$, $\sum_i w_i = 1$, minimizing the *risk* of the portfolio, which we measure as the variance $w^T \Sigma w$ of the portfolio. The requirement that the portfolio allocation achieve the target expected return can be expressed as $w^T \mu >= R_\text{target}$. We suppose further that our portfolio allocation must comply with some lower and upper bounds on the allocation, $w_\text{lower} \leq w \leq w_\text{upper}$.

This problem can be written as

$$\begin{array}{ll}
    \text{minimize}   & w^T \Sigma w \\
    \text{subject to} & w^T \mu >= R_\text{target} \\
                      & \sum_i w_i = 1 \\
                      & w_\text{lower} \leq w \leq w_\text{upper}
\end{array}$$

where $w \in \mathbf{R}^n$ is our optimization variable.
"""

# ╔═╡ 6d6972a5-40c2-46be-9a2d-a12cd0a60e68
md"""
- We set up the optimization problem as follows.
"""

# ╔═╡ 2113af57-bc73-4351-bc87-12ce3e17b7bc
let
	R_target = 0.01
	w_lower = 0
	w_upper = 0.5

	w = Variable(n)

	ret = dot(w, μ_Stocks)
	risk = quadform(w, Σ_Stocks)

	p = minimize(risk,
			ret >= R_target,
			sum(w) == 1,
			w_lower <= w,
			w <= w_upper)

	solve!(p, () -> SCS.Optimizer())

	with_terminal() do
		printred("Portfolio Weights")
		printmat( evaluate(w), colNames="w", rowNames=StockTicker )
		
	end

	
end

# ╔═╡ 00000000-0000-0000-0000-000000000001
PLUTO_PROJECT_TOML_CONTENTS = """
[deps]
CSV = "336ed68f-0bac-5ca0-87d4-7b16caf5d00b"
Chain = "8be319e6-bccf-4806-a6f7-6fae938471bc"
Convex = "f65535da-76fb-5f13-bab9-19810c17039a"
DataFrames = "a93c6f00-e57d-5684-b7b6-d8193f3e46c0"
Dates = "ade2ca70-3891-5945-98fb-dc099432e06a"
Ipopt = "b6b21f68-93f8-5de0-b562-5493be1d77c9"
JuMP = "4076af6c-e467-56ae-b986-b466b2749572"
LaTeXStrings = "b964fa9f-0449-5b57-a5c2-d3ea65f4040f"
LinearAlgebra = "37e2e46d-f89d-539d-b4ee-838fcccc9c8e"
Logging = "56ddb016-857b-54e1-b83d-db4d58db5568"
Plots = "91a5bcdd-55d7-5caf-9e0b-520d859cae80"
PlutoUI = "7f904dfe-b85e-4ff6-b463-dae2292396a8"
Printf = "de0858da-6303-5e67-8744-51eddeeeb8d7"
SCS = "c946c3f1-0d1f-5ce8-9dea-7daa1f7e2d13"
ShiftedArrays = "1277b4bf-5013-50f5-be3d-901d8477a67a"
Statistics = "10745b16-79ce-11e8-11f9-7d13ad32a3b2"

[compat]
CSV = "~0.10.2"
Chain = "~0.4.10"
Convex = "~0.15.1"
DataFrames = "~1.3.2"
Ipopt = "~1.0.2"
JuMP = "~1.0.0"
LaTeXStrings = "~1.3.0"
Plots = "~1.26.0"
PlutoUI = "~0.7.35"
SCS = "~1.1.1"
ShiftedArrays = "~1.0.0"
"""

# ╔═╡ 00000000-0000-0000-0000-000000000002
PLUTO_MANIFEST_TOML_CONTENTS = """
# This file is machine-generated - editing it directly is not advised

[[AMD]]
deps = ["Libdl", "LinearAlgebra", "SparseArrays", "Test"]
git-tree-sha1 = "fc66ffc5cff568936649445f58a55b81eaf9592c"
uuid = "14f7f29c-3bd6-536c-9a0b-7339e30b5a3e"
version = "0.4.0"

[[ASL_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "6252039f98492252f9e47c312c8ffda0e3b9e78d"
uuid = "ae81ac8f-d209-56e5-92de-9978fef736f9"
version = "0.1.3+0"

[[AbstractPlutoDingetjes]]
deps = ["Pkg"]
git-tree-sha1 = "8eaf9f1b4921132a4cff3f36a1d9ba923b14a481"
uuid = "6e696c72-6542-2067-7265-42206c756150"
version = "1.1.4"

[[AbstractTrees]]
git-tree-sha1 = "03e0550477d86222521d254b741d470ba17ea0b5"
uuid = "1520ce14-60c1-5f80-bbc7-55ef81b5835c"
version = "0.3.4"

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

[[BenchmarkTools]]
deps = ["JSON", "Logging", "Printf", "Profile", "Statistics", "UUIDs"]
git-tree-sha1 = "4c10eee4af024676200bc7752e536f858c6b8f93"
uuid = "6e4b80f9-dd63-53aa-95a3-0cdb28fa8baf"
version = "1.3.1"

[[Bzip2_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "19a35467a82e236ff51bc17a3a44b69ef35185a2"
uuid = "6e34b625-4abd-537c-b88f-471c36dfa7a0"
version = "1.0.8+0"

[[CSV]]
deps = ["CodecZlib", "Dates", "FilePathsBase", "InlineStrings", "Mmap", "Parsers", "PooledArrays", "SentinelArrays", "Tables", "Unicode", "WeakRefStrings"]
git-tree-sha1 = "9519274b50500b8029973d241d32cfbf0b127d97"
uuid = "336ed68f-0bac-5ca0-87d4-7b16caf5d00b"
version = "0.10.2"

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

[[CodecBzip2]]
deps = ["Bzip2_jll", "Libdl", "TranscodingStreams"]
git-tree-sha1 = "2e62a725210ce3c3c2e1a3080190e7ca491f18d7"
uuid = "523fee87-0ab8-5b00-afb7-3ecf72e48cfd"
version = "0.7.2"

[[CodecZlib]]
deps = ["TranscodingStreams", "Zlib_jll"]
git-tree-sha1 = "ded953804d019afa9a3f98981d99b33e3db7b6da"
uuid = "944b1d66-785c-5afd-91f1-9de20f533193"
version = "0.7.0"

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

[[CommonSubexpressions]]
deps = ["MacroTools", "Test"]
git-tree-sha1 = "7b8a93dba8af7e3b42fecabf646260105ac373f7"
uuid = "bbf7d656-a473-5ed7-a52c-81e309532950"
version = "0.3.0"

[[Compat]]
deps = ["Base64", "Dates", "DelimitedFiles", "Distributed", "InteractiveUtils", "LibGit2", "Libdl", "LinearAlgebra", "Markdown", "Mmap", "Pkg", "Printf", "REPL", "Random", "SHA", "Serialization", "SharedArrays", "Sockets", "SparseArrays", "Statistics", "Test", "UUIDs", "Unicode"]
git-tree-sha1 = "44c37b4636bc54afac5c574d2d02b625349d6582"
uuid = "34da2185-b29b-5c13-b0c7-acf172513d20"
version = "3.41.0"

[[CompilerSupportLibraries_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "e66e0078-7015-5450-92f7-15fbd957f2ae"

[[Contour]]
deps = ["StaticArrays"]
git-tree-sha1 = "9f02045d934dc030edad45944ea80dbd1f0ebea7"
uuid = "d38c429a-6771-53c6-b99e-75d170b6e991"
version = "0.5.7"

[[Convex]]
deps = ["AbstractTrees", "BenchmarkTools", "LDLFactorizations", "LinearAlgebra", "MathOptInterface", "OrderedCollections", "SparseArrays", "Test"]
git-tree-sha1 = "9573bc2746465b659785bf23b56ba8c423adeb88"
uuid = "f65535da-76fb-5f13-bab9-19810c17039a"
version = "0.15.1"

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

[[DiffResults]]
deps = ["StaticArrays"]
git-tree-sha1 = "c18e98cba888c6c25d1c3b048e4b3380ca956805"
uuid = "163ba53b-c6d8-5494-b064-1a9d43ac40c5"
version = "1.0.3"

[[DiffRules]]
deps = ["IrrationalConstants", "LogExpFunctions", "NaNMath", "Random", "SpecialFunctions"]
git-tree-sha1 = "dd933c4ef7b4c270aacd4eb88fa64c147492acf0"
uuid = "b552c78f-8df3-52c6-915a-8e097449b14b"
version = "1.10.0"

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
git-tree-sha1 = "ae13fcbc7ab8f16b0856729b050ef0c446aa3492"
uuid = "2e619515-83b5-522b-bb60-26c02a35a201"
version = "2.4.4+0"

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

[[ForwardDiff]]
deps = ["CommonSubexpressions", "DiffResults", "DiffRules", "LinearAlgebra", "LogExpFunctions", "NaNMath", "Preferences", "Printf", "Random", "SpecialFunctions", "StaticArrays"]
git-tree-sha1 = "1bd6fc0c344fc0cbee1f42f8d2e7ec8253dda2d2"
uuid = "f6369f11-7733-5829-9624-2563aa707210"
version = "0.10.25"

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
git-tree-sha1 = "a7254c0acd8e62f1ac75ad24d5db43f5f19f3c65"
uuid = "3587e190-3f89-42d0-90ee-14403ec27112"
version = "0.1.2"

[[InvertedIndices]]
git-tree-sha1 = "bee5f1ef5bf65df56bdd2e40447590b272a5471f"
uuid = "41ab1584-1d38-5bbf-9106-f11c6c58b48f"
version = "1.1.0"

[[Ipopt]]
deps = ["Ipopt_jll", "MathOptInterface"]
git-tree-sha1 = "8b7b5fdbc71d8f88171865faa11d1c6669e96e32"
uuid = "b6b21f68-93f8-5de0-b562-5493be1d77c9"
version = "1.0.2"

[[Ipopt_jll]]
deps = ["ASL_jll", "Artifacts", "CompilerSupportLibraries_jll", "JLLWrappers", "Libdl", "MUMPS_seq_jll", "OpenBLAS32_jll", "Pkg"]
git-tree-sha1 = "e3e202237d93f18856b6ff1016166b0f172a49a8"
uuid = "9cc047cb-c261-5740-88fc-0cf96f7bdcc7"
version = "300.1400.400+0"

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

[[JuMP]]
deps = ["Calculus", "DataStructures", "ForwardDiff", "LinearAlgebra", "MathOptInterface", "MutableArithmetics", "NaNMath", "OrderedCollections", "Printf", "SparseArrays", "SpecialFunctions"]
git-tree-sha1 = "936e7ebf6c84f0c0202b83bb22461f4ebc5c9969"
uuid = "4076af6c-e467-56ae-b986-b466b2749572"
version = "1.0.0"

[[LAME_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "f6250b16881adf048549549fba48b1161acdac8c"
uuid = "c1c5ebd0-6772-5130-a774-d5fcae4a789d"
version = "3.100.1+0"

[[LDLFactorizations]]
deps = ["AMD", "LinearAlgebra", "SparseArrays", "Test"]
git-tree-sha1 = "399bbe845e06e1c2d44ebb241f554d45eaf66788"
uuid = "40e66cde-538c-5869-a4ad-c39174c6795b"
version = "0.8.1"

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
git-tree-sha1 = "a6552bfeab40de157a297d84e03ade4b8177677f"
uuid = "23fbe1c1-3f47-55db-b15f-69d7ec21a316"
version = "0.15.12"

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
git-tree-sha1 = "e5718a00af0ab9756305a0392832c8952c7426c1"
uuid = "2ab3a3ac-af41-5b50-aa03-7779005ae688"
version = "0.3.6"

[[Logging]]
uuid = "56ddb016-857b-54e1-b83d-db4d58db5568"

[[METIS_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "1d31872bb9c5e7ec1f618e8c4a56c8b0d9bddc7e"
uuid = "d00139f3-1899-568f-a2f0-47f597d42d70"
version = "5.1.1+0"

[[MUMPS_seq_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "JLLWrappers", "Libdl", "METIS_jll", "OpenBLAS32_jll", "Pkg"]
git-tree-sha1 = "29de2841fa5aefe615dea179fcde48bb87b58f57"
uuid = "d7ed1dd3-d0ae-5e8e-bfb4-87a502085b8d"
version = "5.4.1+0"

[[MacroTools]]
deps = ["Markdown", "Random"]
git-tree-sha1 = "3d3e902b31198a27340d0bf00d6ac452866021cf"
uuid = "1914dd2f-81c6-5fcd-8719-6d5c9610ff09"
version = "0.5.9"

[[Markdown]]
deps = ["Base64"]
uuid = "d6f4376e-aef5-505a-96c1-9c027394607a"

[[MathOptInterface]]
deps = ["BenchmarkTools", "CodecBzip2", "CodecZlib", "JSON", "LinearAlgebra", "MutableArithmetics", "OrderedCollections", "Printf", "SparseArrays", "Test", "Unicode"]
git-tree-sha1 = "779ad2ee78c4a24383887fdba177e9e5034ce207"
uuid = "b8f27783-ece8-5eb3-8dc8-9495eed66fee"
version = "1.1.2"

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

[[MozillaCACerts_jll]]
uuid = "14a3606d-f60d-562e-9121-12d972cd8159"

[[MutableArithmetics]]
deps = ["LinearAlgebra", "SparseArrays", "Test"]
git-tree-sha1 = "ba8c0f8732a24facba709388c74ba99dcbfdda1e"
uuid = "d8a4904e-b15c-11e9-3269-09a3773c0cb0"
version = "1.0.0"

[[NaNMath]]
git-tree-sha1 = "b086b7ea07f8e38cf122f5016af580881ac914fe"
uuid = "77ba4419-2d1f-58cd-9bb1-8ffee604a2e3"
version = "0.3.7"

[[NetworkOptions]]
uuid = "ca575930-c2e3-43a9-ace4-1e988b2c1908"

[[Ogg_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "887579a3eb005446d514ab7aeac5d1d027658b8f"
uuid = "e7412a2a-1a6e-54c0-be00-318e2571c051"
version = "1.3.5+1"

[[OpenBLAS32_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "ba4a8f683303c9082e84afba96f25af3c7fb2436"
uuid = "656ef2d0-ae68-5445-9ca0-591084a874a2"
version = "0.3.12+1"

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

[[Parsers]]
deps = ["Dates"]
git-tree-sha1 = "13468f237353112a01b2d6b32f3d0f80219944aa"
uuid = "69de0a69-1ddd-5017-9359-2bf0b02dc9f0"
version = "2.2.2"

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
deps = ["Base64", "Contour", "Dates", "Downloads", "FFMPEG", "FixedPointNumbers", "GR", "GeometryBasics", "JSON", "Latexify", "LinearAlgebra", "Measures", "NaNMath", "PlotThemes", "PlotUtils", "Printf", "REPL", "Random", "RecipesBase", "RecipesPipeline", "Reexport", "Requires", "Scratch", "Showoff", "SparseArrays", "Statistics", "StatsBase", "UUIDs", "UnicodeFun", "Unzip"]
git-tree-sha1 = "23d109aad5d225e945c813c6ebef79104beda955"
uuid = "91a5bcdd-55d7-5caf-9e0b-520d859cae80"
version = "1.26.0"

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

[[Preferences]]
deps = ["TOML"]
git-tree-sha1 = "de893592a221142f3db370f48290e3a2ef39998f"
uuid = "21216c6a-2e73-6563-6e65-726566657250"
version = "1.2.4"

[[PrettyTables]]
deps = ["Crayons", "Formatting", "Markdown", "Reexport", "Tables"]
git-tree-sha1 = "dfb54c4e414caa595a1f2ed759b160f5a3ddcba5"
uuid = "08abe8d2-0d0c-5749-adfa-8a2ac140af0d"
version = "1.3.1"

[[Printf]]
deps = ["Unicode"]
uuid = "de0858da-6303-5e67-8744-51eddeeeb8d7"

[[Profile]]
deps = ["Printf"]
uuid = "9abbd945-dff8-562f-b5e8-e1ebf5ef1b79"

[[Qt5Base_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "Fontconfig_jll", "Glib_jll", "JLLWrappers", "Libdl", "Libglvnd_jll", "OpenSSL_jll", "Pkg", "Xorg_libXext_jll", "Xorg_libxcb_jll", "Xorg_xcb_util_image_jll", "Xorg_xcb_util_keysyms_jll", "Xorg_xcb_util_renderutil_jll", "Xorg_xcb_util_wm_jll", "Zlib_jll", "xkbcommon_jll"]
git-tree-sha1 = "ad368663a5e20dbb8d6dc2fddeefe4dae0781ae8"
uuid = "ea2cea3b-5b76-57ae-a6ef-0a8af62496e1"
version = "5.15.3+0"

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

[[SCS]]
deps = ["MathOptInterface", "Requires", "SCS_GPU_jll", "SCS_jll", "SparseArrays"]
git-tree-sha1 = "65ecfd7602cdb2aa617fb6034a75fa7ac7d318dc"
uuid = "c946c3f1-0d1f-5ce8-9dea-7daa1f7e2d13"
version = "1.1.1"

[[SCS_GPU_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "OpenBLAS32_jll", "Pkg"]
git-tree-sha1 = "f912271ecccb00acaddfab2943e9b33d5ec36d3b"
uuid = "af6e375f-46ec-5fa0-b791-491b0dfa44a4"
version = "3.2.0+0"

[[SCS_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "OpenBLAS32_jll", "Pkg"]
git-tree-sha1 = "ba5c0d3b23220d3598d2877b4cf913e3fcf8add3"
uuid = "f4f2fc5b-1d94-523c-97ea-2ab488bedf4b"
version = "3.2.0+0"

[[SHA]]
uuid = "ea8e919c-243c-51af-8825-aaa63cd721ce"

[[Scratch]]
deps = ["Dates"]
git-tree-sha1 = "0b4b7f1393cff97c33891da2a0bf69c6ed241fda"
uuid = "6c6a2e73-6563-6170-7368-637461726353"
version = "1.1.0"

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

[[WeakRefStrings]]
deps = ["DataAPI", "InlineStrings", "Parsers"]
git-tree-sha1 = "c69f9da3ff2f4f02e811c3323c22e5dfcb584cfa"
uuid = "ea10d353-3f73-51f8-a26c-33c1cb351aa5"
version = "1.4.1"

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
# ╠═3d731294-71d0-4b34-85e2-96d29bd8a7ca
# ╟─c43df4a3-a1d8-433e-9a1c-f7c0984be879
# ╟─5141ad80-2374-11ec-2455-c7ff63842559
# ╟─5fc44a1a-2c5c-4cdf-b2da-4cbc17c8f8a3
# ╟─72c219a7-1c04-4678-988c-8f17674d9d75
# ╠═652dc6ae-6f9d-4ebd-9251-00821a8ee09d
# ╟─d558b927-2cea-45ef-b50b-0f53640e49d4
# ╟─fae8140d-9eea-432b-a858-ec12d3728ef2
# ╠═3fca5d0e-c342-4520-867c-917116495ac2
# ╟─b16ecff8-2dc1-4827-b33c-5eb0af4a6961
# ╠═95a13699-95b4-4f7b-baaa-80627f33841f
# ╟─63c6b02c-4674-45b4-a96a-2545959d9d7c
# ╠═c40dc723-1169-4826-941d-ba162d2bf7b8
# ╠═0e91775f-b061-445c-adcd-d453f98c594f
# ╟─ef313c73-14bd-42d9-a1d4-c35709dec26c
# ╟─69f5983a-5e47-4331-9518-6c8c30340b24
# ╠═212685c3-9049-42e9-b535-6e515f19153b
# ╟─77a08244-548c-4a7a-be3e-cdc8c6e29622
# ╠═f3f52640-d079-4d6c-8c2a-63f89c53b061
# ╟─fa8d6069-e8da-4bba-8475-d20c5c6dc090
# ╟─67c26862-f6b3-48cc-a884-36adb554c378
# ╟─33fe5282-db49-4732-9c80-cc1537c120c5
# ╠═ecf84238-b03d-4036-8fd3-88bcb394c925
# ╠═92a880c2-91c3-4db4-bcb4-a376572396c9
# ╟─2e082d7a-7fec-44e9-8697-78a1328e7d3d
# ╠═e1ac8d53-000e-44d7-8090-334468a6d2ad
# ╟─aa126d99-3fba-49bc-91c0-bde7cdfeb3bb
# ╠═a0782ccf-5fb4-4ca3-9e3e-c8da69909847
# ╟─74e6c915-5dca-4c84-a890-0b829ba6cd92
# ╟─5be9cfce-89e0-4ad7-9aef-dd3252829c18
# ╠═fe44f447-72db-4c05-ba03-6c26e1573bd6
# ╠═e45ff2ab-eb7d-40af-887c-b9f56cbcf6a6
# ╟─3652b806-8991-47d4-ad82-e8affde497f6
# ╠═fc304ff1-e4ff-4047-a8d3-353816202724
# ╟─0b30896b-d6da-4391-a2dc-e9e0bcbe2fa2
# ╠═83a1fb1d-221b-4f26-8e65-e319ea9b899d
# ╟─3f73b5cf-fe07-4bd5-8306-e319024f4bb9
# ╟─dff92924-af65-444c-9ac6-62c0449f7c41
# ╟─e0bffa07-2db9-4ec1-a116-474d71050df6
# ╠═296b85d2-2dac-4cf4-aa86-b33f0ca13650
# ╟─322112e3-3aee-4e70-8203-6a1227a448de
# ╠═624bfb0a-3336-448d-982a-511f9743202c
# ╟─e2e22821-c6c1-4e98-a5f4-725595c3f250
# ╠═b3ac63b0-aebd-4fff-9e98-82d0f4bbb8dc
# ╟─d751ef72-7ab1-45f0-8375-7831175f075f
# ╠═7465eeed-9af0-4504-9154-be22c8160a4b
# ╟─f07348bf-2ec1-4a83-9f1d-6af7787bd42d
# ╠═a53c4108-7ee7-4547-afee-dfaacf87d944
# ╠═3d053364-5a63-41b1-b654-9d99e00cc6c3
# ╟─db15e7a8-6912-49ae-81b4-c22abc40aac5
# ╠═5deea522-f60b-4242-8c9f-26ee7c243677
# ╟─b26bf12b-f377-4228-9ac6-9af67eaf6880
# ╠═3f2eb8f8-b30f-487d-a915-bcfa6ebd0954
# ╟─b4a689de-eb5a-42a0-b7f1-274bb81884f4
# ╠═201bdaf1-bcb4-4e9e-8b38-fba457527ba6
# ╠═1af038cd-1f76-4473-9abb-ca25efd5ef5b
# ╟─b10c5e71-2cd7-46ff-9a49-c749312becc9
# ╠═9583fcde-ddc0-4463-b9e1-7c8f69921f17
# ╟─63e089e7-19da-4b22-b4f8-93c7dcf3fffd
# ╠═41b4f432-2218-427b-b9b0-c882e3e4c0f0
# ╟─0d2c744c-cfbc-4ee7-b50f-56fa5f2faef6
# ╠═68c13c22-274b-4e9b-ba45-6cd7054b1642
# ╟─66b6a673-8a3f-46e6-ad02-43451c6b75f2
# ╠═28059fe1-7051-4846-83c4-4dc6a3a0217c
# ╟─f652f235-ea75-4498-b1a7-e2d8d6a9839a
# ╠═dbc7a54a-2010-4ece-9f5c-830ca732bc2e
# ╟─46e17e2e-6b1f-459f-8586-409f84a1e70d
# ╠═1f75311f-7311-4b1a-bdf7-b904645f3433
# ╠═ed6bd06e-91ca-4904-b419-9224c506ba06
# ╟─7bc6b7af-3734-4bbd-9fd1-918b809ac824
# ╟─368f33fe-3efb-48ed-ab41-67c6f9afe51c
# ╟─a7ba3e82-7035-4eb0-8d0c-dfad466628d9
# ╟─530de26b-4ef0-4985-b2b5-c173848554bf
# ╠═49fdad82-e563-4e4c-88a1-33166e9ddee4
# ╟─20e2e63b-a815-48e1-8c9b-fc58c37fc7f4
# ╠═3f1f9312-5aa7-4659-a9cd-a6087325db6f
# ╠═032fa9cd-59a0-4a8d-a11f-035185b4a68d
# ╟─2a1d3f42-4a6a-4463-a7e8-1d42faeef67e
# ╠═cf0f4d47-b2a5-4068-9bb4-8a7d92b220a5
# ╟─66f01e97-7b13-430c-ac17-9a790d54b9bc
# ╟─2f70bd24-6b2f-4dab-895d-3a58759e42bc
# ╠═81404cf5-bd49-4dda-8471-fc1c8417199b
# ╟─069eb48b-ef05-45e3-9415-db5e083d59aa
# ╠═deab3f39-c1df-40ca-b8be-4f069fd9bd96
# ╟─a6de58d1-40a3-445a-8e4d-9629ef44b5ad
# ╠═683643a3-fbb9-4971-ad24-38df63a1e8b4
# ╟─9a9c7a6c-1ec2-41b7-917a-fc5fb6455302
# ╠═f6c08155-4fa8-453e-b030-f3fd1a848762
# ╟─c68a311d-a336-48ec-80da-4d26556d8041
# ╠═bd01048f-4c5b-4bfb-b0c3-d6eb09aa6cd6
# ╟─1d9b10c4-f5e8-4992-b7f2-8f10cce049e2
# ╠═6b8170eb-9f1f-4e65-8681-fc6f7018bfd5
# ╟─10f0edd8-157e-43e5-a64a-40fdcf1d9dd4
# ╟─6d6972a5-40c2-46be-9a2d-a12cd0a60e68
# ╠═2113af57-bc73-4351-bc87-12ce3e17b7bc
# ╟─00000000-0000-0000-0000-000000000001
# ╟─00000000-0000-0000-0000-000000000002
