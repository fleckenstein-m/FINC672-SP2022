### A Pluto.jl notebook ###
# v0.19.0

using Markdown
using InteractiveUtils

# ╔═╡ 8b90477d-5887-49bd-bcf3-d2d7276885af
begin
using CSV, DataFrames, Dates, Plots, LaTeXStrings, LinearAlgebra, LinearRegressionKit, PlutoUI, Printf,  StatsBase, Statistics, StatsModels
	
	gr(size=(480,320))
end

# ╔═╡ 3820a86d-fefa-43ca-a308-40d9ff2b3599
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

# ╔═╡ 653d8f23-3a60-4778-897d-c092106ce20d
using Convex, SCS

# ╔═╡ 72cce814-aa8f-4ac8-98c1-f556ce43ed6d
TableOfContents( indent=true, depth=1, aside=true)

# ╔═╡ 960b2ed0-2394-11ec-17c0-5bdaeef72b5d
md"""
# FINC 672: Performance Evaluation

- In this notebook, we summarize conventional performance measures for an investment fund. 
- Then, we perform a "style analysis" to find out how a fund has changed its portfolio weights over time.

"""

# ╔═╡ e7029f0e-832a-4b62-a4f4-09002dbd7159
md"""
We are going to use the following dataset in the performance analysis.
1. S&P 500
2. S&P MidCap 400
3. S&P Small Cap 600
4. World Developed - Ex. U.S.
5. Emerging Markets
6. US Corporate Bonds
7. U.S. Treasury Bills	
8. US Treasury	Bonds/Notes
9. Putnam Asset Allocation: Growth A
10. Vanguard Wellington Mutual Fund

This dataset is provided to you in the file `lecture_14_PerfEval.csv` (available on Canvas).
"""

# ╔═╡ ba9c36aa-e791-4411-85f6-5c230d19b017
md"""
## The Idea behind Performance Evaluation

- Traditional performance analysis tries to answer the following question: “should we include an asset in our portfolio, assuming that future returns will have the same distribution as in a historical sample.” 

- Since returns are random variables (although with different means, variances, etc.) and investors are risk averse, this means that performance analysis will typically not rank the fund with the highest return (in a historical sample) first.

- Although that high return certainly was good for the old investors, it is more interesting to understand what kind of distribution of future returns this investment strategy might entail. In short, the high return will be compared with the risk of the strategy.

- Most performance measures are based on mean-variance (MV) analysis, but the full MV portfolio choice problem is not solved. Instead, the performance measures can be seen as different approximations of the MV problem, where the issue is whether we should invest in fund `p` or in fund `q`. (We don’t allow a mix of them.) 

- There are several popular performance measures, corresponding to different situations: is this an investment of your entire wealth, or just a small increment? 
  - However, all these measures are (increasing) functions of Jensen’s alpha, the intercept in the CAPM regression.


"""

# ╔═╡ 833249db-8526-4fa1-b136-c48850d7e37f
md"""
## Data
"""

# ╔═╡ c67dda3c-8044-436e-b668-e5c7c8feddaf
PerfDat = CSV.File("lecture_14_PerfEval.csv") |> DataFrame
	


# ╔═╡ 4efcb4b6-9000-4db6-b951-37c9c9c9676d
md"""
- Let's get some information on the dataset.
"""

# ╔═╡ 3d51f5f0-752d-407e-80e3-7e44849dd76b
describe(PerfDat, :eltype, :mean, :std, :min, :median, :max, (x->length(collect(skipmissing(x)))) =>:nobs, :nmissing)

# ╔═╡ a2946345-b457-4e02-a3eb-7646a7083058
md"""
- Let's take a look at the first six rows.
"""

# ╔═╡ c02ae1b0-ba3f-4c94-83d8-425af6cf8ed9
first(PerfDat, 6)

# ╔═╡ c8c2e3c2-d2dc-448e-9d32-8ba29697a1f3
md"""
- Let's take a look at the last six rows.
"""

# ╔═╡ 0580541d-29f0-4731-9ea9-f36494f70516
last(PerfDat,6)

# ╔═╡ 561f5088-7a2a-47f4-bfbf-6977ae6a27a5
md"""
- Let's work with matrices going forward.
"""

# ╔═╡ fb093381-1109-450b-898e-1a0f87bd54d4
begin
	x=Matrix(PerfDat)
	
	IndNames = names(PerfDat)[2:9]
	FundNames = names(PerfDat)[10:11]

	dN             = PerfDat[:,1]
	(Rb,RFunds,Rf) = (convert.(Float64,x[:,2:9]),convert.(Float64,x[:,10:11]),
					  convert.(Float64,x[:,12]));     #convert to Float64
	display("")
end

# ╔═╡ 5595417a-6333-4acd-b14b-27865d4f8736
md"""
# Sharpe Ratio and M2
"""

# ╔═╡ a9e20df4-96aa-4798-a5ef-204671de44af
md"""
- Suppose we want to know if fund `p` is better than fund `q` to place all our savings in. (We don’t allow a mix of them.) 
- The answer is that `p` is better if it has a higher Sharpe ratio—defined as

$$SR_p = \mu_{p}^{e}/\sigma_p$$

 - where $\mu_{p}^{e}$ is the excess return of fund `p` and $\sigma_p$ is fund `p`'s standard deviation of returns.

"""

# ╔═╡ 2732613f-2546-4fb4-93ee-1805a43e7521
md"""

- The reason is that MV behaviour (MV preferences or normally distributed returns) implies that we should maximize the Sharpe ratio (selecting the tangency portfolio). 

- Intuitively, for a given volatility, we then get the highest expected return.

- A version of the Sharpe ratio, called $M^2$ is

$$M_p^2 = \mu_{p^{\star}}^e - \mu_{m}^e $$

where $\mu_{p^{\star}}^e$ is the expected return on a mix of portfolio $p$ and the riskfree asset such that the volatility is the same as for the market return.

$$R_{p^{\star}} = a R_p + (1-a) R_f$$

with $a=\sigma_m/\sigma_p.$

- This gives the mean and standard deviation of portfolio $p^{\star}$

$$\mu_{p^{\star}}^e = a \mu_{p}^e = \mu_{p}^e \sigma_m \ \sigma_p$$

$$\sigma_{p^{\star}} = a \sigma_p= \sigma_m$$

- Thus, $R_p$ indeed has the same volatility as the market.

"""

# ╔═╡ 9280b21d-d44e-49e5-a0c0-f31dbf90f51a
md""" 
-  $M^2$ has the advantage of being easily interpreted because it is just a comparison of two returns. It shows how much better (or worse) this asset is compared to the capital market line (which is the location of efficient portfolios provided the market is MV efficient). """

# ╔═╡ 59e2e16a-d5ee-4fb0-ae02-94c05b07b306
md"""
Let's now calculate $SR$ and $M^2$ empirically using our data.
"""

# ╔═╡ 8ee5dcd0-0ae6-4d7e-9270-38f36c9260d4
begin
	Re  = RFunds .- Rf           #excess returns of the funds
	Rme = Rb[:,1] - Rf           #excess returns of the market (S&P 500)

	μᵉp = mean(Re,dims=1)        #average excess returns of funds, 1xn
	σp  = std(Re,dims=1)         #std, 1xn

	μᵉm = mean(Rme)              #average excess returns of market
	σm  = std(Rme)

	SRp = (μᵉp./σp)*sqrt(52)       #Sharpe ratio
	SRm = (μᵉm/σm)*sqrt(52)

	M2p = (SRp.-SRm)*σm*sqrt(52)*100
	M2m = 0                      #(SRm.-SRm)*σm=0

	xut = hcat([μᵉm;μᵉp']*52*100,[SRm;SRp'],[M2m;M2p'])
	
	with_terminal() do
	 	printmat(xut,colNames=["ERe","SR","M2"],rowNames=["Market";FundNames])
	end
end

# ╔═╡ 658f89c1-5452-491d-bed4-e0b189c7f457
md"""
# Appraisal Ratio
"""

# ╔═╡ 9e7b7ef3-856d-4fca-907b-845666463e42
md"""
- If the question is “should we add fund `p` or fund `q` to our holding of the market portfolio?,” then the appraisal ratio can be used to provide an answer. 

- The appraisal ratio of fund `p` is

$$AR_p = \alpha_p / Std(\epsilon_{pt})$$

where $\alpha_p$ is the intercept and $Std(\epsilon_{pt})$ is the volatility of the residual of a CAPM regression. (The residual is often called the tracking error.) 

- A higher appraisal ratio is better. The intuition is as follows. 
 - If you think of $b_p R_{mt}^e$, as the benchmark return, then $AR_p$ is the average extra return per unit of extra volatility (standard deviation). 
 - For instance, a ratio of 1.7 could be interpreted as a 1.7 USD profit per each dollar at risk.

"""

# ╔═╡ ba46a9a7-b469-401f-a3b4-10ab4a6f47cb
md"""
Let's calculate the Appraisal Ratios in our data.
"""

# ╔═╡ 6024747f-e21a-40d7-8cd1-1681ca12718e
begin
	T_app = size(Re,1)
	x_app = [ones(T_app) Rme]

	b_app  = x_app\Re        	#2xn, OLS Re = α + β*Rme + ϵ, n = number of funds
	ϵ_app  = Re - x_app*b_app   #Txn, residuals
	σϵ_app = std(ϵ_app,dims=1)   #1xn

	ARp = (b_app[1:1,:]*52*100)./(σϵ_app*sqrt(52)*100)   #b[1:1,:] is 1xn
	ARm = 0

	xut_app = [ARm;ARp']
	
	with_terminal() do
		printmat(xut_app,colNames=["AR"],rowNames=["Market";FundNames])
	end
end

# ╔═╡ 1424b025-89be-4adc-9c0f-38abca54bb2a
md"""
# Treynor's Ratio and T2
"""

# ╔═╡ c42c62c4-db4d-44c3-81a6-0573f8d03258
md"""
- Suppose instead that the issue is whether we should add a small amount of fund `p` or fund `q` to an already well diversified portfolio (not the market portfolio). 
- In this case, Treynor’s ratio might be useful 

$$TR_p = \mu_{p}^e/\beta_p$$

- A higher Treynor’s ratio is better.

- The TR measure can be rephrased in terms of expected returns---and could then be called the $T^2$ measure.  
 - Mix `p` and `q` with the riskfree rate to get the same $\beta$ for both portfolios (here 1 to make it comparable with the market), the one with the highest Treynor’s ratio has the highest expected return ($T^2$ measure).

- The $T^2$ measure is defined as

$$T_p^2=\mu_{p^{\star}}^e - \mu_m^e = \mu_p^e/\beta_p - \mu_m^e.$$

"""

# ╔═╡ 6fd86104-021a-4ec7-acdb-4ac4c771f03e
md"""
Let's now calculate Treynor's Ratio and $T^2$ in the data.
"""

# ╔═╡ 40109f1e-f391-485e-80d1-5b60bd1ede63
begin
	T_tr = size(Re,1)
	x_tr = [ones(T_tr) Rme]

	b_tr  = x_tr\Re        		#2xn, OLS Re = α + β*Rme + ϵ, n = number of funds
	ϵ_tr  = Re - x_tr*b_tr   #Txn, residuals
	σϵ_tr = std(ϵ_tr,dims=1)   #1xn
	
	TRp = 52*100*μᵉp./b_tr[2:2,:]
	TRm = 52*100*μᵉm/1           #market has β=1

	T2p = TRp .- μᵉm*52*100
	T2m = 0                      #TRm - μᵉm*52*100

	with_terminal() do
		xut = hcat([TRm;TRp'],[T2m;T2p'])
		printmat(xut,colNames=["TR","T2"],rowNames=["Market";FundNames])
	end
end

# ╔═╡ 0f0a5bcf-f0ca-4cd8-9722-ce8282780a3a
md"""
# Style Analysis
"""

# ╔═╡ 5f3ab6a9-d093-4fcc-9370-79bc76a94b0c
md"""
- Style analysis is a way to use econometric tools to find out the portfolio composition from a series of the returns, at least in broad terms.
- The basic idea is to identify a number (5 to 10 perhaps) return indices that are expected to account for the brunt of the portfolio's returns, and then run a regression to find the portfolio “weights.” 
- It is essentially a multi-factor regression without any intercept and where the coefficients are constrained to sum to unity and to be positive.

  - The regression is $R^{e}_{pt} = b_1 X_1 + \ldots + b_K X_K + \epsilon_{pt}$, where $b_j \geq 0$ for all $j$, and $\sum_{i=1}^K b_i = 1$.
  - The coefficients are typically estimated by minimizing the sum of squared residuals. This is a nonlinear estimation problem, but there are very efficient methods for it (since it is a quadratic problem).
  - We will use the optimization packages [Convex.jl](https://jump.dev/Convex.jl/stable/) for the interface and [SCS.jl](https://github.com/jump-dev/SCS.jl) for the optimization algorithm.

- A pseudo-$R^2$ (the squared correlation of the fitted and actual values) is sometimes used to gauge how well the regression captures the returns of the portfolio. 
- The residuals can be thought of as the effect of stock selection, or possibly changing portfolio weights more generally. 
  - One way to get a handle of the latter is to run the regression on a moving data sample. 
  - The time-varying weights are often compared with the returns on the indices to see if the weights were moved in the right direction.

"""

# ╔═╡ d86594f9-b563-42d9-812c-7dc663b9b8c3
md"""
- Note on the implementation. 
- The regression is 
$$Y = b_1 X_1 + \ldots + b_K X_K + u,$$ where $b_j \geq 0$ for all $j$, and $\sum_{i=1}^K b_i = 1$.
- We write the sum of squared residuals as
$$(Y- X\,b)' (Y - X\,b) = YY' - 2Y'Xb + b'X'Xb.$$
  - Only the two last terms matter for the choice of $b$.
"""

# ╔═╡ 4a60f8df-a871-4f96-9c1c-499a36310985
md"""
- Let's write a function to implement the style analysis regression.
"""

# ╔═╡ 28c593e8-0ce6-4db6-a8bb-f64ec8307391
begin

md"""
StyleAnalysisPs(Y,X)


# Input:
- `Y::Vector`:          T-vector, returns of a fund
- `X ::Matrix`:         TxK matrix, returns on m benchmarks

 # Output:
- `b_sa::Vector`:       K-vector, restricted regression coefficients
- `b_ls::Vector`:       K-vector, OLS regression coefficients

"""
function StyleAnalysisPs(Y,X)

    K = size(X,2)
    b_ls = X\Y                          #LS estimate of weights, no restrictions

    b    = Variable(K)
    Q    = X'X
    L1   = quadform(b,Q)              #b'X'X*b
    c    = X'Y
    L2   = dot(c,b)                   #same as Y'X*b 
    
    c1   = sum(b) == 1                #sum of coeffs = 1
    c2   = 0.0 <= b                   #0 <= coeffs <= 1
    c3   = b <= 1.0
    sc   = length(Q)/sum(abs,Q)       #scale up L1 and L2 by this (better precision)  
   
    Sol  = minimize(sc*L1-2*sc*L2,c1,c2,c3)
    solve!(Sol,SCS.Optimizer;silent_solver = true)
    if Sol.status == Convex.MOI.OPTIMAL
        b_sa = vec(evaluate(b))
    else
        b_sa = NaN
    end

    return b_sa, b_ls

end
	
display("")
end

# ╔═╡ 49a66213-b8b7-4789-b967-7bdfb859de9d
md"""
- Next, we run the style regression.
- The next cell makes a "style analysis regression" based on the entire sample. 
- The dependent variable is the first mutual fund (_Putnam Asset Allocation: Growth A_) and the regressors include all indices.
"""

# ╔═╡ b22fb358-658a-44be-a99b-b5301c2dcadf
let
	
	(b,b_ls) = StyleAnalysisPs(RFunds[:,1],Rb)
	
	printblue("OLS and style analysis coeffs:")
	colNames = ["OLS" "Restricted LS"]
	xut      = [b_ls b;sum([b_ls b],dims=1)]

	with_terminal() do
		printmat(xut,colNames=colNames,rowNames=[IndNames;"Sum"],width=15)
		
		printred("Notice that the restricted LS has (approximately) no negative coeffs and that sum(coeffs) = 1")
	end
	
end

# ╔═╡ e79edd61-e5ea-4da2-a28a-483d13e6da71
md"""
- We also run the style analysis on a moving window and then we plot how coefficients change over time.
"""

# ╔═╡ 925fd389-4d7d-4fdd-b84c-c81c7f6408e4
let

	(T,K)   = size(Rb)
	
	WinSize = 104

	b = fill(NaN,(T,K))
	
	for t = (WinSize+1):T
	    vv     = (t-WinSize):t    #moving data window
	    b[t,:] = StyleAnalysisPs(RFunds[vv,1],Rb[vv,:])[1]
	end

	xTicksLoc = [Date(2000);Date(2005);Date(2010);Date(2015)]
	xTicksLab = Dates.format.(xTicksLoc,"Y")

	p1 = plot( dN,b,
           #layout = (3,3),
           layout = @layout[a a a;a a a;a a _],   #_ to get blank subplot
           legend = false,
           size = (800,600),
           linecolor = :blue,
           xticks = (xTicksLoc,xTicksLab),
           ylims = (0,0.6),
           title = reshape(string.(IndNames),1,:),
           titlefont = font(10) )

end

# ╔═╡ 00000000-0000-0000-0000-000000000001
PLUTO_PROJECT_TOML_CONTENTS = """
[deps]
CSV = "336ed68f-0bac-5ca0-87d4-7b16caf5d00b"
Convex = "f65535da-76fb-5f13-bab9-19810c17039a"
DataFrames = "a93c6f00-e57d-5684-b7b6-d8193f3e46c0"
Dates = "ade2ca70-3891-5945-98fb-dc099432e06a"
LaTeXStrings = "b964fa9f-0449-5b57-a5c2-d3ea65f4040f"
LinearAlgebra = "37e2e46d-f89d-539d-b4ee-838fcccc9c8e"
LinearRegressionKit = "e91d531d-6e51-44a8-96b7-a10d5d51daa3"
Logging = "56ddb016-857b-54e1-b83d-db4d58db5568"
Plots = "91a5bcdd-55d7-5caf-9e0b-520d859cae80"
PlutoUI = "7f904dfe-b85e-4ff6-b463-dae2292396a8"
Printf = "de0858da-6303-5e67-8744-51eddeeeb8d7"
SCS = "c946c3f1-0d1f-5ce8-9dea-7daa1f7e2d13"
Statistics = "10745b16-79ce-11e8-11f9-7d13ad32a3b2"
StatsBase = "2913bbd2-ae8a-5f71-8c99-4fb6c76f3a91"
StatsModels = "3eaba693-59b7-5ba5-a881-562e759f1c8d"

[compat]
CSV = "~0.10.3"
Convex = "~0.15.0"
DataFrames = "~1.2.2"
LaTeXStrings = "~1.3.0"
LinearRegressionKit = "~0.7.4"
Plots = "~1.27.0"
PlutoUI = "~0.7.37"
SCS = "~1.1.0"
StatsBase = "~0.33.16"
StatsModels = "~0.6.29"
"""

# ╔═╡ 00000000-0000-0000-0000-000000000002
PLUTO_MANIFEST_TOML_CONTENTS = """
# This file is machine-generated - editing it directly is not advised

[[AMD]]
deps = ["Libdl", "LinearAlgebra", "SparseArrays", "Test"]
git-tree-sha1 = "fc66ffc5cff568936649445f58a55b81eaf9592c"
uuid = "14f7f29c-3bd6-536c-9a0b-7339e30b5a3e"
version = "0.4.0"

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
git-tree-sha1 = "9310d9495c1eb2e4fa1955dd478660e2ecab1fbb"
uuid = "336ed68f-0bac-5ca0-87d4-7b16caf5d00b"
version = "0.10.3"

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

[[Combinatorics]]
git-tree-sha1 = "08c8b6831dc00bfea825826be0bc8336fc369860"
uuid = "861a8166-3701-5b0c-9a16-15d98fcdc6aa"
version = "1.0.2"

[[CommonSolve]]
git-tree-sha1 = "68a0743f578349ada8bc911a5cbd5a2ef6ed6d1f"
uuid = "38540f10-b2f7-11e9-35d8-d573e4eb0ff2"
version = "0.2.0"

[[Compat]]
deps = ["Base64", "Dates", "DelimitedFiles", "Distributed", "InteractiveUtils", "LibGit2", "Libdl", "LinearAlgebra", "Markdown", "Mmap", "Pkg", "Printf", "REPL", "Random", "SHA", "Serialization", "SharedArrays", "Sockets", "SparseArrays", "Statistics", "Test", "UUIDs", "Unicode"]
git-tree-sha1 = "96b0bc6c52df76506efc8a441c6cf1adcb1babc4"
uuid = "34da2185-b29b-5c13-b0c7-acf172513d20"
version = "3.42.0"

[[CompilerSupportLibraries_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "e66e0078-7015-5450-92f7-15fbd957f2ae"

[[ConstructionBase]]
deps = ["LinearAlgebra"]
git-tree-sha1 = "f74e9d5388b8620b4cee35d4c5a618dd4dc547f4"
uuid = "187b0558-2788-49d3-abe0-74a17ed4e7c9"
version = "1.3.0"

[[Contour]]
deps = ["StaticArrays"]
git-tree-sha1 = "9f02045d934dc030edad45944ea80dbd1f0ebea7"
uuid = "d38c429a-6771-53c6-b99e-75d170b6e991"
version = "0.5.7"

[[Convex]]
deps = ["AbstractTrees", "BenchmarkTools", "LDLFactorizations", "LinearAlgebra", "MathOptInterface", "OrderedCollections", "SparseArrays", "Test"]
git-tree-sha1 = "88746604d00ba4441d13d5eb2332977a0e428f3b"
uuid = "f65535da-76fb-5f13-bab9-19810c17039a"
version = "0.15.0"

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

[[DataStructures]]
deps = ["Compat", "InteractiveUtils", "OrderedCollections"]
git-tree-sha1 = "3daef5523dd2e769dad2365274f760ff5f282c7d"
uuid = "864edb3b-99cc-5e75-8d2d-829cb0a9cfe8"
version = "0.18.11"

[[DataValueInterfaces]]
git-tree-sha1 = "bfc1187b79289637fa0ef6d4436ebdfe6905cbd6"
uuid = "e2d170a0-9d28-54be-80f0-106bbe20a464"
version = "1.0.0"

[[DataValues]]
deps = ["DataValueInterfaces", "Dates"]
git-tree-sha1 = "d88a19299eba280a6d062e135a43f00323ae70bf"
uuid = "e7dc6d0d-1eca-5fa6-8ad6-5aecde8b7ea5"
version = "0.4.13"

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

[[FileIO]]
deps = ["Pkg", "Requires", "UUIDs"]
git-tree-sha1 = "80ced645013a5dbdc52cf70329399c35ce007fae"
uuid = "5789e2e9-d7fb-5bc7-8068-2c6fae9b9549"
version = "1.13.0"

[[FilePaths]]
deps = ["FilePathsBase", "MacroTools", "Reexport", "Requires"]
git-tree-sha1 = "919d9412dbf53a2e6fe74af62a73ceed0bce0629"
uuid = "8fc22ac5-c921-52a6-82fd-178b2807b824"
version = "0.8.3"

[[FilePathsBase]]
deps = ["Compat", "Dates", "Mmap", "Printf", "Test", "UUIDs"]
git-tree-sha1 = "04d13bfa8ef11720c24e4d840c0033d145537df7"
uuid = "48062228-2e41-5def-b9a4-89aafe57970f"
version = "0.9.17"

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

[[FreqTables]]
deps = ["CategoricalArrays", "Missings", "NamedArrays", "Tables"]
git-tree-sha1 = "488ad2dab30fd2727ee65451f790c81ed454666d"
uuid = "da1fdf0e-e0ff-5433-a45f-9bb5ff651cb1"
version = "0.4.5"

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

[[HypergeometricFunctions]]
deps = ["DualNumbers", "LinearAlgebra", "SpecialFunctions", "Test"]
git-tree-sha1 = "65e4589030ef3c44d3b90bdc5aac462b4bb05567"
uuid = "34004b35-14d8-5ef3-9330-4cdb6864b03a"
version = "0.3.8"

[[Hyperscript]]
deps = ["Test"]
git-tree-sha1 = "8d511d5b81240fc8e6802386302675bdf47737b9"
uuid = "47d2ed2b-36de-50cf-bf87-49c2cf4b8b91"
version = "0.0.4"

[[HypertextLiteral]]
git-tree-sha1 = "2b078b5a615c6c0396c77810d92ee8c6f470d238"
uuid = "ac1192a8-f4b3-4bfe-ba22-af5b92cd3ab2"
version = "0.9.3"

[[HypothesisTests]]
deps = ["Combinatorics", "Distributions", "LinearAlgebra", "Random", "Rmath", "Roots", "Statistics", "StatsBase"]
git-tree-sha1 = "d49e34c0b93e4281391710f70ae648d76c377d35"
uuid = "09f84164-cd44-5f33-b23f-e6b0d136a0d5"
version = "0.10.8"

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

[[JSONSchema]]
deps = ["HTTP", "JSON", "URIs"]
git-tree-sha1 = "2f49f7f86762a0fbbeef84912265a1ae61c4ef80"
uuid = "7d188eb4-7ad8-530c-ae41-71a32a6d4692"
version = "0.3.4"

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
git-tree-sha1 = "4f00cc36fede3c04b8acf9b2e2763decfdcecfa6"
uuid = "23fbe1c1-3f47-55db-b15f-69d7ec21a316"
version = "0.15.13"

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

[[LinearRegressionKit]]
deps = ["DataFrames", "Distributions", "FreqTables", "HypothesisTests", "LinearAlgebra", "NamedArrays", "Printf", "Random", "StatsBase", "StatsModels", "VegaLite"]
git-tree-sha1 = "1550b95049b8b894aaba17b32b669258af6d61d1"
uuid = "e91d531d-6e51-44a8-96b7-a10d5d51daa3"
version = "0.7.4"

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

[[MathOptInterface]]
deps = ["BenchmarkTools", "CodecBzip2", "CodecZlib", "JSON", "LinearAlgebra", "MutableArithmetics", "OrderedCollections", "Printf", "SparseArrays", "Test", "Unicode"]
git-tree-sha1 = "a62df301482a41cb7b1db095a4e6949ba7eb3349"
uuid = "b8f27783-ece8-5eb3-8dc8-9495eed66fee"
version = "1.1.0"

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
git-tree-sha1 = "737a5957f387b17e74d4ad2f440eb330b39a62c5"
uuid = "77ba4419-2d1f-58cd-9bb1-8ffee604a2e3"
version = "1.0.0"

[[NamedArrays]]
deps = ["Combinatorics", "DataStructures", "DelimitedFiles", "InvertedIndices", "LinearAlgebra", "Random", "Requires", "SparseArrays", "Statistics"]
git-tree-sha1 = "2fd5787125d1a93fbe30961bd841707b8a80d75b"
uuid = "86f7a689-2022-50b4-a561-43c23ac3c673"
version = "0.9.6"

[[NetworkOptions]]
uuid = "ca575930-c2e3-43a9-ace4-1e988b2c1908"

[[NodeJS]]
deps = ["Pkg"]
git-tree-sha1 = "905224bbdd4b555c69bb964514cfa387616f0d3a"
uuid = "2bd173c7-0d6d-553b-b6af-13a54713934c"
version = "1.3.0"

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

[[PlutoUI]]
deps = ["AbstractPlutoDingetjes", "Base64", "ColorTypes", "Dates", "Hyperscript", "HypertextLiteral", "IOCapture", "InteractiveUtils", "JSON", "Logging", "Markdown", "Random", "Reexport", "UUIDs"]
git-tree-sha1 = "bf0a1121af131d9974241ba53f601211e9303a9e"
uuid = "7f904dfe-b85e-4ff6-b463-dae2292396a8"
version = "0.7.37"

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

[[Profile]]
deps = ["Printf"]
uuid = "9abbd945-dff8-562f-b5e8-e1ebf5ef1b79"

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

[[Roots]]
deps = ["CommonSolve", "Printf", "Setfield"]
git-tree-sha1 = "554149b8b82e167c1fa79df99aeabed4f8404119"
uuid = "f2b01f46-fcfa-551c-844a-d8ac1e96c665"
version = "1.3.15"

[[SCS]]
deps = ["MathOptInterface", "Requires", "SCS_GPU_jll", "SCS_jll", "SparseArrays"]
git-tree-sha1 = "8663c54fd5312f6ee27a94285a9a0381b29e6707"
uuid = "c946c3f1-0d1f-5ce8-9dea-7daa1f7e2d13"
version = "1.1.0"

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

[[Setfield]]
deps = ["ConstructionBase", "Future", "MacroTools", "Requires"]
git-tree-sha1 = "fca29e68c5062722b5b4435594c3d1ba557072a3"
uuid = "efcf1570-3423-57d1-acb7-fd33fddbac46"
version = "0.7.1"

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

[[TableTraitsUtils]]
deps = ["DataValues", "IteratorInterfaceExtensions", "Missings", "TableTraits"]
git-tree-sha1 = "78fecfe140d7abb480b53a44f3f85b6aa373c293"
uuid = "382cd787-c1b6-5bf2-a167-d5b971a19bda"
version = "1.0.2"

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

[[TranscodingStreams]]
deps = ["Random", "Test"]
git-tree-sha1 = "216b95ea110b5972db65aa90f88d8d89dcb8851c"
uuid = "3bb67fe8-82b1-5028-8e26-92a6c54297fa"
version = "0.9.6"

[[URIParser]]
deps = ["Unicode"]
git-tree-sha1 = "53a9f49546b8d2dd2e688d216421d050c9a31d0d"
uuid = "30578b45-9adc-5946-b283-645ec420af67"
version = "0.4.1"

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

[[Vega]]
deps = ["DataStructures", "DataValues", "Dates", "FileIO", "FilePaths", "IteratorInterfaceExtensions", "JSON", "JSONSchema", "MacroTools", "NodeJS", "Pkg", "REPL", "Random", "Setfield", "TableTraits", "TableTraitsUtils", "URIParser"]
git-tree-sha1 = "43f83d3119a868874d18da6bca0f4b5b6aae53f7"
uuid = "239c3e63-733f-47ad-beb7-a12fde22c578"
version = "2.3.0"

[[VegaLite]]
deps = ["Base64", "DataStructures", "DataValues", "Dates", "FileIO", "FilePaths", "IteratorInterfaceExtensions", "JSON", "MacroTools", "NodeJS", "Pkg", "REPL", "Random", "TableTraits", "TableTraitsUtils", "URIParser", "Vega"]
git-tree-sha1 = "3e23f28af36da21bfb4acef08b144f92ad205660"
uuid = "112f6efa-9a02-5b7d-90c0-432ed331239a"
version = "2.6.0"

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
git-tree-sha1 = "b1be2855ed9ed8eac54e5caff2afcdb442d52c23"
uuid = "ea10d353-3f73-51f8-a26c-33c1cb351aa5"
version = "1.4.2"

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
# ╠═8b90477d-5887-49bd-bcf3-d2d7276885af
# ╟─3820a86d-fefa-43ca-a308-40d9ff2b3599
# ╟─72cce814-aa8f-4ac8-98c1-f556ce43ed6d
# ╟─960b2ed0-2394-11ec-17c0-5bdaeef72b5d
# ╟─e7029f0e-832a-4b62-a4f4-09002dbd7159
# ╟─ba9c36aa-e791-4411-85f6-5c230d19b017
# ╟─833249db-8526-4fa1-b136-c48850d7e37f
# ╠═c67dda3c-8044-436e-b668-e5c7c8feddaf
# ╟─4efcb4b6-9000-4db6-b951-37c9c9c9676d
# ╠═3d51f5f0-752d-407e-80e3-7e44849dd76b
# ╟─a2946345-b457-4e02-a3eb-7646a7083058
# ╠═c02ae1b0-ba3f-4c94-83d8-425af6cf8ed9
# ╟─c8c2e3c2-d2dc-448e-9d32-8ba29697a1f3
# ╠═0580541d-29f0-4731-9ea9-f36494f70516
# ╟─561f5088-7a2a-47f4-bfbf-6977ae6a27a5
# ╠═fb093381-1109-450b-898e-1a0f87bd54d4
# ╟─5595417a-6333-4acd-b14b-27865d4f8736
# ╟─a9e20df4-96aa-4798-a5ef-204671de44af
# ╟─2732613f-2546-4fb4-93ee-1805a43e7521
# ╟─9280b21d-d44e-49e5-a0c0-f31dbf90f51a
# ╟─59e2e16a-d5ee-4fb0-ae02-94c05b07b306
# ╠═8ee5dcd0-0ae6-4d7e-9270-38f36c9260d4
# ╟─658f89c1-5452-491d-bed4-e0b189c7f457
# ╟─9e7b7ef3-856d-4fca-907b-845666463e42
# ╟─ba46a9a7-b469-401f-a3b4-10ab4a6f47cb
# ╠═6024747f-e21a-40d7-8cd1-1681ca12718e
# ╟─1424b025-89be-4adc-9c0f-38abca54bb2a
# ╟─c42c62c4-db4d-44c3-81a6-0573f8d03258
# ╟─6fd86104-021a-4ec7-acdb-4ac4c771f03e
# ╠═40109f1e-f391-485e-80d1-5b60bd1ede63
# ╟─0f0a5bcf-f0ca-4cd8-9722-ce8282780a3a
# ╟─5f3ab6a9-d093-4fcc-9370-79bc76a94b0c
# ╟─d86594f9-b563-42d9-812c-7dc663b9b8c3
# ╟─4a60f8df-a871-4f96-9c1c-499a36310985
# ╠═653d8f23-3a60-4778-897d-c092106ce20d
# ╠═28c593e8-0ce6-4db6-a8bb-f64ec8307391
# ╟─49a66213-b8b7-4789-b967-7bdfb859de9d
# ╠═b22fb358-658a-44be-a99b-b5301c2dcadf
# ╟─e79edd61-e5ea-4da2-a28a-483d13e6da71
# ╠═925fd389-4d7d-4fdd-b84c-c81c7f6408e4
# ╟─00000000-0000-0000-0000-000000000001
# ╟─00000000-0000-0000-0000-000000000002
