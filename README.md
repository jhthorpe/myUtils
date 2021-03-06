# myUtils
Library of useful utilities in Fortran 90.  

## Using
To use, compile your code by linking to the myUtils.a library, and include the .mod files with the -I flag.  
Your code must have 'use myUtils' at the top. 

## Dynamic Arrays
A set of functions useful for dynamically alterning arrays.  
Synatx:  
	kind_(#D)(operation)(dimension)  
Currently supported dynamic functions:  
- bool_2Dgrow1		:	double first dimension of 2D logical
- chr8_1Dgrow		:	double size of 1D char(len=8)
- int4_2Dgrow1		:	double first dimension of 2D integer(kind=4)
- int4_1Dzero		:	zero 1D integer(kind=4)
- real8_2Dgrow1		:	double first dimension of 2D real(kind=8) 	
- real8_2Dzero		:	zero all elements of 2D real(kind=8)
- real8_3Dgrow1		:	double first dimension of 3D real(kind=8)  

## Hash Tables
A set of functions that implement dynamic hash tables. The user can define their own hash function, which is passed in as an arguement. Everything else is completely abstracted from the user's concern.   
Syntax:  
	hash_operation_keyKind_valKind  
Currently supported hash functions:
- hash_qinit_1Dint4_bool	:	initialize quadratically probed hash tables with 1D integer(kind=4) keys and logical values
- hash_qinsert_1Dint4_bool	:	insert key:value pair, quadratic probing, 1D integer(kind=4) keys, logical values
- hash_qrehash_1Dint4_bool	:	rehash key:value pair, quadratic probing, 1D integer(kind=4) keys, logical values
- hash_qsearch_1Dint4_bool	:	search key:value pair, quadratic probing, 1D integer(kind=4) keys, logical values
  
A note about hash functions...  
The quadtractically probed hash functions here are of a special kind. The tables are dynamically resized so that they are always a power of two. This allows us to use fancy bitops to perform expensive modulus operations, and it guarentees that an empty slot will always be found. It should be noted that the table dynamically resizes at a load factor of 0.5, which I am not sure is ideal! 

## Linear Algebra
A set of functions and subroutines that perform linear algebra. Of specific note is the implimentation of the Lanczos algorithm fora set number of lowest eigenvalues. 
Syntax:
	linal_operation_dataType
Currently supported functions:
- linal_lanczos_2Dreal8		:	find lowest m eigenvalues and eigenvectors of a symmetric, real matrix
- linal_eunorm_1Dreal8		:	find Euclidian norm of a 1D real vector
- linal_onvec_2Dreal8		:	finds a vector orthonormal to a current set of (what are assumed to be) othogonol vectors
- linal_proj_1Dreal8		:	determines the projection of one real8 vector upon another
