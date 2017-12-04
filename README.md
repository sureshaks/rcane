<img src="man/figures/logo.png" />

rcane
=====

[![Build Status](https://travis-ci.org/sureshaks/rcane.svg?branch=akshay)](https://travis-ci.org/sureshaks/rcane)

What is it?
-----------

An R package to find parameter estimates of Linear Regression

No seriously, what is it?
-------------------------

This is what we did...

<img src="man/figures/whatwedid.png" align="center" />

We start with arbitrary parameter estimates for linear regression and slide down to the optimal values.

But how?
--------

We use 4 different algorithms:

-   Batch Gradient Descent

-   Stochastic Gradient Descent

-   Mini-batch Gradient Descent

-   Coordinate Descent

### Time for some Math

<img src="man/figures/dreadfulmath.png" align="center" />

<table>
<tr>
    <th> 
      Batch
    </th>
    <th> 
      Stochastic
    </th>
    <th>
      Coordinate
    </th>

</tr>
<tr>
    <td>
      <img src="man/figures/batch.png" align="center" />
    </td>
    <td>
      <img src="man/figures/stochastic.png" align="center" />
    </td>
    <td>
      <img src="man/figures/coordinate.png" align="center" />
    </td>

</tr>
</table>
