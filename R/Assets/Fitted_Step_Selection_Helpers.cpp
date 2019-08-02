#include <ctime>
#include <RcppArmadilloExtensions/sample.h>
#include <stdio.h>
#include <stdlib.h>
#include <vector>

/*
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

//[[Rcpp::depends(RcppArmadillo)]]

/* name spaces */
using namespace Rcpp;
using namespace std;


/* Prototypes */
int mod(int a, int b);
int nei (int i, int w, int dir);
double frand(double upper);
int rsamp(vector<int>& nums, vector<double>& probs);


// http://stackoverflow.com/questions/2704521/generate-random-double-numbers-in-c
double frand(double upper) {
  double f = (double)rand() / RAND_MAX;
  return f * upper;
}


// http://stackoverflow.com/questions/1761626/weighted-random-numbers
int rsamp(vector<int>& nums, vector<double>& probs) {
  
  double sum_of_weight = 0, rnd;
  vector<int>::iterator it_num;
  vector<double>::iterator it_prob;
  
  // sum weights
  for (it_prob = probs.begin(); it_prob < probs.end(); it_prob++) {
    sum_of_weight += *it_prob;
  }
  
  rnd = frand(sum_of_weight);
  
  for(it_prob = probs.begin(), it_num = nums.begin(); it_prob < probs.end(); it_prob++, it_num++) {
    if(rnd < *it_prob)
      return *it_num;
    rnd -= *it_prob;
  }
}

// Thanks: http://stackoverflow.com/questions/4003232/how-to-code-a-modulo-operator-in-c-c-obj-c-that-handles-negative-numbers
int mod (int a, int b) {
   if(b < 0) //you can check for b == 0 separately and do what you want
     return mod(-a, -b);   
   int ret = a % b;
   if(ret < 0)
     ret += b;
   return ret;
}

int nei (int i, int w, int dir) {
  if (dir == 0) // stay
    return i;
  if (dir == 1) // east
    return mod((i % w) + 1, w) + w * (i / w);
  if (dir == 2) // west
    return mod((i % w) - 1, w) + w * (i / w);
  if (dir == 3) // north
    return (i % w) + w * mod(((i / w) + 1), w);
  if (dir == 4) // south
    return (i % w) + w * mod(((i / w) - 1), w);
  return -1;
}


//[[Rcpp::export]]
NumericMatrix tpm_func(double alpha, NumericVector omegas, NumericMatrix resources, NumericVector d, int nc) {
  int i, ii, j, k, n = nc * nc, l = omegas.length();
  double one;
  NumericMatrix tp(n, 5), num(5); 
  IntegerVector p(5);
  
  for (i = 0; i < n; i++) {
    /*
     * get neighbour pixels 
     * 0 = stay
     * 1 = east
     * 2 = west
     * 3 = north
     * 4 south
     */
    for (ii = 0; ii < 5; ii++)
      p(ii) = nei(i, nc, ii);
    
    for (j = 0; j < 5; j++) {
      one = -alpha * d[j];
      for (int k = 0; k < l; k++) {
        one += resources(p[j], k) * omegas(k);
      }
      num[j] = exp(one);
    }
    tp(i, _) = num / sum(num);
  }
  return tp;
}


//[[Rcpp::export]]
IntegerVector walk_func1(NumericMatrix tpm, int n, NumericVector xy0, int nc) {

	IntegerVector c(n), w, ch = Range(0, 4); // choices for the new yx
  int i, k; 
  c(0) = xy0(0) + xy0(1) * nc;

	for (i = 1; i < n; i++) {
	  k = c(i-1);
		w = RcppArmadillo::sample(ch, 1, true, (NumericVector)(tpm(k, _) / sum(tpm(k, _)))); // weights of the neighbours
		c(i) = nei(k, nc, w(0));
	}
	return c;
}



//[[Rcpp::export]]
/* changing over time */
IntegerVector walk_func_time(double alpha, NumericMatrix resources, NumericMatrix omegas, int n, NumericVector xy0, int nc, IntegerVector d) {

	IntegerVector c(n), w, ch = Range(0, 4); // choices for the new yx
  int i, ii, j, jj, k, l = omegas.ncol(); 
  IntegerVector p(5);
  NumericVector prob(5), num(5);
  double one = 0;
  
  c(0) = xy0(0) + xy0(1) * nc;

	for (i = 1; i < n; i++) {
	  k = c(i-1);
	  
    /*
     * get neighbour pixels 
     * 0 = stay
     * 1 = east
     * 2 = west
     * 3 = north
     * 4 south
     */
    for (ii = 0; ii < 5; ii++)
      p(ii) = nei(k, nc, ii);
    
    // prob to go to each nei
    one = 0;
    for (j = 0; j < 5; j++) {
      one = -alpha * d[j];
      for (jj = 0; jj < l; jj++) {
        one += resources(p[j], jj) * omegas(i, jj);
      }
      num[j] = exp(one);
    }
    prob = num / sum(num);
	  
		w = RcppArmadillo::sample(ch, 1, true, prob); // weights of the neighbours
		c(i) = nei(k, nc, w(0));
	}
	return c;
}

//[[Rcpp::export]]
IntegerVector simulate_ssa(int n_steps, int start, int nc, NumericMatrix moveK, NumericMatrix habK) {
  
  int i, j, npot = moveK.nrow();
  IntegerVector c(n_steps), pot(npot);
  NumericVector ch(npot), w(npot);
  c(0) = start;
  
  for (i = 1; i < n_steps; i++) {
    for (j = 0; j < npot; j++) {
      pot(j) = mod((c(i-1) % nc) + moveK(j, 0), nc) + nc * mod(((c(i-1) / nc) + moveK(j, 1)), nc);
      ch(j) = habK(pot(j), 1) * moveK(j, 3);
    }
    w = RcppArmadillo::sample(pot, 1, true, ch); 
    c(i) = w(0);
  }
  return c;
}


//[[Rcpp::export]]
NumericMatrix simulate_ssa1(int n_steps, NumericVector start_xy, int nc, NumericMatrix moveK, NumericVector &habK, 
                            NumericVector &xshift, NumericVector &yshift) {
  
  int i, j, npot = moveK.nrow();
  NumericMatrix c(n_steps,2); 
  IntegerVector pot_x(npot), pot_y(npot), pot_cell(npot);
  NumericVector ch(npot), w(npot);
  c(0,_) = start_xy;
  
  for (i = 1; i < n_steps; i++) {
    
    pot_x = c(i-1, 0) + xshift;
    pot_y = c(i-1, 0) + yshift;
    
    for (j = 0; j < npot; j++) {
      //pot_cell(j) = mod(pot_x(j), nc) + mod(pot_y(j), nc) * nc;
      pot_cell(j) = pot_x(j) + pot_y(j) * nc;
      ch(j) = habK[pot_cell(j)] * moveK(j, 3);
    }
    w = RcppArmadillo::sample(pot_cell, 1, true, ch); 
    c(i,0) = (int)w(0) % nc;
    c(i,1) = (int)w(0) / nc;
  }
  return c;
}

//[[Rcpp::export]]
IntegerVector simulate_ssaP1(int n_steps, int start, int nc, NumericMatrix moveK, NumericMatrix habK) {
  
  int step, j, npot = moveK.nrow();
  int ncell = habK.nrow();
  
  vector<vector<int> > cells (ncell);  // create vector of pointers
  vector<vector<double> > probs (ncell);
  IntegerVector c(n_steps);
  
  c(0) = start;
  
  for (step = 1; step < n_steps; step++) {
    int cur_cell = c(step-1);
    if (cells[cur_cell].empty()) { // Allocate memory if needed
      cells[cur_cell].resize(npot);
      probs[cur_cell].resize(npot);
      for (j = 0; j < npot; j++) {
        cells[cur_cell][j] = mod((c(step-1) % nc) + moveK(j, 0), nc) + nc * mod(((c(step-1) / nc) + moveK(j, 1)), nc);
        probs[cur_cell][j] = habK(cells[cur_cell][j], 1) * moveK(j, 3);
      }
    }
   
   c(step) = rsamp(cells[cur_cell], probs[cur_cell]);
    
  }
  return c;
}

