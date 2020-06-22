#include <Rcpp.h>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include "ontologyReader.h"
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


std::map<std::string,Term> transformRobj2Term(List ontology,std::vector<std::string>& ids){
  std::map<std::string,Term> str2Term;
  for(std::string id:ids){
    if(ontology.containsElementNamed(id.c_str())){
    List term = ontology[id];
    std::vector<std::string> parents;
    std::vector<size_t>  sp = term["parents"];
    for(size_t p : sp){
      parents.push_back(ids.at(p-1));
    }
    std::vector<std::string> children;
    std::vector<size_t>  sc = term["children"];
    for(size_t c : sc){
      children.push_back(ids.at(c-1));
    }
    std::string name = term["name"];
    std::string top = term["top"];
    size_t depth = term["depth"];
    bool obso = true;

    Term t(id,name,top,depth,obso,parents,children);
    str2Term.insert(std::pair<std::string,Term>(id,t));
    }
  }
  return str2Term;
}


void goUniverseIC(std::string t, std::map<std::string,Term>& str2Term,
                                        std::map<std::string,std::vector<long double>>& str2alphaBeta,
                                        std::string top, size_t ontoChild){

  long double alpha = 1.;
  long double beta = 0.;
  for(std::string p : str2Term.at(t).getParent()) {
    if(p.compare(top)!=0){
      //  std::cout<<t<<" "<<p<<"\n";
      if(str2alphaBeta.find(p)==str2alphaBeta.end()){
        goUniverseIC(p,str2Term,str2alphaBeta,top,ontoChild);
     }


      std::vector<long double> pereAlphaBeta = str2alphaBeta.at(p);

      long double division =pereAlphaBeta[0]/(long double) str2Term.at(p).getChildren().size();
      long double b = floor(log10(division));
      long double a =division/pow(10,b);
      alpha = alpha*a;
      beta = beta + pereAlphaBeta[1] + b;
    }
    else{
      long double division =1./(long double) ontoChild;
      long double b = floor(log10(division));
      long double a =division/pow(10,b);
      alpha = alpha*a;
      beta = beta + b;
    }
  }
  std::vector<long double> res;
  res.push_back(alpha);
  res.push_back(beta);
  str2alphaBeta.insert(std::pair<std::string,std::vector<long double>>(t,res));

}
//[[Rcpp::export]]
std::vector<long double> computegoUIC(List& ontology, std::vector<std::string>& ids){
  std::cout<<"Computing IC\n";
  std::map<std::string,Term> str2Term = transformRobj2Term(ontology,ids);
  std::map<std::string,std::vector<long double>> str2alphaBeta;
  for(std::string id : ids){

    if(ontology.containsElementNamed(id.c_str()) && str2Term.at(id).getChildren().size()==0){
    goUniverseIC(id, str2Term, str2alphaBeta, str2Term.at(id).getTop(), str2Term.at(str2Term.at(id).getTop()).getChildren().size());
    }
  }

 std::vector<long double> vics;
  for(std::string id : ids){
    if(ontology.containsElementNamed(id.c_str())&&str2Term.at(id).getParent().size()>0){
  std::vector<long double> s = str2alphaBeta.at(id);
  vics.push_back(-log(s.at(0))-s.at(1)*log(10));
    }else{
      vics.push_back(0);
    }
  }
return vics;
}






