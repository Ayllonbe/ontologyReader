#include <Rcpp.h>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm> 
#include <set>
#include <iterator>
#include <cmath>
#include <map>
#include <deque>
#include "term_module.cpp"


using namespace Rcpp;
using namespace std;



RCPP_EXPOSED_WRAP(Term);

// Fill the zipped vector with pairs consisting of the
// corresponding elements of a and b. (This assumes 
// that the vectors have equal length)
template <typename A, typename B>
void zip(
    const std::vector<A> &a, 
    const std::vector<B> &b, 
    std::vector<std::pair<A,B>> &zipped)
{
  for(size_t i=0; i<a.size(); ++i)
  {
    zipped.push_back(std::make_pair(a[i], b[i]));
  }
}

// Write the first and second element of the pairs in 
// the given zipped vector into a and b. (This assumes 
// that the vectors have equal length)
template <typename A, typename B>
void unzip(
    const std::vector<std::pair<A, B>> &zipped, 
    std::vector<A> &a, 
    std::vector<B> &b)
{
  for(size_t i=0; i<a.size(); i++)
  {
    a[i] = zipped[i].first;
    b[i] = zipped[i].second;
  }
}

void sort(std::vector<std::string>& names, std::vector<size_t>& score)
{
  
  // Zip the vectors together
  std::vector<std::pair<std::string,size_t>> zipped;
  zip(names, score, zipped);
  
  // Sort the vector of pairs
  std::sort(std::begin(zipped), std::end(zipped), 
            [&](const std::pair<std::string,size_t>& a, const std::pair<std::string,size_t>& b)
            {
              return a.second < b.second;
            });
  
  // Write the sorted pairs back to the original vectors
  unzip(zipped, names, score);
  
}

void getDesc(string term, vector<string>& dsc, map<string,vector<string>>& t2child){
  vector<string> ch = t2child.at(term);
  
  for(string child : ch){
    dsc.insert(dsc.end(), child);
    getDesc(child,dsc,t2child);
  }
}
set<string> getDescendants(string term, map<string,vector<string>>& t2child) {
  vector<string> dscTerm=vector<string>();
  getDesc(term,dscTerm,t2child);
  set<string> s = set<string>(dscTerm.begin(),dscTerm.end());
  return s;
}

void getAnc(string term, vector<string>& anc, map<string,vector<string>>& t2parent){
  vector<string> p = t2parent.at(term);
  
  for(string parent : p){
    anc.insert(anc.end(), parent);
    getDesc(parent,anc,t2parent);
  }
}
set<string> getAncestors(string term, map<string,vector<string>>& t2parent) {
  vector<string> ancTerm=vector<string>();
  getDesc(term,ancTerm,t2parent);
  set<string> s = set<string>(ancTerm.begin(),ancTerm.end());
  return s;
}



// [[Rcpp::export]]
List reader(String go_file) {
  Rcout <<fixed << setprecision(14)<<"Charging the ontology\n";
  string line;
  ifstream myfile (go_file);
  string delimiter = ":";
  string token = "";
  int count = 0;
  int countTerms =0;
  
  map<string, vector<string>> termId2info;
  map<string, size_t> termId2depth;
  map<string, vector<string>> termId2AltId;
  map<string, vector<string>> termId2parent;
  map<string, bool> termId2obsolete;
  map<string, vector<string>> termId2children;
  
  map<string,string> namespace2root;
  
  string id = "";
  
  vector<string> ids;
  
  if (myfile.is_open())
  {
    while ( getline (myfile,line) )
    {
      if(line==""){
        token=line;
        id="";
      }
      if(line=="[Term]"){
        token=line;
        continue;
      }
      if(token=="[Term]") {
        string key = line.substr(0, line.find(delimiter));
        string val = line.substr(line.find(delimiter)+2,line.size());
        if(key=="id"){
          id = val;
          ids.push_back(val);
          termId2info.insert(pair<string,vector<string>>(id,vector<string>()));
          termId2AltId.insert(pair<string,vector<string>>(id,vector<string>()));
          termId2parent.insert(pair<string,vector<string>>(id,vector<string>()));
          termId2children.insert(pair<string,vector<string>>(id,vector<string>()));
          termId2obsolete.insert(pair<string,bool>(id,false));
        }else if(key=="name"){
          termId2info.at(id).push_back(val);
        }else if(key=="is_a"){
          string delimiter = " ! ";
          string rel = val.substr(0, val.find(delimiter));
          termId2parent.at(id).push_back(rel);
        }/*else if(key=="relationship"){
 string delimiter = " ! ";
        string rel = val.substr(0, val.find(delimiter));
        val =  rel.substr( rel.find(" ")+1,rel.size());
        rel = rel.substr(0,rel.find(" "));
        if(rel=="part_of"){
        part_of.parents.push_back(val);
        }else if(rel=="regulates"){
        reg=val;
        }else if(rel=="positively_regulates"){
        positive_reg=val;
        }else if(rel=="negatively_regulates"){
        negative_reg = val;
        }
      }*/
        else if(key =="alt_id"){
          termId2AltId.at(id).push_back(val);
        }
        else if(key=="namespace"){
          termId2info.at(id).push_back(val);
        }
        else if(key=="is_obsolete"){
          if(val=="true"){
            termId2obsolete.at(id)= true;
          }
        }
        
    }
  }
    myfile.close();
}
  
  for(string id : ids){
    if(termId2parent.at(id).size()>0){
      for(string p : termId2parent.at(id)){
        termId2children.at(p).push_back(id);
      }
      
    }else{
      if(!termId2obsolete.at(id)){
        namespace2root.insert(pair<string,string>(termId2info.at(id).at(0),id));
      }else{
        termId2depth.insert(pair<string,size_t>(id,0)); 
      }
    }
  }
  
  for(map<string,string>::iterator it = namespace2root.begin(); it!=namespace2root.end();it++){
    string sub = it->second;
    int flag=1;
    size_t level = 0;
    termId2depth.insert(pair<string,size_t>(sub,level));
    level++;
    vector<string> vec1 = vector<string>(termId2children.at(sub));
    vector<string> vec2 = vector<string>();
    while(flag==1){
      
      for(string element:vec1){
        termId2depth.insert(pair<string,size_t>(element,level));
        vector<string> ch = termId2children.at(element);
        vec2.insert(vec2.end(),ch.begin(),ch.end());
      }
      vec1.clear();
      if(vec2.size()>0){
        vec1.insert(vec1.end(),vec2.begin(),vec2.end());
        vec2.clear();
        level++;
      }else {
        flag=0;
      }
    }
  }
  
  vector<Term> vecT;
  vector<size_t> depths;
  for(string id : ids){
    depths.push_back(termId2depth.at(id));
  }
  sort(ids,depths);
  map<string,size_t> id2pos;
  for(size_t i =0;i<ids.size();i++){
    id2pos.insert(pair<string,size_t>(ids.at(i),i));
  }
 // NumericMatrix matrix( ids.size(), ids.size());
  List alternative2id;
  for(string id : ids){
    Term t(id, termId2info.at(id).at(0),namespace2root.at(termId2info.at(id).at(1)),termId2depth.at(id),
           termId2obsolete.at(id),termId2parent.at(id),termId2children.at(id));    
    vecT.push_back(t);
    
    if(!termId2obsolete.at(id)){
      
      for(string d : getDescendants(id,termId2children)){
        vecT.at(id2pos.at(id)).setDescedants(d);
      }
      for(string a : getAncestors(id,termId2parent)){
        vecT.at(id2pos.at(id)).setAncestors(a);
      }
      for(string a : termId2AltId.at(id)){
        alternative2id[a] = id;
      }
    }
    
  }
  
  
  return List::create(_["termOBJ"] = vecT,
                            //   _["Fulladjacency"] = matrix,
                               _["alternativeIDs"] = alternative2id,
                               _["name"] = ids);
  }





RCPP_MODULE(ontology){
  
  using namespace Rcpp ;
  
  // function("reader", &reader);
  
  
  class_<Term>("Term")
    // expose the default constructor
    .constructor<std::string,std::string,std::string,size_t,bool, std::vector<std::string>, std::vector<std::string>>()
    .method("isObsolete", &Term::isObsolete, "return TRUE if the term is obsolete")
    .method("getId", &Term::getId, "return the term id")
    .method("getName", &Term::getName, "return the term name")
    .method("getDepth",&Term::getDepth, "return the term level in the ontology (longest path to root)")
    .method("getTop", &Term::getTop, "return the top term")
    .method("getParents",&Term::getParent, "return the term id parents of term x")
    .method("getChildren",&Term::getChildren, "return the term id childrens of term x")
    .method("getDescendants",&Term::getDescedants, "return the term id descendants of term x")
    .method("getAncestors",&Term::getAncestors, "return the term id ancestors of term x")
    ;
  
}

