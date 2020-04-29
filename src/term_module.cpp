#include <Rcpp.h>
#include <set>

class Term
{
public:
  Term(std::string id, std::string name, std::string top, size_t d, bool obso, std::vector<std::string> p, std::vector<std::string> c){
    this->id = id;
    this->name = name;
    this->top = top;
    this->h_score = d;
    this->obso = obso;
    this->parents = p;
    this->childrens = c;
  }
  bool isObsolete(){
    return this->obso;
  }
  std::string getId(){
    return this->id;
  }
  
  std::vector<std::string> getParent(){
    return this->parents;
  }
  std::vector<std::string> getChildren(){
    return this->childrens;
  }
  
//  void setChildren(std::string child);
//  vec getChildren(){
//    return this->childrensIsA;
//  }
//  std::vector<std::string> getPOFParent();
//  void setPOFChildren(std::string child);
//  std::vector<std::string> getPOFChildren();
//  std::string getRegulateTerm();
  std::string getTop(){
    return this->top;
  }
  std::string getName(){
    return this->name;
  }
  size_t getDepth(){
    return this->h_score;
  }
  void setDescedants(std::string d){
    
    this->descendants.push_back(d);
    
  }
  std::vector<std::string> getDescedants(){
    
    return  this->descendants;
  }
  void setAncestors(std::string d){
    
    this->ancestors.push_back(d);
    
  }
  std::vector<std::string> getAncestors(){
    
    return  this->ancestors;
  }
private:
  std::string id;
  std::string name;
  std::string top;
  std::vector<std::string> parents;
  std::vector<std::string> childrens;
  std::vector<std::string> descendants;
  std::vector<std::string> ancestors;
  bool obso;
  size_t h_score;
};



