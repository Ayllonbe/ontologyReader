#ifndef MODSTRING_H
#define MODSTRING_H

#include <Rcpp.h>

class Term
{
public:
  Term(std::string id, std::string name, std::string top, size_t d, bool obso, std::vector<std::string> p, std::vector<std::string> c);
  bool isObsolete();
  std::string getId();
  std::vector<std::string> getParent();
  std::vector<std::string> getChildren();
  std::string getTop();
  std::string getName();
  size_t getDepth();
  void setDescedants(std::string d);
  std::vector<std::string> getDescedants();
  void setAncestors(std::string d);
  std::vector<std::string> getAncestors();
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





#endif
