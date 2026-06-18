// tree_index.h — shared TreeIndex struct + XPtr typedef for R parse trees.
// Hash-map layout (parent/row/children/by_line) giving O(1) tree lookups,
// included by tree_index.cpp and symbol_resolve.cpp.

#ifndef SCIPR_TREE_INDEX_H
#define SCIPR_TREE_INDEX_H

#include <Rcpp.h>
#include <vector>
#include <unordered_map>

using namespace Rcpp;

// TreeIndex structure for efficient tree lookups
// Stored as external pointer (XPtr) for zero-copy access from R
struct TreeIndex {
  // parent: node_id -> parent_id
  std::unordered_map<int, int> parent;

  // row: node_id -> row index (0-based in C++, converted to 1-based for R)
  std::unordered_map<int, int> row;

  // children: parent_id -> vector of child ids
  std::unordered_map<int, std::vector<int>> children;

  // by_line: line_number -> vector of row indices
  std::unordered_map<int, std::vector<int>> by_line;

  // n: number of nodes
  int n;
};

typedef Rcpp::XPtr<TreeIndex> TreeIndexPtr;

// Destructor helper - Rcpp handles cleanup via XPtr destructor
inline void tree_index_finalizer(TreeIndex* ptr) {
  if (ptr != nullptr) {
    delete ptr;
  }
}

#endif  // SCIPR_TREE_INDEX_H
