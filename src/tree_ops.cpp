// tree_ops.cpp - C++ implementations for tree traversal operations
// These functions provide O(n) tree operations with pre-allocated vectors,
// replacing the O(n^2) R implementations that used vector growth patterns.

#include <Rcpp.h>
#include <unordered_map>
#include <vector>
#include <string>

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
void tree_index_finalizer(TreeIndex* ptr) {
  if (ptr != nullptr) {
    delete ptr;
  }
}

//' Build a tree index from parse data (C++ implementation)
//'
//' Creates a TreeIndex structure stored as an external pointer for
//' efficient O(1) lookups of parent, children, and row indices.
//'
//' @param id Integer vector of node IDs
//' @param parent Integer vector of parent IDs
//' @param line1 Integer vector of starting line numbers
//' @return External pointer (XPtr) to TreeIndex structure
//' @keywords internal
// [[Rcpp::export]]
SEXP build_tree_index_cpp(IntegerVector id, IntegerVector parent, IntegerVector line1) {
  int n = id.size();

  TreeIndex* idx = new TreeIndex();
  idx->n = n;

  // Reserve space in hash maps for better performance
  idx->parent.reserve(n);
  idx->row.reserve(n);

  for (int i = 0; i < n; i++) {
    int node_id = id[i];
    int parent_id = parent[i];
    int line = line1[i];

    // Map node_id -> parent_id
    idx->parent[node_id] = parent_id;

    // Map node_id -> row index (0-based)
    idx->row[node_id] = i;

    // Map parent_id -> children
    // Only add if parent is not NA and not 0 (root marker)
    if (parent_id != NA_INTEGER && parent_id != 0) {
      idx->children[parent_id].push_back(node_id);
    }

    // Map line -> row indices
    idx->by_line[line].push_back(i);
  }

  // Create XPtr with destructor
  TreeIndexPtr ptr(idx, true);  // true = release memory when R GCs the XPtr
  ptr.attr("class") = "TreeIndexPtr";

  return ptr;
}

//' Get children of a node using C++ tree index
//'
//' @param node_id Node ID to get children for
//' @param index_ptr External pointer to TreeIndex
//' @return Integer vector of child node IDs
//' @keywords internal
// [[Rcpp::export]]
IntegerVector get_children_cpp(int node_id, SEXP index_ptr) {
  TreeIndexPtr idx(index_ptr);

  auto it = idx->children.find(node_id);
  if (it == idx->children.end()) {
    return IntegerVector(0);
  }

  return wrap(it->second);
}

//' Get parent ID of a node using C++ tree index
//'
//' @param node_id Node ID to get parent for
//' @param index_ptr External pointer to TreeIndex
//' @return Integer parent ID, or NA if not found
//' @keywords internal
// [[Rcpp::export]]
int get_parent_cpp(int node_id, SEXP index_ptr) {
  TreeIndexPtr idx(index_ptr);

  auto it = idx->parent.find(node_id);
  if (it == idx->parent.end()) {
    return NA_INTEGER;
  }

  return it->second;
}

//' Get row index of a node using C++ tree index
//'
//' @param node_id Node ID to get row index for
//' @param index_ptr External pointer to TreeIndex
//' @return Integer row index (1-based for R), or NA if not found
//' @keywords internal
// [[Rcpp::export]]
int get_row_cpp(int node_id, SEXP index_ptr) {
  TreeIndexPtr idx(index_ptr);

  auto it = idx->row.find(node_id);
  if (it == idx->row.end()) {
    return NA_INTEGER;
  }

  // Convert from 0-based C++ to 1-based R
  return it->second + 1;
}

//' Get all descendant IDs using pre-allocated DFS (C++ implementation)
//'
//' This is the key optimization: uses pre-allocated vectors with stack-based
//' DFS traversal. Complexity is O(n) with low constant factors, compared to
//' the O(n^2) R implementation that used c() for vector growth.
//'
//' @param node_id Starting node ID
//' @param index_ptr External pointer to TreeIndex
//' @return Integer vector of all descendant node IDs
//' @keywords internal
// [[Rcpp::export]]
IntegerVector get_descendant_ids_cpp(int node_id, SEXP index_ptr) {
  TreeIndexPtr idx(index_ptr);

  // Pre-allocate result vector with reasonable estimate
  std::vector<int> result;
  result.reserve(256);  // Initial capacity, will grow if needed

  // Use vector as stack for better cache locality than std::stack
  std::vector<int> stack;
  stack.reserve(64);

  // Initialize stack with direct children of node_id
  auto it = idx->children.find(node_id);
  if (it != idx->children.end()) {
    // Push children in reverse order so we process in original order
    for (auto rit = it->second.rbegin(); rit != it->second.rend(); ++rit) {
      stack.push_back(*rit);
    }
  }

  // DFS traversal
  while (!stack.empty()) {
    int current = stack.back();
    stack.pop_back();

    result.push_back(current);

    // Push children of current node
    auto cit = idx->children.find(current);
    if (cit != idx->children.end()) {
      // Push in reverse order for correct traversal
      for (auto rit = cit->second.rbegin(); rit != cit->second.rend(); ++rit) {
        stack.push_back(*rit);
      }
    }
  }

  return wrap(result);
}

//' Check if any descendant has a specific token type
//'
//' Uses early-exit DFS to efficiently check if any descendant matches
//' one of the target token types.
//'
//' @param node_id Starting node ID
//' @param target_tokens Character vector of token types to search for
//' @param index_ptr External pointer to TreeIndex
//' @param tokens Character vector of all token types (parallel to parse_data)
//' @return TRUE if a matching descendant exists, FALSE otherwise
//' @keywords internal
// [[Rcpp::export]]
bool has_descendant_of_type_cpp(
    int node_id,
    CharacterVector target_tokens,
    SEXP index_ptr,
    CharacterVector tokens
) {
  TreeIndexPtr idx(index_ptr);

  // Build set of target tokens for O(1) lookup
  std::unordered_set<std::string> targets;
  for (int i = 0; i < target_tokens.size(); i++) {
    targets.insert(as<std::string>(target_tokens[i]));
  }

  // DFS with early exit
  std::vector<int> stack;

  auto it = idx->children.find(node_id);
  if (it != idx->children.end()) {
    for (int child_id : it->second) {
      stack.push_back(child_id);
    }
  }

  while (!stack.empty()) {
    int current = stack.back();
    stack.pop_back();

    // Get row index for this node
    auto row_it = idx->row.find(current);
    if (row_it != idx->row.end()) {
      int row_idx = row_it->second;  // 0-based
      std::string tok = as<std::string>(tokens[row_idx]);

      if (targets.count(tok) > 0) {
        return true;  // Early exit on first match
      }
    }

    // Push children
    auto cit = idx->children.find(current);
    if (cit != idx->children.end()) {
      for (int child_id : cit->second) {
        stack.push_back(child_id);
      }
    }
  }

  return false;
}

//' Compute inside-function IDs using single-pass algorithm
//'
//' Pre-computes which nodes are inside function bodies by collecting
//' all descendants of FUNCTION/OP-LAMBDA nodes. Returns unique IDs.
//'
//' @param func_ids Integer vector of FUNCTION/OP-LAMBDA node IDs
//' @param index_ptr External pointer to TreeIndex
//' @return Integer vector of all node IDs inside function bodies
//' @keywords internal
// [[Rcpp::export]]
IntegerVector compute_inside_function_ids_cpp(IntegerVector func_ids, SEXP index_ptr) {
  TreeIndexPtr idx(index_ptr);

  // Use set to automatically handle uniqueness
  std::unordered_set<int> inside_set;

  // For each function node, collect all descendants
  for (int i = 0; i < func_ids.size(); i++) {
    int func_id = func_ids[i];

    std::vector<int> stack;

    auto it = idx->children.find(func_id);
    if (it != idx->children.end()) {
      for (int child_id : it->second) {
        stack.push_back(child_id);
      }
    }

    while (!stack.empty()) {
      int current = stack.back();
      stack.pop_back();

      inside_set.insert(current);

      auto cit = idx->children.find(current);
      if (cit != idx->children.end()) {
        for (int child_id : cit->second) {
          stack.push_back(child_id);
        }
      }
    }
  }

  // Convert set to vector
  std::vector<int> result(inside_set.begin(), inside_set.end());
  return wrap(result);
}

//' Check if C++ tree index is available
//'
//' Helper function to check if an object is a C++ tree index pointer.
//'
//' @param x Object to check
//' @return TRUE if x is a TreeIndexPtr, FALSE otherwise
//' @keywords internal
// [[Rcpp::export]]
bool is_tree_index_ptr(SEXP x) {
  return TYPEOF(x) == EXTPTRSXP && Rf_inherits(x, "TreeIndexPtr");
}

//' Get number of nodes in tree index
//'
//' @param index_ptr External pointer to TreeIndex
//' @return Number of nodes
//' @keywords internal
// [[Rcpp::export]]
int tree_index_size(SEXP index_ptr) {
  TreeIndexPtr idx(index_ptr);
  return idx->n;
}
