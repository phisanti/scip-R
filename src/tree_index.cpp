// tree_index.cpp — CSR-free TreeIndex build + traversal over R parse data.
// Exports build_tree_index_cpp plus O(1) parent/child/row/descendant lookups
// and build_scope_index_cpp, all backed by the TreeIndex struct in tree_index.h.

#include "tree_index.h"
#include <algorithm>
#include <string>
#include <unordered_set>

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

//' Build scope index using position-based containment (C++ implementation)
//'
//' For each node, determines the innermost enclosing function based on
//' source position containment. Processes functions from largest to smallest
//' so smaller (inner) functions overwrite larger (outer) ones.
//'
//' @param id Integer vector of all node IDs
//' @param line1 Integer vector of start lines
//' @param col1 Integer vector of start columns
//' @param line2 Integer vector of end lines
//' @param col2 Integer vector of end columns
//' @param func_ids Integer vector of function node IDs
//' @param func_line1 Integer vector of function start lines
//' @param func_col1 Integer vector of function start columns
//' @param func_line2 Integer vector of function end lines
//' @param func_col2 Integer vector of function end columns
//' @return Integer vector of enclosing function IDs (NA for top-level nodes)
//' @keywords internal
// [[Rcpp::export]]
IntegerVector build_scope_index_cpp(
    IntegerVector id,
    IntegerVector line1, IntegerVector col1,
    IntegerVector line2, IntegerVector col2,
    IntegerVector func_ids,
    IntegerVector func_line1, IntegerVector func_col1,
    IntegerVector func_line2, IntegerVector func_col2
) {
  int n = id.size();
  int nf = func_ids.size();

  // Initialize all nodes as having no enclosing function
  IntegerVector enclosing(n, NA_INTEGER);

  if (nf == 0) {
    return enclosing;
  }

  // Sort functions by size descending (largest/outermost first)
  // Size approximation: (line_span * 10000) + col_span
  std::vector<std::pair<int, int>> func_order(nf);
  for (int i = 0; i < nf; i++) {
    int size = (func_line2[i] - func_line1[i]) * 10000 +
               (func_col2[i] - func_col1[i]);
    func_order[i] = {-size, i};  // Negative for descending sort
  }
  std::sort(func_order.begin(), func_order.end());

  // Process from largest to smallest
  // Later (smaller) functions overwrite, giving innermost scope
  for (const auto& fo : func_order) {
    int fi = fo.second;
    int fid = func_ids[fi];
    int fl1 = func_line1[fi], fc1 = func_col1[fi];
    int fl2 = func_line2[fi], fc2 = func_col2[fi];

    // Check each node for containment within this function
    for (int i = 0; i < n; i++) {
      // Node start must be >= function start
      bool start_ok = (line1[i] > fl1) ||
                      (line1[i] == fl1 && col1[i] >= fc1);
      // Node end must be <= function end
      bool end_ok = (line2[i] < fl2) ||
                    (line2[i] == fl2 && col2[i] <= fc2);

      if (start_ok && end_ok) {
        enclosing[i] = fid;  // Overwrite with current (smaller) function
      }
    }
  }

  return enclosing;
}
