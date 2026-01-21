// tree_ops.cpp - C++ implementations for tree traversal operations
// These functions provide O(n) tree operations with pre-allocated vectors,
// replacing the O(n^2) R implementations that used vector growth patterns.

#include <Rcpp.h>
#include <algorithm>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

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

// ============================================================================
// PHASE 4B: C++ SYMBOL RESOLUTION
// ============================================================================
//
// PURPOSE:
//   This section implements the core symbol resolution loop in C++ for
//   performance. It replaces the R implementation in R/symbols.R:resolve_symbols()
//   which was the dominant bottleneck (>90% of Phase 3 execution time).
//
// PERFORMANCE:
//   - Before (R): ~200s for 89-file project (playbase)
//   - After (C++): ~97s for same project
//   - Speedup: ~2x overall (the loop itself is ~50x faster, but other R
//     code still runs for S3/R6/S4/Shiny detection)
//
// ALGORITHM OVERVIEW:
//   1. Build lookup indices for O(1) queries:
//      - assignment_by_line: which lines have assignment operators
//      - func_lines: which lines have FUNCTION/OP-LAMBDA tokens
//      - base_r_set: set of base R function names
//
//   2. For each symbol token (SYMBOL, SYMBOL_FUNCTION_CALL, SYMBOL_FORMALS):
//      a. Determine if it's a definition (assignment target, loop var, param)
//      b. Determine if it's in local scope (inside a function body)
//      c. Classify the symbol kind (function, variable, parameter, call)
//      d. Resolve the symbol ID (package global, base R, or local)
//
//   3. Output symbols (definitions) and occurrences (all uses)
//
// MAINTENANCE NOTES:
//   - If symbol resolution logic changes in R, update here too
//   - The R fallback in symbols.R should remain in sync as a reference
//   - Test any changes with: R CMD INSTALL . && run indexer on playbase
//   - Compare output with scripts/compare_scip_indexes.R
//
// SEE ALSO:
//   - R/symbols.R:resolve_symbols() - R implementation and fallback
//   - R/symbol-table.R:BASE_R_FUNCTIONS - list of base R functions
//   - R/symbol-table.R:build_*_symbol_id() - symbol ID format specs
// ============================================================================

namespace {  // Anonymous namespace for internal helpers (not exported to R)

// ----------------------------------------------------------------------------
// SYMBOL ID CONSTRUCTION
// ----------------------------------------------------------------------------

/// Sanitize a string for use in SCIP symbol IDs.
/// Replaces any character that isn't alphanumeric, underscore, or slash with _.
/// This ensures symbol IDs are valid identifiers across different tools.
///
/// @param s Input string (typically a file path or symbol name)
/// @return Sanitized string safe for symbol ID use
std::string sanitize_for_symbol_id(const std::string& s) {
  std::string result;
  result.reserve(s.size());
  for (char c : s) {
    // Allow: A-Z, a-z, 0-9, underscore, forward slash (for paths)
    bool is_valid = (c >= 'A' && c <= 'Z') ||
                    (c >= 'a' && c <= 'z') ||
                    (c >= '0' && c <= '9') ||
                    c == '_' || c == '/';
    result.push_back(is_valid ? c : '_');
  }
  return result;
}

/// Build a local (file-scoped) symbol ID.
/// Local symbols are not resolvable from other files.
/// Format: "local <sanitized_path>#<sanitized_name>"
///
/// @param name Symbol name (e.g., "x", "my_var")
/// @param path Relative file path (e.g., "R/utils.R")
/// @return Symbol ID string
std::string build_local_symbol_id(const std::string& name, const std::string& path) {
  return "local " + sanitize_for_symbol_id(path) + "#" + sanitize_for_symbol_id(name);
}

/// Build a global (package-level) or external symbol ID.
/// Format: "scip-r r <package> <version> <name><suffix>"
///
/// Suffix conventions (matching SCIP standard):
///   - Functions: "()" suffix, e.g., "foo()."
///   - Variables: "." suffix, e.g., "foo."
///   - Classes: "#" suffix, e.g., "Foo#"
///
/// @param name Symbol name
/// @param pkg Package name (e.g., "base", "dplyr")
/// @param version Package version (e.g., "4.3.2")
/// @param kind Symbol kind: "function", "call", "variable", "r6_class", etc.
/// @return Symbol ID string
std::string build_global_symbol_id(
    const std::string& name,
    const std::string& pkg,
    const std::string& version,
    const std::string& kind
) {
  // Determine suffix based on kind
  std::string suffix;
  if (kind == "function" || kind == "call" ||
      kind == "s3_generic" || kind == "s3_method") {
    suffix = name + "().";
  } else if (kind == "r6_class" || kind == "s4_class") {
    suffix = name + "#";
  } else {
    // Variables and unknown kinds get simple suffix
    suffix = name + ".";
  }

  return "scip-r r " + pkg + " " + version + " " + suffix;
}

// ----------------------------------------------------------------------------
// AST ANALYSIS HELPERS
// ----------------------------------------------------------------------------

/// Check if a node's parent is a "forcond" (for-loop condition).
/// This identifies loop variables like `i` in `for (i in 1:10)`.
///
/// @param node_id The node ID to check
/// @param idx Pointer to the TreeIndex structure
/// @param tokens Character vector of token types (parallel to parse_data)
/// @return true if the node's parent is a forcond token
bool is_loop_variable(int node_id, TreeIndex* idx, CharacterVector& tokens) {
  // Find parent ID
  auto parent_it = idx->parent.find(node_id);
  if (parent_it == idx->parent.end()) return false;

  int parent_id = parent_it->second;
  if (parent_id == NA_INTEGER || parent_id == 0) return false;

  // Find parent's row to get its token type
  auto row_it = idx->row.find(parent_id);
  if (row_it == idx->row.end()) return false;

  std::string parent_token = as<std::string>(tokens[row_it->second]);
  return parent_token == "forcond";
}

/// Check if a parse data row is inside a function body.
/// Uses the pre-computed inside_function vector for O(1) lookup.
///
/// @param row_idx Row index in parse_data (0-based)
/// @param inside_function Pre-computed logical vector from build_tree_index()
/// @return true if the row is inside a function body
inline bool is_inside_function(int row_idx, LogicalVector& inside_function) {
  if (row_idx < 0 || row_idx >= inside_function.size()) return false;
  return inside_function[row_idx] == TRUE;
}

/// Determine the kind of a symbol based on context.
/// This replicates the logic from R's determine_symbol_kind_indexed().
///
/// @return One of: "parameter", "variable", "function", "call"
std::string determine_symbol_kind(
    int row_idx,                    // Row index in parse_data
    const std::string& token_type,  // Token type (SYMBOL, SYMBOL_FUNCTION_CALL, etc.)
    int node_id,                    // Node ID for tree lookups
    TreeIndex* idx,                 // Tree index for parent lookups
    CharacterVector& tokens,        // All token types
    IntegerVector& line1,           // Start line numbers
    IntegerVector& col1,            // Start column numbers
    const std::unordered_map<int, std::vector<int>>& assignment_by_line,
    const std::unordered_set<int>& func_lines
) {
  // 1. Function parameters (SYMBOL_FORMALS) are always "parameter"
  if (token_type == "SYMBOL_FORMALS") {
    return "parameter";
  }

  // 2. Loop variables (parent is forcond) are "variable"
  if (is_loop_variable(node_id, idx, tokens)) {
    return "variable";
  }

  // 3. Check for function definition pattern:
  //    - Symbol appears on a line with FUNCTION/OP-LAMBDA
  //    - Symbol appears before an assignment operator
  //    Example: `foo <- function(x) { ... }`
  //             ^^^-- this symbol is kind "function"
  int line = line1[row_idx];
  int col = col1[row_idx];

  if (func_lines.count(line) > 0) {  // Line has a FUNCTION token
    auto ait = assignment_by_line.find(line);
    if (ait != assignment_by_line.end()) {
      for (int assign_col : ait->second) {
        if (assign_col > col) {
          // Symbol is before an assignment, and line has FUNCTION
          return "function";
        }
      }
    }
  }

  // 4. Function calls (SYMBOL_FUNCTION_CALL) are "call"
  if (token_type == "SYMBOL_FUNCTION_CALL") {
    return "call";
  }

  // 5. Default: regular variable
  return "variable";
}

/// Resolve a symbol to its unique ID.
/// Resolution priority:
///   1. Package-level definition (from symbol_table)
///   2. Base R function (for function calls)
///   3. Local symbol (file-scoped)
///
/// @param name Symbol name
/// @param kind Symbol kind (from determine_symbol_kind)
/// @param is_definition Whether this occurrence is a definition
/// @param is_local_scope Whether the symbol is inside a function body
/// @param is_call Whether this is a function call (SYMBOL_FUNCTION_CALL)
/// @param relative_path Current file's relative path
/// @param definitions_env R environment from symbol_table (name -> definition info)
/// @param base_r_set Set of base R function names
/// @param local_registry Cache for local symbol IDs (prevents duplicates)
/// @param package_name Package name for external symbol IDs
/// @param r_version R version string for base R references
/// @return Symbol ID string
std::string resolve_symbol(
    const std::string& name,
    const std::string& kind,
    bool is_definition,
    bool is_local_scope,
    bool is_call,
    const std::string& relative_path,
    Environment& definitions_env,
    const std::unordered_set<std::string>& base_r_set,
    std::unordered_map<std::string, std::string>& local_registry,
    const std::string& package_name,
    const std::string& r_version
) {
  // Local definitions (inside functions) that aren't function definitions
  // should always get local symbol IDs
  if (is_local_scope && is_definition && kind != "function") {
    return build_local_symbol_id(name, relative_path);
  }

  // Try to find in package symbol table (cross-file resolution)
  if (definitions_env.exists(name)) {
    List def = definitions_env[name];
    if (def.containsElementNamed("symbol_id")) {
      return as<std::string>(def["symbol_id"]);
    }
  }

  // For function calls, check if it's a base R function
  if (is_call && base_r_set.count(name) > 0) {
    return build_global_symbol_id(name, "base", r_version, "function");
  }

  // Fall back to local symbol (with deduplication via registry)
  std::string local_key = relative_path + "#" + name;
  auto it = local_registry.find(local_key);
  if (it != local_registry.end()) {
    return it->second;
  }

  std::string symbol_id = build_local_symbol_id(name, relative_path);
  local_registry[local_key] = symbol_id;
  return symbol_id;
}

}  // end anonymous namespace

// ============================================================================
// EXPORTED FUNCTION: resolve_symbols_cpp
// ============================================================================

//' Resolve symbols in parse data (C++ implementation)
//'
//' This is the C++ implementation of the core symbol resolution loop,
//' providing significant performance improvement over the R implementation.
//' It processes all SYMBOL, SYMBOL_FUNCTION_CALL, and SYMBOL_FORMALS tokens
//' to produce symbol definitions and occurrence records.
//'
//' @param parse_data DataFrame from R's getParseData() with columns:
//'   id, parent, token, text, line1, col1, line2, col2
//' @param tree_index_ptr External pointer (XPtr) to TreeIndex from build_tree_index_cpp()
//' @param inside_function LogicalVector indicating which rows are inside function bodies
//'   (pre-computed by build_tree_index() for O(1) lookup)
//' @param definitions_env R environment from symbol_table$definitions_env mapping
//'   symbol name -> list(symbol_id=..., file=..., kind=..., ...)
//' @param base_r_funcs CharacterVector of base R function names (from BASE_R_FUNCTIONS)
//' @param relative_path String: current file's path relative to project root
//' @param package_name String: package name for symbol IDs
//' @param r_version String: R version (e.g., "4.3.2") for base R symbol IDs
//'
//' @return List with two DataFrames:
//'   \describe{
//'     \item{symbols}{DataFrame with columns: symbol, display_name, kind
//'       (one row per unique definition in this file)}
//'     \item{occurrences}{DataFrame with columns: symbol, line1, col1, line2, col2, role
//'       (one row per symbol occurrence, role is "definition" or "reference")}
//'   }
//'
//' @details
//' The function mirrors the logic in R/symbols.R:resolve_symbols() but runs
//' entirely in C++ for performance. If this function fails or is unavailable,
//' the R implementation serves as fallback.
//'
//' Memory estimates for pre-allocation:
//'   - Symbols: ~20% of parse_data rows are symbol tokens
//'   - Definitions: ~10% of symbol tokens are definitions
//'
//' @seealso
//' \code{\link{resolve_symbols}} for the R implementation and fallback
//'
//' @keywords internal
// [[Rcpp::export]]
List resolve_symbols_cpp(
    DataFrame parse_data,
    SEXP tree_index_ptr,
    LogicalVector inside_function,
    Environment definitions_env,
    CharacterVector base_r_funcs,
    std::string relative_path,
    std::string package_name,
    std::string r_version
) {
  TreeIndexPtr idx(tree_index_ptr);

  // -------------------------------------------------------------------------
  // STEP 1: Extract parse_data columns for efficient access
  // -------------------------------------------------------------------------
  IntegerVector id = parse_data["id"];
  IntegerVector parent = parse_data["parent"];
  IntegerVector line1 = parse_data["line1"];
  IntegerVector col1 = parse_data["col1"];
  IntegerVector line2 = parse_data["line2"];
  IntegerVector col2 = parse_data["col2"];
  CharacterVector token = parse_data["token"];
  CharacterVector text = parse_data["text"];

  int n = id.size();

  // -------------------------------------------------------------------------
  // STEP 2: Build lookup indices for O(1) queries
  // -------------------------------------------------------------------------

  // Set of base R function names for O(1) membership test
  std::unordered_set<std::string> base_r_set;
  base_r_set.reserve(base_r_funcs.size());
  for (int i = 0; i < base_r_funcs.size(); i++) {
    base_r_set.insert(as<std::string>(base_r_funcs[i]));
  }

  // Map: line number -> column positions of assignment operators on that line
  // Used to detect "x <- value" patterns (x is before assignment)
  std::unordered_map<int, std::vector<int>> assignment_by_line;
  for (int i = 0; i < n; i++) {
    std::string tok = as<std::string>(token[i]);
    if (tok == "LEFT_ASSIGN" || tok == "EQ_ASSIGN" || tok == "RIGHT_ASSIGN") {
      assignment_by_line[line1[i]].push_back(col1[i]);
    }
  }

  // Set of line numbers that contain FUNCTION or OP-LAMBDA tokens
  // Used as quick filter before checking for function definitions
  std::unordered_set<int> func_lines;
  for (int i = 0; i < n; i++) {
    std::string tok = as<std::string>(token[i]);
    if (tok == "FUNCTION" || tok == "OP-LAMBDA") {
      func_lines.insert(line1[i]);
    }
  }

  // -------------------------------------------------------------------------
  // STEP 3: Identify symbol rows to process
  // -------------------------------------------------------------------------
  // Only process SYMBOL, SYMBOL_FUNCTION_CALL, and SYMBOL_FORMALS tokens.
  // Typically ~20% of all parse_data rows are symbol tokens.
  std::vector<int> symbol_indices;
  symbol_indices.reserve(n / 5);
  for (int i = 0; i < n; i++) {
    std::string tok = as<std::string>(token[i]);
    if (tok == "SYMBOL" || tok == "SYMBOL_FUNCTION_CALL" || tok == "SYMBOL_FORMALS") {
      symbol_indices.push_back(i);
    }
  }

  // -------------------------------------------------------------------------
  // STEP 4: Pre-allocate result vectors
  // -------------------------------------------------------------------------
  // Symbols (definitions): typically ~10% of symbol tokens
  std::vector<std::string> sym_id, sym_name, sym_kind;
  size_t estimated_definitions = symbol_indices.size() / 10;
  sym_id.reserve(estimated_definitions);
  sym_name.reserve(estimated_definitions);
  sym_kind.reserve(estimated_definitions);

  // Occurrences: one per symbol token
  std::vector<std::string> occ_symbol, occ_role;
  std::vector<int> occ_line1, occ_col1, occ_line2, occ_col2;
  occ_symbol.reserve(symbol_indices.size());
  occ_role.reserve(symbol_indices.size());
  occ_line1.reserve(symbol_indices.size());
  occ_col1.reserve(symbol_indices.size());
  occ_line2.reserve(symbol_indices.size());
  occ_col2.reserve(symbol_indices.size());

  // Track seen symbol IDs to deduplicate definitions
  std::unordered_set<std::string> seen_definitions;

  // Cache for local symbol IDs (prevents creating duplicates for same name)
  std::unordered_map<std::string, std::string> local_registry;

  // -------------------------------------------------------------------------
  // STEP 5: Main resolution loop
  // -------------------------------------------------------------------------
  for (int row_idx : symbol_indices) {
    std::string tok = as<std::string>(token[row_idx]);
    std::string name = as<std::string>(text[row_idx]);
    int node_id = id[row_idx];

    // --- Determine if this is a definition ---
    bool is_definition = false;
    bool is_local_scope = false;

    if (tok == "SYMBOL_FORMALS") {
      // Function parameters are always definitions and always local
      is_definition = true;
      is_local_scope = true;
    } else {
      // Check if loop variable (for-loop index)
      bool is_loop_var = is_loop_variable(node_id, idx.get(), token);

      // Check if on assignment line (to the left of <-)
      int line = line1[row_idx];
      int col = col1[row_idx];
      bool on_assignment_line = false;

      auto ait = assignment_by_line.find(line);
      if (ait != assignment_by_line.end()) {
        for (int assign_col : ait->second) {
          if (assign_col > col) {
            on_assignment_line = true;
            break;
          }
        }
      }

      is_definition = is_loop_var || (tok == "SYMBOL" && on_assignment_line);
      is_local_scope = is_loop_var || is_inside_function(row_idx, inside_function);
    }

    // --- Determine symbol kind ---
    std::string kind = determine_symbol_kind(
      row_idx, tok, node_id, idx.get(), token,
      line1, col1, assignment_by_line, func_lines
    );

    // --- Resolve symbol ID ---
    std::string symbol_id = resolve_symbol(
      name, kind, is_definition, is_local_scope,
      (tok == "SYMBOL_FUNCTION_CALL"),
      relative_path, definitions_env, base_r_set,
      local_registry, package_name, r_version
    );

    // --- Deduplicate definitions ---
    if (is_definition && seen_definitions.count(symbol_id) > 0) {
      is_definition = false;  // Already recorded, treat as reference
    }

    // --- Record outputs ---
    if (is_definition) {
      sym_id.push_back(symbol_id);
      sym_name.push_back(name);
      sym_kind.push_back(kind);
      seen_definitions.insert(symbol_id);
    }

    occ_symbol.push_back(symbol_id);
    occ_line1.push_back(line1[row_idx]);
    occ_col1.push_back(col1[row_idx]);
    occ_line2.push_back(line2[row_idx]);
    occ_col2.push_back(col2[row_idx]);
    occ_role.push_back(is_definition ? "definition" : "reference");
  }

  // -------------------------------------------------------------------------
  // STEP 6: Build and return result DataFrames
  // -------------------------------------------------------------------------
  DataFrame symbols_df = DataFrame::create(
    Named("symbol") = wrap(sym_id),
    Named("display_name") = wrap(sym_name),
    Named("kind") = wrap(sym_kind),
    Named("stringsAsFactors") = false
  );

  DataFrame occurrences_df = DataFrame::create(
    Named("symbol") = wrap(occ_symbol),
    Named("line1") = wrap(occ_line1),
    Named("col1") = wrap(occ_col1),
    Named("line2") = wrap(occ_line2),
    Named("col2") = wrap(occ_col2),
    Named("role") = wrap(occ_role),
    Named("stringsAsFactors") = false
  );

  return List::create(
    Named("symbols") = symbols_df,
    Named("occurrences") = occurrences_df
  );
}
