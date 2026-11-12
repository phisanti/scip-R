// symbol_resolve.cpp — Phase 4B C++ symbol resolution loop.
// Classifies SYMBOL/SYMBOL_FUNCTION_CALL/SYMBOL_FORMALS tokens into defs and
// occurrences via TreeIndex (tree_index.h) and SCIP IDs (symbol_id.h).

#include "tree_index.h"
#include "symbol_id.h"
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

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
//   - Before (R): ~200s for an 89-file package
//   - After (C++): ~97s for the same package
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
//   - Reinstall the package after editing this file; tests exercise both the
//     C++ and R backends (with_resolver_backend() in the test helper)
//
// SEE ALSO:
//   - R/symbols.R:resolve_symbols() - R implementation and fallback
//   - R/symbol-table.R:BASE_R_FUNCTIONS - list of base R functions
//   - R/symbol-table.R:build_*_symbol_id() - symbol ID format specs
// ============================================================================

namespace {  // Anonymous namespace for internal helpers (not exported to R)

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
    const std::string& r_version,
    const std::string& enclosing_name,
    const std::string& qualified_pkg,
    const std::string& lexical_id
) {
  // Explicit pkg::func() — highest priority, emit external symbol ID directly.
  // Check definitions_env first — if qualified pkg matches our own package,
  // use the definition's symbol_id (correct version).
  if (!qualified_pkg.empty()) {
    if (definitions_env.exists(name)) {
      List def = definitions_env[name];
      if (def.containsElementNamed("symbol_id")) {
        std::string def_id = as<std::string>(def["symbol_id"]);
        if (def_id.find(qualified_pkg) != std::string::npos) {
          return def_id;
        }
      }
    }
    return build_global_symbol_id(name, qualified_pkg, "0.0.0", kind);
  }

  // BUG FIX: use the shared lexical plan before package/base
  // lookup, after explicit namespace qualification. Do not collapse a local
  // function definition or its captured calls into a package definition.
  // Anti-regression tests: tests/testthat/test-symbols-lexical-binding.R
  //   - local function bindings shadow package and base names in both backends
  //   - same named nested scopes keep distinct function bindings and captured calls
  //   - lexical calls do not capture cross file package callers or unshadowed closures
  if (!lexical_id.empty()) return lexical_id;

  if (is_local_scope && is_definition) {
    return build_local_symbol_id(name, relative_path, enclosing_name);
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
  // Include scope in dedup key to avoid collisions between same-named vars
  // in different functions (e.g., x inside func_a vs x inside func_b)
  std::string local_key;
  if (!enclosing_name.empty()) {
    local_key = relative_path + "#" + enclosing_name + "#" + name;
  } else {
    local_key = relative_path + "#" + name;
  }
  auto it = local_registry.find(local_key);
  if (it != local_registry.end()) {
    return it->second;
  }

  std::string symbol_id = build_local_symbol_id(name, relative_path, enclosing_name);
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
//' @param enclosing_function IntegerVector of enclosing function node IDs for each row
//'   (pre-computed by build_scope_index() for scope encoding in local symbol IDs)
//' @param lexical_symbols CharacterVector of nearest lexical binding IDs per row
//'   (empty entries retain package/base lookup; qualified calls take precedence)
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
    IntegerVector enclosing_function,
    CharacterVector lexical_symbols,
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
  // STEP 2b: Build function_node_id -> function_name map
  // -------------------------------------------------------------------------
  // For each FUNCTION or OP-LAMBDA token, find the assigned name on the same
  // line by looking for a SYMBOL token before the assignment operator.
  // This enables scope encoding in local symbol IDs (2-hash IDs for locals
  // inside named functions vs 1-hash IDs for top-level assignments).

  std::unordered_map<int, std::string> function_names;
  for (int i = 0; i < n; i++) {
    std::string tok = as<std::string>(token[i]);
    if (tok != "FUNCTION" && tok != "OP-LAMBDA") continue;

    int func_node_id = id[i];
    int func_line = line1[i];
    int func_col = col1[i];

    // Look for assignment on same line
    auto ait = assignment_by_line.find(func_line);
    if (ait == assignment_by_line.end()) continue;

    // Find the leftmost assignment column on this line
    int min_assign_col = ait->second[0];
    for (int ac : ait->second) {
      if (ac < min_assign_col) min_assign_col = ac;
    }

    // Skip if there's a SYMBOL_FUNCTION_CALL between the assignment and FUNCTION.
    // This filters out cases like: result <- lapply(1:10, function(x) ...)
    // where the FUNCTION is nested inside a call, not the direct assignment RHS.
    bool has_call_between = false;
    for (int j = 0; j < n; j++) {
      if (line1[j] == func_line &&
          as<std::string>(token[j]) == "SYMBOL_FUNCTION_CALL" &&
          col1[j] > min_assign_col &&
          col1[j] < func_col) {
        has_call_between = true;
        break;
      }
    }
    if (has_call_between) continue;

    // Find SYMBOL on same line, before assignment
    std::string best_name;
    int best_col = -1;
    for (int j = 0; j < n; j++) {
      if (line1[j] == func_line &&
          as<std::string>(token[j]) == "SYMBOL" &&
          col1[j] < min_assign_col) {
        // Take the closest SYMBOL to the assignment (rightmost before it)
        if (col1[j] > best_col) {
          best_col = col1[j];
          best_name = as<std::string>(text[j]);
        }
      }
    }

    if (!best_name.empty()) {
      function_names[func_node_id] = best_name;
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

    // --- Look up enclosing function name for scope encoding ---
    // For local-scope symbols, find the name of the enclosing function
    // so the symbol ID encodes the scope (path#func#name vs path#name).
    std::string enclosing_name;
    if (is_local_scope &&
        row_idx >= 0 && row_idx < enclosing_function.size() &&
        enclosing_function[row_idx] != NA_INTEGER) {
      int enc_func_id = enclosing_function[row_idx];
      auto fit = function_names.find(enc_func_id);
      if (fit != function_names.end()) {
        enclosing_name = fit->second;
      }
    }

    // --- Detect pkg::func() qualified calls — extract package from siblings ---
    std::string qualified_pkg;
    if (tok == "SYMBOL_FUNCTION_CALL" || tok == "SYMBOL") {
      auto pit = idx->parent.find(node_id);
      if (pit != idx->parent.end()) {
        int parent_id = pit->second;
        auto cit = idx->children.find(parent_id);
        if (cit != idx->children.end()) {
          bool has_ns_get = false;
          std::string pkg_text;
          for (int sib_id : cit->second) {
            auto rit = idx->row.find(sib_id);
            if (rit != idx->row.end()) {
              std::string sib_tok = as<std::string>(token[rit->second]);
              if (sib_tok == "NS_GET" || sib_tok == "NS_GET_INT") {
                has_ns_get = true;
              } else if (sib_tok == "SYMBOL_PACKAGE") {
                pkg_text = as<std::string>(text[rit->second]);
              }
            }
          }
          if (has_ns_get && !pkg_text.empty()) {
            qualified_pkg = pkg_text;
          }
        }
      }
    }

    // --- Resolve symbol ID ---
    std::string symbol_id = resolve_symbol(
      name, kind, is_definition, is_local_scope,
      (tok == "SYMBOL_FUNCTION_CALL"),
      relative_path, definitions_env, base_r_set,
      local_registry, package_name, r_version,
      enclosing_name, qualified_pkg, as<std::string>(lexical_symbols[row_idx])
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
