// symbol_id.cpp — SCIP symbol-ID construction (definitions).
// Implements path sanitizing, SCIP identifier escaping, and local/global
// symbol-ID builders declared in symbol_id.h (extern linkage for sharing).

#include "symbol_id.h"

// ----------------------------------------------------------------------------
// SYMBOL ID CONSTRUCTION
// ----------------------------------------------------------------------------

/// Sanitize a file path for use in SCIP symbol IDs.
/// Replaces any character that isn't alphanumeric, underscore, or slash with _.
/// Used only for the path component of local symbol IDs, NOT for symbol names.
///
/// @param s Input string (a file path)
/// @return Sanitized string safe for symbol ID path component
std::string sanitize_path_for_symbol_id(const std::string& s) {
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

/// Escape a symbol name for SCIP identifiers per the SCIP spec.
/// SCIP identifier characters: _ + - $ ASCII letters and digits.
/// Names containing anything else (notably '.' in R) must be backtick-quoted.
/// Internal backticks are escaped by doubling.
///
/// @param name Raw symbol name (e.g., "pgx.createPGX")
/// @return Escaped name (e.g., "`pgx.createPGX`"), or unchanged if no escaping needed
std::string escape_scip_identifier(const std::string& name) {
  bool needs_escape = false;
  for (char c : name) {
    bool is_ident_char = (c >= 'A' && c <= 'Z') ||
                         (c >= 'a' && c <= 'z') ||
                         (c >= '0' && c <= '9') ||
                         c == '_' || c == '+' || c == '-' || c == '$';
    if (!is_ident_char) {
      needs_escape = true;
      break;
    }
  }
  if (!needs_escape) return name;

  std::string result;
  result.reserve(name.size() + 4);
  result.push_back('`');
  for (char c : name) {
    if (c == '`') result.push_back('`'); // double backticks
    result.push_back(c);
  }
  result.push_back('`');
  return result;
}

/// Build a local (file-scoped) symbol ID.
/// Local symbols are not resolvable from other files.
/// Format without scope: "local <sanitized_path>#<escaped_name>"
/// Format with scope:    "local <sanitized_path>#<escaped_scope>#<escaped_name>"
///
/// When scope_id is provided (enclosing function name), it creates a 2-hash ID
/// that distinguishes local variables inside functions from top-level file
/// assignments. This is critical for dead code detection to avoid false positives
/// on function-local variables.
///
/// Uses SCIP backtick-quoting for names with special chars (e.g. dots in R)
/// to preserve the original name instead of stripping information.
///
/// @param name Symbol name (e.g., "x", "has.pgx")
/// @param path Relative file path (e.g., "R/utils.R")
/// @param scope_id Optional enclosing function name (e.g., "my_func", "pgx.create")
/// @return Symbol ID string
std::string build_local_symbol_id(
    const std::string& name,
    const std::string& path,
    const std::string& scope_id
) {
  std::string base = "local " + sanitize_path_for_symbol_id(path);

  if (!scope_id.empty()) {
    return base + "#" + escape_scip_identifier(scope_id) + "#" + escape_scip_identifier(name);
  }

  return base + "#" + escape_scip_identifier(name);
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
  // Escape names with non-identifier chars (e.g. dots in R) per SCIP spec
  std::string esc_name = escape_scip_identifier(name);

  // Determine suffix based on kind
  std::string suffix;
  if (kind == "function" || kind == "call" ||
      kind == "s3_generic" || kind == "s3_method") {
    suffix = esc_name + "().";
  } else if (kind == "r6_class" || kind == "s4_class") {
    suffix = esc_name + "#";
  } else {
    // Variables and unknown kinds get simple suffix
    suffix = esc_name + ".";
  }

  return "scip-r r " + pkg + " " + version + " " + suffix;
}
